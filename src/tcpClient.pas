unit tcpClient;

{
  TCP client for communication with hJOPserver.

  Specification of communication protocol is available at:
  https://github.com/kmzbrnoI/hJOPserver/wiki/panelServer.
}

interface

uses SysUtils, IdTCPClient, tcpThread, IdTCPConnection, IdGlobal, ExtCtrls,
     Classes, StrUtils, Generics.Collections, resusc, parseHelper, Windows,
     Forms, Graphics, IdStack;

const
  _DEFAULT_PORT = 5896;
  _PING_TIMER_PERIOD_MS = 20000;

  // all acceptable protocl versions (server -> client)
  protocol_version_accept : array[0..1] of string =
    (
      '1.0', '1.1'
    );

type
  TPanelConnectionStatus = (closed, opening, handshake, opened);

  TTCPClient = class
   private const
    _PROTOCOL_VERSION = '1.1';

   private
    rthread: TReadingThread;
    tcpClient: TIdTCPClient;
    fstatus : TPanelConnectionStatus;
    parsed: TStrings;
    data:string;
    control_disconnect:boolean;
    resusct : TResuscitation;
    pingTimer:TTimer;

     procedure OnTcpClientConnected(Sender: TObject);
     procedure OnTcpClientDisconnected(Sender: TObject);
     procedure DataReceived(const data: string);
     procedure Timeout();   // timeout from socket = broken pipe

     procedure Parse();

     procedure ConnetionResusced(Sender:TObject);
     procedure SendPing(Sedner:TObject);

   public

     constructor Create();
     destructor Destroy(); override;

     function Connect(host:string; port:Word):Integer;
     function Disconnect():Integer;

     procedure SendLn(str:string);

     procedure InitResusc(server: string; port: Word);

      property status:TPanelConnectionStatus read fstatus;
  end;//TPanelTCPClient

var
  client : TTCPClient;

implementation

uses globConfig, main, modelTime;

////////////////////////////////////////////////////////////////////////////////

constructor TTCPClient.Create();
begin
 inherited;

 Self.parsed := TStringList.Create;

 Self.pingTimer := TTimer.Create(nil);
 Self.pingTimer.Enabled := false;
 Self.pingTimer.Interval := _PING_TIMER_PERIOD_MS;
 Self.pingTimer.OnTimer := Self.SendPing;

 Self.tcpClient := TIdTCPClient.Create(nil);
 Self.tcpClient.OnConnected := Self.OnTcpClientConnected;
 Self.tcpClient.OnDisconnected := Self.OnTcpClientDisconnected;
 Self.tcpClient.ConnectTimeout := 1500;

 Self.fstatus := TPanelConnectionStatus.closed;

 Self.resusct := nil;
end;

destructor TTCPClient.Destroy();
begin
 try
   if (Self.tcpClient.Connected) then
     Self.tcpClient.Disconnect();
 except

 end;

 // Destroy resusc thread.
 if (Assigned(Self.resusct)) then
  begin
   try
     TerminateThread(Self.resusct.Handle, 0);
   finally
     if Assigned(Self.resusct) then
     begin
       Resusct.WaitFor;
       FreeAndNil(Self.resusct);
     end;
   end;
  end;

 try
   if (Assigned(Self.tcpClient)) then
     FreeAndNil(Self.tcpClient);

   if (Assigned(Self.parsed)) then
     FreeAndNil(Self.parsed);

   Self.pingTimer.Free();
 finally
   inherited;
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TTCPClient.Connect(host:string; port:Word):Integer;
begin
 try
   // without .Clear() .Connected() sometimes returns true when actually not connected
   if (Self.tcpClient.IOHandler <> nil) then
     Self.tcpClient.IOHandler.InputBuffer.Clear();
   if (Self.tcpClient.Connected) then Exit(1);
 except
   try
     Self.tcpClient.Disconnect(False);
   except
   end;
   if (Self.tcpClient.IOHandler <> nil) then
     Self.tcpClient.IOHandler.InputBuffer.Clear();
 end;

 Self.tcpClient.Host := host;
 Self.tcpClient.Port := port;

 Self.fstatus := TPanelConnectionStatus.opening;
 F_Main.UpdateStatus('Pøipojuji...', clBlue);

 try
   Self.tcpClient.Connect();
 except
   Self.fstatus := TPanelConnectionStatus.closed;
   raise;
 end;

 Self.tcpClient.IOHandler.DefStringEncoding := TIdEncoding.enUTF8;
 Self.control_disconnect := false;

 Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////

function TTCPClient.Disconnect():Integer;
begin
 try
   if (not Self.tcpClient.Connected) then Exit(1);
 except

 end;

 Self.control_disconnect := true;
 if Assigned(Self.rthread) then Self.rthread.Terminate;
 try
   Self.tcpClient.Disconnect();
 finally
   if Assigned(Self.rthread) then
   begin
     Self.rthread.WaitFor;
     FreeAndNil(Self.rthread);
   end;
 end;

 Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////
// IdTCPClient events

procedure TTCPClient.OnTcpClientConnected(Sender: TObject);
begin
 try
  Self.rthread := TReadingThread.Create((Sender as TIdTCPClient));
  Self.rthread.OnData := DataReceived;
  Self.rthread.OnTimeout := Timeout;
  Self.rthread.Resume;
 except
  (Sender as TIdTCPClient).Disconnect;
  raise;
 end;

 F_Main.UpdateStatus('Pøipojeno', clBlack);

 Self.fstatus := TPanelConnectionStatus.handshake;
 Self.pingTimer.Enabled := true;

 // send handshake
 Self.SendLn('-;HELLO;'+Self._PROTOCOL_VERSION+';');
end;//procedure

procedure TTCPClient.OnTcpClientDisconnected(Sender: TObject);
begin
 if Assigned(Self.rthread) then Self.rthread.Terminate;

 Self.fstatus := TPanelConnectionStatus.closed;
 Self.pingTimer.Enabled := false;

 mt.Reset();
 F_Main.UpdateStatus('Odpojeno', clRed);

 // resuscitation
 if (not Self.control_disconnect) then
   Self.InitResusc(config.data.server.host, config.data.server.port);
end;

////////////////////////////////////////////////////////////////////////////////

// parsing prijatych dat
procedure TTCPClient.DataReceived(const data: string);
begin
 Self.parsed.Clear();
 ExtractStringsEx([';'], [#13, #10], data, Self.parsed);

 Self.data := data;

 try
   Self.Parse();
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTCPClient.Timeout();
begin
 Self.OnTcpClientDisconnected(Self);
 // Errors.writeerror('Spojení se serverem pøerušeno', 'KLIENT', '-'); TODO
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTCPClient.Parse();
var i:Integer;
    found:boolean;
begin
 // parse handhake
 if (Self.parsed[1] = 'HELLO') then
  begin
   // kontrola verze protokolu
   found := false;
   for i := 0 to Length(protocol_version_accept)-1 do
    begin
     if (Self.parsed[2] = protocol_version_accept[i]) then
      begin
       found := true;
       break;
      end;
    end;//for i

   if (not found) then
     Application.MessageBox(PChar('Verze protokolu, kterou požívá server ('+Self.parsed[2]+') není podporována'),
       'Upozornìní', MB_OK OR MB_ICONWARNING);

   Self.fstatus := TPanelConnectionStatus.opened;
  end

 else if (parsed[1] = 'MOD-CAS') then
   mt.ParseData(parsed);

end;

////////////////////////////////////////////////////////////////////////////////

procedure TTCPClient.SendLn(str:string);
begin
 try
   if (not Self.tcpClient.Connected) then Exit;
 except

 end;

 try
   Self.tcpClient.Socket.WriteLn(str);
 except
   if (Self.fstatus = opened) then
    Self.OnTcpClientDisconnected(Self);
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTCPClient.ConnetionResusced(Sender:TObject);
begin
 try
   Self.Connect(config.data.server.host, config.data.server.port);
 except
   on E:EIdSocketError do
     Self.InitResusc(config.data.server.host, config.data.server.port);
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTCPClient.SendPing(Sedner:TObject);
begin
 try
   if (Self.tcpClient.Connected) then
     Self.SendLn('-;PING');
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TTCPClient.InitResusc(server: string; port: Word);
begin
 if (Self.resusct = nil) then
   Self.resusct := TResuscitation.Create(true, Self.ConnetionResusced);
 Self.resusct.server_ip   := server;
 Self.resusct.server_port := port;
 Self.resusct.Resume();
end;

////////////////////////////////////////////////////////////////////////////////

initialization
 client := TTCPClient.Create;

finalization
 FreeAndNil(cLient);

end.
