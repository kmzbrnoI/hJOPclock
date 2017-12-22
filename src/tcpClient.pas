unit tcpClient;

{
  TCP client for communication with hJOPserver

  Specification of communication protocol is available at:
  https://github.com/kmzbrnoI/hJOPserver/wiki/panelServer.
}

interface

uses SysUtils, IdTCPClient, tcpThread, IdTCPConnection, IdGlobal, ExtCtrls,
     Classes, StrUtils, Generics.Collections, resusc, parseHelper, Windows;

const
  _DEFAULT_PORT = 5896;
  _PING_TIMER_PERIOD_MS = 20000;

  // all acceptable protocl versions (server -> client)
  protocol_version_accept : array[0..0] of string =
    (
      '1.0'
    );

type
  TPanelConnectionStatus = (closed, opening, handshake, opened);

  TPanelTCPClient = class
   private const
    _PROTOCOL_VERSION = '1.1';

   private
    rthread: TReadingThread;
    tcpClient: TIdTCPClient;
    fstatus : TPanelConnectionStatus;
    parsed: TStrings;
    data:string;
    control_disconnect:boolean;
    recusc_destroy:boolean;
    pingTimer:TTimer;

     procedure OnTcpClientConnected(Sender: TObject);
     procedure OnTcpClientDisconnected(Sender: TObject);
     procedure DataReceived(const data: string);
     procedure Timeout();   // timeout from socket = broken pipe

     procedure ParseGlobal();
     procedure ParseOR();
     procedure ParseORChange();

     procedure ConnetionResusced(Sender:TObject);
     procedure SendPing(Sedner:TObject);

   public

    resusct : TResuscitation;
    openned_by_ipc: boolean;

     constructor Create();
     destructor Destroy(); override;

     function Connect(host:string; port:Word):Integer;
     function Disconnect():Integer;

     procedure SendLn(str:string);

     procedure Update();

      property status:TPanelConnectionStatus read fstatus;
  end;//TPanelTCPClient

var
  PanelTCPClient : TPanelTCPClient;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor TPanelTCPClient.Create();
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
 self.recusc_destroy := false;
end;//ctor

destructor TPanelTCPClient.Destroy();
begin
 try
   if (Self.tcpClient.Connected) then
     Self.tcpClient.Disconnect();
 except

 end;

 // Znicime resuscitacni vlakno (vlakno obnovujici spojeni).
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
end;//dtor

////////////////////////////////////////////////////////////////////////////////

function TPanelTCPClient.Connect(host:string; port:Word):Integer;
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

 Self.openned_by_ipc := false;

 Self.fstatus := TPanelConnectionStatus.opening;
 // F_Main.T_MainTimer(nil); TODO

 try
   Self.tcpClient.Connect();
 except
   Self.fstatus := TPanelConnectionStatus.closed;
   raise;
 end;

 Self.tcpClient.IOHandler.DefStringEncoding := TIdEncoding.enUTF8;
 Self.control_disconnect := false;

 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////

function TPanelTCPClient.Disconnect():Integer;
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
end;//function

////////////////////////////////////////////////////////////////////////////////
// eventy z IdTCPClient

procedure TPanelTCPClient.OnTcpClientConnected(Sender: TObject);
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

 {
 F_Main.A_Connect.Enabled    := false;
 F_Main.A_ReAuth.Enabled     := true;
 F_Main.A_Disconnect.Enabled := true; TODO
 }

 Self.fstatus := TPanelConnectionStatus.handshake;
 Self.pingTimer.Enabled := true;

 // send handshake
 Self.SendLn('-;HELLO;'+Self._PROTOCOL_VERSION+';');
end;//procedure

procedure TPanelTCPClient.OnTcpClientDisconnected(Sender: TObject);
begin
 if Assigned(Self.rthread) then Self.rthread.Terminate;

 Self.fstatus := TPanelConnectionStatus.closed;
 Self.pingTimer.Enabled := false;

 { TODO main for controls }

 // resuscitace
 // Resuscitaci povolime, pokud jsme od serveru byli odpojeni jinak, nez vlastni vuli.
 if ((not Self.control_disconnect) and (GlobConfig.data.resuscitation)) then
  begin
   Resusct := TResuscitation.Create(true, Self.ConnetionResusced);
   Resusct.server_ip   := GlobConfig.data.server.host;
   Resusct.server_port := GlobConfig.data.server.port;
   Resusct.Resume();
  end;

 if (F_Main.close_app) then
   F_Main.Close();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// parsing prijatych dat
procedure TPanelTCPClient.DataReceived(const data: string);
begin
 Self.parsed.Clear();
 ExtractStringsEx([';'], [#13, #10], data, Self.parsed);

 Self.data := data;

 try
   // zakladni rozdeleni parsovani - na data, ktera jsou obecna a na data pro konkretni oblast rizeni
   if (Self.parsed[0] = '-') then
    Self.ParseGlobal()
   else
    Self.ParseOR();
 except

 end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TPanelTCPClient.Timeout();
begin
 Self.OnTcpClientDisconnected(Self);
 // Errors.writeerror('Spojení se serverem pøerušeno', 'KLIENT', '-'); TODO
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TPanelTCPClient.ParseGlobal();
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
   Self.SendLn('-;OR-LIST;');
   PanelTCPClient.SendLn('-;F-VYZN-GET;');
   PanelTCPClient.SendLn('-;MAUS;'+IntToStr(Integer(BridgeClient.authStatus = tuLiAuthStatus.yes)));
  end

 else if (parsed[1] = 'MOD-CAS') then
  ModCas.ParseData(parsed)

end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TPanelTCPClient.SendLn(str:string);
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
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TPanelTCPClient.ConnetionResusced(Sender:TObject);
begin
 Self.Connect(GlobConfig.data.server.host, GlobConfig.data.server.port);
 while (Errors.Count > 0) do Errors.removeerror();
 Self.recusc_destroy := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPanelTCPClient.Update();
begin
 if (Self.recusc_destroy) then
  begin
   Self.recusc_destroy := false;
   try
     Self.resusct.Terminate();
   finally
     if Assigned(Self.resusct) then
     begin
       Self.resusct.WaitFor;
       FreeAndNil(Self.resusct);
     end;
   end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPanelTCPClient.SendPing(Sedner:TObject);
begin
 try
   if (Self.tcpClient.Connected) then
     Self.SendLn('-;PING');
 except

 end;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
 PanelTCPClient := TPanelTCPClient.Create;

finalization
 FreeAndNil(PanelTCPCLient);

end.//unit
