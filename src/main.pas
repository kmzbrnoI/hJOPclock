unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ActnList;

type
  TF_Main = class(TForm)
    P_Time: TPanel;
    L_Time: TLabel;
    AL_Main: TActionList;
    A_Seconds: TAction;
    A_Help: TAction;
    A_Maximize: TAction;
    procedure P_TimeResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure A_SecondsExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure A_HelpExecute(Sender: TObject);
    procedure A_MaximizeExecute(Sender: TObject);
  private
    { Private declarations }
  public
    procedure UpdateStatus(status:string; color:TColor = clBlack);
    procedure UpdateTime(time:string; color:TColor = clBlack);
  end;

var
  F_Main: TF_Main;

implementation

{$R *.dfm}

uses tcpClient, globConfig, modelTime, version;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.A_SecondsExecute(Sender: TObject);
begin
 config.data.seconds := not config.data.seconds;
 if (client.status = TPanelConnectionStatus.opened) then
  begin
   mt.Show();
   Self.P_TimeResize(Self.P_Time);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 try
   config.SaveFile();
 except

 end;

 try
   client.Disconnect();
 except
   on E:Exception do
     Application.MessageBox(PChar(E.Message), 'Chyba', MB_OK or MB_ICONERROR);
 end;
end;

procedure TF_Main.FormCreate(Sender: TObject);
begin
 Self.Caption := Application.Title + '  v' + GetVersion(Application.ExeName);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.P_TimeResize(Sender: TObject);
var w, h:Integer;
const _OFFSET = 10;
begin
 w := Self.L_Time.Canvas.TextWidth(Self.L_Time.Caption);
 h := Self.L_Time.Canvas.TextHeight(Self.L_Time.Caption);

 while ((w < Self.P_Time.ClientWidth - _OFFSET * 2) and
     (h < Self.P_Time.ClientHeight - _OFFSET * 2)) do
  begin
   Self.L_Time.Font.Size := Self.L_Time.Font.Size + 1;

   w := Self.L_Time.Canvas.TextWidth(Self.L_Time.Caption);
   h := Self.L_Time.Canvas.TextHeight(Self.L_Time.Caption);
  end;

 while (((w > Self.P_Time.ClientWidth - _OFFSET * 2) or
     (h > Self.P_Time.ClientHeight - _OFFSET * 2)) and (Self.L_Time.Font.Size > 1)) do
  begin
   Self.L_Time.Font.Size := Self.L_Time.Font.Size - 1;

   w := Self.L_Time.Canvas.TextWidth(Self.L_Time.Caption);
   h := Self.L_Time.Canvas.TextHeight(Self.L_Time.Caption);
  end;

 Self.L_Time.Width := w;
 Self.L_Time.Height := h;
 Self.L_Time.Left := (Self.P_Time.Width div 2) - (Self.L_Time.Width div 2);
 Self.L_Time.Top := (Self.P_Time.Height div 2) - (Self.L_Time.Height div 2);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.UpdateStatus(status:string; color:TColor = clBlack);
begin
 Self.L_Time.Caption := status;
 Self.L_Time.Font.Color := color;
 Self.P_TimeResize(Self.P_Time);
end;

procedure TF_Main.UpdateTime(time:string; color:TColor = clBlack);
begin
 Self.L_Time.Caption := time;
 Self.L_Time.Font.Color := color;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.A_HelpExecute(Sender: TObject);
begin
 Application.MessageBox(PChar('hJOPclock v' + GetVersion(Application.ExeName)+'.'+#13#10+
                        'Vytvoøil Jan Horáèek pro KMŽ Brno I'),
                        'Informace',
                        MB_OK or MB_ICONINFORMATION)
end;

////////////////////////////////////////////////////////////////////////////////

procedure TF_Main.A_MaximizeExecute(Sender: TObject);
begin
 if (Self.BorderStyle = bsNone) then
  begin
   Self.BorderStyle := bsSizeable;
  end else begin
   Self.BorderStyle := bsNone;
   Self.WindowState := wsMaximized;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

end.
