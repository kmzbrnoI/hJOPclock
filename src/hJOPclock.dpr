// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program hJOPclock;

uses
  Forms,
  SysUtils,
  Windows,
  main in 'main.pas' {F_Main},
  parseHelper in 'parseHelper.pas',
  version in 'version.pas',
  resusc in 'resusc.pas',
  tcpThread in 'tcpThread.pas',
  tcpClient in 'tcpClient.pas',
  globConfig in 'globConfig.pas',
  modelTime in 'modelTime.pas';

{$R *.res}

begin
  DecimalSeparator := '.';

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'hJOPclock';
  Application.CreateForm(TF_Main, F_Main);

  try
    if (ParamCount > 0) then
      config.LoadFile(ParamStr(1))
    else
      config.LoadFile();

  except
    on E:Exception do
      Application.MessageBox(PChar('Nepodaøilo se naèíst konfiguraèní soubor'+#1310+E.Message),
                             'Varování', MB_OK or MB_ICONWARNING);
  end;

  client.InitResusc(config.data.server.host, config.data.server.port);

  Application.Run;
end.
