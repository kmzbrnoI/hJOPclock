// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program hJOPclock;

uses
  Forms,
  main in 'main.pas' {F_Main},
  parseHelper in 'parseHelper.pas',
  version in 'version.pas',
  resusc in 'resusc.pas',
  tcpThread in 'tcpThread.pas',
  tcpClient in 'tcpClient.pas',
  globConfig in 'globConfig.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TF_Main, F_Main);
  Application.Run;
end.
