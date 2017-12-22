unit globConfig;

{
  Global configuration of SW.
}

interface

uses IniFiles, SysUtils, Types, Generics.Collections, Classes;

type
  TServerConfig = record
    host:string;
    port:Word;
  end;

  TGlobConfigData = record
    server:TServerConfig;
  end;

  TGlobConfig = class
    public const
      _DEFAULT_FN = 'config.ini';

    private
      filename:string;

    public

      data:TGlobConfigData;

      procedure LoadFile(const filename:string = _DEFAULT_FN);
      procedure SaveFile(const filename:string); overload;
      procedure SaveFile(); overload;

      function GetAuthNonNullORSCnt():Cardinal;

      property fn:string read filename;
  end;

var
  config:TGlobConfig;

implementation

uses tcpClient;

////////////////////////////////////////////////////////////////////////////////

procedure TGlobConfig.LoadFile(const filename:string = _DEFAULT_FN);
var ini:TMemIniFile;
    str:TStrings;
    i:Integer;
begin
 ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 try
   Self.filename := filename;

   Self.data.server.host := ini.ReadString('server', 'host', 'localhost');
   Self.data.server.port := ini.ReadInteger('server', 'port', _DEFAULT_PORT);
 finally
   ini.Free();
 end;
end;

procedure TGlobConfig.SaveFile(const filename:string);
var ini:TMemIniFile;
    i:Integer;
    str:string;
begin
 ini := TMemIniFile.Create(filename, TEncoding.UTF8);

 try
   ini.WriteString('server', 'host', Self.data.server.host);
   ini.WriteInteger('server', 'port', Self.data.server.port);

   ini.UpdateFile();
 finally
   ini.Free();
 end;
end;

procedure TGlobConfig.SaveFile();
begin
 Self.SaveFile(Self.filename);
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  GlobConfig := TGlobConfig.Create();

finalization
  FreeAndNil(GlobConfig);

end.//unit
