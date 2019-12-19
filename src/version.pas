﻿unit version;

{
  Get version of an application.
}

interface

uses Windows, SysUtils, Forms, jclPEImage;

 function GetVersion(const FileName: string):string;
 function GetLastBuildDate():string;
 function GetLastBuildTime():string;

implementation

function GetVersion(const FileName: string): string;
var
  size, len: longword;
  handle: THandle;
  buffer: pchar;
  pinfo: ^VS_FIXEDFILEINFO;
  Major, Minor: word;
begin
  Result := 'Není dostupná';
  size := GetFileVersionInfoSize(Pointer(FileName), handle);
  if size > 0 then begin
    GetMem(buffer, size);
    if GetFileVersionInfo(Pointer(FileName), 0, size, buffer)
    then
      if VerQueryValue(buffer, '\', pointer(pinfo), len) then begin
        Major   := HiWord(pinfo.dwFileVersionMS);
        Minor   := LoWord(pinfo.dwFileVersionMS);
        Result := Format('%d.%d',[Major, Minor]);
      end;
    FreeMem(buffer);
  end;
end;

function GetLastBuildDate():string;
 begin
  DateTimeToString(Result, 'dd.mm.yyyy', jclPEImage.PeReadLinkerTimeStamp(Application.ExeName));
 end;//function

function GetLastBuildTime():string;
 begin
  DateTimeToString(Result, 'hh:mm:ss', jclPEImage.PeReadLinkerTimeStamp(Application.ExeName));
 end;//function

end.//unit
