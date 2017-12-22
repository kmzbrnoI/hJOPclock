unit parseHelper;

{
  This unit implements parsing function.
}

interface

uses Types, Classes, SysUtils;

procedure ExtractStringsEx(Separators: TSysCharSet; Ignore: TSysCharSet;
                           Content: string; var Strings: TStrings);

implementation

////////////////////////////////////////////////////////////////////////////////
// Semicolon-separated-string parsing.
// This function is used to parse TCP incoming data.
// Example input: "ahoj;ja;jsem;{Honza;Horazek}"
// Example result: ["ahoj", "ja", "jsem", "Honza;Horacek"]

procedure ExtractStringsEx(Separators: TSysCharSet; Ignore: TSysCharSet;
                           Content: string; var Strings: TStrings);
var i: word;
    s: string;
    plain_cnt:Integer;
begin
  s := '';
  plain_cnt := 0;
  if (Length(Content) = 0) then Exit();

  for i := 1 to Length(Content) do
   begin
    if (Content[i] = '{') then
     begin
      if (plain_cnt > 0) then s := s + Content[i];
      Inc(plain_cnt);
     end
    else if ((Content[i] = '}') and (plain_cnt > 0)) then
     begin
      Dec(plain_cnt);
      if (plain_cnt > 0) then s := s + Content[i];
     end
    else begin
      if ((CharInSet(Content[i], Separators)) and (plain_cnt = 0)) then
       begin
        Strings.Add(s);
        s := '';
       end else
        if (not CharInSet(Content[i], Ignore) or (plain_cnt > 0)) then
          s := s + Content[i];
    end;// else Content[i]
   end;

  if (s <> '') then Strings.Add(s);
end;

end.
