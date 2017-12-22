unit modelTime;

{
  Model time maintainance.
}

interface

uses Classes, SysUtils, IniFiles, IDContext, Graphics, ExtCtrls, DateUtils;

type

  TModelTime = class
   private
    ftime:TTime;
    fnasobic:Integer;
    fstarted:boolean;
    timer:TTimer;
    last_call:TDateTime;

    procedure OnTimer(Sender:TObject);

   public
     constructor Create();
     destructor Destroy(); override;

     procedure ParseData(data:TStrings);

     procedure Show();
     procedure Reset();

     property time:TTime read ftime;
     property nasobic:Integer read fnasobic;
     property started:boolean read fstarted;
  end;

var
  mt:TModelTime;

implementation

uses main, globConfig;

////////////////////////////////////////////////////////////////////////////////

constructor TModelTime.Create();
begin
 inherited Create();

 Self.timer          := TTimer.Create(nil);
 Self.timer.Enabled  := false;
 Self.timer.Interval := 100;
 Self.timer.OnTimer  := OnTimer;
end;

destructor TModelTime.Destroy();
begin
 try
   FreeAndNil(Self.timer);
 finally
   inherited Destroy();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

//-;MOD-CAS;running;nasobic;cas;
procedure TModelTime.ParseData(data:TStrings);
begin
 Self.fstarted      := (data[2] = '1');
 Self.timer.Enabled := Self.fstarted;
 Self.last_call     := Now;

 Self.fnasobic := StrToInt(data[3]);
 Self.ftime    := StrToTime(data[4]);

 Self.Show();
 F_Main.P_TimeResize(F_Main.P_Time);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TModelTime.OnTimer(Sender:TObject);
var diff:TTime;
begin
 // pocitani aktualniho modeloveho casu:
 diff := Now - Self.last_call;
 Self.ftime := Self.time + (diff*Self.nasobic);

 Self.last_call := now;
 Self.Show();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TModelTime.Show();
var format:string;
begin
 if (config.data.seconds) then
   format := 'hh:nn:ss'
 else
   format := 'hh:nn';

 if (Self.started) then
  begin
   F_Main.UpdateTime(FormatDateTime(format, Self.ftime), clBlack);
  end else begin
   F_Main.UpdateTime(FormatDateTime(format, Self.ftime), clRed);
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TModelTime.Reset();
begin
 Self.ftime    := EncodeTime(0, 0, 0, 0);
 Self.fstarted := false;
 Self.fnasobic := 1;

 Self.timer.Enabled := false;

 Self.Show();
end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization
 mt := TModelTime.Create();

finalization
 FreeAndNil(mt);

end.//unit

