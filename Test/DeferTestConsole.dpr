program DeferTestConsole;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Defer in '..\System.Defer.pas';

procedure f;
begin
  var Test := TObject.Create;
  defer(Test.Free);
end;

begin
  var s: string := 'hello';
  f();
  readln;
  writeln(s);
end.
