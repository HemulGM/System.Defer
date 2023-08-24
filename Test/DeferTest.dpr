program DeferTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  DeferTest.Main in 'DeferTest.Main.pas' {Form5},
  System.Defer in '..\System.Defer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
