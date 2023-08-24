unit DeferTest.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, System.Defer;

type
  TForm5 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TTestList = class(TStringList)
    procedure DoRaise;
    destructor Destroy; override;
  end;

var
  Form5: TForm5;

implementation

{$R *.fmx}

procedure TForm5.Button1Click(Sender: TObject);
begin
  var Test := TTestList.Create;
  defer(Test.Free);

  Test.Add('1');
  Test.Add('2');
  Test.Add('3');
  Test.DoRaise;
  Test.Add('4');
end;

{ TTestList }

destructor TTestList.Destroy;
begin
  ShowMessage('test list destroy');
  inherited;
end;

procedure TTestList.DoRaise;
begin
  raise Exception.Create('Error Message');
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.

