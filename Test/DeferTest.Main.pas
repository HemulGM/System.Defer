unit DeferTest.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, System.Defer;

type
  TForm5 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
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

function GetList: TTestList;
begin
  var List := TTestList.Create;
  errdefer(List.Free);
  List.DoRaise;                //test exception
  Result := List;
end;

procedure TForm5.Button1Click(Sender: TObject);
begin
  for var i := 1 to 4 do
  begin
    var Test := TTestList.Create;
    defer(Test.Free);      //free all iter

    Test.Add('1');
    Test.Add('2');
    Test.Add('3');
  end;
  //Test.DoRaise;
  //Test.Add('4');
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

procedure TForm5.Button2Click(Sender: TObject);
begin
  Canvas.BeginScene;
  defer(Canvas.EndScene);

  with Canvas do
  begin
    FillRect(TRectF.Create(10, 10, 50, 50), 1);
  end;
end;

procedure TForm5.Button3Click(Sender: TObject);
begin
  var p: Pointer;
  GetMem(p, 1024);
  defer(
    procedure
    begin
      FreeMem(p)
    end);
  // work with p
end;

procedure TForm5.Button5Click(Sender: TObject);
begin
  var List := GetList;
  List.Free;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.

