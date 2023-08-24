# System.Defer
 Defer for Delphi

Using
```pascal
uses
  ..., System.Defer;

type
...
  TTestList = class(TStringList)
    procedure DoRaise;
    destructor Destroy; override;
  end;
...

procedure TForm5.Button1Click(Sender: TObject);
begin
  var Test := TTestList.Create;
  defer(Test.Free);  //defer action

  Test.Add('1');
  Test.Add('2');
  Test.Add('3');
  Test.DoRaise;      //test raise
  Test.Add('4');
end;                 //free

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
```
