# System.Defer
 Defer for Delphi

An analogue of the mechanism for delayed release of resources (object, memory or cancellation of an action) from the Zig programming language.

The essence of the mechanism is that after some action, create a canceling action that will be performed when the block exits.

That is, for example, we created an object, after creation we write how to release it and it will be released automatically after exiting the block.

The mechanism works on the basis of interfaces.

## Using

Object
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

Method
```pascal
procedure TForm5.Button4Click(Sender: TObject);
begin
  BeginUpdate;
  defer(EndUpdate);   //defer action
  /// your code with BeginUpdate
end;
```

Memory
```pascal
 var p: Pointer;
 GetMem(p, 1024);  //allocate mem
 defer(procedure begin FreeMem(p) end);  //defer action for free mem
 // work with p 
```
