# System.Defer
 Defer for Delphi

An analogue of the mechanism for delayed release of resources (object, memory or cancellation of an action) from the Zig programming language.

The essence of the mechanism is that after some action, create a canceling action that will be performed when the block exits.

That is, for example, we created an object, after creation we write how to release it and it will be released automatically after exiting the block.

The mechanism works on the basis of interfaces. 

> Throwing an exception in a block does not prevent the release of the object.

## Description
`defer` is used to execute a statement while exiting the current block.
`errdefer` works like `defer`, but only executing when the function is returned from with an error inside of the errdefer’s block.

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
  defer(Test);  //defer action

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

```pascal
 for var i := 1 to 4 do
 begin
   var Test := TTestList.Create;
   defer(Test);

   Test.Add('1');
   Test.Add('2');
   Test.Add('3');
 end;  //free of Test each iteration
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

Errdefer
```pascal
function GetList: TTestList;
begin
  var List := TTestList.Create;
  errdefer(List);
  List.DoRaise;
  Result := List;
end;

procedure TForm5.Button5Click(Sender: TObject);
begin
  var List := GetList; //exception, and no leaks
  List.Free;
end;
```
