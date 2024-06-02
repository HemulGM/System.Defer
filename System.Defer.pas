unit System.Defer;

interface

uses
  System.SysUtils;

{$SCOPEDENUMS ON}

type
  IDefer = interface
  end;

  TDeferAction = (Always, OnError, NoError);

  TDefer = class(TInterfacedObject, IDefer)
  protected
    FProc: TProc;
    FObj: TObject;
    FAction: TDeferAction;
    function NeedDestroy: Boolean; virtual;
  public
    constructor Create(Proc: TProc; Action: TDeferAction = TDeferAction.Always); reintroduce; overload;
    constructor Create(Obj: TObject; Action: TDeferAction = TDeferAction.Always); reintroduce; overload;
    destructor Destroy; override;
  end;

/// <summary>
/// Performs the procedure when leaving the block
/// </summary>
function defer(Proc: TProc): IDefer; inline; overload;

/// <summary>
/// Performs the procedure when leaving the block due to exclusion
/// </summary>
function errdefer(Proc: TProc): IDefer; inline; overload;

/// <summary>
/// Performs the procedure when leaving the block if there was no exception
/// </summary>
function noerrdefer(Proc: TProc): IDefer; inline; overload;

/// <summary>
/// Releases the object when leaving the block
/// </summary>
function defer(Obj: TObject): IDefer; inline; overload;

/// <summary>
/// Releases the object when leaving the block due to exclusion
/// </summary>
function errdefer(Obj: TObject): IDefer; inline; overload;

/// <summary>
/// Releases the object when leaving the block if there was no exception
/// </summary>
function noerrdefer(Obj: TObject): IDefer; inline; overload;

implementation

function defer(Proc: TProc): IDefer;
begin
  Result := TDefer.Create(Proc);
end;

function errdefer(Proc: TProc): IDefer;
begin
  Result := TDefer.Create(Proc, TDeferAction.OnError);
end;

function noerrdefer(Proc: TProc): IDefer;
begin
  Result := TDefer.Create(Proc, TDeferAction.NoError);
end;

function defer(Obj: TObject): IDefer;
begin
  Result := TDefer.Create(Obj);
end;

function errdefer(Obj: TObject): IDefer;
begin
  Result := TDefer.Create(Obj, TDeferAction.OnError);
end;

function noerrdefer(Obj: TObject): IDefer;
begin
  Result := TDefer.Create(Obj, TDeferAction.NoError);
end;

{ TDefer }

constructor TDefer.Create(Proc: TProc; Action: TDeferAction);
begin
  inherited Create;
  FAction := Action;
  FProc := Proc;
  FObj := nil;
end;

constructor TDefer.Create(Obj: TObject; Action: TDeferAction);
begin
  inherited Create;
  FAction := Action;
  FProc := nil;
  FObj := Obj;
end;

destructor TDefer.Destroy;
begin
  if not NeedDestroy then
    Exit;
  if Assigned(FProc) then
    FProc();
  FObj.Free;
  inherited;
end;

function TDefer.NeedDestroy: Boolean;
begin
  case FAction of
    TDeferAction.Always:
      Exit(True);
    TDeferAction.OnError:
      Exit(Assigned(ExceptObject()));
    TDeferAction.NoError:
      Exit(not Assigned(ExceptObject()));
  else
    Result := True;
  end;
end;

end.

