unit System.Defer;

interface

uses
  System.SysUtils;

type
  IDefer = interface
  end;

  TDefer = class(TInterfacedObject, IDefer)
  private
    FProc: TProc;
  public
    constructor Create(Proc: TProc); reintroduce;
    destructor Destroy; override;
  end;

  TDeferErr = class(TInterfacedObject, IDefer)
  private
    FProc: TProc;
  public
    constructor Create(Proc: TProc); reintroduce;
    destructor Destroy; override;
  end;

/// <summary>
/// defer is used to execute a statement while exiting the current block.
/// </summary>
function defer(Proc: TProc): IDefer; inline;

/// <summary>
/// errdefer works like defer, but only executing when the function is returned from with an error inside of the errdefer’s block.
/// </summary>
function errdefer(Proc: TProc): IDefer; inline;

implementation

function defer(Proc: TProc): IDefer;
begin
  Result := TDefer.Create(Proc);
end;

function errdefer(Proc: TProc): IDefer;
begin
  Result := TDeferErr.Create(Proc);
end;

{ TDefer }

constructor TDefer.Create(Proc: TProc);
begin
  inherited Create;
  FProc := Proc;
end;

destructor TDefer.Destroy;
begin
  if Assigned(FProc) then
    FProc();
  inherited;
end;

{ TDeferErr }

constructor TDeferErr.Create(Proc: TProc);
begin
  inherited Create;
  FProc := Proc;
end;

destructor TDeferErr.Destroy;
begin
  if Assigned(ExceptObject()) then
    if Assigned(FProc) then
      FProc();
  inherited;
end;

end.