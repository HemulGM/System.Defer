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

function defer(Proc: TProc): IDefer; inline;

implementation

function defer(Proc: TProc): IDefer;
begin
  Result := TDefer.Create(Proc);
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

end.

