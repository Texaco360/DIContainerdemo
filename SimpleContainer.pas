unit SimpleContainer;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, Classes;

type
  TFactory = function: IInterface;
  PTFactory = ^TFactory;

  TSimpleContainer = class
  private
    FKeys: TStringList;
    FFacts: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Register(const Key: string; const Fact: TFactory);
    function Resolve(const Key: string): IInterface;
  end;

implementation

constructor TSimpleContainer.Create;
begin
  inherited;
  FKeys := TStringList.Create;
  FKeys.CaseSensitive := False;
  FFacts := TList.Create;
end;

destructor TSimpleContainer.Destroy;
var
  i: Integer;
  pFact: PTFactory;
begin
  for i := 0 to FFacts.Count - 1 do
  begin
    pFact := PTFactory(FFacts.Items[i]);
    Dispose(pFact);
  end;

  FFacts.Free;
  FKeys.Free;
  inherited;
end;

procedure TSimpleContainer.Register(const Key: string; const Fact: TFactory);
var
  pFact: PTFactory;
begin
  New(pFact);
  pFact^ := Fact;
  FKeys.Add(Key);
  FFacts.Add(pFact);
end;

function TSimpleContainer.Resolve(const Key: string): IInterface;
var
  idx: Integer;
  pFact: PTFactory;
begin
  idx := FKeys.IndexOf(Key);
  if idx < 0 then
    raise Exception.CreateFmt('No factory registered for key "%s"', [Key]);

  pFact := PTFactory(FFacts.Items[idx]);
  Result := pFact^();
end;

end.
