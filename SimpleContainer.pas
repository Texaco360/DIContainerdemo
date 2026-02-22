unit SimpleContainer;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils;

type
  TFactory = function: IInterface;

  IServiceFactory = interface
    ['{E8F20C7B-9E97-4CF8-8F0A-2D7D75A8F9A1}']
    function CreateService: IInterface;
  end;

  TSimpleContainer = class
  private type
    TServiceEntry = class
    public
      Key: string;
      Factory: IServiceFactory;
    end;
  private
    FEntries: TList;
    function FindEntry(const Key: string): TServiceEntry;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Register(const Key: string; FactoryFunc: TFactory); overload;
    procedure Register(const Key: string; const FactoryObj: IServiceFactory); overload;
    function Resolve(const Key: string): IInterface;
  end;

implementation

type
  TDelegateFactory = class(TInterfacedObject, IServiceFactory)
  private
    FFactory: TFactory;
  public
    constructor Create(AFactory: TFactory);
    function CreateService: IInterface;
  end;

constructor TDelegateFactory.Create(AFactory: TFactory);
begin
  inherited Create;
  FFactory := AFactory;
end;

function TDelegateFactory.CreateService: IInterface;
begin
  if not Assigned(FFactory) then
    raise Exception.Create('Factory function is not assigned');
  Result := FFactory();
end;

constructor TSimpleContainer.Create;
begin
  inherited;
  FEntries := TList.Create;
end;

destructor TSimpleContainer.Destroy;
var
  i: Integer;
begin
  for i := 0 to FEntries.Count - 1 do
    TObject(FEntries[i]).Free;

  FEntries.Free;
  inherited Destroy;
end;

function TSimpleContainer.FindEntry(const Key: string): TServiceEntry;
var
  i: Integer;
  Entry: TServiceEntry;
begin
  for i := 0 to FEntries.Count - 1 do
  begin
    Entry := TServiceEntry(FEntries[i]);
    if SameText(Entry.Key, Key) then
      Exit(Entry);
  end;
  Result := nil;
end;

procedure TSimpleContainer.Register(const Key: string; FactoryFunc: TFactory);
begin
  Register(Key, TDelegateFactory.Create(FactoryFunc));
end;

procedure TSimpleContainer.Register(const Key: string; const FactoryObj: IServiceFactory);
var
  Entry: TServiceEntry;
begin
  if Key = '' then
    raise Exception.Create('Service key cannot be empty');
  if not Assigned(FactoryObj) then
    raise Exception.Create('Factory object is not assigned');
  if Assigned(FindEntry(Key)) then
    raise Exception.CreateFmt('Service "%s" is already registered', [Key]);

  Entry := TServiceEntry.Create;
  Entry.Key := Key;
  Entry.Factory := FactoryObj;
  FEntries.Add(Entry);
end;

function TSimpleContainer.Resolve(const Key: string): IInterface;
var
  Entry: TServiceEntry;
begin
  Entry := FindEntry(Key);
  if not Assigned(Entry) then
    raise Exception.CreateFmt('Service "%s" not found', [Key]);

  Result := Entry.Factory.CreateService;
end;

end.
