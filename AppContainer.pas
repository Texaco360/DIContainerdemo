unit AppContainer;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils;

type
  TFactory = function: IInterface;

  // Service lifetime enumeration
  TServiceLifetime = (slTransient, slSingleton, slScoped);

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
      Lifetime: TServiceLifetime;
      SingletonInstance: IInterface;  // Cached instance for singletons
    end;
  private
    FEntries: TList;
    FCurrentScope: TList;  // For scoped instances
    function FindEntry(const Key: string): TServiceEntry;
    function CreateScopedInstance(Entry: TServiceEntry): IInterface;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Registration methods with lifetime support
    procedure Register(const Key: string; FactoryFunc: TFactory; 
      Lifetime: TServiceLifetime = slTransient); overload;
    procedure Register(const Key: string; const FactoryObj: IServiceFactory; 
      Lifetime: TServiceLifetime = slTransient); overload;
    
    // Convenience methods for specific lifetimes
    procedure RegisterSingleton(const Key: string; FactoryFunc: TFactory); overload;
    procedure RegisterSingleton(const Key: string; const FactoryObj: IServiceFactory); overload;
    procedure RegisterTransient(const Key: string; FactoryFunc: TFactory); overload;
    procedure RegisterTransient(const Key: string; const FactoryObj: IServiceFactory); overload;
    procedure RegisterScoped(const Key: string; FactoryFunc: TFactory); overload;
    procedure RegisterScoped(const Key: string; const FactoryObj: IServiceFactory); overload;
    
    function Resolve(const Key: string): IInterface;
    
    // Scope management
    procedure BeginScope;
    procedure EndScope;
    procedure ClearSingletons;  // For testing/cleanup
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
  FCurrentScope := TList.Create;
end;

destructor TSimpleContainer.Destroy;
var
  i: Integer;
begin
  // Clean up scoped instances
  for i := 0 to FCurrentScope.Count - 1 do
    TObject(FCurrentScope[i]).Free;
  FCurrentScope.Free;

  // Clean up service entries
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

procedure TSimpleContainer.Register(const Key: string; FactoryFunc: TFactory; Lifetime: TServiceLifetime);
begin
  Register(Key, TDelegateFactory.Create(FactoryFunc), Lifetime);
end;

procedure TSimpleContainer.Register(const Key: string; const FactoryObj: IServiceFactory; Lifetime: TServiceLifetime);
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
  Entry.Lifetime := Lifetime;
  Entry.SingletonInstance := nil;
  FEntries.Add(Entry);
end;

// Convenience methods for specific lifetimes
procedure TSimpleContainer.RegisterSingleton(const Key: string; FactoryFunc: TFactory);
begin
  Register(Key, FactoryFunc, slSingleton);
end;

procedure TSimpleContainer.RegisterSingleton(const Key: string; const FactoryObj: IServiceFactory);
begin
  Register(Key, FactoryObj, slSingleton);
end;

procedure TSimpleContainer.RegisterTransient(const Key: string; FactoryFunc: TFactory);
begin
  Register(Key, FactoryFunc, slTransient);
end;

procedure TSimpleContainer.RegisterTransient(const Key: string; const FactoryObj: IServiceFactory);
begin
  Register(Key, FactoryObj, slTransient);
end;

procedure TSimpleContainer.RegisterScoped(const Key: string; FactoryFunc: TFactory);
begin
  Register(Key, FactoryFunc, slScoped);
end;

procedure TSimpleContainer.RegisterScoped(const Key: string; const FactoryObj: IServiceFactory);
begin
  Register(Key, FactoryObj, slScoped);
end;

function TSimpleContainer.Resolve(const Key: string): IInterface;
var
  Entry: TServiceEntry;
begin
  Entry := FindEntry(Key);
  if not Assigned(Entry) then
    raise Exception.CreateFmt('Service "%s" not found', [Key]);

  case Entry.Lifetime of
    slSingleton:
      begin
        // Return existing singleton or create new one
        if not Assigned(Entry.SingletonInstance) then
          Entry.SingletonInstance := Entry.Factory.CreateService;
        Result := Entry.SingletonInstance;
      end;
      
    slScoped:
      begin
        // Return scoped instance (one per scope)
        Result := CreateScopedInstance(Entry);
      end;
      
    slTransient:
      begin
        // Always create new instance
        Result := Entry.Factory.CreateService;
      end;
  end;
end;

function TSimpleContainer.CreateScopedInstance(Entry: TServiceEntry): IInterface;
type
  TScopedInstance = class
  public
    Key: string;
    Instance: IInterface;
  end;
var
  i: Integer;
  ScopedInstance: TScopedInstance;
begin
  // Check if we already have this service in current scope
  for i := 0 to FCurrentScope.Count - 1 do
  begin
    ScopedInstance := TScopedInstance(FCurrentScope[i]);
    if SameText(ScopedInstance.Key, Entry.Key) then
      Exit(ScopedInstance.Instance);
  end;
  
  // Create new scoped instance
  Result := Entry.Factory.CreateService;
  
  // Store in current scope
  ScopedInstance := TScopedInstance.Create;
  ScopedInstance.Key := Entry.Key;
  ScopedInstance.Instance := Result;
  FCurrentScope.Add(ScopedInstance);
end;

procedure TSimpleContainer.BeginScope;
begin
  // For this simple implementation, we just clear current scope
  // In a more advanced implementation, you'd stack scopes
  EndScope;
end;

procedure TSimpleContainer.EndScope;
var
  i: Integer;
begin
  // Clear all scoped instances
  for i := 0 to FCurrentScope.Count - 1 do
    TObject(FCurrentScope[i]).Free;
  FCurrentScope.Clear;
end;

procedure TSimpleContainer.ClearSingletons;
var
  i: Integer;
  Entry: TServiceEntry;
begin
  // Clear all singleton instances - useful for testing
  for i := 0 to FEntries.Count - 1 do
  begin
    Entry := TServiceEntry(FEntries[i]);
    Entry.SingletonInstance := nil;
  end;
end;

end.
