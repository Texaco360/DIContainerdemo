unit ContainerLifetimeTests;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, AppContainer, LoggerIntf, EnhancedLoggers;

type
  // Simple test framework
  TProcedure = procedure of object;
  
  TTestResult = record
    TestName: string;
    Success: Boolean;
    ErrorMessage: string;
  end;
  
  TContainerLifetimeTests = class
  private
    FContainer: TAppContainer;
    FResults: array of TTestResult;
    procedure AddResult(const TestName: string; Success: Boolean; const ErrorMsg: string = '');
    procedure RunTest(const TestName: string; TestProc: TProcedure);
  public
    constructor Create;
    destructor Destroy; override;
    
    // Test methods
    procedure TestSingletonLifetime;
    procedure TestTransientLifetime;
    procedure TestScopedLifetime;
    procedure TestClearSingletons;
    
    // Test runner
    procedure RunAllTests;
    procedure PrintResults;
  end;

implementation

type
  // Simple factory classes for testing
  TTestApplicationLoggerFactory = class(TInterfacedObject, IServiceFactory)
  private
    FLogFile: string;
  public
    constructor Create(const ALogFile: string);
    function CreateService: IInterface;
  end;

  TTestRequestLoggerFactory = class(TInterfacedObject, IServiceFactory)
  private
    FRequestId: string;
  public
    constructor Create(const ARequestId: string);
    function CreateService: IInterface;
  end;

constructor TTestApplicationLoggerFactory.Create(const ALogFile: string);
begin
  inherited Create;
  FLogFile := ALogFile;
end;

function TTestApplicationLoggerFactory.CreateService: IInterface;
begin
  Result := TApplicationLogger.Create(FLogFile);
end;

constructor TTestRequestLoggerFactory.Create(const ARequestId: string);
begin
  inherited Create;
  FRequestId := ARequestId;
end;

function TTestRequestLoggerFactory.CreateService: IInterface;
begin
  Result := TRequestLogger.Create(FRequestId);
end;

constructor TContainerLifetimeTests.Create;
begin
  inherited Create;
  FContainer := TAppContainer.Create;
  SetLength(FResults, 0);
end;

destructor TContainerLifetimeTests.Destroy;
begin
  FContainer.Free;
  inherited Destroy;
end;

procedure TContainerLifetimeTests.AddResult(const TestName: string; Success: Boolean; const ErrorMsg: string);
var
  Idx: Integer;
begin
  Idx := Length(FResults);
  SetLength(FResults, Idx + 1);
  FResults[Idx].TestName := TestName;
  FResults[Idx].Success := Success;
  FResults[Idx].ErrorMessage := ErrorMsg;
end;

procedure TContainerLifetimeTests.RunTest(const TestName: string; TestProc: TProcedure);
begin
  try
    TestProc();
    AddResult(TestName, True);
    WriteLn(Format('✓ %s', [TestName]));
  except
    on E: Exception do
    begin
      AddResult(TestName, False, E.Message);
      WriteLn(Format('✗ %s: %s', [TestName, E.Message]));
    end;
  end;
end;

procedure TContainerLifetimeTests.TestSingletonLifetime;
var
  Instance1, Instance2: IInterface;
begin
  // Register singleton
  FContainer.RegisterSingleton('test.singleton',
    TTestApplicationLoggerFactory.Create('test.log'));

  // Resolve twice
  Instance1 := FContainer.Resolve('test.singleton');
  Instance2 := FContainer.Resolve('test.singleton');
  
  // Should be same instance
  if Instance1 <> Instance2 then
    raise Exception.Create('Singleton instances should be identical');
    
  // Should create only one instance
  if TApplicationLogger.GetInstanceCount <> 1 then
    raise Exception.CreateFmt('Expected 1 singleton instance, got %d', 
      [TApplicationLogger.GetInstanceCount]);
end;

procedure TContainerLifetimeTests.TestTransientLifetime;
var
  Instance1, Instance2: IInterface;
  InitialCount: Integer;
begin
  InitialCount := TApplicationLogger.GetInstanceCount;
  
  // Register transient
  FContainer.RegisterTransient('test.transient',
    TTestApplicationLoggerFactory.Create('test.log'));

  // Resolve twice
  Instance1 := FContainer.Resolve('test.transient');
  Instance2 := FContainer.Resolve('test.transient');
  
  // Should be different instances
  if Instance1 = Instance2 then
    raise Exception.Create('Transient instances should be different');
    
  // Should create two new instances
  if TApplicationLogger.GetInstanceCount <> (InitialCount + 2) then
    raise Exception.CreateFmt('Expected %d instances, got %d', 
      [InitialCount + 2, TApplicationLogger.GetInstanceCount]);
end;

procedure TContainerLifetimeTests.TestScopedLifetime;
var
  Instance1, Instance2, Instance3: IInterface;
begin
  // Register scoped
  FContainer.RegisterScoped('test.scoped',
    TTestRequestLoggerFactory.Create('TEST-SCOPE'));

  // Test same scope
  FContainer.BeginScope;
  try
    Instance1 := FContainer.Resolve('test.scoped');
    Instance2 := FContainer.Resolve('test.scoped');
    
    // Should be same instance within scope
    if Instance1 <> Instance2 then
      raise Exception.Create('Scoped instances should be identical within same scope');
  finally
    FContainer.EndScope;
  end;
  
  // Test new scope
  FContainer.BeginScope;
  try
    Instance3 := FContainer.Resolve('test.scoped');
    
    // Should be different instance in new scope
    if Instance1 = Instance3 then
      raise Exception.Create('Scoped instances should be different across scopes');
  finally  
    FContainer.EndScope;
  end;
end;

procedure TContainerLifetimeTests.TestClearSingletons;
var
  Instance1, Instance2: IInterface;
  Count1, Count2: Integer;
begin
  // Register singleton
  FContainer.RegisterSingleton('test.clear',
    TTestApplicationLoggerFactory.Create('test.log'));

  // Get first instance
  Count1 := TApplicationLogger.GetInstanceCount;
  Instance1 := FContainer.Resolve('test.clear');
  
  // Clear singletons
  FContainer.ClearSingletons;
  
  // Get second instance (should be new)
  Count2 := TApplicationLogger.GetInstanceCount;
  Instance2 := FContainer.Resolve('test.clear');
  
  // Should be different instances after clear
  if Instance1 = Instance2 then
    raise Exception.Create('Singleton should be recreated after ClearSingletons');
    
  // Should have created new instance
  if Count2 = Count1 then
    raise Exception.Create('New singleton instance should have been created');
end;

procedure TContainerLifetimeTests.RunAllTests;
begin
  WriteLn('=== Container Lifetime Tests ===');
  WriteLn;
  
  // Setup services
  TEnhancedLoggerModule.RegisterServices(FContainer);
  
  // Run tests
  RunTest('Singleton Lifetime', @TestSingletonLifetime);
  RunTest('Transient Lifetime', @TestTransientLifetime);  
  RunTest('Scoped Lifetime', @TestScopedLifetime);
  RunTest('Clear Singletons', @TestClearSingletons);
  
  WriteLn;
  PrintResults;
end;

procedure TContainerLifetimeTests.PrintResults;
var
  i, Passed, Failed: Integer;
begin
  Passed := 0;
  Failed := 0;
  
  WriteLn('=== Test Results ===');
  for i := 0 to High(FResults) do
  begin
    if FResults[i].Success then
      Inc(Passed)
    else
    begin
      Inc(Failed);
      WriteLn(Format('FAILED: %s - %s', [FResults[i].TestName, FResults[i].ErrorMessage]));
    end;
  end;
  
  WriteLn(Format('Tests: %d, Passed: %d, Failed: %d', [Length(FResults), Passed, Failed]));
  
  if Failed = 0 then
    WriteLn('🎉 All tests passed!')
  else
    WriteLn('❌ Some tests failed!');
end;

end.