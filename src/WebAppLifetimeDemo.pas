program WebAppLifetimeDemo;

{$mode objfpc}
{$H+}

uses
  SysUtils, SimpleContainer, LoggerIntf, EnhancedLoggers;

{=================================================================
  WEB APPLICATION SIMULATION
  Demonstrates realistic usage of different service lifetimes
  =================================================================}

type
  // Simulate a web request processor
  TRequestProcessor = class
  private
    FContainer: TAppContainer;
  public
    constructor Create(AContainer: TAppContainer);
    procedure ProcessRequest(const RequestId: string);
  end;

constructor TRequestProcessor.Create(AContainer: TAppContainer);
begin
  inherited Create;
  FContainer := AContainer;
end;

procedure TRequestProcessor.ProcessRequest(const RequestId: string);
var
  AppLogger, ReqLogger, OpLogger1, OpLogger2: IInterface;
  Logger: ILogger;
begin
  WriteLn(Format('=== Processing Request: %s ===', [RequestId]));
  
  // Begin new scope for this request
  FContainer.BeginScope;
  try
    // 1. SINGLETON: Get application logger (same instance always)
    AppLogger := FContainer.Resolve('logger.application');
    if Supports(AppLogger, ILogger, Logger) then
      Logger.Log(Format('Starting request processing: %s', [RequestId]));

    // 2. SCOPED: Get request logger (one per request scope)
    ReqLogger := FContainer.Resolve('logger.request');
    if Supports(ReqLogger, ILogger, Logger) then
      Logger.Log('Request logger initialized');
      
    // 3. Simulate multiple operations in the same request
    WriteLn('  Performing database operation...');
    OpLogger1 := FContainer.Resolve('logger.operation');  // TRANSIENT: new instance
    if Supports(OpLogger1, ILogger, Logger) then
      Logger.Log('Executing SELECT query');
    
    WriteLn('  Performing email operation...');  
    OpLogger2 := FContainer.Resolve('logger.operation');  // TRANSIENT: another new instance
    if Supports(OpLogger2, ILogger, Logger) then
      Logger.Log('Sending notification email');
    
    // 4. Reuse request logger (same scoped instance)
    ReqLogger := FContainer.Resolve('logger.request');
    if Supports(ReqLogger, ILogger, Logger) then
      Logger.Log('Request processing completed');
      
    // 5. Use application logger again (same singleton instance)
    AppLogger := FContainer.Resolve('logger.application');
    if Supports(AppLogger, ILogger, Logger) then
      Logger.Log(Format('Request %s completed successfully', [RequestId]));
    
  finally
    // End scope - cleans up scoped instances (request logger)
    FContainer.EndScope;
  end;
  
  WriteLn(Format('=== Request %s Complete ===', [RequestId]));
  WriteLn;
end;

var
  Container: TAppContainer;
  Processor: TRequestProcessor;
  i: Integer;
begin
  WriteLn('=== Web Application Lifetime Demo ===');
  WriteLn('Simulating multiple web requests with different service lifetimes');
  WriteLn;

  Container := TAppContainer.Create;
  try
    // Register services with appropriate lifetimes
    TEnhancedLoggerModule.RegisterServices(Container);
    
    Processor := TRequestProcessor.Create(Container);
    try
      // Simulate multiple web requests
      for i := 1 to 3 do
      begin
        Processor.ProcessRequest(Format('WEB-REQ-%d', [i]));
        Sleep(100); // Simulate time between requests
      end;
      
      WriteLn('=== Summary ===');
      WriteLn(Format('Application Logger instances created: %d', 
        [TApplicationLogger.GetInstanceCount]));
      WriteLn('Expected: 1 (singleton)');
      WriteLn;
      WriteLn('Request Logger instances: 3 (one per request scope)');
      WriteLn('Operation Logger instances: 6 (two per request, all transient)');
      WriteLn;
      WriteLn('Key Benefits Demonstrated:');
      WriteLn('✓ Singleton: Shared state, resource conservation');
      WriteLn('✓ Scoped: Request isolation, automatic cleanup'); 
      WriteLn('✓ Transient: Operation isolation, no state conflicts');
      
    finally
      Processor.Free;
    end;
    
  finally
    Container.Free;
  end;
end.