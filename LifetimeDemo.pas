program LifetimeDemo;

{$mode objfpc}
{$H+}

uses
  SysUtils, AppContainer, LoggerIntf, Loggers, CalculationService;

{=================================================================
  LIFETIME DEMONSTRATION - Singleton, Transient, Scoped
  =================================================================}
var
  Container: TAppContainer;
  Logger1, Logger2, Logger3: IInterface;
  SingletonLogger1, SingletonLogger2: ILogger;
  TransientLogger1, TransientLogger2: ILogger;
  ScopedLogger1, ScopedLogger2: ILogger;
begin
  WriteLn('=== Container Lifetime Management Demo ===');
  WriteLn;

  Container := TAppContainer.Create;
  try
    {--- Register services with different lifetimes -------------------------}
    
    // SINGLETON: One instance for entire container lifetime
    Container.RegisterSingleton('logger.singleton', 
      function: IInterface 
      begin 
        WriteLn('  [CREATED] Singleton logger instance');
        Result := TConsoleLogger.Create 
      end);
    
    // TRANSIENT: New instance every time (default behavior)  
    Container.RegisterTransient('logger.transient',
      function: IInterface 
      begin 
        WriteLn('  [CREATED] Transient logger instance');
        Result := TConsoleLogger.Create 
      end);
    
    // SCOPED: One instance per scope
    Container.RegisterScoped('logger.scoped',
      function: IInterface 
      begin 
        WriteLn('  [CREATED] Scoped logger instance');
        Result := TConsoleLogger.Create 
      end);

    {--- Demonstrate SINGLETON behavior -------------------------}
    WriteLn('1. SINGLETON LIFETIME:');
    WriteLn('   Resolving singleton twice - should create only once');
    
    Logger1 := Container.Resolve('logger.singleton');
    if not Supports(Logger1, ILogger, SingletonLogger1) then
      raise Exception.Create('Failed to get singleton logger 1');
      
    Logger2 := Container.Resolve('logger.singleton');  
    if not Supports(Logger2, ILogger, SingletonLogger2) then
      raise Exception.Create('Failed to get singleton logger 2');
      
    WriteLn('   Same instance? ', (Logger1 = Logger2));
    WriteLn;

    {--- Demonstrate TRANSIENT behavior -------------------------}
    WriteLn('2. TRANSIENT LIFETIME:');
    WriteLn('   Resolving transient twice - should create each time');
    
    Logger1 := Container.Resolve('logger.transient');
    if not Supports(Logger1, ILogger, TransientLogger1) then
      raise Exception.Create('Failed to get transient logger 1');
      
    Logger2 := Container.Resolve('logger.transient');
    if not Supports(Logger2, ILogger, TransientLogger2) then
      raise Exception.Create('Failed to get transient logger 2');
      
    WriteLn('   Same instance? ', (Logger1 = Logger2));
    WriteLn;

    {--- Demonstrate SCOPED behavior -------------------------}
    WriteLn('3. SCOPED LIFETIME:');
    
    WriteLn('   Scope 1 - Resolving scoped twice in same scope');
    Container.BeginScope;
    try
      Logger1 := Container.Resolve('logger.scoped');
      if not Supports(Logger1, ILogger, ScopedLogger1) then
        raise Exception.Create('Failed to get scoped logger 1');
        
      Logger2 := Container.Resolve('logger.scoped');
      if not Supports(Logger2, ILogger, ScopedLogger2) then
        raise Exception.Create('Failed to get scoped logger 2');
        
      WriteLn('   Same instance in scope? ', (Logger1 = Logger2));
      
    finally
      Container.EndScope;
    end;
    
    WriteLn('   Scope 2 - New scope should create new instance');
    Container.BeginScope;
    try
      Logger3 := Container.Resolve('logger.scoped');
      WriteLn('   Different from previous scope? ', (Logger1 <> Logger3));
    finally
      Container.EndScope;
    end;
    WriteLn;

    {--- Practical Example: Database Connection -------------------------}
    WriteLn('4. PRACTICAL EXAMPLE:');
    WriteLn('   Different lifetimes for different purposes');
    
    // Configuration: Singleton (shared settings)
    Container.RegisterSingleton('config.app',
      function: IInterface 
      begin 
        WriteLn('  [CONFIG] Created application configuration');
        Result := nil; // Would be actual config object
      end);
    
    // Database Connection: Scoped (one per request/operation)
    Container.RegisterScoped('db.connection',
      function: IInterface 
      begin 
        WriteLn('  [DB] Created database connection');
        Result := nil; // Would be actual DB connection
      end);
    
    // Email Service: Transient (new for each email)
    Container.RegisterTransient('email.sender',
      function: IInterface 
      begin 
        WriteLn('  [EMAIL] Created email sender');
        Result := nil; // Would be actual email service
      end);

    // Simulate a web request scope
    WriteLn;
    WriteLn('   Simulating web request processing:');
    Container.BeginScope;
    try
      // These would be resolved as needed during request processing
      Container.Resolve('config.app');      // Uses singleton
      Container.Resolve('db.connection');   // Creates scoped instance  
      Container.Resolve('db.connection');   // Reuses scoped instance
      Container.Resolve('email.sender');    // Creates new transient
      Container.Resolve('email.sender');    // Creates another transient
    finally
      Container.EndScope;  // Cleans up scoped instances
    end;

    WriteLn;
    WriteLn('=== Demo Complete ===');
    WriteLn('Key Points:');
    WriteLn('- Singleton: Created once, reused always');
    WriteLn('- Transient: Created every time');  
    WriteLn('- Scoped: Created once per scope, cleared on EndScope');

  finally
    Container.Free;
  end;
end.