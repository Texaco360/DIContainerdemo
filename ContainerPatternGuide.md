# Dependency Injection Container Pattern Guide

## Table of Contents
- [What is a Dependency Injection Container?](#what-is-a-dependency-injection-container)
- [Why Use a Container?](#why-use-a-container)
- [Core Concepts](#core-concepts)
- [Building Your First Container](#building-your-first-container)
- [Registration Patterns](#registration-patterns)
- [Access Patterns](#access-patterns)
- [Best Practices](#best-practices)
- [Common Pitfalls](#common-pitfalls)
- [Advanced Patterns](#advanced-patterns)

## What is a Dependency Injection Container?

A **Dependency Injection (DI) Container** is a design pattern that manages object creation and dependency resolution in your application. Think of it as a smart factory that:

- 📦 **Stores** instructions on how to create objects
- 🔧 **Creates** objects when needed with their dependencies
- 🔄 **Manages** object lifecycles (singleton, transient, etc.)
- 🎯 **Resolves** dependencies automatically

### The Problem It Solves

Without DI Container:
```pascal
// Tightly coupled - hard to test and maintain
var
  Logger: TFileLogger;
  Calculator: TCalculationService;
begin
  Logger := TFileLogger.Create('app.log');  // Hard-coded dependency
  Calculator := TCalculationService.Create(Logger);
  // What if we want ConsoleLogger instead? We have to change code!
end;
```

With DI Container:
```pascal
// Loosely coupled - flexible and testable
var
  Calculator: ICalculationService;
begin
  Calculator := Container.Resolve<ICalculationService>('calc.default');
  // The container handles all dependencies automatically!
end;
```

## Why Use a Container?

### 🧪 **Testability**
DI containers make unit testing dramatically easier by allowing you to replace real dependencies with test doubles (mocks, stubs, fakes).

**Without DI - Hard to Test:**
```pascal
// Tightly coupled - can't test in isolation
TOrderService = class
public
  procedure ProcessOrder(OrderId: Integer);
  begin
    // Direct dependencies - hard to mock
    var Logger := TFileLogger.Create('orders.log');     // Writes to real file!
    var DB := TSQLDatabase.Create(ProductionConnStr);   // Hits real database!
    var Email := TSMTPMailer.Create(ProductionSMTP);    // Sends real emails!
    
    // Business logic mixed with infrastructure
    Logger.Log('Processing order: ' + IntToStr(OrderId));
    var Order := DB.GetOrder(OrderId);
    Email.SendConfirmation(Order.CustomerEmail);
  end;
end;

// Testing this is nightmare:
// - Creates real files
// - Requires database setup  
// - Sends actual emails
// - Slow and unreliable
```

**With DI - Easy to Test:**
```pascal
// Loosely coupled - perfect for testing
TOrderService = class
private
  FLogger: ILogger;
  FDatabase: IDatabase; 
  FMailer: IMailer;
public
  constructor Create(Logger: ILogger; DB: IDatabase; Mailer: IMailer);
  procedure ProcessOrder(OrderId: Integer);
end;

procedure TOrderService.ProcessOrder(OrderId: Integer);
begin
  FLogger.Log('Processing order: ' + IntToStr(OrderId));
  var Order := FDatabase.GetOrder(OrderId);
  FMailer.SendConfirmation(Order.CustomerEmail);
end;
```

**Creating Test Doubles:**
```pascal
// Mock logger that captures log messages
TMockLogger = class(TInterfacedObject, ILogger)
private
  FMessages: TStringList;
public
  constructor Create;
  destructor Destroy; override;
  procedure Log(const Message: string);
  function GetMessages: TStringList;
  procedure ClearMessages;
end;

// Mock database with predictable data
TMockDatabase = class(TInterfacedObject, IDatabase)
private
  FTestOrders: TDictionary<Integer, TOrder>;
public
  procedure AddTestOrder(OrderId: Integer; Order: TOrder);
  function GetOrder(OrderId: Integer): TOrder;
end;

// Mock mailer that tracks sent emails
TMockMailer = class(TInterfacedObject, IMailer)
private
  FSentEmails: TList<string>;
public
  procedure SendConfirmation(const Email: string);
  function GetSentEmails: TList<string>;
  function EmailWasSentTo(const Email: string): Boolean;
end;
```

**Unit Test Example:**
```pascal
procedure TestOrderService.TestProcessOrder_LogsCorrectMessage;
var
  MockLogger: TMockLogger;
  MockDB: TMockDatabase;
  MockMailer: TMockMailer;
  OrderService: TOrderService;
  TestOrder: TOrder;
begin
  // Arrange - Set up test doubles
  MockLogger := TMockLogger.Create;
  MockDB := TMockDatabase.Create;
  MockMailer := TMockMailer.Create;
  
  TestOrder.OrderId := 123;
  TestOrder.CustomerEmail := 'test@example.com';
  MockDB.AddTestOrder(123, TestOrder);
  
  OrderService := TOrderService.Create(MockLogger, MockDB, MockMailer);
  
  try
    // Act - Execute the method under test
    OrderService.ProcessOrder(123);
    
    // Assert - Verify expected behavior
    CheckTrue(MockLogger.GetMessages.Contains('Processing order: 123'));
    CheckTrue(MockMailer.EmailWasSentTo('test@example.com'));
    CheckEquals(1, MockMailer.GetSentEmails.Count);
    
  finally
    OrderService.Free;
    MockLogger.Free;
    MockDB.Free;
    MockMailer.Free;
  end;
end;
```

**Container-Based Test Setup:**
```pascal
procedure TOrderServiceTests.SetUp;
begin
  // Create test container with mocks
  FTestContainer := TAppContainer.Create;
  
  FMockLogger := TMockLogger.Create;
  FMockDB := TMockDatabase.Create;  
  FMockMailer := TMockMailer.Create;
  
  // Register test doubles
  FTestContainer.Register('logger', function: IInterface begin Result := FMockLogger end);
  FTestContainer.Register('database', function: IInterface begin Result := FMockDB end);
  FTestContainer.Register('mailer', function: IInterface begin Result := FMockMailer end);
  
  // Register service under test with test dependencies
  FTestContainer.Register('order.service', TOrderServiceFactory.Create(FTestContainer));
end;

procedure TOrderServiceTests.TestWithContainer;
var
  OrderService: IOrderService;
begin
  // Resolve service with all mocked dependencies
  OrderService := FTestContainer.Resolve('order.service') as IOrderService;
  
  // Test runs with mocks automatically injected
  OrderService.ProcessOrder(123);
  
  // Verify mock interactions
  CheckTrue(FMockLogger.GetMessages.Count > 0);
end;
```

**Benefits for Testing:**
- ✅ **Isolation**: Test only the unit, not its dependencies
- ✅ **Speed**: No I/O operations (files, network, database)
- ✅ **Reliability**: Predictable test data, no external failures
- ✅ **Control**: Simulate error conditions easily
- ✅ **Verification**: Assert on interactions with dependencies

### 🔄 **Flexibility** 
Switch implementations without changing code:
```pascal
// Development: use console logger
Container.Register('logger', TConsoleLogger.Create);

// Production: use file logger  
Container.Register('logger', TFileLogger.Create('prod.log'));
```

### 📈 **Maintainability**
Centralized configuration makes changes easier:
```pascal
// All wiring in one place
procedure ConfigureServices(Container: TContainer);
begin
  Container.Register('logger', TFileLogger.Create('app.log'));
  Container.Register('database', TPostgreSQLConnection.Create(ConnectionString));
  Container.Register('emailer', TSMTPEmailer.Create(SMTPSettings));
end;
```

## Core Concepts

### 1. **Service Registration**
Tell the container how to create objects:
```pascal
// Register a concrete implementation
Container.Register('logger.console', TConsoleLogger.Create);

// Register with a factory function
Container.Register('logger.file', function: IInterface 
begin 
  Result := TFileLogger.Create('app.log') 
end);
```

### 2. **Service Resolution**
Ask the container to create objects:
```pascal
var
  Service: IInterface;
  Logger: ILogger;
begin
  Service := Container.Resolve('logger.console');
  if Supports(Service, ILogger, Logger) then
    Logger.Log('Hello World!');
end;
```

### 3. **Dependency Injection**
Objects get their dependencies automatically:
```pascal
// Calculator needs a logger - container provides it automatically
TCalculationService = class
private
  FLogger: ILogger;
public
  constructor Create(ALogger: ILogger);  // Container will inject this
end;
```

## Building Your First Container

### Step 1: Define Core Interface

```pascal
unit SimpleContainer;

interface

type
  // Factory function type
  TFactory = function: IInterface;
  
  // Service factory interface
  IServiceFactory = interface
    function CreateService: IInterface;
  end;
  
  // Main container class
  TAppContainer = class
  public
    procedure Register(const Key: string; Factory: TFactory); overload;
    procedure Register(const Key: string; const Factory: IServiceFactory); overload;
    function Resolve(const Key: string): IInterface;
  end;
```

### Step 2: Implement Registration

```pascal
procedure TAppContainer.Register(const Key: string; Factory: TFactory);
begin
  // Store the factory function with the key
  FFactories.Add(Key, Factory);
end;

procedure TAppContainer.Register(const Key: string; const Factory: IServiceFactory);
begin
  // Store the factory object with the key
  FServiceFactories.Add(Key, Factory);
end;
```

### Step 3: Implement Resolution

```pascal
function TAppContainer.Resolve(const Key: string): IInterface;
var
  Factory: TFactory;
begin
  if FFactories.TryGetValue(Key, Factory) then
    Result := Factory()
  else
    raise Exception.CreateFmt('Service "%s" not registered', [Key]);
end;
```

### Step 4: Create Your First Service

```pascal
// 1. Define interface
ILogger = interface
  procedure Log(const Message: string);
end;

// 2. Implement concrete class
TConsoleLogger = class(TInterfacedObject, ILogger)
public
  procedure Log(const Message: string);
end;

procedure TConsoleLogger.Log(const Message: string);
begin
  WriteLn('[LOG] ', Message);
end;

// 3. Register with container
Container.Register('logger', 
  function: IInterface 
  begin 
    Result := TConsoleLogger.Create 
  end);

// 4. Use the service
var
  LoggerService: IInterface;
  Logger: ILogger;
begin
  LoggerService := Container.Resolve('logger');
  if Supports(LoggerService, ILogger, Logger) then
    Logger.Log('Container is working!');
end;
```

## Registration Patterns

### 1. **Module Pattern** ⭐ Recommended
Group related service registrations:

```pascal
TLoggerModule = class
public
  class procedure RegisterServices(Container: TAppContainer);
end;

class procedure TLoggerModule.RegisterServices(Container: TAppContainer);
begin
  // Register all logger-related services
  Container.Register('logger.console', TConsoleLoggerFactory.Create);
  Container.Register('logger.file', TFileLoggerFactory.Create('app.log'));
  Container.Register('logger.email', TEmailLoggerFactory.Create(SMTPConfig));
end;

// Usage
TLoggerModule.RegisterServices(Container);
TDatabaseModule.RegisterServices(Container);
TEmailModule.RegisterServices(Container);
```

### 2. **Factory Pattern**
Handle complex object creation:

```pascal
TCalculationServiceFactory = class(TInterfacedObject, IServiceFactory)
private
  FContainer: TAppContainer;
  FLoggerKey: string;
public
  constructor Create(Container: TAppContainer; LoggerKey: string);
  function CreateService: IInterface;
end;

function TCalculationServiceFactory.CreateService: IInterface;
var
  Logger: ILogger;
begin
  // Resolve dependency from container
  Logger := Container.Resolve(FLoggerKey) as ILogger;
  
  // Create service with dependency
  Result := TCalculationService.Create(Logger);
end;
```

### 3. **Configuration-Based Registration**
Use configuration files or constants:

```pascal
const
  // Service keys
  LOGGER_CONSOLE = 'logger.console';
  LOGGER_FILE = 'logger.file';
  CALC_SERVICE = 'calc.default';
  
procedure RegisterFromConfig(Container: TAppContainer);
begin
  // Read from config file or use defaults
  if ConfigUseFileLogger then
    Container.Register(LOGGER_CONSOLE, TFileLoggerFactory.Create(LogFileName))
  else
    Container.Register(LOGGER_CONSOLE, TConsoleLoggerFactory.Create);
    
  Container.Register(CALC_SERVICE, TCalculationServiceFactory.Create(Container));
end;
```

## Access Patterns

### 1. **Service Locator** 🥉 Simple but Limited

```pascal
unit ServiceLocator;

type
  TServices = class
  private
    class var FContainer: TAppContainer;
  public
    class procedure Initialize(Container: TAppContainer);
    class function GetLogger: ILogger;
    class function GetCalculator: ICalculationService;
  end;

// Usage anywhere in your app
procedure DoSomething;
var
  Logger: ILogger;
begin
  Logger := TServices.GetLogger;
  Logger.Log('Doing something...');
end;
```

**Pros:** Easy to use everywhere  
**Cons:** Hidden dependencies, harder to test

### 2. **Constructor Injection** ⭐ Recommended

```pascal
TOrderProcessor = class
private
  FLogger: ILogger;
  FCalculator: ICalculationService;
public
  constructor Create(Logger: ILogger; Calculator: ICalculationService);
  procedure ProcessOrder(Order: TOrder);
end;

// Factory handles the injection
TOrderProcessorFactory = class(IServiceFactory)
  function CreateService: IInterface;
  begin
    Result := TOrderProcessor.Create(
      Container.Resolve('logger') as ILogger,
      Container.Resolve('calculator') as ICalculationService
    );
  end;
```

**Pros:** Explicit dependencies, testable, follows SOLID principles  
**Cons:** More setup code required

### 3. **Property Injection** 🥈 Good for Optional Dependencies

```pascal
TReportGenerator = class
private
  FLogger: ILogger;
public
  property Logger: ILogger read FLogger write FLogger;  // Optional
  procedure GenerateReport;
end;

// Container can set optional properties after creation
```

## Best Practices

### ✅ Do's

1. **Use Interfaces**: Always register and resolve interfaces, not concrete classes
   ```pascal
   // Good
   Container.Register('logger', ILogger, TConsoleLogger.Create);
   
   // Avoid
   Container.Register('logger', TConsoleLogger, TConsoleLogger.Create);
   ```

2. **Group Registrations**: Use modules to organize related services
   ```pascal
   TLoggingModule.RegisterServices(Container);
   TDataAccessModule.RegisterServices(Container);
   ```

3. **Use Descriptive Keys**: Make service keys meaningful
   ```pascal
   // Good
   Container.Register('logger.application', ...);
   Container.Register('repository.user', ...);
   
   // Avoid
   Container.Register('svc1', ...);
   Container.Register('thing2', ...);
   ```

4. **Validate Dependencies**: Check dependencies exist before using
   ```pascal
   if not Supports(Service, ILogger, Logger) then
     raise Exception.Create('Service does not implement ILogger');
   ```

### ❌ Don'ts

1. **Don't Use as God Object**: Container shouldn't know about every class
2. **Don't Mix Concerns**: Keep business logic out of container configuration
3. **Don't Ignore Lifecycle**: Consider if services should be singletons or transient
4. **Don't Skip Error Handling**: Always handle resolve failures gracefully

## Common Pitfalls

### 1. **Circular Dependencies** 
```pascal
// Problem: A depends on B, B depends on A
TServiceA = class
  constructor Create(ServiceB: IServiceB);
end;

TServiceB = class  
  constructor Create(ServiceA: IServiceA);  // Circular!
end;

// Solution: Use interfaces and factories to break cycles
```

### 2. **Registration Order Issues**
```pascal
// Problem: Trying to resolve before registration
var Logger := Container.Resolve('logger');  // Error: not registered yet
Container.Register('logger', TConsoleLogger.Create);

// Solution: Register first, resolve later
Container.Register('logger', TConsoleLogger.Create);
var Logger := Container.Resolve('logger');  // OK
```

### 3. **Memory Leaks**
```pascal
// Problem: Not managing container lifecycle
procedure BadExample;
var
  Container: TAppContainer;
begin
  Container := TAppContainer.Create;
  // ... use container ...
  // Missing: Container.Free;  <- Memory leak!
end;

// Solution: Always clean up
procedure GoodExample;
var
  Container: TAppContainer;
begin
  Container := TAppContainer.Create;
  try
    // ... use container ...
  finally
    Container.Free;  // Proper cleanup
  end;
end;
```

## Advanced Patterns

### 1. **Decorator Pattern with DI**
Add functionality without changing existing code:

```pascal
// Original logger
ILogger = interface
  procedure Log(const Message: string);
end;

// Decorator that adds timestamps
TTimestampLoggerDecorator = class(TInterfacedObject, ILogger)
private
  FInnerLogger: ILogger;
public
  constructor Create(InnerLogger: ILogger);
  procedure Log(const Message: string);
end;

procedure TTimestampLoggerDecorator.Log(const Message: string);
begin
  FInnerLogger.Log(Format('[%s] %s', [DateTimeToStr(Now), Message]));
end;

// Registration with decoration
Container.Register('logger.base', TFileLogger.Create('app.log'));
Container.Register('logger.decorated', 
  function: IInterface
  var BaseLogger: ILogger;
  begin
    BaseLogger := Container.Resolve('logger.base') as ILogger;
    Result := TTimestampLoggerDecorator.Create(BaseLogger);
  end);
```

### 2. **Conditional Registration**
Register different services based on configuration:

```pascal
procedure RegisterLoggers(Container: TAppContainer; Config: TAppConfig);
begin
  case Config.LogLevel of
    llDebug: 
      Container.Register('logger', TVerboseLogger.Create);
    llInfo: 
      Container.Register('logger', TStandardLogger.Create);
    llError: 
      Container.Register('logger', TErrorOnlyLogger.Create);
  end;
  
  if Config.EnableFileLogging then
    Container.Register('logger.file', TFileLogger.Create(Config.LogFile));
    
  if Config.EnableEmailAlerts then
    Container.Register('logger.email', TEmailLogger.Create(Config.SMTPSettings));
end;
```

### 3. **Scoped Services**
Different object lifetimes for different needs:

```pascal
type
  TServiceScope = (ssTransient, ssSingleton, ssScoped);
  
  TScopedContainer = class(TAppContainer)
  private
    FSingletons: TDictionary<string, IInterface>;
  public
    procedure RegisterScoped(const Key: string; Factory: TFactory; Scope: TServiceScope);
    function Resolve(const Key: string): IInterface; override;
  end;

// Usage
Container.RegisterScoped('logger', TLoggerFactory, ssSingleton);      // One instance
Container.RegisterScoped('repository', TRepoFactory, ssTransient);    // New each time
Container.RegisterScoped('dbcontext', TContextFactory, ssScoped);     // One per scope
```

---

## Putting It All Together

Here's a complete example showing a well-structured application:

```pascal
program DIDemo;

uses
  Container, LoggingModule, DataModule, BusinessModule;

var
  AppContainer: TAppContainer;
  App: IApplication;
begin
  AppContainer := TAppContainer.Create;
  try
    // Register all modules
    TLoggingModule.RegisterServices(AppContainer);
    TDataModule.RegisterServices(AppContainer);
    TBusinessModule.RegisterServices(AppContainer);
    
    // Resolve and run application
    App := AppContainer.Resolve('application') as IApplication;
    App.Run;
    
  finally
    AppContainer.Free;
  end;
end.
```

## Conclusion

The Dependency Injection Container pattern is powerful for creating maintainable, testable applications. Start simple with basic registration and resolution, then gradually adopt more advanced patterns as your application grows.

**Key Takeaways:**
- 🎯 Containers manage object creation and dependencies
- 🧪 They make code more testable and flexible  
- 📦 Use modules to organize service registrations
- ⭐ Constructor injection is the most robust pattern
- 🛡️ Always validate dependencies and handle errors

Remember: A good container should be **invisible** to your business logic. If you find yourself constantly thinking about the container, you might be overcomplicating things!