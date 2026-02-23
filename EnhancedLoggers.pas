unit EnhancedLoggers;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, LoggerIntf, SimpleContainer;

type
  // Thread-safe singleton logger with instance counting
  TApplicationLogger = class(TInterfacedObject, ILogger)
  private
    class var FInstanceCount: Integer;
    FLogFile: string;
    FInstanceId: Integer;
  public
    constructor Create(const ALogFile: string);
    destructor Destroy; override;
    procedure Log(const Msg: string);
    class function GetInstanceCount: Integer;
  end;

  // Request-scoped logger that tracks request context
  TRequestLogger = class(TInterfacedObject, ILogger)
  private
    FRequestId: string;
    FStartTime: TDateTime;
    FOperationCount: Integer;
  public
    constructor Create(const ARequestId: string);
    destructor Destroy; override;
    procedure Log(const Msg: string);
    function GetOperationCount: Integer;
  end;

  // Transient operation logger for specific tasks
  TOperationLogger = class(TInterfacedObject, ILogger)
  private
    FOperationName: string;
    FOperationId: string;
  public
    constructor Create(const AOperationName: string);
    procedure Log(const Msg: string);
  end;

  // Enhanced logger module with lifetime management
  TEnhancedLoggerModule = class
  public
    class procedure RegisterServices(const Container: TSimpleContainer);
  end;

implementation

constructor TApplicationLogger.Create(const ALogFile: string);
begin
  inherited Create;
  Inc(FInstanceCount);
  FInstanceId := FInstanceCount;
  FLogFile := ALogFile;
  WriteLn(Format('[SINGLETON] ApplicationLogger #%d created for file: %s', [FInstanceId, ALogFile]));
end;

destructor TApplicationLogger.Destroy;
begin
  WriteLn(Format('[SINGLETON] ApplicationLogger #%d destroyed', [FInstanceId]));
  inherited Destroy;
end;

procedure TApplicationLogger.Log(const Msg: string);
begin
  WriteLn(Format('[APP-LOG #%d] %s', [FInstanceId, Msg]));
  // In real app: write to file with proper thread safety
end;

class function TApplicationLogger.GetInstanceCount: Integer;
begin
  Result := FInstanceCount;
end;

constructor TRequestLogger.Create(const ARequestId: string);
begin
  inherited Create;
  FRequestId := ARequestId;
  FStartTime := Now;
  FOperationCount := 0;
  WriteLn(Format('[SCOPED] RequestLogger created for request: %s', [ARequestId]));
end;

destructor TRequestLogger.Destroy;
var
  Duration: Double;
begin
  Duration := (Now - FStartTime) * 24 * 60 * 60; // Seconds
  WriteLn(Format('[SCOPED] RequestLogger for %s destroyed. Operations: %d, Duration: %.2fs', 
    [FRequestId, FOperationCount, Duration]));
  inherited Destroy;
end;

procedure TRequestLogger.Log(const Msg: string);
begin
  Inc(FOperationCount);
  WriteLn(Format('[REQ-%s #%d] %s', [FRequestId, FOperationCount, Msg]));
end;

function TRequestLogger.GetOperationCount: Integer;
begin
  Result := FOperationCount;
end;

constructor TOperationLogger.Create(const AOperationName: string);
begin
  inherited Create;
  FOperationName := AOperationName;
  FOperationId := Format('%s-%d', [AOperationName, Random(9999)]);
  WriteLn(Format('[TRANSIENT] OperationLogger created for: %s (%s)', [AOperationName, FOperationId]));
end;

procedure TOperationLogger.Log(const Msg: string);
begin
  WriteLn(Format('[OP-%s] %s', [FOperationId, Msg]));
end;

class procedure TEnhancedLoggerModule.RegisterServices(const Container: TSimpleContainer);
begin
  // SINGLETON: Application-wide logger (one instance for entire app)
  Container.RegisterSingleton('logger.application',
    function: IInterface 
    begin 
      Result := TApplicationLogger.Create('application.log') 
    end);

  // SCOPED: Request logger (one per request/scope)  
  Container.RegisterScoped('logger.request',
    function: IInterface 
    begin 
      Result := TRequestLogger.Create(Format('REQ-%d', [Random(10000)])) 
    end);

  // TRANSIENT: Operation logger (new for each operation)
  Container.RegisterTransient('logger.operation',
    function: IInterface 
    begin 
      Result := TOperationLogger.Create('DefaultOperation') 
    end);
end;

end.