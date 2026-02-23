unit CalculationService;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, LoggerIntf, AppContainer;

type
  ICalculationService = interface
    ['{1E2C7A2D-16E8-43D4-AF18-2A46BBAE8D31}']
    function Add(const A, B: Integer): Integer;
    function Multiply(const A, B: Integer): Integer;
  end;

  TCalculationService = class(TInterfacedObject, ICalculationService)
  private
    FLogger: ILogger;
  public
    constructor Create(const ALogger: ILogger);
    function Add(const A, B: Integer): Integer;
    function Multiply(const A, B: Integer): Integer;
  end;

  TCalculationServiceFactory = class(TInterfacedObject, IServiceFactory)
  private
    FContainer: TAppContainer;
    FLoggerKey: string;
  public
    constructor Create(const AContainer: TAppContainer; const ALoggerKey: string);
    function CreateService: IInterface;
  end;

  TCalculationModule = class
  public
    class procedure RegisterServices(const Container: TAppContainer; const ALoggerKey: string = 'logger.console');
  end;

implementation

constructor TCalculationService.Create(const ALogger: ILogger);
begin
  inherited Create;
  if not Assigned(ALogger) then
    raise Exception.Create('CalculationService requires a logger');
  FLogger := ALogger;
end;

function TCalculationService.Add(const A, B: Integer): Integer;
begin
  Result := A + B;
  FLogger.Log(Format('Add(%d, %d) = %d', [A, B, Result]));
end;

function TCalculationService.Multiply(const A, B: Integer): Integer;
begin
  Result := A * B;
  FLogger.Log(Format('Multiply(%d, %d) = %d', [A, B, Result]));
end;

constructor TCalculationServiceFactory.Create(const AContainer: TAppContainer; const ALoggerKey: string);
begin
  inherited Create;
  FContainer := AContainer;
  FLoggerKey := ALoggerKey;
end;

function TCalculationServiceFactory.CreateService: IInterface;
var
  LoggerSvc: IInterface;
  Logger: ILogger;
begin
  LoggerSvc := FContainer.Resolve(FLoggerKey);
  if not Supports(LoggerSvc, ILogger, Logger) then
    raise Exception.CreateFmt('Service "%s" does not implement ILogger', [FLoggerKey]);

  Result := TCalculationService.Create(Logger);
end;

class procedure TCalculationModule.RegisterServices(const Container: TAppContainer; const ALoggerKey: string);
begin
  Container.Register('calc.default', TCalculationServiceFactory.Create(Container, ALoggerKey));
end;

end.
