unit OrderService;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, LoggerIntf, CalculationService;

type
  IOrderService = interface
    ['{F1B2C3D4-E5F6-4A78-9B1C-2D3E4F567890}']
    procedure ProcessOrder(const OrderId: Integer; const ItemCount: Integer; const UnitPrice: Double);
  end;

  TOrderService = class(TInterfacedObject, IOrderService)
  private
    FLogger: ILogger;
    FCalculator: ICalculationService;
  public
    constructor Create(const ALogger: ILogger; const ACalculator: ICalculationService);
    procedure ProcessOrder(const OrderId: Integer; const ItemCount: Integer; const UnitPrice: Double);
  end;

  TOrderServiceFactory = class(TInterfacedObject, IServiceFactory)
  private
    FContainer: TSimpleContainer;
  public
    constructor Create(const AContainer: TSimpleContainer);
    function CreateService: IInterface;
  end;

  TOrderModule = class
  public
    class procedure RegisterServices(const Container: TSimpleContainer);
  end;

implementation

uses
  SimpleContainer;

constructor TOrderService.Create(const ALogger: ILogger; const ACalculator: ICalculationService);
begin
  inherited Create;
  if not Assigned(ALogger) then
    raise Exception.Create('OrderService requires a logger');
  if not Assigned(ACalculator) then
    raise Exception.Create('OrderService requires a calculator');
    
  FLogger := ALogger;
  FCalculator := ACalculator;
end;

procedure TOrderService.ProcessOrder(const OrderId: Integer; const ItemCount: Integer; const UnitPrice: Double);
var
  Total: Integer;
begin
  FLogger.Log(Format('Processing order %d with %d items at $%.2f each', [OrderId, ItemCount, UnitPrice]));
  
  Total := FCalculator.Multiply(ItemCount, Trunc(UnitPrice * 100)); // Convert to cents
  
  FLogger.Log(Format('Order %d total: $%.2f', [OrderId, Total / 100.0]));
end;

constructor TOrderServiceFactory.Create(const AContainer: TSimpleContainer);
begin
  inherited Create;
  FContainer := AContainer;
end;

function TOrderServiceFactory.CreateService: IInterface;
var
  LoggerSvc, CalcSvc: IInterface;
  Logger: ILogger;
  Calculator: ICalculationService;
begin
  // Resolve dependencies
  LoggerSvc := FContainer.Resolve('logger.file');
  if not Supports(LoggerSvc, ILogger, Logger) then
    raise Exception.Create('Logger service does not implement ILogger');
    
  CalcSvc := FContainer.Resolve('calc.default');
  if not Supports(CalcSvc, ICalculationService, Calculator) then
    raise Exception.Create('Calculator service does not implement ICalculationService');

  // Create service with injected dependencies
  Result := TOrderService.Create(Logger, Calculator);
end;

class procedure TOrderModule.RegisterServices(const Container: TSimpleContainer);
begin
  Container.Register('order.service', TOrderServiceFactory.Create(Container));
end;

end.