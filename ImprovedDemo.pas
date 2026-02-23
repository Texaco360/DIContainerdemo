program ImprovedDemo;

{$mode objfpc}
{$H+}

uses
  SysUtils, 
  SimpleContainer, LoggerIntf, Loggers, CalculationService,
  GlobalContainer, ServiceLocator, BusinessLogic, OrderService, ExampleService;

var
  Container: TSimpleContainer;
  OrderSvc: IInterface;
  Order: IOrderService;
  ExampleSvc: TExampleService;
  BusinessLogicSvc: TBusinessLogic;
begin
  WriteLn('=== Dependency Injection Demo - Multiple Access Patterns ===');
  WriteLn;

  Container := TSimpleContainer.Create;
  try
    {--- Register all services -------------------------}
    TLoggerModule.RegisterServices(Container, 'demo.log');
    TCalculationModule.RegisterServices(Container, 'logger.console');
    TOrderModule.RegisterServices(Container);

    WriteLn('1. Using Global Container Pattern:');
    TGlobalContainer.Initialize('demo.log');
    ExampleSvc := TExampleService.Create;
    try
      ExampleSvc.DoSomething;
    finally
      ExampleSvc.Free;
    end;
    WriteLn;

    WriteLn('2. Using Service Locator Pattern:');
    TServiceLocator.SetContainer(Container);
    BusinessLogicSvc := TBusinessLogic.Create;
    try
      BusinessLogicSvc.ProcessData;
    finally
      BusinessLogicSvc.Free;
    end;
    WriteLn;

    WriteLn('3. Using Constructor Dependency Injection:');
    OrderSvc := Container.Resolve('order.service');
    if Supports(OrderSvc, IOrderService, Order) then
      Order.ProcessOrder(12345, 3, 29.95);
    WriteLn;

    WriteLn('All patterns demonstrated successfully!');
    WriteLn('Check demo.log for file logger output.');

  finally
    Container.Free;
  end;
end.