program Demo;

{$mode objfpc}
{$H+}{$J-}

uses
  SysUtils, AppContainer, LoggerIntf, Loggers, CalculationService;

{=================================================================
  5️⃣  APPLICATION – wiring, resolving, using
  =================================================================}
var
  Container : TAppContainer;
  Service   : IInterface;
  Logger    : ILogger;
  Calc      : ICalculationService;
begin
  Container := TAppContainer.Create;
  try
    {--- Register the logger factories -------------------------}
    TLoggerModule.RegisterServices(Container, 'demo.log');
    TCalculationModule.RegisterServices(Container, 'logger.console');

    {--- Resolve console logger and use it --------------------}
    Service := Container.Resolve('logger.console');
    if not Supports(Service, ILogger, Logger) then
      raise Exception.Create('Resolved service does not implement ILogger');
    Logger.Log('Dependency-Injection container works with console logger.');

    {--- Resolve file logger and use it -----------------------}
    Service := Container.Resolve('logger.file');
    if not Supports(Service, ILogger, Logger) then
      raise Exception.Create('Resolved service does not implement ILogger');
    Logger.Log('Dependency-Injection container works with file logger.');

    {--- Resolve calculator service and use it -----------------}
    Service := Container.Resolve('calc.default');
    if not Supports(Service, ICalculationService, Calc) then
      raise Exception.Create('Resolved service does not implement ICalculationService');
    WriteLn('2 + 3 = ', Calc.Add(2, 3));
    WriteLn('4 * 5 = ', Calc.Multiply(4, 5));

    WriteLn('Wrote file log entry to demo.log');

  finally
    Container.Free;
  end;
end.