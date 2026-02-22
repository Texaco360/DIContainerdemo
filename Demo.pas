program Demo;

{$mode objfpc}
{$H+}

uses
  SysUtils, SimpleContainer, LoggerIntf, Loggers;

{=================================================================
  5️⃣  APPLICATION – wiring, resolving, using
  =================================================================}
var
  Container : TSimpleContainer;
  Service   : IInterface;
  Logger    : ILogger;
begin
  Container := TSimpleContainer.Create;
  try
    {--- Register the logger factories -------------------------}
    Container.Register('logger.console', @CreateConsoleLogger);
    Container.Register('logger.file', @CreateFileLogger);

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

    WriteLn('Wrote file log entry to demo.log');

  finally
    Container.Free;
  end;
end.