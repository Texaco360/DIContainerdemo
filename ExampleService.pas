unit ExampleService;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, GlobalContainer, LoggerIntf;

type
  TExampleService = class
  public
    procedure DoSomething;
  end;

implementation

procedure TExampleService.DoSomething;
var
  Service: IInterface;
  Logger: ILogger;
begin
  // Easy access to container from any unit
  Service := TGlobalContainer.Instance.Resolve('logger.console');
  if Supports(Service, ILogger, Logger) then
    Logger.Log('ExampleService is doing something...');
end;

end.