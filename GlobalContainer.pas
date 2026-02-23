unit GlobalContainer;

{$mode objfpc}
{$H+}

interface

uses
  SimpleContainer, LoggerIntf, Loggers, CalculationService;

type
  TGlobalContainer = class
  private
    class var FInstance: TSimpleContainer;
    class constructor Create;
    class destructor Destroy;
  public
    class function Instance: TSimpleContainer;
    class procedure Initialize(const ALogFileName: string = 'demo.log');
  end;

implementation

class constructor TGlobalContainer.Create;
begin
  FInstance := TSimpleContainer.Create;
end;

class destructor TGlobalContainer.Destroy;
begin
  FInstance.Free;
end;

class function TGlobalContainer.Instance: TSimpleContainer;
begin
  Result := FInstance;
end;

class procedure TGlobalContainer.Initialize(const ALogFileName: string);
begin
  // Register all modules
  TLoggerModule.RegisterServices(FInstance, ALogFileName);
  TCalculationModule.RegisterServices(FInstance, 'logger.console');
end;

end.