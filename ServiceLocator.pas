unit ServiceLocator;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, SimpleContainer, LoggerIntf, CalculationService;

type
  TServiceLocator = class
  private
    class var FContainer: TSimpleContainer;
  public
    class procedure SetContainer(const AContainer: TSimpleContainer);
    
    // Typed service accessors
    class function GetLogger(const LoggerType: string = 'logger.console'): ILogger;
    class function GetCalculationService: ICalculationService;
    
    // Generic resolver
    class function Resolve<T: IInterface>(const ServiceKey: string): T;
  end;

implementation

class procedure TServiceLocator.SetContainer(const AContainer: TSimpleContainer);
begin
  FContainer := AContainer;
end;

class function TServiceLocator.GetLogger(const LoggerType: string): ILogger;
var
  Service: IInterface;
begin
  if not Assigned(FContainer) then
    raise Exception.Create('Container not set. Call SetContainer first.');
    
  Service := FContainer.Resolve(LoggerType);
  if not Supports(Service, ILogger, Result) then
    raise Exception.CreateFmt('Service "%s" does not implement ILogger', [LoggerType]);
end;

class function TServiceLocator.GetCalculationService: ICalculationService;
var
  Service: IInterface;
begin
  if not Assigned(FContainer) then
    raise Exception.Create('Container not set. Call SetContainer first.');
    
  Service := FContainer.Resolve('calc.default');
  if not Supports(Service, ICalculationService, Result) then
    raise Exception.Create('Calculation service does not implement ICalculationService');
end;

class function TServiceLocator.Resolve<T>(const ServiceKey: string): T;
var
  Service: IInterface;
begin
  if not Assigned(FContainer) then
    raise Exception.Create('Container not set. Call SetContainer first.');
    
  Service := FContainer.Resolve(ServiceKey);
  if not Supports(Service, T, Result) then
    raise Exception.CreateFmt('Service "%s" does not implement the requested interface', [ServiceKey]);
end;

end.