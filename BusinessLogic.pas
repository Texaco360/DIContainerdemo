unit BusinessLogic;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, ServiceLocator, LoggerIntf, CalculationService;

type
  TBusinessLogic = class
  public
    procedure ProcessData;
  end;

implementation

procedure TBusinessLogic.ProcessData;
var
  Logger: ILogger;
  Calc: ICalculationService;
  Result: Integer;
begin
  // Clean, typed access to services
  Logger := TServiceLocator.GetLogger('logger.file');
  Calc := TServiceLocator.GetCalculationService;
  
  Logger.Log('Starting business logic processing...');
  
  Result := Calc.Add(10, 20);
  Logger.Log(Format('Processing result: %d', [Result]));
  
  Logger.Log('Business logic processing completed.');
end;

end.