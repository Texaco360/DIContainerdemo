program CalculationService_FakeLoggerTest;

{$mode objfpc}
{$H+}

uses
  SysUtils, Classes, AppContainer, LoggerIntf, CalculationService;

type
  IFakeLogger = interface
    ['{A9F4D3B7-6A47-4C62-87A3-57AF31343EF5}']
    function MessageCount: Integer;
    function LastMessage: string;
  end;

  TFakeLogger = class(TInterfacedObject, ILogger, IFakeLogger)
  private
    FMessages: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Log(const Msg: string);
    function MessageCount: Integer;
    function LastMessage: string;
  end;

  TFakeLoggerFactory = class(TInterfacedObject, IServiceFactory)
  private
    FInstance: IFakeLogger;
  public
    function CreateService: IInterface;
  end;

procedure AssertEquals(const TestName: string; const Expected, Actual: Integer); overload;
begin
  if Expected <> Actual then
    raise Exception.CreateFmt('%s failed. Expected %d, got %d', [TestName, Expected, Actual]);
end;

procedure AssertEquals(const TestName: string; const Expected, Actual: string); overload;
begin
  if Expected <> Actual then
    raise Exception.CreateFmt('%s failed. Expected "%s", got "%s"', [TestName, Expected, Actual]);
end;

constructor TFakeLogger.Create;
begin
  inherited Create;
  FMessages := TStringList.Create;
end;

destructor TFakeLogger.Destroy;
begin
  FMessages.Free;
  inherited Destroy;
end;

procedure TFakeLogger.Log(const Msg: string);
begin
  FMessages.Add(Msg);
end;

function TFakeLogger.MessageCount: Integer;
begin
  Result := FMessages.Count;
end;

function TFakeLogger.LastMessage: string;
begin
  if FMessages.Count = 0 then
    Exit('');
  Result := FMessages[FMessages.Count - 1];
end;

function TFakeLoggerFactory.CreateService: IInterface;
begin
  if not Assigned(FInstance) then
    FInstance := TFakeLogger.Create;
  Result := FInstance;
end;

var
  Container: TAppContainer;
  Service: IInterface;
  Calc: ICalculationService;
  FakeLogger: IFakeLogger;
  Value: Integer;
begin
  Container := TAppContainer.Create;
  try
    Container.Register('logger.fake', TFakeLoggerFactory.Create);
    TCalculationModule.RegisterServices(Container, 'logger.fake');

    Service := Container.Resolve('calc.default');
    if not Supports(Service, ICalculationService, Calc) then
      raise Exception.Create('Resolved service does not implement ICalculationService');

    Value := Calc.Add(2, 3);
    AssertEquals('Add result', 5, Value);

    Value := Calc.Multiply(4, 6);
    AssertEquals('Multiply result', 24, Value);

    Service := Container.Resolve('logger.fake');
    if not Supports(Service, IFakeLogger, FakeLogger) then
      raise Exception.Create('Resolved logger does not implement IFakeLogger');

    AssertEquals('Log count', 2, FakeLogger.MessageCount);
    AssertEquals('Last log entry', 'Multiply(4, 6) = 24', FakeLogger.LastMessage);

    WriteLn('All CalculationService fake-logger tests passed.');
  finally
    Container.Free;
  end;
end.
