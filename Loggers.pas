unit Loggers;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, LoggerIntf, SimpleContainer;

type
  TConsoleLogger = class(TInterfacedObject, ILogger)
  public
    procedure Log(const Msg: string);
  end;

  TFileLogger = class(TInterfacedObject, ILogger)
  private
    FFileName: string;
  public
    constructor Create(const AFileName: string);
    procedure Log(const Msg: string);
  end;

  TConsoleLoggerFactory = class(TInterfacedObject, IServiceFactory)
  public
    function CreateService: IInterface;
  end;

  TFileLoggerFactory = class(TInterfacedObject, IServiceFactory)
  private
    FFileName: string;
  public
    constructor Create(const AFileName: string);
    function CreateService: IInterface;
  end;

  TLoggerModule = class
  public
    class procedure RegisterServices(const Container: TAppContainer; const AFileName: string = 'demo.log');
  end;

implementation

procedure TConsoleLogger.Log(const Msg: string);
begin
  WriteLn('[Console] ', Msg);
end;

constructor TFileLogger.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
end;

procedure TFileLogger.Log(const Msg: string);
var
  LogFile: TextFile;
begin
  AssignFile(LogFile, FFileName);
  if FileExists(FFileName) then
    Append(LogFile)
  else
    Rewrite(LogFile);
  try
    WriteLn(LogFile, '[File] ', Msg);
  finally
    CloseFile(LogFile);
  end;
end;

function TConsoleLoggerFactory.CreateService: IInterface;
begin
  Result := TConsoleLogger.Create;
end;

constructor TFileLoggerFactory.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
end;

function TFileLoggerFactory.CreateService: IInterface;
begin
  Result := TFileLogger.Create(FFileName);
end;

class procedure TLoggerModule.RegisterServices(const Container: TAppContainer; const AFileName: string);
begin
  Container.Register('logger.console', TConsoleLoggerFactory.Create);
  Container.Register('logger.file', TFileLoggerFactory.Create(AFileName));
end;

end.
