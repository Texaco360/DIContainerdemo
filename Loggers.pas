unit Loggers;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, LoggerIntf;

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

function CreateConsoleLogger: IInterface;
function CreateFileLogger: IInterface;

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

function CreateConsoleLogger: IInterface;
begin
  Result := TConsoleLogger.Create;
end;

function CreateFileLogger: IInterface;
begin
  Result := TFileLogger.Create('demo.log');
end;

end.
