program Demo;

{$mode objfpc}
{$H+}

uses
  SysUtils, Classes;

{=================================================================
  1️⃣  INTERFACES – the abstractions you will inject
  =================================================================}
type
  ILogger = interface
    ['{F0A5E2C8-2B6D-4E73-9C5A-9F2A6D5E7A1B}']
    procedure Log(const Msg: string);
  end;

{=================================================================
  2️⃣  CONCRETE IMPLEMENTATION (a simple console logger)
  =================================================================}
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

{=================================================================
  3️⃣  THE ANONYMOUS‑METHOD FACTORY TYPE
  =================================================================}
type
  // function: IInterface = a callable factory that
  // returns an interface instance (no parameters).
  TFactory = function: IInterface;
  PTFactory = ^TFactory;

{=================================================================
  4️⃣  A MINIMAL SERVICE CONTAINER
  =================================================================}
type
  TSimpleContainer = class
  private
    FKeys  : TStringList;          // stores the string keys
    FFacts : TList;                // stores pointers to TFactory objects
  public
    constructor Create;
    destructor Destroy; override;
    procedure Register(const Key: string; const Fact: TFactory);
    function Resolve(const Key: string): IInterface;
  end;

constructor TSimpleContainer.Create;
begin
  inherited;
  FKeys  := TStringList.Create;
  FKeys.CaseSensitive := False;
  FFacts := TList.Create;
end;

destructor TSimpleContainer.Destroy;
var
  i: Integer;
  pFact: PTFactory;
begin
  for i := 0 to FFacts.Count - 1 do
  begin
    pFact := PTFactory(FFacts.Items[i]);
    Dispose(pFact);
  end;
  FFacts.Free;
  FKeys.Free;
  inherited;
end;

procedure TSimpleContainer.Register(const Key: string; const Fact: TFactory);
var
  pFact: PTFactory;
begin
  // Allocate a heap copy of the anonymous method
  New(pFact);
  pFact^ := Fact;
  FKeys.Add(Key);
  FFacts.Add(pFact);
end;

function TSimpleContainer.Resolve(const Key: string): IInterface;
var
  idx   : Integer;
  pFact : PTFactory;
begin
  idx := FKeys.IndexOf(Key);
  if idx < 0 then
    raise Exception.CreateFmt('No factory registered for key "%s"', [Key]);

  pFact := PTFactory(FFacts.Items[idx]);
  Result := pFact^();   // invoke the factory function
end;

function CreateConsoleLogger: IInterface;
begin
  Result := TConsoleLogger.Create;
end;

function CreateFileLogger: IInterface;
begin
  Result := TFileLogger.Create('demo.log');
end;

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