program ContainerDemo;

{$H+}
{$mode delphi}   // <-- this line is the key
{$H+}

uses
  SysUtils, Classes;

type
  {--- Logger ---------------------------------------------------}
  ILogger = interface
    ['{F0A5E2C8-2B6D-4E73-9C5A-9F2A6D5E7A1B}']
    procedure Log(const Msg: string);
  end;

  {--- Database ------------------------------------------------}
  IDatabase = interface
    ['{C1D4B7A9-5F3E-4A2C-8E6F-2B9C1D3E4F5A}']
    procedure Open;
    procedure Close;
    function  ExecuteSQL(const SQL: string): Boolean;
  end;

  TConsoleLogger = class(TInterfacedObject, ILogger)
  public
    procedure Log(const Msg: string);
  end;

  TFileLogger = class(TInterfacedObject, ILogger)
  private
    FFileName: string;
    FStream: TFileStream;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    procedure Log(const Msg: string);
  end;


  {--- Logger implementations ------------------------------------}
procedure TConsoleLogger.Log(const Msg: string);
begin
  WriteLn('[Console] ', Msg);
end;

constructor TFileLogger.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  // Append mode – creates the file if it does not exist
  FStream := TFileStream.Create(FFileName, fmOpenReadWrite or fmShareDenyWrite);
  FStream.Seek(0, soEnd); // start writing at the end
end;

destructor TFileLogger.Destroy;
begin
  FStream.Free;
  inherited;
end;

procedure TFileLogger.Log(const Msg: string);
var
  Line: AnsiString;
begin
  Line := AnsiString('[' + DateTimeToStr(Now) + '] ' + Msg + sLineBreak);
  FStream.WriteBuffer(Line[1], Length(Line));
end;

{--- Database implementations ---------------------------------}
type
  TSQLiteDatabase = class(TInterfacedObject, IDatabase)
  private
    FFileName: string;
  public
    constructor Create(const AFileName: string);
    procedure Open;
    procedure Close;
    function  ExecuteSQL(const SQL: string): Boolean;
  end;

  TInMemoryDatabase = class(TInterfacedObject, IDatabase)
  public
    procedure Open;
    procedure Close;
    function  ExecuteSQL(const SQL: string): Boolean;
  end;

constructor TSQLiteDatabase.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
end;

procedure TSQLiteDatabase.Open;
begin
  // Here you would normally load the SQLite DLL and open the file.
  // For brevity we just write a trace line.
  WriteLn('Opening SQLite DB: ', FFileName);
end;

procedure TSQLiteDatabase.Close;
begin
  WriteLn('Closing SQLite DB');
end;

function TSQLiteDatabase.ExecuteSQL(const SQL: string): Boolean;
begin
  WriteLn('SQLite exec: ', SQL);
  Result := True; // pretend success
end;

procedure TInMemoryDatabase.Open;
begin
  WriteLn('Opening In‑Memory DB (test stub)');
end;

procedure TInMemoryDatabase.Close;
begin
  WriteLn('Closing In‑Memory DB');
end;

function TInMemoryDatabase.ExecuteSQL(const SQL: string): Boolean;
begin
  WriteLn('In‑Memory exec (no persistence): ', SQL);
  Result := True;
end;


{--- The container ---------------------------------}
type
  TFactory = reference to function: IInterface;

  TAppContainer = class
  private
    FFactories: TStringList; // key -> factory
  public
    constructor Create;
    destructor Destroy; override;
    procedure Register(const Key: string; const Factory: TFactory);
    function Resolve<T: IInterface>(const Key: string): T;
  end;

constructor TAppContainer.Create;
begin
  inherited;
  FFactories := TStringList.Create;
  FFactories.CaseSensitive := False;
end;

destructor TAppContainer.Destroy;
begin
  FFactories.Free;
  inherited;
end;

procedure TAppContainer.Register(const Key: string; const Factory: TFactory);
begin
  // Overwrite if already registered
  FFactories.Values[Key] := '';
  FFactories.AddObject(Key, TObject(@Factory));
end;

function TAppContainer.Resolve<T>(const Key: string): T;
var
  idx: Integer;
  factoryRef: ^TFactory;
begin
  idx := FFactories.IndexOf(Key);
  if idx < 0 then
    raise Exception.CreateFmt('No registration for "%s"', [Key]);

  factoryRef := FFactories.Objects[idx];
  Result := T(factoryRef^());
end;

begin

end.

