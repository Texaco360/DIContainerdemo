unit LoggerIntf;

{$mode objfpc}
{$H+}

interface

type
  ILogger = interface
    ['{F0A5E2C8-2B6D-4E73-9C5A-9F2A6D5E7A1B}']
    procedure Log(const Msg: string);
  end;

implementation

end.
