program TestLifetimeFeatures;

{$mode objfpc}
{$H+}

uses
  SysUtils, ContainerLifetimeTests;

var
  Tests: TContainerLifetimeTests;
begin
  Tests := TContainerLifetimeTests.Create;
  try
    Tests.RunAllTests;
  finally
    Tests.Free;
  end;
  
  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.