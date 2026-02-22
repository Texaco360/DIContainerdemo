program ContainerDemo;

uses
  SysUtils, Classes;

{$mode objfpc}   // <<< THIS LINE MUST BE FIRST
{$H+}

type
  TFactory = reference to function: IInterface;

begin
end.



