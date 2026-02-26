program EnumHelperDemo;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

uses
  Classes, SysUtils;

type
 // Our enum (simular to Laravel enum cases)

 TStatus = (stDraft, stPublished, stArchived);

 // Helper adds methods to the enum
    TStatusHelper = type helper for TStatus
    public 
        function Value: string;
        function LabelText: string;

        class function FromValue(const AValue: string): TStatus; static;
        class function TryFromValue(const AValue: string; out AStatus: TStatus): Boolean; static;
    end;

{ TStatusHelper }
function TStatusHelper.LabelText: string;
begin
  case Self of
    stDraft: Result := 'Draft';
    stPublished: Result := 'Published';
    stArchived: Result := 'Archived';
  end;
end;

function TStatusHelper.Value: string;
begin
  case Self of
    stDraft: Result := 'draft';
    stPublished: Result := 'published';
    stArchived: Result := 'archived';
  end;
end;

class function TStatusHelper.TryFromValue(const AValue: string; out AStatus: TStatus): Boolean;
begin
  Result := True;
  if AValue = 'draft' then
    AStatus := stDraft
  else if AValue = 'published' then
    AStatus := stPublished
  else if AValue = 'archived' then
    AStatus := stArchived
  else
    Result := False;
end;

class function TStatusHelper.FromValue(const AValue: string): TStatus;
begin
  if not TryFromValue(AValue, Result) then
    raise Exception.CreateFmt('Invalid status value: %s', [AValue]);
end;

var
  Status: TStatus;
begin
  // Example usage
  Status := stPublished;
  WriteLn('Status Value: ', Status.Value); // Output: published
  WriteLn('Status Label: ', Status.LabelText); // Output: Published

    // Convert from string to enum
    if TStatus.TryFromValue('archived', Status) then
      WriteLn('Converted Status: ', Status.LabelText); // Output: Archived
    try
      Status := TStatus.FromValue('invalid'); // This will raise an exception
    except
      on E: Exception do
        WriteLn(E.Message); // Output: Invalid status value: invalid
    end;
end.        
