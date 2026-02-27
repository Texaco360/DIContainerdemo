unit NumPascal;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, Math;

type
  { Forward declarations }
  TNDArray = class;
  
  { Data types }
  TNumPyFloat = Double;
  TNumPyFloatArray = array of TNumPyFloat;
  TNumPyFloat2DArray = array of array of TNumPyFloat;
  TShape = array of Integer;
  TIndices = array of Integer;
  TAxis = Integer;
  
  { N-dimensional array class - core of the library }
  TNDArray = class
  private
    FData: TNumPyFloatArray;
    FShape: TShape;
    FSize: Integer;
    
    function GetFlatIndex(const Indices: TIndices): Integer;
    function LinearIndexToMultiIndex(FlatIndex: Integer): TShape;
    procedure ValidateIndices(const Indices: TIndices);
    function GetItem(const Indices: TIndices): TNumPyFloat;
    procedure SetItem(const Indices: TIndices; const Value: TNumPyFloat);
    function GetNDim: Integer;
    
  public
    constructor Create(const AShape: TIndices); overload;
    constructor Create(const AData: TNumPyFloatArray; const AShape: TIndices); overload;
    constructor CreateFromArray1D(const AData: TNumPyFloatArray);
    constructor CreateFromArray2D(const AData: TNumPyFloat2DArray);
    
    destructor Destroy; override;
    
    { Properties }
    property Items[const Indices: TIndices]: TNumPyFloat read GetItem write SetItem; default;
    property Shape: TShape read FShape;
    property Size: Integer read FSize;
    property NDim: Integer read GetNDim;
    
    { Core methods }
    function Copy: TNDArray;
    function Reshape(const NewShape: TIndices): TNDArray;
    function Flatten: TNDArray;
    function Transpose: TNDArray;
    
    { Array operations }
    function Add(const Other: TNDArray): TNDArray; overload;
    function Add(const Scalar: TNumPyFloat): TNDArray; overload;
    function Subtract(const Other: TNDArray): TNDArray; overload;
    function Subtract(const Scalar: TNumPyFloat): TNDArray; overload;
    function Multiply(const Other: TNDArray): TNDArray; overload;
    function Multiply(const Scalar: TNumPyFloat): TNDArray; overload;
    function Divide(const Other: TNDArray): TNDArray; overload;
    function Divide(const Scalar: TNumPyFloat): TNDArray; overload;
    
    { Concatenation operations }
    function HStack(const Other: TNDArray): TNDArray;  // Horizontal concatenation
    function VStack(const Other: TNDArray): TNDArray;  // Vertical concatenation
    
    { Mathematical functions }
    function Sin: TNDArray;
    function Cos: TNDArray;
    function Exp: TNDArray;
    function Log: TNDArray;
    function Sqrt: TNDArray;
    function Power(const Exponent: TNumPyFloat): TNDArray;
    
    { Statistical functions }
    function Mean: TNumPyFloat;
    function Sum: TNumPyFloat;
    function Min: TNumPyFloat;
    function Max: TNumPyFloat;
    function Std: TNumPyFloat;
    function Variance: TNumPyFloat;
    
    { Utility methods }
    procedure Print;
    function ToString: string; override;
  end;

{ Array creation functions }
function np_zeros(const Shape: TIndices): TNDArray;
function np_ones(const Shape: TIndices): TNDArray;
function np_full(const Shape: TIndices; FillValue: TNumPyFloat): TNDArray;
function np_eye(N: Integer): TNDArray;
function np_linspace(Start, Stop: TNumPyFloat; Num: Integer; EndPoint: Boolean = True): TNDArray;
function np_arange(Start, Stop, Step: TNumPyFloat): TNDArray; overload;
function np_arange(Stop: TNumPyFloat): TNDArray; overload;
function np_random_rand(const Shape: TIndices): TNDArray;
function np_array(const Data: TNumPyFloatArray): TNDArray; overload;
function np_array(const Data: TNumPyFloat2DArray): TNDArray; overload;

{ Mathematical functions }
function np_sin(const Arr: TNDArray): TNDArray;
function np_cos(const Arr: TNDArray): TNDArray;
function np_exp(const Arr: TNDArray): TNDArray;
function np_log(const Arr: TNDArray): TNDArray;
function np_sqrt(const Arr: TNDArray): TNDArray;

{ Concatenation functions }
function np_hstack(const A, B: TNDArray): TNDArray;
function np_vstack(const A, B: TNDArray): TNDArray;
function np_concatenate(const A, B: TNDArray; Axis: Integer): TNDArray;

{ Utility functions }
procedure np_print(const Arr: TNDArray);

implementation

{ TNDArray }

constructor TNDArray.Create(const AShape: TIndices);
var
  i: Integer;
begin
  inherited Create;
  SetLength(FShape, Length(AShape));
  FSize := 1;
  
  for i := 0 to High(AShape) do
  begin
    FShape[i] := AShape[i];
    FSize := FSize * AShape[i];
  end;
  
  SetLength(FData, FSize);
  // Initialize to zero
  for i := 0 to FSize - 1 do
    FData[i] := 0.0;
end;

constructor TNDArray.Create(const AData: TNumPyFloatArray; const AShape: TIndices);
var
  i, ExpectedSize: Integer;
begin
  inherited Create;
  
  ExpectedSize := 1;
  for i := 0 to High(AShape) do
    ExpectedSize := ExpectedSize * AShape[i];
    
  if Length(AData) <> ExpectedSize then
    raise Exception.Create('Data length does not match shape');
  
  SetLength(FShape, Length(AShape));
  for i := 0 to High(AShape) do
    FShape[i] := AShape[i];
    
  FSize := Length(AData);
  SetLength(FData, FSize);
  
  for i := 0 to FSize - 1 do
    FData[i] := AData[i];
end;

constructor TNDArray.CreateFromArray1D(const AData: TNumPyFloatArray);
var
  i: Integer;
begin
  inherited Create;
  SetLength(FShape, 1);
  FShape[0] := Length(AData);
  FSize := Length(AData);
  
  SetLength(FData, FSize);
  for i := 0 to FSize - 1 do
    FData[i] := AData[i];
end;

constructor TNDArray.CreateFromArray2D(const AData: TNumPyFloat2DArray);
var
  i, j: Integer;
  FlatIndex: Integer;
begin
  inherited Create;
  SetLength(FShape, 2);
  FShape[0] := Length(AData);
  FShape[1] := Length(AData[0]);
  FSize := FShape[0] * FShape[1];
  
  SetLength(FData, FSize);
  FlatIndex := 0;
  
  for i := 0 to High(AData) do
    for j := 0 to High(AData[i]) do
    begin
      FData[FlatIndex] := AData[i][j];
      Inc(FlatIndex);
    end;
end;

destructor TNDArray.Destroy;
begin
  SetLength(FData, 0);
  SetLength(FShape, 0);
  inherited Destroy;
end;

function TNDArray.GetFlatIndex(const Indices: TIndices): Integer;
var
  i, Multiplier: Integer;
begin
  ValidateIndices(Indices);
  Result := 0;
  Multiplier := 1;
  
  for i := High(FShape) downto 0 do
  begin
    Result := Result + Indices[i] * Multiplier;
    Multiplier := Multiplier * FShape[i];
  end;
end;

function TNDArray.LinearIndexToMultiIndex(FlatIndex: Integer): TShape;
var
  i, Temp: Integer;
begin
  SetLength(Result, Length(FShape));
  Temp := FlatIndex;
  
  for i := High(FShape) downto 0 do
  begin
    Result[i] := Temp mod FShape[i];
    Temp := Temp div FShape[i];
  end;
end;

procedure TNDArray.ValidateIndices(const Indices: TIndices);
var
  i: Integer;
begin
  if Length(Indices) <> Length(FShape) then
    raise Exception.Create('Number of indices must match number of dimensions');
    
  for i := 0 to High(Indices) do
    if (Indices[i] < 0) or (Indices[i] >= FShape[i]) then
      raise Exception.CreateFmt('Index %d is out of bounds for dimension %d', [Indices[i], i]);
end;

function TNDArray.GetItem(const Indices: TIndices): TNumPyFloat;
begin
  Result := FData[GetFlatIndex(Indices)];
end;

procedure TNDArray.SetItem(const Indices: TIndices; const Value: TNumPyFloat);
begin
  FData[GetFlatIndex(Indices)] := Value;
end;

function TNDArray.GetNDim: Integer;
begin
  Result := Length(FShape);
end;

function TNDArray.Copy: TNDArray;
begin
  Result := TNDArray.Create(FData, FShape);
end;

function TNDArray.Add(const Other: TNDArray): TNDArray;
var
  i: Integer;
begin
  if FSize <> Other.FSize then
    raise Exception.Create('Arrays must have the same size for element-wise operations');
    
  Result := TNDArray.Create(FShape);
  for i := 0 to FSize - 1 do
    Result.FData[i] := FData[i] + Other.FData[i];
end;

function TNDArray.Add(const Scalar: TNumPyFloat): TNDArray;
var
  i: Integer;
begin
  Result := TNDArray.Create(FShape);
  for i := 0 to FSize - 1 do
    Result.FData[i] := FData[i] + Scalar;
end;

function TNDArray.Multiply(const Scalar: TNumPyFloat): TNDArray;
var
  i: Integer;
begin
  Result := TNDArray.Create(FShape);
  for i := 0 to FSize - 1 do
    Result.FData[i] := FData[i] * Scalar;
end;

function TNDArray.Sin: TNDArray;
var
  i: Integer;
begin
  Result := TNDArray.Create(FShape);
  for i := 0 to FSize - 1 do
    Result.FData[i] := System.Sin(FData[i]);
end;

function TNDArray.Mean: TNumPyFloat;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FSize - 1 do
    Result := Result + FData[i];
  Result := Result / FSize;
end;

function TNDArray.Sum: TNumPyFloat;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FSize - 1 do
    Result := Result + FData[i];
end;

procedure TNDArray.Print;
begin
  WriteLn(ToString);
end;

function TNDArray.ToString: string;
var
  i, MaxShow: Integer;
begin
  Result := 'Array(shape=(';
  for i := 0 to High(FShape) do
  begin
    Result := Result + IntToStr(FShape[i]);
    if i < High(FShape) then Result := Result + ', ';
  end;
  Result := Result + '), data=[';
  
  MaxShow := FSize - 1;
  if MaxShow > 9 then MaxShow := 9;  // Show first 10 elements
  
  for i := 0 to MaxShow do
  begin
    Result := Result + Format('%.3f', [FData[i]]);
    if i < MaxShow then Result := Result + ', ';
  end;
  
  if FSize > 10 then Result := Result + '...';
  Result := Result + '])';
end;

{ Stub implementations for remaining methods }
function TNDArray.Subtract(const Other: TNDArray): TNDArray;
var i: Integer;
begin
  Result := TNDArray.Create(FShape);
  for i := 0 to FSize - 1 do
    Result.FData[i] := FData[i] - Other.FData[i];
end;

function TNDArray.Subtract(const Scalar: TNumPyFloat): TNDArray;
var i: Integer;
begin
  Result := TNDArray.Create(FShape);
  for i := 0 to FSize - 1 do
    Result.FData[i] := FData[i] - Scalar;
end;

function TNDArray.Multiply(const Other: TNDArray): TNDArray;
var i: Integer;
begin
  Result := TNDArray.Create(FShape);
  for i := 0 to FSize - 1 do
    Result.FData[i] := FData[i] * Other.FData[i];
end;

function TNDArray.Divide(const Other: TNDArray): TNDArray;
var i: Integer;
begin
  Result := TNDArray.Create(FShape);
  for i := 0 to FSize - 1 do
    Result.FData[i] := FData[i] / Other.FData[i];
end;

function TNDArray.Divide(const Scalar: TNumPyFloat): TNDArray;
var i: Integer;
begin
  Result := TNDArray.Create(FShape);
  for i := 0 to FSize - 1 do
    Result.FData[i] := FData[i] / Scalar;
end;

function TNDArray.Cos: TNDArray;
var i: Integer;
begin
  Result := TNDArray.Create(FShape);
  for i := 0 to FSize - 1 do
    Result.FData[i] := System.Cos(FData[i]);
end;

function TNDArray.Exp: TNDArray;
var i: Integer;
begin
  Result := TNDArray.Create(FShape);
  for i := 0 to FSize - 1 do
    Result.FData[i] := System.Exp(FData[i]);
end;

function TNDArray.Log: TNDArray;
var i: Integer;
begin
  Result := TNDArray.Create(FShape);
  for i := 0 to FSize - 1 do
    Result.FData[i] := System.Ln(FData[i]);
end;

function TNDArray.Sqrt: TNDArray;
var i: Integer;
begin
  Result := TNDArray.Create(FShape);
  for i := 0 to FSize - 1 do
    Result.FData[i] := System.Sqrt(FData[i]);
end;

function TNDArray.Power(const Exponent: TNumPyFloat): TNDArray;
var i: Integer;
begin
  Result := TNDArray.Create(FShape);
  for i := 0 to FSize - 1 do
    Result.FData[i] := Math.Power(FData[i], Exponent);
end;

function TNDArray.Min: TNumPyFloat;
var i: Integer;
begin
  Result := FData[0];
  for i := 1 to FSize - 1 do
    if FData[i] < Result then
      Result := FData[i];
end;

function TNDArray.Max: TNumPyFloat;
var i: Integer;
begin
  Result := FData[0];
  for i := 1 to FSize - 1 do
    if FData[i] > Result then
      Result := FData[i];
end;

function TNDArray.Std: TNumPyFloat;
var
  MeanVal: TNumPyFloat;
  i: Integer;
  VarValue: TNumPyFloat;
begin
  MeanVal := Mean();
  VarValue := 0;
  
  for i := 0 to FSize - 1 do
    VarValue := VarValue + Sqr(FData[i] - MeanVal);
    
  VarValue := VarValue / FSize;
  Result := System.Sqrt(VarValue);
end;

function TNDArray.Variance: TNumPyFloat;
var
  MeanVal: TNumPyFloat;
  i: Integer;
begin
  MeanVal := Mean();
  Result := 0;
  
  for i := 0 to FSize - 1 do
    Result := Result + Sqr(FData[i] - MeanVal);
    
  Result := Result / FSize;
end;

function TNDArray.Reshape(const NewShape: TIndices): TNDArray;
var
  i, NewSize: Integer;
begin
  NewSize := 1;
  for i := 0 to High(NewShape) do
    NewSize := NewSize * NewShape[i];
    
  if NewSize <> FSize then
    raise Exception.Create('Cannot reshape array: total size must remain the same');
    
  Result := TNDArray.Create(FData, NewShape);
end;

function TNDArray.Flatten: TNDArray;
begin
  Result := TNDArray.Create(FData, [FSize]);
end;

function TNDArray.Transpose: TNDArray;
var
  i, j: Integer;
  NewShape: TShape;
begin
  if NDim <> 2 then
    raise Exception.Create('Transpose only implemented for 2D arrays');
    
  SetLength(NewShape, 2);
  NewShape[0] := FShape[1];
  NewShape[1] := FShape[0];
  
  Result := TNDArray.Create(NewShape);
  
  for i := 0 to FShape[0] - 1 do
    for j := 0 to FShape[1] - 1 do
      Result[[j, i]] := Self[[i, j]];
end;

function TNDArray.HStack(const Other: TNDArray): TNDArray;
var
  i, j: Integer;
  NewShape: TShape;
begin
  // Both arrays must be 2D and have same number of rows
  if (NDim <> 2) or (Other.NDim <> 2) then
    raise Exception.Create('HStack requires 2D arrays');
    
  if FShape[0] <> Other.FShape[0] then
    raise Exception.Create('Arrays must have same number of rows for horizontal concatenation');
  
  // Create result array: same rows, combined columns  
  SetLength(NewShape, 2);
  NewShape[0] := FShape[0];  // Same rows
  NewShape[1] := FShape[1] + Other.FShape[1];  // Combined columns
  
  Result := TNDArray.Create(NewShape);
  
  // Copy data from first array
  for i := 0 to FShape[0] - 1 do
    for j := 0 to FShape[1] - 1 do
      Result[[i, j]] := Self[[i, j]];
  
  // Copy data from second array (offset by first array's column count)
  for i := 0 to Other.FShape[0] - 1 do
    for j := 0 to Other.FShape[1] - 1 do
      Result[[i, j + FShape[1]]] := Other[[i, j]];
end;

function TNDArray.VStack(const Other: TNDArray): TNDArray;
var
  i, j: Integer;
  NewShape: TShape;
begin
  // Both arrays must be 2D and have same number of columns
  if (NDim <> 2) or (Other.NDim <> 2) then
    raise Exception.Create('VStack requires 2D arrays');
    
  if FShape[1] <> Other.FShape[1] then
    raise Exception.Create('Arrays must have same number of columns for vertical concatenation');
  
  // Create result array: combined rows, same columns
  SetLength(NewShape, 2);
  NewShape[0] := FShape[0] + Other.FShape[0];  // Combined rows
  NewShape[1] := FShape[1];  // Same columns
  
  Result := TNDArray.Create(NewShape);
  
  // Copy data from first array
  for i := 0 to FShape[0] - 1 do
    for j := 0 to FShape[1] - 1 do
      Result[[i, j]] := Self[[i, j]];
  
  // Copy data from second array (offset by first array's row count)  
  for i := 0 to Other.FShape[0] - 1 do
    for j := 0 to Other.FShape[1] - 1 do
      Result[[i + FShape[0], j]] := Other[[i, j]];
end;

{ Array creation functions }
function np_zeros(const Shape: TIndices): TNDArray;
begin
  Result := TNDArray.Create(Shape);
  // Already initialized to zeros in constructor
end;

function np_ones(const Shape: TIndices): TNDArray;
var
  i: Integer;
begin
  Result := TNDArray.Create(Shape);
  for i := 0 to Result.Size - 1 do
    Result.FData[i] := 1.0;
end;

function np_full(const Shape: TIndices; FillValue: TNumPyFloat): TNDArray;
var
  i: Integer;
begin
  Result := TNDArray.Create(Shape);
  for i := 0 to Result.Size - 1 do
    Result.FData[i] := FillValue;
end;

function np_linspace(Start, Stop: TNumPyFloat; Num: Integer; EndPoint: Boolean = True): TNDArray;
var
  i: Integer;
  Step: TNumPyFloat;
  Data: TNumPyFloatArray;
begin
  SetLength(Data, Num);
  
  if Num <= 1 then
  begin
    if Num = 1 then Data[0] := Start;
  end
  else
  begin
    if EndPoint then
      Step := (Stop - Start) / (Num - 1)
    else
      Step := (Stop - Start) / Num;
    
    for i := 0 to Num - 1 do
      Data[i] := Start + (Step * i);
      
    if EndPoint and (Num > 1) then
      Data[Num - 1] := Stop;
  end;
  
  Result := TNDArray.CreateFromArray1D(Data);
end;

function np_arange(Start, Stop, Step: TNumPyFloat): TNDArray;
var
  Count, i: Integer;
  Current: TNumPyFloat;
  Data: TNumPyFloatArray;
begin
  Count := 0;
  Current := Start;
  
  while ((Step > 0) and (Current < Stop)) or ((Step < 0) and (Current > Stop)) do
  begin
    Inc(Count);
    Current := Current + Step;
  end;
  
  SetLength(Data, Count);
  Current := Start;
  
  for i := 0 to Count - 1 do
  begin
    Data[i] := Current;
    Current := Current + Step;
  end;
  
  Result := TNDArray.CreateFromArray1D(Data);
end;

function np_arange(Stop: TNumPyFloat): TNDArray;
begin
  Result := np_arange(0, Stop, 1);
end;

function np_array(const Data: TNumPyFloatArray): TNDArray;
begin
  Result := TNDArray.CreateFromArray1D(Data);
end;

function np_array(const Data: TNumPyFloat2DArray): TNDArray;
begin
  Result := TNDArray.CreateFromArray2D(Data);
end;

function np_eye(N: Integer): TNDArray;
var
  i: Integer;
begin
  Result := np_zeros([N, N]);
  for i := 0 to N - 1 do
    Result[[i, i]] := 1.0;
end;

function np_random_rand(const Shape: TIndices): TNDArray;
var
  i: Integer;
begin
  Result := TNDArray.Create(Shape);
  Randomize;
  for i := 0 to Result.Size - 1 do
    Result.FData[i] := Random;
end;

{ Mathematical functions }
function np_sin(const Arr: TNDArray): TNDArray;
begin
  Result := Arr.Sin;
end;

function np_cos(const Arr: TNDArray): TNDArray;
begin
  Result := Arr.Cos;
end;

function np_exp(const Arr: TNDArray): TNDArray;
begin
  Result := Arr.Exp;
end;

function np_log(const Arr: TNDArray): TNDArray;
begin
  Result := Arr.Log;
end;

function np_sqrt(const Arr: TNDArray): TNDArray;
begin
  Result := Arr.Sqrt;
end;

{ Concatenation functions }
function np_hstack(const A, B: TNDArray): TNDArray;
begin
  Result := A.HStack(B);
end;

function np_vstack(const A, B: TNDArray): TNDArray;
begin
  Result := A.VStack(B);
end;

function np_concatenate(const A, B: TNDArray; Axis: Integer): TNDArray;
begin
  case Axis of
    0: Result := A.VStack(B);  // Vertical concatenation (along rows)
    1: Result := A.HStack(B);  // Horizontal concatenation (along columns)
  else
    raise Exception.CreateFmt('Invalid axis %d for concatenation. Use 0 (vertical) or 1 (horizontal)', [Axis]);
  end;
end;

procedure np_print(const Arr: TNDArray);
begin
  Arr.Print;
end;

end.