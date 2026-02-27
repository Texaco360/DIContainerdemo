program NumPascalDemo;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, NumPascal;

procedure DemoArrayCreation;
var
  A, B, C: TNDArray;
begin
  WriteLn('=== Array Creation ===');
  
  // Create arrays using different methods
  A := np_array([1.0, 2.0, 3.0, 4.0, 5.0]);
  WriteLn('1D array: ', A.ToString);
  
  B := np_zeros([2, 3]);
  WriteLn('Zeros 2x3: ', B.ToString);
  
  C := np_ones([3, 2]);
  WriteLn('Ones 3x2: ', C.ToString);
  
  A.Free;
  B.Free;  
  C.Free;
  WriteLn;
end;

procedure DemoLinspace;
var
  A: TNDArray;
begin
  WriteLn('=== Linspace Demo ===');
  
  A := np_linspace(0, 10, 11);
  WriteLn('Linspace(0, 10, 11): ', A.ToString);
  A.Free;
  
  A := np_linspace(-1, 1, 5);
  WriteLn('Linspace(-1, 1, 5): ', A.ToString);
  A.Free;
  WriteLn;
end;

procedure DemoArithmetic;
var
  A, B, Result: TNDArray;
begin
  WriteLn('=== Arithmetic Operations ===');
  
  A := np_array([1.0, 2.0, 3.0, 4.0]);
  B := np_array([5.0, 6.0, 7.0, 8.0]);
  
  WriteLn('A = ', A.ToString);
  WriteLn('B = ', B.ToString);
  
  Result := A.Add(B);
  WriteLn('A + B = ', Result.ToString);
  Result.Free;
  
  Result := A.Multiply(2.0);
  WriteLn('A * 2 = ', Result.ToString);
  Result.Free;
  
  Result := B.Subtract(A);
  WriteLn('B - A = ', Result.ToString);
  Result.Free;
  
  A.Free;
  B.Free;
  WriteLn;
end;

procedure DemoMathFunctions;
var
  A, Result: TNDArray;
begin
  WriteLn('=== Mathematical Functions ===');
  
  A := np_linspace(0, 3.14159/2, 5);
  WriteLn('A = ', A.ToString);
  
  Result := A.Sin;
  WriteLn('sin(A) = ', Result.ToString);
  Result.Free;
  
  Result := A.Cos;  
  WriteLn('cos(A) = ', Result.ToString);
  Result.Free;
  
  A.Free;
  
  A := np_array([1.0, 4.0, 9.0, 16.0]);
  WriteLn('A = ', A.ToString);
  
  Result := A.Sqrt;
  WriteLn('sqrt(A) = ', Result.ToString);
  Result.Free;
  
  Result := A.Power(0.5);
  WriteLn('A^0.5 = ', Result.ToString);
  Result.Free;
  
  A.Free;
  WriteLn;
end;

procedure DemoStatistics;
var
  A: TNDArray;
begin
  WriteLn('=== Statistical Functions ===');
  
  A := np_array([1.0, 2.0, 3.0, 4.0, 5.0]);
  WriteLn('A = ', A.ToString);
  WriteLn('Mean: ', Format('%.3f', [A.Mean()]));
  WriteLn('Sum: ', Format('%.3f', [A.Sum()]));
  WriteLn('Min: ', Format('%.3f', [A.Min]));
  WriteLn('Max: ', Format('%.3f', [A.Max]));
  WriteLn('Std: ', Format('%.3f', [A.Std]));
  WriteLn('Var: ', Format('%.3f', [A.Variance]));
  
  A.Free;
  WriteLn;
end;

procedure Demo2DArrays;
var
  A, B, Transposed: TNDArray;
  Data2D: TNumPyFloat2DArray;
  i, j: Integer;
begin
  WriteLn('=== 2D Arrays ===');
  
  // Create a 2D array
  SetLength(Data2D, 3, 3);
  for i := 0 to 2 do
    for j := 0 to 2 do
      Data2D[i][j] := i * 3 + j + 1;
      
  A := np_array(Data2D);
  WriteLn('3x3 Matrix:');
  WriteLn(A.ToString);
  WriteLn('Shape: (', A.Shape[0], ', ', A.Shape[1], ')');
  WriteLn('NDim: ', A.NDim);
  
  // Test indexing
  WriteLn('A[1,2] = ', Format('%.3f', [A[[1, 2]]]));
  
  // Set a value
  A[[0, 0]] := 99.0;
  WriteLn('After setting A[0,0] = 99:');
  WriteLn(A.ToString);
  
  // Transpose
  Transposed := A.Transpose;
  WriteLn('Transposed:');
  WriteLn(Transposed.ToString);
  
  // Identity matrix
  B := np_eye(3);
  WriteLn('Identity 3x3:');
  WriteLn(B.ToString);
  
  A.Free;
  B.Free;
  Transposed.Free;
  WriteLn;
end;

procedure DemoReshaping;
var
  A, Reshaped, Flattened: TNDArray;
begin
  WriteLn('=== Reshaping ===');
  
  A := np_arange(12);
  WriteLn('Original 1D array: ', A.ToString);
  
  Reshaped := A.Reshape([3, 4]);
  WriteLn('Reshaped to 3x4:');
  WriteLn(Reshaped.ToString);
  
  Flattened := Reshaped.Flatten;
  WriteLn('Flattened back: ', Flattened.ToString);
  
  A.Free;
  Reshaped.Free;
  Flattened.Free;
  WriteLn;
end;

procedure DemoRandomArrays;
var
  A: TNDArray;
begin
  WriteLn('=== Random Arrays ===');
  
  A := np_random_rand([2, 3]);
  WriteLn('Random 2x3 array:');
  WriteLn(A.ToString);
  
  A.Free;
  WriteLn;
end;

procedure DemoConcatenation;
var
  A, B, HStackResult, VStackResult, ConcatResult: TNDArray;
  DataA, DataB: TNumPyFloat2DArray;
  i, j: Integer;
begin
  WriteLn('=== Array Concatenation ===');
  
  // Create first 2x2 matrix
  SetLength(DataA, 2, 2);
  DataA[0][0] := 1; DataA[0][1] := 2;
  DataA[1][0] := 3; DataA[1][1] := 4;
  A := np_array(DataA);
  
  // Create second 2x2 matrix  
  SetLength(DataB, 2, 2);
  DataB[0][0] := 5; DataB[0][1] := 6;
  DataB[1][0] := 7; DataB[1][1] := 8;
  B := np_array(DataB);
  
  WriteLn('Matrix A (2x2):');
  WriteLn(A.ToString);
  WriteLn('Matrix B (2x2):');
  WriteLn(B.ToString);
  WriteLn;
  
  // Horizontal concatenation (side by side) - should result in 2x4
  HStackResult := A.HStack(B);
  WriteLn('Horizontal concatenation A.HStack(B) -> 2x4:');
  WriteLn(HStackResult.ToString);
  WriteLn('Shape: (', HStackResult.Shape[0], ', ', HStackResult.Shape[1], ')');
  WriteLn;
  
  // Vertical concatenation (stacked) - should result in 4x2  
  VStackResult := A.VStack(B);
  WriteLn('Vertical concatenation A.VStack(B) -> 4x2:');
  WriteLn(VStackResult.ToString);
  WriteLn('Shape: (', VStackResult.Shape[0], ', ', VStackResult.Shape[1], ')');
  WriteLn;
  
  // Using np_concatenate with axis parameter
  ConcatResult := np_concatenate(A, B, 1);  // axis=1 means horizontal
  WriteLn('np_concatenate(A, B, axis=1) -> 2x4:');
  WriteLn(ConcatResult.ToString);
  ConcatResult.Free;
  
  ConcatResult := np_concatenate(A, B, 0);  // axis=0 means vertical
  WriteLn('np_concatenate(A, B, axis=0) -> 4x2:');
  WriteLn(ConcatResult.ToString);
  ConcatResult.Free;
  
  // Using standalone functions
  ConcatResult := np_hstack(A, B);
  WriteLn('np_hstack(A, B) -> 2x4:');
  WriteLn(ConcatResult.ToString);
  ConcatResult.Free;
  
  ConcatResult := np_vstack(A, B);
  WriteLn('np_vstack(A, B) -> 4x2:');
  WriteLn(ConcatResult.ToString);
  ConcatResult.Free;
  
  A.Free;
  B.Free;
  HStackResult.Free;
  VStackResult.Free;
  WriteLn;
end;

begin
  WriteLn('NumPascal - A NumPy-like Library for Pascal');
  WriteLn('==========================================');
  WriteLn;
  
  DemoArrayCreation;
  DemoLinspace;
  DemoArithmetic;
  DemoMathFunctions;
  DemoStatistics;
  Demo2DArrays;
  DemoReshaping;
  DemoRandomArrays;
  DemoConcatenation;
  
  WriteLn('Demo completed successfully!');
end.