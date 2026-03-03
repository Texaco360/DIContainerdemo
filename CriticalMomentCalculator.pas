unit CriticalMomentCalculator;
interface
uses
  SysUtils, Classes, Math;

type
  TSectionProperties = record
    Iz : Double; // Moment of inertia about the z-axis
    It : Double; // Torsional constant
    Iw : Double; // Warping constant
    end;

    TMemberloads = record
        q: Double; // Uniformly distributed load
        MStart: Double; // Start moment
        MEnd: Double; // End moment
    end;

  TCriticalMomentCalculator = class
    private
        FCriticalMoment: Double;
        FC1, FC2: Double;
        FE: Double; // Modulus of elasticity in N/mm^2
        FG: Double; // Shear modulus in N/mm^2
        FS: Double; // Slenderness ratio
        FsectionProps: TSectionProperties;
        FLoads: TMemberloads;

        function calculateM1andM2(const Moments: array of Double): Array of Double;
        procedure calculateS();
        procedure calculateCriticalMoment();
     public
        constructor Create(SectionProps: TSectionProperties);
        function SetEndmomentStart(Moment: Double): TCriticalMomentCalculator;
        function SetEndmomentEnd(Moment: Double): TCriticalMomentCalculator;
        function SetUniformLoad(q: Double): TCriticalMomentCalculator;
        
  end;



    

implementation

constructor TCriticalMomentCalculator.Create(SectionProps: TSectionProperties);
begin
    FsectionProps := SectionProps;
    FE := 210000; // Modulus of elasticity for steel in N/mm^2
    FG := 81000; // Shear modulus for steel in N/mm^2
end;

function TCriticalMomentCalculator.SetEndmomentStart(Moment: Double): TCriticalMomentCalculator;
begin
    FLoads.MStart := Moment;
    Result := Self;
end;

function TCriticalMomentCalculator.SetEndmomentEnd(Moment: Double): TCriticalMomentCalculator;
begin
    FLoads.MEnd := Moment;
    Result := Self;
end;

function TCriticalMomentCalculator.SetUniformLoad(q: Double): TCriticalMomentCalculator;
begin
    FLoads.q := q;
    Result := Self;
end;

procedure TCriticalMomentCalculator.calcualteC1andC2();
var
 A_1, B_1, C_1, D_1, E_1: Double;
 C1, C2: Double;
 M1, M2, M, q: Double;
 Beta : Double;

begin
    //Define M1 and M2
    M1 := calculateM1andM2([FLoads.MStart, FLoads.MEnd])[0];
    M2 := calculateM1andM2([FLoads.MStart, FLoads.MEnd])[1]; 
    M := M1;
    q := FLoads.q;
    Beta := M2 / M1;

    //Calculate A_1, B_1, C_1, D_1, E_1
    A_1 := q * sqr(Lspan) / (8*abs(M)+q*sqr(Lspan));
    B_1 := 8*abs(M) / (8*abs(M)+q*sqr(Lspan));
    C_1 := 94*abs(M) / (q*sqr(Lspan));
    D_1 := -72*sqr(abs(M)/(q*sqr(Lspan)));
    E_1 := min(1.75 - 1.05*Beta + 0.3*sqr(Beta), 2.3);

    //Calculate C1 and C2
    if M < 0 then
        begin
            C1:= A_1*(1.45*B_1 +1)*1.13 + B_1*(-0.71 * A_1 +1)*E_1;
            C2:= 0.45*A_1*(1+C_1*exp(D_1)*(1/2*Beta+1/2));
        end;

    if M >= 0 then
        begin
            C1:= 1.13 * A_1 + B_1 * E_1;
            C2:= 0.45*A_1;
        end;
    
    FC1 := C1;
    FC2 := C2;

end;

procedure TCriticalMomentCalculator.calculateS();
begin
    FS := sqrt(FE*FsectionProps.Iz / FG*FsectionProps.It);
end;

function TCriticalMomentCalculator.calculateM1andM2(const Moments: array of Double): Array of Double;
var
  M1, M2: Double;
begin
  M1 := Moments[0];
  M2 := Moments[1];
  if Moments[0] < Moments[1] then
    begin
        M1 := Moments[1];
        M2 := Moments[0];    
    end;
  Result := [M1, M2];

  end;

end.