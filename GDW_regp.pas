unit GDW_regp;
{$N+}


interface

uses
 mathproc, gdw_varb,
 gd_matrix,
  Dialogs, SysUtils;


function TMultiplier (N : double) : double;
function Ar40Ar39Age(X : double; ArArJ : double; ArArJ1sig : double) : double;
function Ar40Ar39AgeWithDCErr(X : double; ArArJ : double; ArArJ1sig : double;
                                        T_Mult : double) : double;
function InitialRatio(Age : double; I : integer): double;
function DMRatioAtAge(Age : double): double;
function DMEpsilonAtAge(Age : double): double;
function Epsilon(Age: double; J : integer): double;
function EpsilonGamma(Age: double; J : integer; iAnalTyp : integer): double;
function RoError(Age,AgeError: double; J : integer): double;
function EpError(Age,AgeError : double; I : integer) : double;
function EpGammaError(Age,AgeError : double; I : integer; iAnalTyp : integer) : double;
function ModelAge(InitRatio : double; J : integer) : double;
function ModelAgeError(InitRatio,InitRatioError : double; J : integer): double;
function CHUR_Age(tx, ty : double) : double;
function CHUR_Age_Error(J : integer) : double;
function DM_Age(tx, ty : double) : double;
function DM_Age_Error(J : integer) : double;
function DM2_Age(AgePref, tx, ty : double) : double;
function RD_Age(tx, ty : double) : double;
function RD_Age_Error(J : integer) : double;
function PbPbAge(Slope : double;
                 IncludeDCUncertainty : boolean;
                 AgePlusAgeMinus : string ) : double;
function PbModelAge(Slope: double): double;
function CalcModelPbAge (tx, ty : double) : double;
function PbModel_Age_Error(J : integer) : double;
function Model76_Age (J : integer) : double;
procedure Get_NewSlope;
function MuValue(MaxMu,Age,OldAge,X0,Y0,Slope,Intercept: double): double;
function MuErr(LowUp:shortint;MaxMu,Age,OldAge,
                             X0,Y0,Slope,Intercept,
                             SlopeError,InterceptError,
                             Xcentroid: double): double;
procedure MuErrors;
procedure ModelMuSourceInitialValue(Mu,Age,OldAge,X0,Y0: double; var Pb64initial: double; var Pb74initial: double);
function ConcordiaIntercept ( MaxAge : double;
                              Slope, Intercept : double ): double;
function ConcordiaAgeErr ( LowUp : shortint; MaxAge : double;
                           Slope, SlopeError,
                           Intercept, InterceptError : double;
                           IncludeDCUncertainty : boolean;
                           AgePlusAgeMinus : string ): double;
procedure ConcordiaErrors;
procedure NewConcordiaErrors;
procedure CalcMuErr (     Age    : double;
                          J      : integer;
                      var Mu     : double;
                      var MuErr  : double );
function CalcModelMu (Age : double;
                      J   : integer) : double;
procedure WtAver ( NS                : integer;
                  var WtAver, MSWD,
                      WESDoM, WOSDoM : double;
                  var N              : integer);
procedure Model76_AgeandError ( var Age,
                                    UprUprAgeError, UprLwrAgeError : double;
                                    Incl : char;
                                var Ninc : integer);
procedure Model76_AgeError ( var Age,
                                UprUprAgeError, UprLwrAgeError : double;
                            J : integer);
function TeraWasserburgIntercept ( MaxAge : double;
                              Slope, Intercept : double ): double;
function TeraWasserburgAgeErr ( LowUp : shortint; MaxAge : double;
                           Slope, SlopeError,
                           Intercept, InterceptError : double;
                           IncludeDCUncertainty : boolean;
                           AgePlusAgeMinus : string ): double;
procedure NewTeraWasserburgErrors;
function CalcArInverseError(LowUp : smallint; TrialX : double;
                            Slope, SlopeError,
                            Intercept, InterceptError : double):double;
function Q235(Age : double) : extended;
function Q238(Age : double) : extended;
function P235(Age : double) : extended;
function P238(Age : double) : extended;
function E235(Age : double) : extended;
function E238(Age : double) : extended;
procedure ConcordiaInterceptDateErrors( t1,t2 : double;
                     SlopeError, InterceptError : double;
                     CovarianceSlopeIntercept : double;
                     Lambda235, SigmaLambda235 : double;
                     Lambda238, SigmaLambda238 : double;
                 var Sigmat1 : double;
                 var Sigmat2 : double;
                 var SigmaCovt1t2 : double);
function SigmatLambda (temp : double; Lambda : double; SigmaLambda : double) : double;
function Sigmat76Lambda (t : double;
           Pb76 : double; SigmaPb76 : double;
           Lambda235 : double; SigmaLambda235 : double;
           Lambda238 : double; SigmaLambda238 : double) : double;
procedure WtAverConcordia ( NS                  : integer;
                   var EquivalenceXVar  : double;
                   var EquivalenceYVar  : double;
                   var EquivalenceCoVar : double;
                   var N                : integer;
                   var MSWDEquivalence  : double;
                   var df               : integer;
                   var ProbEquivalence  : double;
                   var DateWO           : double;
                   var VarDateWO        : double;
                   var MSWDwo           : double;
                   var Probwo           : double;
                   var DateW            : double;
                   var VarDateW         : double;
                   var MSWDw            : double;
                   var Probw            : double);

procedure ConcordiaAgeSingle ( iRec     : integer;
                   var DateWO           : double;
                   var VarDateWO        : double;
                   var DateW            : double;
                   var VarDateW         : double);
function exp5t (t : double) : extended;
function exp5tPlus (t : double) : extended;
function exp5tMinus (t : double) : extended;
function exp8t (t : double) : extended;
function exp8tPlus (t : double) : extended;
function exp8tMinus (t : double) : extended;
function S (X, Y, t, Omega11, Omega22, Omega12 : extended) : extended;
function dSdt (X, Y, t, Omega11, Omega22, Omega12 : extended) : extended;
procedure CalculateConcordiaDate ( X, Y, t : double;
                                   lt, ut : double;
                                   Omega11, Omega22, Omega12 :extended;
                               var Date : double);
function Age238(X : double;
                IncludeDCUncertainty : boolean;
                AgePlusAgeMinus : string ) : double;
procedure ConvertConcordia2TeraWasserburg;
procedure ConvertTeraWasserburg2Concordia;
function DM2_Age_From_Epsilon(AgePref, Epsilon : double) : double;
function Get_IAnal_from_AnalType (AnalType : string) : integer;
procedure Convert2Inverse;

implementation

{----------York regression routines----------}
    //Private declarations
    //  0 = X-Y general
    //  1 = Rb-Sr
    //  2 = Sm-Nd
    //  3 = Pb-Pb
    //  4 = 238U-Pb
    //  5 = 235U-Pb
    //  6 = Th-Pb
    //  7 = Lu-Hf
    //  8 = Concordia
    //  9 = La-Ce
    //  A 10 = Tera-Wasserburg
    //  B 11 = K-Ar
    //  C 12 = Ar-Ar
    //  D 13 = Ar inverse
    //  E 14 = K-Ca
    //  F 15 = Re-Os
    //  G 16 = La-Ba
    //  H 17 = Evaporation Pb
    //  I 18 = Ar plateau
    //  J 19 = T(2DM) from Age-Epsilon values
    //  K 20 = Lu-Hf inverse
    //  L 21 = K-Ca inverse
    //  M 22 = Re-Os inverse

function TMultiplier (N : double) : double;
{from Ludwig 1990 - ISOPLOT}
var
  W    : double;
  temp : double;
begin
  temp := -999.99;
  if N = 1.0 then temp := 12.706;
  if N > 1.0 then
  begin
    if N > 998.0 then
    begin
      temp := 1.96;
    end else
    begin
      W := N-0.85;
      temp := 1.96 - 0.00554568/Sqrt(W)
                      + 2.4059333/W
                      + 0.6711777/(W*W)
                      - 0.3831214/(W*W*W);
    end;
  end;
  if N < 1.0 then
  begin
    if (N > 0.0) then
    begin
      temp := 1.0/(  N * -0.00765
                      + (N*N) * 0.11085
                      - (N*N*N) * 0.02446);
    end else temp := 1.96;
  end;
  if (temp >= -999.990) then Result := Abs(temp);
end;{TMultiplier}

function Ar40Ar39Age(X : double; ArArJ : double; ArArJ1sig : double) : double;
begin
  if ((X * ArArJ) > -1.0) then Result:=Ln(1.0+X*ArArJ)/(DecayConst[ord(atKAr)]+DecayConst[ord(atKCa)])
  else Result:=0.0;
end;

function Ar40Ar39AgeWithDCErr(X : double; ArArJ : double; ArArJ1sig : double;
                                        T_Mult : double) : double;
var
  sigArArJ, sigDCKAr, sigDCKCa : double;
  tArAr, tDecayConst : double;
begin
  sigArArJ := ArArJ1sig;
  //sigArArJ := 0.0;   // temporary to check influence of decay constants alone
  sigDCKAr := DecayConstUncertainty[ord(atKAr)]*DecayConst[ord(atKAr)]/100.0;
  sigDCKCa := DecayConstUncertainty[ord(atKCa)]*DecayConst[ord(atKCa)]/100.0;
  tArAr := ArArJ + sigArArJ;
  tDecayConst := (DecayConst[ord(atKAr)]-sigDCKAr) + (DecayConst[ord(atKCa)]-sigDCKCa);
  if ((X * ArArJ) > -1.0) then Result:=Ln(1.0+X*tArAr)/tDecayConst
  else Result:=0.0;
end;

function InitialRatio(Age : double; I : integer): double;
begin
  InitialRatio := -999.99;
  case AnalType of
    '0'      : InitialRatio:=-1.0;
    '1','2','4'..'7','9','B','E','F','G'  : begin
      if ((Ratio[I,2] <> 0.0) and (Ratio[I,1] <> 0.0)) then
         InitialRatio:=Ratio[I,2]-Ratio[I,1]*(Exp(DecayConst[iAnalTyp]*Age*1.0E6)-1.0)
      else
         InitialRatio := -1.0;
    end;
    '3','8','A','C','D','H'  : InitialRatio:=-1.0;
   end;
end;{function InitialRatio}

function DMRatioAtAge(Age : double): double;
// Age expected in Ma
var
  DMY1 : double;
begin
  DMY1 := -999.99;
  DMRatioAtAge := -999.99;
  case AnalType of
    '0' : DMRatioAtAge :=-1.0;
    '1','2','4'..'7','9','B','E','F','G'  : begin
      if (DM[iAnalTyp,3] > 0.0) then
      begin
        if (DM[iAnalTyp,1] <> 0.0) then
        begin
          DMY1:=DM[iAnalTyp,1]*(Age*1.0e6)*(Age*1.0e6)
            +DM[iAnalTyp,2]*(Age*1.0e6)
            +DM[iAnalTyp,3];
        end;
        if ((DM[iAnalTyp,1] = 0.0) and (DM[iAnalTyp,2] > 0.0)) then
        begin
          DMY1 := DM[iAnalTyp,3] - DM[iAnalTyp,2]*(Exp(DecayConst[iAnalTyp]*Age*1.0e6)-1.0);
        end;
      end;
      DMRatioAtAge := DMY1;
    end;
    '3','8','A','C','D','H'  : DMRatioAtAge :=-1.0;
   end;
end;{function DMRatioAtAge}

function DMEpsilonAtAge(Age : double): double;
// Age expected in Ma
var
  DMY1, URY1 : double;
  T1, T2      :  double;
begin
  DMY1 := -999.99;
  URY1 := -999.99;
  T1 := -999.99;
  T2 := -999.99;
  case AnalType of
    '0' : DMY1 :=-1.0;
    '1','2','4'..'7','9','B','E','F','G'  : begin
      if (DM[iAnalTyp,3] > 0.0) then
      begin
        if (DM[iAnalTyp,1] <> 0.0) then
        begin
          DMY1:=DM[iAnalTyp,1]*(Age*1.0e6)*(Age*1.0e6)
            +DM[iAnalTyp,2]*(Age*1.0e6)
            +DM[iAnalTyp,3];
        end;
        if ((DM[iAnalTyp,1] = 0.0) and (DM[iAnalTyp,2] > 0.0)) then
        begin
          DMY1 := DM[iAnalTyp,3] - DM[iAnalTyp,2]*(Exp(DecayConst[iAnalTyp]*Age*1.0e6)-1.0);
        end;
      end;
    end;
    '3','8','A','C','D','H'  : DMY1 :=-1.0;
   end;
  case AnalType of
    '0' : URY1 :=-1.0;
    '1','2','4'..'7','9','B','E','F','G'  : begin
      if (CHUR[iAnalTyp,3] > 0.0) then
      begin
        if (CHUR[iAnalTyp,1] <> 0.0) then
        begin
          URY1:=CHUR[iAnalTyp,1]*(Age*1.0e6)*(Age*1.0e6)
            +CHUR[iAnalTyp,2]*(Age*1.0e6)
            +CHUR[iAnalTyp,3];
        end;
        if ((CHUR[iAnalTyp,1] = 0.0) and (CHUR[iAnalTyp,2] > 0.0)) then
        begin
          URY1 := CHUR[iAnalTyp,3] - CHUR[iAnalTyp,2]*(Exp(DecayConst[iAnalTyp]*Age*1.0e6)-1.0);
        end;
      end;
    end;
    '3','8','A','C','D','H'  : URY1 :=-1.0;
   end;
  if (URY1 > 0.0) then
  begin
    if (Age <= 0.0) then
    begin
      if (Age < 0.0) then Age := 0.0;
      T1 := DM[iAnalTyp,3];
      T2 := CHUR[iAnalTyp,3];
    end;
    if (Age > 0.0) then
    begin
      T1 := DMY1;
      T2 := URY1;
    end;
    DMEpsilonAtAge := 10000.0*(T1/T2-1.0);
  end else
  begin
    DMEpsilonAtAge := -9999.0;
  end;
end;{function DMEpsilonAtAge}

function Epsilon(Age: double; J : integer): double;
var
  T1, T2      :  double;
begin
  //Age expected in Ma
  T1 := -999.99;
  T2 := -999.99;
  if (CHUR[iAnalTyp,2] > 0.0) then
  begin
    if (Age > 0.0) then
    begin
      T1:=Ratio[J,2]-Ratio[J,1]*(Exp(DecayConst[iAnalTyp]*Age*1.0E6)-1.0);
      T2:=CHUR[iAnalTyp,3]-CHUR[iAnalTyp,2]*(Exp(DecayConst[iAnalTyp]*Age*1.0E6)-1.0);
    end;
    if (Age <= 0.0) then
    begin
      if (Age <= 0.0) then Age:=0.0;
      T1:=Ratio[J,2];
      T2:=CHUR[iAnalTyp,3];
    end;
    Epsilon:=10000.0*(T1/T2-1.0);
  end else
  begin
    Epsilon:=-9999.0;
  end;
end;{function Epsilon}

function EpsilonGamma(Age: double; J : integer; iAnalTyp : integer): double;
var
  T1, T2      :  double;
begin
  T1 := -999.99;
  T2 := -999.99;
  if (CHUR[iAnalTyp,2] > 0.0) then
  begin
    if (Age > 0.0) then
    begin
      T1:=Ratio[J,2]-Ratio[J,1]*(Exp(DecayConst[iAnalTyp]*Age*1.0E6)-1.0);
      T2:=CHUR[iAnalTyp,3]-CHUR[iAnalTyp,2]*(Exp(DecayConst[iAnalTyp]*Age*1.0E6)-1.0);
    end;
    if (Age <= 0.0) then
    begin
      if (Age <= 0.0) then Age:=0.0;
      T1:=Ratio[J,2];
      T2:=CHUR[iAnalTyp,3];
    end;
    if (iAnalTyp = 15) then
    begin
      Result:=100.0*(T1/T2-1.0);
    end else
    begin
      Result:=10000.0*(T1/T2-1.0);
    end;
  end else
  begin
    Result:=-9999.0;
  end;
end;{function EpsilonGamma}

function RoError(Age,AgeError: double; J : integer): double;
var
  temp    :  double;
  temp1   :  double;
  temp2   :  double;
  t1, t2  :  double;
begin
  t1 := -999.99;
  t2 := -999.99;
  case ErrTyp[J] of
    '1' : begin
            t1:=ErrorWt[J,1]*Ratio[J,1]/100.0;
            t2:=ErrorWt[J,2]*Ratio[J,2]/100.0;
          end;
    '2' : begin
            t1:=ErrorWt[J,1]*Ratio[J,1]/100.0;
            t2:=ErrorWt[J,2];
          end;
    '3' : begin
            t1:=ErrorWt[J,1];
            t2:=ErrorWt[J,2]*Ratio[J,2]/100.0;
          end;
    '4' : begin
            t1:=ErrorWt[J,1];
            t2:=ErrorWt[J,2];
          end;
  end;{case}
  if (t1<=0.0) then t1:=1.0e-5;
  if (t2<=0.0) then t2:=1.0e-5;
  temp:=((Exp(DecayConst[iAnalTyp]*Age*1.0E6)-1.0));
  temp1:=(-1*Ratio[J,1]*DecayConst[iAnalTyp]*(temp+1.0)*(AgeError*1.0e6));
  temp2:=(t2*t2 + t1*t1*temp*temp + temp1*temp1+ 2.0*temp*R[J]*t1*t2);
  if (temp2>0.0) then RoError:=Sqrt(temp2)*T_Mult
                 else RoError:=9.999999;
end;

function EpError(Age,AgeError : double; I : integer) : double;
var
  t1 : double;
begin
  t1:=RoError(Age,AgeError,I);
  if (t1<>9.999999) then EpError:=10000.0*t1/InitialRatio(Age,I)
                    else EpError:=99.999;
end;

function EpGammaError(Age,AgeError : double; I : integer; iAnalTyp : integer) : double;
var
  t1 : double;
begin
  t1:=RoError(Age,AgeError,I);
  if (t1<>9.999999) then Result:=10000.0*t1/InitialRatio(Age,I)
                    else Result:=99.999;
  if (iAnalTyp = 15) then Result := Result/100.0;
end;

function ModelAge(InitRatio : double; J : integer) : double;
var
  temp    : double;
begin
  temp:=0.0;
  if (Ratio[J,1] > 0.0) then
        temp:=((Ratio[J,2]-InitRatio)/Ratio[J,1])+1;
  if (temp>0.0) then ModelAge:=(Ln(temp))/(DecayConst[iAnalTyp]*1.0E6)
                else ModelAge:=0.0;
end;


function ModelAgeError(InitRatio,InitRatioError : double; J : integer): double;
var
  temp    :  double;
  tempX, tempY,
  tempI   :  double;
  temp2   :  double;
  t1, t2  :  double;
begin
  t1 := 0.0;
  t2 := 0.0;
  case ErrTyp[J] of
    '1' : begin
            t1:=ErrorWt[J,1]*Ratio[J,1]/100.0;
            t2:=ErrorWt[J,2]*Ratio[J,2]/100.0;
          end;
    '2' : begin
            t1:=ErrorWt[J,1]*Ratio[J,1]/100.0;
            t2:=ErrorWt[J,2];
          end;
    '3' : begin
            t1:=ErrorWt[J,1];
            t2:=ErrorWt[J,2]*Ratio[J,2]/100.0;
          end;
    '4' : begin
            t1:=ErrorWt[J,1];
            t2:=ErrorWt[J,2];
          end;
  end;{case}
  if (t1<=0.0) then t1:=1.0e-5;
  if (t2<=0.0) then t2:=1.0e-5;
  temp:=1.0/DecayConst[iAnalTyp];
  temp:=temp/(((Ratio[J,2]-InitRatio)/Ratio[J,1])+1.0);
  temp:=temp/Ratio[J,1];
  tempY:=temp;
  tempX:=temp*(InitRatio-Ratio[J,2])/Ratio[J,1];
  tempI:=temp*(-1.0);
  tempY:=tempY*t2;
  tempX:=tempX*t1;
  tempI:=tempI*InitRatioError;
  temp2:=(tempX*tempX + tempY*tempY + tempI*tempI
          + 2.0*tempX*tempY*R[J]*t1*t2);
  if (temp2>0.0) then ModelAgeError:=Sqrt(temp2)/1.0E6*T_Mult
                 else ModelAgeError:=-9.99;
end;


function CHUR_Age(tx, ty : double) : double;
var
  I            : integer;
  temp, temp1  : double;
  URX1, URY1, URY2, Difference,
  AgeMax, ApproxAge
               :  double;
  ThisDone         :  Boolean;
begin
  AgeMax := 4.570e9;
  if (CHUR[iAnalTyp,3] > 0.0) then
  begin
    if (CHUR[iAnalTyp,1] <> 0.0) then
    begin
      ThisDone:=false;
      I:=0;
      AgeMax:=4.60E9;
      repeat
        URX1:=ty-tx*(Exp(DecayConst[iAnalTyp]*AgeMax)-1.0);
        URY1:=CHUR[iAnalTyp,1]*(AgeMax)*(AgeMax)
            +CHUR[iAnalTyp,2]*(AgeMax)
            +CHUR[iAnalTyp,3];
        URY2:=(CHUR[iAnalTyp,2])+tx*DecayConst[iAnalTyp]
            *(Exp(DecayConst[iAnalTyp]*AgeMax));
        URY2:=URY2+2.0*CHUR[iAnalTyp,1]*AgeMax;
        URY1:=URY1-URX1;
        if (URY2 <> 0.0) then
          ApproxAge:=AgeMax-URY1/URY2
        else begin
          ApproxAge:=1.0e6;
          I:=51;
        end;
        Difference:=Abs(AgeMax-ApproxAge);
        AgeMax:=ApproxAge;
        I:=I+1;
        if Difference<AgeTolerance then ThisDone:=true;
        if I>50 then
        begin
          AgeMax:=9999.0E6;
          ThisDone:=true;
        end;
      until ThisDone;
    end;
    if ((CHUR[iAnalTyp,1] = 0.0) and (CHUR[iAnalTyp,2] > 0.0)) then
    begin
      temp:=((ty-CHUR[iAnalTyp,3])/(tx-CHUR[iAnalTyp,2])+1.0);
      if (temp>0.0) then temp1:=Ln(temp)
                    else temp1:=0.0;
      AgeMax:=(temp1/DecayConst[iAnalTyp]);
    end;
    CHUR_Age:=AgeMax/1.0E6;
  end else
  begin
    CHUR_Age := 9999.99;
  end;
  {
  if (CHUR[iAnalTyp,3] > 0.0) then
  begin
    temp:=((ty-CHUR[iAnalTyp,3])/(tx-CHUR[iAnalTyp,2])+1.0);
    if (temp>0.0) then temp1:=Ln(temp)
                  else temp1:=0.0;
    CHUR_Age:=(temp1/DecayConst[iAnalTyp])/1.0E6;
  end else
  begin
    CHUR_Age := 9999.99;
  end;
  }
end;


function CHUR_Age_Error(J : integer) : double;
var
  tage, tplus1,
  tminus1,
  t1, t2    : double;
begin
  t1 := -999.99;
  t2 := -999.99;
  case ErrTyp[J] of
    '1' : begin
            t1:=ErrorWt[J,1]*Ratio[J,1]/100.0;
            t2:=ErrorWt[J,2]*Ratio[J,2]/100.0;
          end;
    '2' : begin
            t1:=ErrorWt[J,1]*Ratio[J,1]/100.0;
            t2:=ErrorWt[J,2];
          end;
    '3' : begin
            t1:=ErrorWt[J,1];
            t2:=ErrorWt[J,2]*Ratio[J,2]/100.0;
          end;
    '4' : begin
            t1:=ErrorWt[J,1];
            t2:=ErrorWt[J,2];
          end;
  end;{case}
  if (t1<=0.0) then t1:=1.0e-5;
  if (t2<=0.0) then t2:=1.0e-5;
  tage:=CHUR_Age(Ratio[J,1],Ratio[J,2]);
  tplus1:=CHUR_Age(Ratio[J,1]+t1*T_Mult,Ratio[J,2]+t2*T_Mult);
  tminus1:=CHUR_Age(Ratio[J,1]-t1*T_Mult,Ratio[J,2]-t2*T_Mult);
  CHUR_Age_Error:=Sqrt(((tplus1-tage)*(tplus1-tage)+(tage-tminus1)*(tage-tminus1))+0.000001);
end;


function DM2_Age(AgePref, tx, ty : double) : double;
var
  temp, temp1  : double;
  AgeMax       :  double;
begin
  // Age expected in Ma
  if (DM[iAnalTyp,3] > 0.0) then
  begin
    if ((DM[iAnalTyp,1] = 0.0) and (DM[iAnalTyp,2] > 0.0)) then
    begin
      temp:=((ty-(exp(DecayConst[iAnalTyp]*AgePref*1.0e6)-1.0)*(tx-CC[iAnalTyp,2])-DM[iAnalTyp,3])/(CC[iAnalTyp,2]-DM[iAnalTyp,2])+1.0);
      if (temp>0.0) then temp1:=Ln(temp)
                    else temp1:=0.0;
      AgeMax:=(temp1/DecayConst[iAnalTyp]);
    end else
    begin
      AgeMax := -999.99*1.0e6;
    end;
    DM2_Age:=AgeMax/1.0E6;
  end else
  begin
    DM2_Age := 9999.99;
  end;
  {
    if ((DM[iAnalTyp,1] = 0.0) and (DM[iAnalTyp,2] > 0.0)) then
    begin
      temp:=((ty-DM[iAnalTyp,3])/(tx-DM[iAnalTyp,2])+1.0);
      if (temp>0.0) then temp1:=Ln(temp)
                    else temp1:=0.0;
      AgeMax:=(temp1/DecayConst[iAnalTyp]);
    end;
    DM_Age:=AgeMax/1.0E6;
  }
end;

function DM_Age(tx, ty : double) : double;
var
  I            : integer;
  temp, temp1  : double;
  DMX1, DMY1, DMY2, Difference,
  AgeMax, ApproxAge
               :  double;
  ThisDone         :  Boolean;
begin
  AgeMax := -999.99;
  if (DM[iAnalTyp,3] > 0.0) then
  begin
    if (DM[iAnalTyp,1] <> 0.0) then
    begin
      ThisDone:=false;
      I:=0;
      AgeMax:=4.60E9;
      repeat
        DMX1:=ty-tx*(Exp(DecayConst[iAnalTyp]*AgeMax)-1.0);
        DMY1:=DM[iAnalTyp,1]*(AgeMax)*(AgeMax)
            +DM[iAnalTyp,2]*(AgeMax)
            +DM[iAnalTyp,3];
        DMY2:=(DM[iAnalTyp,2])+tx*DecayConst[iAnalTyp]
            *(Exp(DecayConst[iAnalTyp]*AgeMax));
        DMY2:=DMY2+2.0*DM[iAnalTyp,1]*AgeMax;
        DMY1:=DMY1-DMX1;
        if (DMY2 <> 0.0) then
          ApproxAge:=AgeMax-DMY1/DMY2
        else begin
          ApproxAge:=1.0e6;
          I:=51;
        end;
        Difference:=Abs(AgeMax-ApproxAge);
        AgeMax:=ApproxAge;
        I:=I+1;
        if Difference<AgeTolerance then ThisDone:=true;
        if I>50 then
        begin
          AgeMax:=9999.0E6;
          ThisDone:=true;
        end;
      until ThisDone;
    end;
    if ((DM[iAnalTyp,1] = 0.0) and (DM[iAnalTyp,2] > 0.0)) then
    begin
      temp:=((ty-DM[iAnalTyp,3])/(tx-DM[iAnalTyp,2])+1.0);
      if (temp>0.0) then temp1:=Ln(temp)
                    else temp1:=0.0;
      AgeMax:=(temp1/DecayConst[iAnalTyp]);
    end;
    DM_Age:=AgeMax/1.0E6;
  end else
  begin
    DM_Age := 9999.99;
  end;
end;


function DM_Age_Error(J : integer) : double;
var
  tage, tplus1,
  tminus1,
  t1, t2    : double;
begin
  t1 := -999.99;
  t2 := -999.99;
  case ErrTyp[J] of
    '1' : begin
            t1:=ErrorWt[J,1]*Ratio[J,1]/100.0;
            t2:=ErrorWt[J,2]*Ratio[J,2]/100.0;
          end;
    '2' : begin
            t1:=ErrorWt[J,1]*Ratio[J,1]/100.0;
            t2:=ErrorWt[J,2];
          end;
    '3' : begin
            t1:=ErrorWt[J,1];
            t2:=ErrorWt[J,2]*Ratio[J,2]/100.0;
          end;
    '4' : begin
            t1:=ErrorWt[J,1];
            t2:=ErrorWt[J,2];
          end;
  end;{case}
  if (t1<=0.0) then t1:=1.0e-5;
  if (t2<=0.0) then t2:=1.0e-5;
  tage:=DM_Age(Ratio[J,1],Ratio[J,2]);
  tplus1:=DM_Age(Ratio[J,1]+t1*T_Mult,Ratio[J,2]+t2*T_Mult);
  tminus1:=DM_Age(Ratio[J,1]-t1*T_Mult,Ratio[J,2]-t2*T_Mult);
  DM_Age_Error:=Sqrt(((tplus1-tage)*(tplus1-tage)+(tage-tminus1)*(tage-tminus1))+0.000001);
end;


function RD_Age(tx, ty : double) : double;
var
  I            : integer;
  temp, temp1  : double;
  DMX1, DMY1, DMY2, Difference,
  AgeMax, ApproxAge
               :  double;
  ThisDone         :  Boolean;
begin
  AgeMax := -999.99;
  tx := 0.0;
  if (DM[iAnalTyp,3] > 0.0) then
  begin
    if (DM[iAnalTyp,1] <> 0.0) then
    begin
      ThisDone:=false;
      I:=0;
      AgeMax:=4.60E9;
      repeat
        DMX1:=ty-tx*(Exp(DecayConst[iAnalTyp]*AgeMax)-1.0);
        DMY1:=DM[iAnalTyp,1]*(AgeMax)*(AgeMax)
            +DM[iAnalTyp,2]*(AgeMax)
            +DM[iAnalTyp,3];
        DMY2:=(DM[iAnalTyp,2])+tx*DecayConst[iAnalTyp]
            *(Exp(DecayConst[iAnalTyp]*AgeMax));
        DMY2:=DMY2+2.0*DM[iAnalTyp,1]*AgeMax;
        DMY1:=DMY1-DMX1;
        if (DMY2 <> 0.0) then
          ApproxAge:=AgeMax-DMY1/DMY2
        else begin
          ApproxAge:=1.0e6;
          I:=51;
        end;
        Difference:=Abs(AgeMax-ApproxAge);
        AgeMax:=ApproxAge;
        I:=I+1;
        if Difference<AgeTolerance then ThisDone:=true;
        if I>50 then
        begin
          AgeMax:=9999.0E6;
          ThisDone:=true;
        end;
      until ThisDone;
    end;
    if ((DM[iAnalTyp,1] = 0.0) and (DM[iAnalTyp,2] > 0.0)) then
    begin
      //temp:=((ty-DM[iAnalTyp,3])/(tx-DM[iAnalTyp,2])+1.0);
      temp:=(DM[iAnalTyp,3]-ty)/DM[iAnalTyp,2]+1.0;
      if (temp>0.0) then temp1:=Ln(temp)
                    else temp1:=0.0;
      AgeMax:=(temp1/DecayConst[iAnalTyp]);
    end;
    RD_Age:=AgeMax/1.0E6;
  end else
  begin
    RD_Age := 9999.99;
  end;
end;


function RD_Age_Error(J : integer) : double;
var
  tage, tplus1,
  tminus1,
  t1, t2    : double;
begin
  t1 := -999.99;
  t2 := -999.99;
  case ErrTyp[J] of
    '1' : begin
            t1:=ErrorWt[J,1]*Ratio[J,1]/100.0;
            t2:=ErrorWt[J,2]*Ratio[J,2]/100.0;
          end;
    '2' : begin
            t1:=ErrorWt[J,1]*Ratio[J,1]/100.0;
            t2:=ErrorWt[J,2];
          end;
    '3' : begin
            t1:=ErrorWt[J,1];
            t2:=ErrorWt[J,2]*Ratio[J,2]/100.0;
          end;
    '4' : begin
            t1:=ErrorWt[J,1];
            t2:=ErrorWt[J,2];
          end;
  end;{case}
  if (t1<=0.0) then t1:=1.0e-5;
  if (t2<=0.0) then t2:=1.0e-5;
  tage:=RD_Age(Ratio[J,1],Ratio[J,2]);
  tplus1:=RD_Age(Ratio[J,1]+t1*T_Mult,Ratio[J,2]+t2*T_Mult);
  tminus1:=RD_Age(Ratio[J,1]-t1*T_Mult,Ratio[J,2]-t2*T_Mult);
  RD_Age_Error:=Sqrt(((tplus1-tage)*(tplus1-tage)+(tage-tminus1)*(tage-tminus1))+0.000001);
end;

function PbPbAge(Slope : double;
                 IncludeDCUncertainty : boolean;
                 AgePlusAgeMinus : string ) : double;
const
  T_MultDC = 1.96;
var
  X1, X2, Y1, Y2, Difference, AgeMax, ApproxAge
               :  double;
  ThisDone         :  Boolean;
begin
  if ((Slope>0.0) and (Slope<2.0)) then
  begin
    //ShowMessage(FormatFloat('###0.000000',Slope)+'   '+FormatFloat('###0.000000',TMultiplier(1.0*N_Rep)));
    ThisDone:=false;
    AgeMax:=5.0E9;
    repeat
      try
        if IncludeDCUncertainty then
        begin
          if (AgePlusAgeMinus = UncertaintyPlus) then
          begin
            //X1:=Exp((DecayConst[ord(at238UPb)]-T_MultDC*DecayConstUncertainty[ord(at238UPb)]*DecayConst[ord(at238UPb)]/100.0)*AgeMax)-1.0;
            //X2:=Exp((DecayConst[ord(at235UPb)]+T_MultDC*DecayConstUncertainty[ord(at235UPb)]*DecayConst[ord(at235UPb)]/100.0)*AgeMax)-1.0;
            X1:=Exp((DecayConst[ord(at238UPb)]-DecayConstUncertainty[ord(at238UPb)]*DecayConst[ord(at238UPb)]/100.0)*AgeMax)-1.0;
            X2:=Exp((DecayConst[ord(at235UPb)]+DecayConstUncertainty[ord(at235UPb)]*DecayConst[ord(at235UPb)]/100.0)*AgeMax)-1.0;
            Y1:=X2/X1-U238U235*Slope;
            Y2:=X1*(X2+1.0)*(DecayConst[ord(at235UPb)]+DecayConstUncertainty[ord(at235UPb)]*DecayConst[ord(at235UPb)]/100.0)-X2*(X1+1.0)*(DecayConst[ord(at238UPb)]-DecayConstUncertainty[ord(at238UPb)]*DecayConst[ord(at238UPb)]/100.0);
            //ShowMessage('plus '+FormatFloat('###0.000000',AgeMax/1.0e6)+'__'+FormatFloat('###.00000000',Slope)+'__'+FormatFloat('###.00000000',X1)+'__'+FormatFloat('###.00000000',X2)+'__'+FormatFloat('###.00000000',Y1)+'__'+FormatFloat('###.00000000',Y2));
          end;
          if (AgePlusAgeMinus = UncertaintyMinus) then
          begin
            //X1:=Exp((DecayConst[ord(at238UPb)]+T_MultDC*DecayConstUncertainty[ord(at238UPb)]*DecayConst[ord(at238UPb)]/100.0)*AgeMax)-1.0;
            //X2:=Exp((DecayConst[ord(at235UPb)]-T_MultDC*DecayConstUncertainty[ord(at235UPb)]*DecayConst[ord(at235UPb)]/100.0)*AgeMax)-1.0;
            X1:=Exp((DecayConst[ord(at238UPb)]+DecayConstUncertainty[ord(at238UPb)]*DecayConst[ord(at238UPb)]/100.0)*AgeMax)-1.0;
            X2:=Exp((DecayConst[ord(at235UPb)]-DecayConstUncertainty[ord(at235UPb)]*DecayConst[ord(at235UPb)]/100.0)*AgeMax)-1.0;
            Y1:=X2/X1-U238U235*Slope;
            Y2:=X1*(X2+1.0)*(DecayConst[ord(at235UPb)]-DecayConstUncertainty[ord(at235UPb)]*DecayConst[ord(at235UPb)]/100.0)-X2*(X1+1.0)*(DecayConst[ord(at238UPb)]+DecayConstUncertainty[ord(at238UPb)]*DecayConst[ord(at238UPb)]/100.0);
            //ShowMessage('minus '+FormatFloat('###0.000000',AgeMax/1.0e6)+'__'+FormatFloat('###.00000000',Slope)+'__'+FormatFloat('###.00000000',X1)++'__'+FormatFloat('###.00000000',X2)+'__'+FormatFloat('###.00000000',Y1)+'__'+FormatFloat('###.00000000',Y2));
          end;
        end else
        begin
          X1:=Exp(DecayConst[ord(at238UPb)]*AgeMax)-1.0;
          X2:=Exp(DecayConst[ord(at235UPb)]*AgeMax)-1.0;
          Y1:=X2/X1-U238U235*Slope;
          Y2:=X1*(X2+1.0)*DecayConst[ord(at235UPb)]-X2*(X1+1.0)*DecayConst[ord(at238UPb)];
        end;
        Y2:=Y2/(X1*X1);
        ApproxAge:=AgeMax-Y1/Y2;
        Difference:=Abs(AgeMax-ApproxAge);
        AgeMax:=ApproxAge;
        if Difference<AgeTolerance then ThisDone:=true;
      except
        AgeMax := ApproxAge;
        if (AgeMax < 0.0) then AgeMax := 0.0;
        ThisDone := true;
      end;
    until ThisDone;
  end
  else AgeMax:=0.0;
  if (AgeMax < 0.0) then AgeMax := 0.0;
  PbPbAge:=AgeMax;
end;{function PbPbAge}

function PbModelAge(Slope: double): double;
var
  X1, X2, Y1, Y2, Difference, AgeMax, ApproxAge,
  Z1, Z2       :  double;
  ThisDone         :  Boolean;
begin
  if ((Slope>0.0) and (Slope<2.0)) then begin
    ThisDone:=false;
    AgeMax:=5.0E9;
    Z1:=Exp(DecayConst[ord(at238UPb)]*MuV[mu_choice,1]);
    Z2:=Exp(DecayConst[ord(at235UPb)]*MuV[mu_choice,1]);
    repeat
      X1:=Z1-Exp(DecayConst[ord(at238UPb)]*AgeMax);
      X2:=Z2-Exp(DecayConst[ord(at235UPb)]*AgeMax);
      Y1:=X2/X1-U238U235*Slope;
      Y2:=X1*(Z2+X2)*DecayConst[ord(at235UPb)]-X2*(Z1+X1)*DecayConst[ord(at238UPb)];
      Y2:=Y2/(X1*X1);
      ApproxAge:=AgeMax-Y1/Y2;
      Difference:=Abs(AgeMax-ApproxAge);
      AgeMax:=ApproxAge;
      if Difference<AgeTolerance then ThisDone:=true;
    until ThisDone;
  end
  else AgeMax:=0.0;
    PbModelAge:=AgeMax;
end;{function PbModelAge}

function CalcModelPbAge (tx, ty : double) : double;
begin
  if (Abs(tx-MuV[mu_choice,2]) > 0.000001) then
  begin
    Slope:=(ty-MuV[mu_choice,3])/(tx-MuV[mu_choice,2]);
    CalcModelPbAge:=PbModelAge(Slope);
  end
  else CalcModelPbAge:=-5e9;
end;

function PbModel_Age_Error(J : integer) : double;
var
  tage, tplus1,
  tminus1,
  t1, t2    : double;
begin
  case ErrTyp[J] of
    '1' : begin
            t1:=ErrorWt[J,1]*Ratio[J,1]/100.0;
            t2:=ErrorWt[J,2]*Ratio[J,2]/100.0;
          end;
    '2' : begin
            t1:=ErrorWt[J,1]*Ratio[J,1]/100.0;
            t2:=ErrorWt[J,2];
          end;
    '3' : begin
            t1:=ErrorWt[J,1];
            t2:=ErrorWt[J,2]*Ratio[J,2]/100.0;
          end;
    '4' : begin
            t1:=ErrorWt[J,1];
            t2:=ErrorWt[J,2];
          end;
  end;{case}
  if (t1<=0.0) then t1:=1.0e-5;
  if (t2<=0.0) then t2:=1.0e-5;
  tage:=CalcModelPbAge(Ratio[J,1],Ratio[J,2]);
  tplus1:=CalcModelPbAge(Ratio[J,1]+t1*T_Mult,Ratio[J,2]+t2*T_Mult);
  tminus1:=CalcModelPbAge(Ratio[J,1]-t1*T_Mult,Ratio[J,2]-t2*T_Mult);
  PbModel_Age_Error:=Abs(((tplus1-tage)+(tage-tminus1))/2.0)/1.0e6;
end;


function Model76_Age (J : integer) : double;
var
  IncludeDCUncertainty : boolean;
  AgePlusAgeMinus : string;
begin
  IncludeDCUncertainty := false;
  AgePlusAgeMinus := 'neither';
  if ((Ratio[J,2] > 0.000001) or (Ratio[J,3] > 0.00001)) then
  begin
    if (Ratio[J,3] > 0.01) then Slope:=Ratio[J,3]
                           else Slope:=Ratio[J,1]/Ratio[J,2]/U238U235;
    IncludeDCUncertainty := false;
    AgePlusAgeMinus := 'neither';
    Model76_Age:=PbPbAge(Slope,IncludeDCUncertainty,AgePlusAgeMinus);
  end
  else Model76_Age:=-5e9;
end;

function MuValue(MaxMu,Age,OldAge,X0,Y0,Slope,Intercept: double): double;
{Mu for a regression line}
var
  M, B, ApproxMu, Difference, U
               :  double;
  ThisDone         :  Boolean;
begin
  ThisDone:=false;
  ApproxMu := MaxMu;
  repeat
    M:=(Exp(DecayConst[ord(at235UPb)]*OldAge)-Exp(DecayConst[ord(at235UPb)]*Age));
    M:=M/(U238U235*(Exp(DecayConst[ord(at238UPb)]*OldAge)-Exp(DecayConst[ord(at238UPb)]*Age)));
    B:=Y0+MaxMu/U238U235*(Exp(DecayConst[ord(at235UPb)]*OldAge)-Exp(DecayConst[ord(at235UPb)]*Age));
    B:=B-M*(X0+MaxMu*(Exp(DecayConst[ord(at238UPb)]*OldAge)-Exp(DecayConst[ord(at238UPb)]*Age)));
    U:=(Intercept-B)/(M-Slope);
    ApproxMu:=(U-X0)/(Exp(DecayConst[ord(at238UPb)]*OldAge)-Exp(DecayConst[ord(at238UPb)]*Age));
    Difference:=Abs(MaxMu-ApproxMu);
    if Difference<MuTolerance then ThisDone:=true;
    MaxMu:=ApproxMu;
  until ThisDone;
  MuValue:=MaxMu;
end;{MuValue}


function MuErr(LowUp:shortint;MaxMu,Age,OldAge,
                             X0,Y0,Slope,Intercept,
                             SlopeError,InterceptError,
                             Xcentroid: double): double;
{Mu error for a regression line}
var
  M, B, ApproxMu, Difference, U, D, E, V
               :  double;
  ThisDone         :  Boolean;
begin
  ThisDone:=false;
  ApproxMu := MaxMu;
  repeat
    M:=(Exp(DecayConst[ord(at235UPb)]*OldAge)-Exp(DecayConst[ord(at235UPb)]*Age));
    M:=M/(U238U235*(Exp(DecayConst[ord(at238UPb)]*OldAge)-Exp(DecayConst[ord(at238UPb)]*Age)));
    B:=Y0+MaxMu/U238U235*(Exp(DecayConst[ord(at235UPb)]*OldAge)-Exp(DecayConst[ord(at235UPb)]*Age));
    B:=B-M*(X0+MaxMu*(Exp(DecayConst[ord(at238UPb)]*OldAge)-Exp(DecayConst[ord(at238UPb)]*Age)))-Intercept;
    D:=2.0*(B*(M-Slope)+Xcentroid*SlopeError*SlopeError);
    E:=(M-Slope)*(M-Slope)-SlopeError*SlopeError;
    V:=B*B-InterceptError*InterceptError;
    if (D*D-4.0*E*V)>0.0 then begin
      U:=(-D+(1.0*LowUp)*Sqrt(D*D-4.0*E*V))/(2.0*E);
      if U>-1 then
        ApproxMu:=(U-X0)/(Exp(DecayConst[ord(at238UPb)]*OldAge)-Exp(DecayConst[ord(at238UPb)]*Age))
      else ThisDone:=true;
    end
    else ThisDone:=true;
    Difference:=Abs(MaxMu-ApproxMu);
    if Difference<MuTolerance then ThisDone:=true;
    MaxMu:=ApproxMu;
  until ThisDone;
  MuErr:=MaxMu;
end;{MuErr}

procedure MuErrors;
{Mu and errors for a regression line}
var
  LL   : byte;
begin
  LL:=mu_choice;
  Get_NewSlope;
  Mu:=MuValue(50.0,Age,MuV[LL,1],MuV[LL,2],MuV[LL,3],NewSlope,NewIntercept);
  UprMuError:=MuErr(1,50.0,Age,MuV[LL,1],MuV[LL,2],MuV[LL,3],
                    NewSlope,NewIntercept,NewSlopeError*T_Mult,NewInterceptError*T_Mult,Xcentroid)-Mu;
  LwrMuError:=Mu-MuErr(-1,1.0,Age,MuV[LL,1],MuV[LL,2],MuV[LL,3],
                       NewSlope,NewIntercept,NewSlopeError*T_Mult,NewInterceptError*T_Mult,Xcentroid);
end;{procedure MuErrors}


procedure CalcMuErr (     Age    : double;
                          J      : integer;
                      var Mu     : double;
                      var MuErr  : double );
{Mu and errors for individual points}
var
  A, B, C, G,
  U, V, W,
  t1, t2,
  dUdXs,
  dUdYs, dVdXs,
  dWdXs, dVdYs,
  dWdYs,
  SigXsYs          : double;
  SigXs, SigYs,
  SigU             : double;

function Age5 ( Age : double) : double;
begin
  Age5:=Exp(DecayConst[ord(at235UPb)]*Age);
end;

function Age8 ( Age : double) : double;
begin
  Age8:=Exp(DecayConst[ord(at238UPb)]*Age);
end;

begin
  t1 := -999.99;
  t2 := -999.99;
  Age:=Age*1.0e6;
  case ErrTyp[J] of
    '1' : begin
            t1:=ErrorWt[J,1]*Ratio[J,1]/100.0;
            t2:=ErrorWt[J,2]*Ratio[J,2]/100.0;
          end;
    '2' : begin
            t1:=ErrorWt[J,1]*Ratio[J,1]/100.0;
            t2:=ErrorWt[J,2];
          end;
    '3' : begin
            t1:=ErrorWt[J,1];
            t2:=ErrorWt[J,2]*Ratio[J,2]/100.0;
          end;
    '4' : begin
            t1:=ErrorWt[J,1];
            t2:=ErrorWt[J,2];
          end;
  end;{case}
  t1:=t1*T_Mult;
  t2:=t2*T_Mult;
  if (t1<=0.0) then t1:=1.0e-5;
  if (t2<=0.0) then t2:=1.0e-5;
  SigXs:=t1;
  SigYs:=t2;
  A:=Age5(Age)-1.0;
  B:=Age8(Age)-1.0;
  C:=Age5(MuV[mu_choice,1])-Age5(Age);
  G:=Age8(MuV[mu_choice,1])-Age8(Age);
  V:=(A/B)*(MuV[mu_choice,2]-Ratio[J,1])/U238U235 + Ratio[J,2]-MuV[mu_choice,3];
  W:=C/U238U235 - (A/B)*G/U238U235;
  U:=V/W;
  dVdXs:=(-1.0*A)/(U238U235*B);
  dWdXs:=0.0;
  dUdXs:=(W*dVdXs-V*dWdXs)/(W*W);
  dVdYs:=1.0;
  dWdYs:=0.0;
  dUdYs:=(W*dVdYs-V*dWdYs)/(W*W);
  SigXsYs:=R[J]*SigXs*SigYs;
  SigU:=(dUdXs*SigXs)*(dUdXs*SigXs) + (dUdYs*SigYs)*(dUdYs*SigYs);
  SigU:=SigU + 2.0*dUdXs*dUdYs*SigXsYs;
  SigU:=Sqrt(SigU);
  MuErr:=SigU;
  Mu:=U;
end;

function CalcModelMu (Age : double;
                      J   : integer) : double;
var
  t1, t2 : double;
begin
  CalcMuErr(Age,J,t1,t2);
  CalcModelMu := t1;
end;

procedure Get_NewSlope;
var
  temp, temp1   : double;
begin
  temp:=ArcTan(Slope);
  temp1:=ArcTan(SlopeError);
  ThetaError:=SlopeError*Cos(temp)*Cos(temp);
  NewSlope:=(Tan(temp+ThetaError)+Tan(temp-ThetaError))/2.0;
  NewIntercept:=Ycentroid-Xcentroid*NewSlope;
  NewSlopeError:=(Tan(temp1+ThetaError)+Tan(temp1-ThetaError))/2.0;
  NewInterceptError:=InterceptError+(NewSlopeError-SlopeError)*Xcentroid;
end;

procedure ModelMuSourceInitialValue(Mu,Age,OldAge,X0,Y0: double; var Pb64initial: double; var Pb74initial: double);
{calculate model source Pb64 and Pb74 initial values for a given mu and age according to given model starting values}
begin
  Pb64initial := X0 + Mu * (exp(OldAge*1.0e6*DecayConst[ord(at238UPb)])-exp(Age*1.0e6*DecayConst[ord(at238UPb)]));
  Pb74initial := Y0 + (Mu/U238U235) * (exp(OldAge*1.0e6*DecayConst[ord(at235UPb)])-exp(Age*1.0e6*DecayConst[ord(at235UPb)]));
end;{MuValue}

function ConcordiaIntercept ( MaxAge : double;
                              Slope, Intercept : double ): double;
var
  M, B, U      :  double;
  ApproxAge, Difference
               :  double;
  ThisDone         :  Boolean;
begin
  ThisDone:=false;
  repeat
    M:=DecayConst[ord(at238UPb)]/DecayConst[ord(at235UPb)]*Exp(DecayConst[ord(at238UPb)]*MaxAge-DecayConst[ord(at235UPb)]*MaxAge);
    B:=(Exp(DecayConst[ord(at238UPb)]*MaxAge)-1.0)-M*(Exp(DecayConst[ord(at235UPb)]*MaxAge)-1.0);
    U:=(Intercept-B)/(M-Slope);
    if (U>-1.0) then ApproxAge:=1.0/DecayConst[ord(at235UPb)]*Ln(1.00+U)
                else begin
                   ApproxAge:=0.0;
                   ThisDone:=true;
                end;
    Difference:=Abs(MaxAge-ApproxAge);
    MaxAge:=ApproxAge;
    if Difference<AgeTolerance then ThisDone:=true;
  until ThisDone;
  ConcordiaIntercept:=MaxAge;
end; //ConcordiaIntercept

function ConcordiaAgeErr ( LowUp : shortint; MaxAge : double;
                           Slope, SlopeError,
                           Intercept, InterceptError : double;
                           IncludeDCUncertainty : boolean;
                           AgePlusAgeMinus : string ): double;
var
  M, B, U, D, EE, V :  double;
  ApproxAge, Difference :  double;
  ThisDone         :  Boolean;
  temp, temp1      :  double;
  tDCx, tDCxErr,
  tDCy, tDCyErr : double;
begin
  temp1 := -999.99;
  ThisDone:=false;
  if (not IncludeDCUncertainty) then
  begin
    repeat
      M:=DecayConst[ord(at238UPb)]/DecayConst[ord(at235UPb)]*Exp(DecayConst[ord(at238UPb)]*MaxAge-DecayConst[ord(at235UPb)]*MaxAge);
      B:=(Exp(DecayConst[ord(at238UPb)]*MaxAge)-1.0)-M*(Exp(DecayConst[ord(at235UPb)]*MaxAge)-1.0)-Intercept;
      D:=2.0*(B*(M-Slope)+Xcentroid*SlopeError*SlopeError);
      EE:=(M-Slope)*(M-Slope)-SlopeError*SlopeError;
      V:=B*B-InterceptError*InterceptError;
      temp:=D*D-4.0*EE*V;
      if temp<0.0 then
      begin
        if (temp > -0.00001) then temp1:=Abs(temp);
      end
      else begin
        temp1:=temp;
      end;

      if temp1>0.0 then begin
        U:=(-1.0*D+(1.0*LowUp)*Sqrt(temp1))/(2.0*EE);
        if U>-1.0 then
          ApproxAge:=1.0/DecayConst[ord(at235UPb)]*Ln(1.0+U)
        else begin
          ThisDone:=true;
          ApproxAge:=-5e9;
        end;
      end
      else begin
        ThisDone:=true;
        ApproxAge:=0.0;
        if (LowUp > 0) then ApproxAge:= 5e9;
        if (LowUp < 0) then ApproxAge:=-5e9;
      end;
      Difference:=Abs(MaxAge-ApproxAge);
      MaxAge:=ApproxAge;
      if Difference<AgeTolerance then ThisDone:=true;
    until ThisDone;
  end;
  if (IncludeDCUncertainty) then
  begin
    repeat
      tDCx := DecayConst[ord(at238UPb)];
      tDCxErr := DecayConstUncertainty[ord(at238UPb)];
      tDCy := DecayConst[ord(at235UPb)];
      tDCyErr := DecayConstUncertainty[ord(at235UPb)];
      //for curve above the normal concordia in Wetherill plot
      if (lowercase(AgePlusAgeMinus) = UncertaintyPlus) then
      begin
        tDCx := tDCx - (tDCx*tDCxErr)*TMultiplier(1.0*N_Rep)/100.0;
        tDCy := tDCy + (tDCy*tDCyErr)*TMultiplier(1.0*N_Rep)/100.0;
      end;
      //for curve below the normal concordia in Wetherill plot
      if (lowercase(AgePlusAgeMinus) = UncertaintyMinus) then
      begin
        tDCx := tDCx + (tDCx*tDCxErr)*TMultiplier(1.0*N_Rep)/100.0;
        tDCy := tDCy - (tDCy*tDCyErr)*TMultiplier(1.0*N_Rep)/100.0;
      end;
      M:=(tDCx)/tDCy*Exp(tDCx*MaxAge-tDCy*MaxAge);
      B:=(Exp(tDCx*MaxAge)-1.0)-M*(Exp(tDCy*MaxAge)-1.0)-Intercept;
      D:=2.0*(B*(M-Slope)+Xcentroid*SlopeError*SlopeError);
      EE:=(M-Slope)*(M-Slope)-SlopeError*SlopeError;
      V:=B*B-InterceptError*InterceptError;
      temp:=D*D-4.0*EE*V;
      if temp<0.0 then
      begin
        if (temp > -0.00001) then temp1:=Abs(temp);
      end
      else begin
        temp1:=temp;
      end;

      if temp1>0.0 then begin
        U:=(-1.0*D+(1.0*LowUp)*Sqrt(temp1))/(2.0*EE);
        if U>-1.0 then
          ApproxAge:=1.0/DecayConst[ord(at235UPb)]*Ln(1.0+U)
        else begin
          ThisDone:=true;
          ApproxAge:=-5e9;
        end;
      end
      else begin
        ThisDone:=true;
        ApproxAge:=0.0;
        if (LowUp > 0) then ApproxAge:= 5e9;
        if (LowUp < 0) then ApproxAge:=-5e9;
      end;
      Difference:=Abs(MaxAge-ApproxAge);
      MaxAge:=ApproxAge;
      if Difference<AgeTolerance then ThisDone:=true;
    until ThisDone;
  end;
  ConcordiaAgeErr:=MaxAge;
end;//ConcordiaAgeErr

procedure ConcordiaErrors;
var
  IncludeDCUncertainty : boolean;
  AgePlusAgeMinus : string;   //used to define which of three concordia curves to use
  //neither - regular concordia curve ignoring decay constant uncertainties
  //plus - concordia curve assuming both decay constants are underestimated
  //minus - concordia curve assuming both decay constants are overestimated
begin
  //calculate values excluding decay constant uncertainties
  IncludeDCUncertainty := false;
  AgePlusAgeMinus := 'neither';
  UprIntercept:=ConcordiaIntercept(5.0E9,Slope,Intercept);
  UprUprAgeError:=ConcordiaAgeErr(1,5.0E9,Slope,SlopeError*T_Mult,
                  Intercept,InterceptError*T_Mult,IncludeDCUncertainty,AgePlusAgeMinus)-UprIntercept;
  UprLwrAgeError:=UprIntercept-ConcordiaAgeErr(-1,5.0E9,Slope,SlopeError*T_Mult,
                  Intercept,InterceptError*T_Mult,IncludeDCUncertainty,AgePlusAgeMinus);
  LwrIntercept:=ConcordiaIntercept(-1.0E9,Slope,Intercept);
  LwrUprAgeError:=ConcordiaAgeErr(1,-1.0E9,Slope,SlopeError*T_Mult,
                  Intercept,InterceptError*T_Mult,IncludeDCUncertainty,AgePlusAgeMinus)-LwrIntercept;
  LwrLwrAgeError:=LwrIntercept-ConcordiaAgeErr(-1,-1.0E9,Slope,SlopeError*T_Mult,
                  Intercept,InterceptError*T_Mult,IncludeDCUncertainty,AgePlusAgeMinus);
  UprIntercept:=UprIntercept/1.0E6;
  UprUprAgeError:=UprUprAgeError/1.0E6;
  UprLwrAgeError:=UprLwrAgeError/1.0E6;
  LwrIntercept:=LwrIntercept/1.0E6;
  LwrUprAgeError:=LwrUprAgeError/1.0E6;
  LwrLwrAgeError:=LwrLwrAgeError/1.0E6;
  //calculate values including decay constant uncertainties
  IncludeDCUncertainty := true;
  AgePlusAgeMinus := 'neither';
  UprIntercept:=ConcordiaIntercept(5.0E9,Slope,Intercept);
  AgePlusAgeMinus := UncertaintyMinus;
  UprUprAgeErrorIncl:=ConcordiaAgeErr(1,5.0E9,Slope,SlopeError*T_Mult,
                  Intercept,InterceptError*T_Mult,IncludeDCUncertainty,AgePlusAgeMinus)-UprIntercept;
  AgePlusAgeMinus := UncertaintyPlus;
  UprLwrAgeErrorIncl:=UprIntercept-ConcordiaAgeErr(-1,5.0E9,Slope,SlopeError*T_Mult,
                  Intercept,InterceptError*T_Mult,IncludeDCUncertainty,AgePlusAgeMinus);
  AgePlusAgeMinus := 'neither';
  LwrIntercept:=ConcordiaIntercept(-1.0E9,Slope,Intercept);
  AgePlusAgeMinus := UncertaintyPlus;
  LwrLwrAgeErrorIncl:=LwrIntercept - ConcordiaAgeErr(1,-1.0E9,Slope,SlopeError*T_Mult,
                  Intercept,InterceptError*T_Mult,IncludeDCUncertainty,AgePlusAgeMinus);
  AgePlusAgeMinus := UncertaintyMinus;
  LwrUprAgeErrorIncl:=ConcordiaAgeErr(-1,-1.0E9,Slope,SlopeError*T_Mult,
                  Intercept,InterceptError*T_Mult,IncludeDCUncertainty,AgePlusAgeMinus) - LwrIntercept;
  UprIntercept:=UprIntercept/1.0E6;
  UprUprAgeErrorIncl:=UprUprAgeErrorIncl/1.0E6;
  UprLwrAgeErrorIncl:=UprLwrAgeErrorIncl/1.0E6;
  LwrIntercept:=LwrIntercept/1.0E6;
  LwrUprAgeErrorIncl:=LwrUprAgeErrorIncl/1.0E6;
  LwrLwrAgeErrorIncl:=LwrLwrAgeErrorIncl/1.0E6;
end;{procedure ConcordiaErrors}

procedure NewConcordiaErrors;
var
  t1   : double;
  IncludeDCUncertainty : boolean;
  AgePlusAgeMinus : string;   //used to define which of three concordia curves to use
  //neither - regular concordia curve ignoring decay constant uncertainties
  //plus - concordia curve including decay constant uncertainties, above the normal one in a Wetherill plot
  //minus - concordia curve including decay constant uncertainties, below the normal one in a Wetherill plot
begin
  //calculate values excluding decay constant uncertainties
  IncludeDCUncertainty := false;
  //calculate values excluding decay constant uncertainties
  Get_NewSlope;
  AgePlusAgeMinus := 'neither';
  UprIntercept:=ConcordiaIntercept(5.0E9,NewSlope,NewIntercept);
  UprUprAgeError:=ConcordiaAgeErr(1,5.0E9,NewSlope,NewSlopeError*T_Mult,
                   NewIntercept,NewInterceptError*T_Mult,IncludeDCUncertainty,UncertaintyNeither)- UprIntercept;
  UprLwrAgeError:=UprIntercept-ConcordiaAgeErr(-1,5.0E9,NewSlope,NewSlopeError*T_Mult,
                   NewIntercept,NewInterceptError*T_Mult,IncludeDCUncertainty,UncertaintyNeither);
  UprUprAgeError2:=UprIntercept;
  AgePlusAgeMinus := 'neither';
  LwrIntercept:=ConcordiaIntercept(-1.0E9,NewSlope,NewIntercept);
  LwrUprAgeError:=ConcordiaAgeErr(1,-1.0E9,NewSlope,NewSlopeError*T_Mult,
                   NewIntercept,NewInterceptError*T_Mult,IncludeDCUncertainty,UncertaintyNeither)- LwrIntercept;
  LwrLwrAgeError:= LwrIntercept-ConcordiaAgeErr(-1,-1.0E9,NewSlope,NewSlopeError*T_Mult,
                    NewIntercept,NewInterceptError*T_Mult,IncludeDCUncertainty,UncertaintyNeither);
  UprIntercept:= UprIntercept/1.0E6;
  UprUprAgeError:= UprUprAgeError/1.0E6;
  UprLwrAgeError:= UprLwrAgeError/1.0E6;
  LwrIntercept:= LwrIntercept/1.0E6;
  LwrUprAgeError:= LwrUprAgeError/1.0E6;
  LwrLwrAgeError:= LwrLwrAgeError/1.0E6;
  IncludeDCUncertainty := false;
  if (LwrIntercept < 0.0) then
  begin
    if ((LwrIntercept+LwrUprAgeError) > 0.0) then
    begin
      AdjustForNegativeIntercept := true;
      //ShowMessage('Adjusted lower regression '+FormatFloat('###0.0000',LwrIntercept)+'___'+FormatFloat('###0.0000',LwrUprAgeError));
      IncludeDCUncertainty := false;
      AgePlusAgeMinus := 'neither';
      t1:=Xcentroid/Ycentroid/U238U235;
      UprUprAgeError2:=PbPbAge(t1,IncludeDCUncertainty,UncertaintyNeither);
    end;
  end;
  UprUprAgeError2:= UprUprAgeError2/1.0E6;
  //calculate values including decay constant uncertainties
  IncludeDCUncertainty := false;
  AgePlusAgeMinus := 'neither';
  UprIntercept:=ConcordiaIntercept(5.0E9,NewSlope,NewIntercept);
  IncludeDCUncertainty := true;
  AgePlusAgeMinus := UncertaintyMinus;
  UprUprAgeErrorIncl:=ConcordiaAgeErr(1,5.0E9,NewSlope,NewSlopeError*T_Mult,
                   NewIntercept,NewInterceptError*T_Mult,IncludeDCUncertainty,UncertaintyMinus) - UprIntercept;
  AgePlusAgeMinus := UncertaintyPlus;
  UprLwrAgeErrorIncl:=UprIntercept - ConcordiaAgeErr(-1,5.0E9,NewSlope,NewSlopeError*T_Mult,
                   NewIntercept,NewInterceptError*T_Mult,IncludeDCUncertainty,UncertaintyPlus);
  //UprUprAgeError2Incl:=UprIntercept;
  IncludeDCUncertainty := false;
  AgePlusAgeMinus := 'neither';
  LwrIntercept:=ConcordiaIntercept(-1.0E9,NewSlope,NewIntercept);
  IncludeDCUncertainty := true;
  AgePlusAgeMinus := UncertaintyPlus;
  LwrUprAgeErrorIncl:=ConcordiaAgeErr(1,-1.0E9,NewSlope,NewSlopeError*T_Mult,
                   NewIntercept,NewInterceptError*T_Mult,IncludeDCUncertainty,UncertaintyPlus) - LwrIntercept;
  AgePlusAgeMinus := UncertaintyMinus;
  LwrLwrAgeErrorIncl:= LwrIntercept - ConcordiaAgeErr(-1,-1.0E9,NewSlope,NewSlopeError*T_Mult,
                    NewIntercept,NewInterceptError*T_Mult,IncludeDCUncertainty,UncertaintyMinus);
  UprIntercept:= UprIntercept/1.0E6;
  UprUprAgeErrorIncl:= UprUprAgeErrorIncl/1.0E6;
  UprLwrAgeErrorIncl:= UprLwrAgeErrorIncl/1.0E6;
  LwrIntercept:= LwrIntercept/1.0E6;
  LwrUprAgeErrorIncl:= LwrUprAgeErrorIncl/1.0E6;
  LwrLwrAgeErrorIncl:= LwrLwrAgeErrorIncl/1.0E6;
  //UprUprAgeError2Incl:= UprUprAgeError2Incl/1.0E6;
end;//procedure NewConcordiaErrors


procedure WtAver ( NS                : integer;
                  var WtAver, MSWD,
                      WESDoM, WOSDoM : double;
                  var N              : integer);
const
  OmitWESDoM : boolean = true;
var
  i            : integer;
  W            : double;
  tProbabilityOfFit : double;
  tMSWD : double;
begin
  //for i := 1 to NS do
  //begin
  //  ShowMessage(IntToStr(i)+'**'+FormatFloat('###0.0000000',Xtra[i])+'**'+FormatFloat('####0.0000000',Xtra1[i]));
  //end;
  N:=0;
  Sum[1]:=0.0; Sum[2]:=0.0; Sum[3]:=0.0; Sum[4]:=0.0; Sum[5]:=0.0;
  if (NS > 1) then
  begin
    if (NS > MaxSamp) then NS := MaxSamp;
    for i:=1 to NS do
    begin
      if ((RFlg[i] = 'Y') and (PFlg[i] = 'Y')) then
      begin
        N:=N+1;
        if (Xtra1[i] <> 0.0) then W:=1.0/(Xtra1[i]*Xtra1[i])
                             else W:=1.0;
        //ShowMessage('W '+IntToStr(i)+'  '+FormatFloat('###########0.000',W));
        //ShowMessage('WX '+IntToStr(i)+'  '+FormatFloat('###########0.000',W*Xtra[i]));
        //ShowMessage('WXX '+IntToStr(i)+'  '+FormatFloat('###########0.000',W*Xtra[i]*Xtra[i]));
        Sum[1]:=Sum[1]+W;
        Sum[2]:=Sum[2]+W*Xtra[i];
        Sum[3]:=Sum[3]+W*Xtra[i]*Xtra[i];
      end;
    end;
    //ShowMessage('Sum1 '+FormatFloat('###########0.000',Sum[1]));
    //ShowMessage('Sum2 '+FormatFloat('###########0.000',Sum[2]));
    //ShowMessage('Sum3 '+FormatFloat('###########0.000',Sum[3]));
    if (N > 1) then
    begin
      WtAver:=Sum[2]/Sum[1];
      MSWD:=( Sum[3]-Sum[2]*Sum[2]/Sum[1])/(1.0*(N-1));
      //ShowMessage('MSWD = '+FormatFloat('####0.000',MSWD));
      tMSWD := MSWD;
      ProbabilityOfFit := ProbabilityOfF(1.0*(N),1.0*(N_Rep),MSWD,1);
      if (Sum[1] > 0) then WESDoM:=1.0/Sqrt(Sum[1])
                      else WESDoM:=-99.9;
      Sum[1]:=0.0; Sum[2]:=0.0; Sum[3]:=0.0; Sum[4]:=0.0; Sum[5]:=0.0;
      for i:=1 to NS do
      begin
        if ((RFlg[i] = 'Y') and (PFlg[i] = 'Y')) then
        begin
          if (Xtra1[i] <> 0.0) then W:=1.0/(Xtra1[i]*Xtra1[i] + WESDoM*WESDoM)
                               else W:=1.0;
          Sum[1]:=Sum[1]+W;
          Sum[2]:=Sum[2]+W*Xtra[i];
          Sum[3]:=Sum[3]+W*Xtra[i]*Xtra[i];
          Sum[4]:=Sum[4]+W;
          Sum[5]:=Sum[5]+W*(Xtra[i]-WtAver)*(Xtra[i]-WtAver);
        end;
      end;
      MSWD:=(Sum[3]-Sum[2]*Sum[2]/Sum[1])/(1.0*(N-1)); //original equation
      //ShowMessage('MSWD = '+FormatFloat('####0.000',MSWD));
      //test this assuming not to include WESDoM for acceptable statistics
      if (tProbabilityOfFit <= 0.05) then MSWD := tMSWD;  //added this for the test
      if (Sum[4] > 0) then WOSDoM:=Sqrt((Sum[5]/Sum[4])/(1.0*(N-1)))
                      else WOSDoM:=-99.9;
    end
    else begin
      WESDoM:=0.0;
      WOSDoM:=0.0;
      WtAver:=0.0;
      MSWD:=0.0;
      if (N = 1) then
      begin
        for i:=1 to NS do
        begin
          if ((RFlg[i] = 'Y') and (PFlg[i] = 'Y')) then
          begin
            WtAver := Xtra[i];
            WESDom := Xtra1[i];
            WOSDom := Xtra1[i];
          end;
        end;
        MSWD := 0.0;
      end;
    end;
    {include Troutman option for MSWD > F}
  end else
  begin
    if (NS = 1) then
    begin
      WtAver := Xtra[1];
      WOSDoM := Xtra1[1];
      WESDom := Xtra1[1];
      MSWD := 0.0;
      N := 1;
    end;
  end;
end;


procedure Model76_AgeandError ( var Age,
                                    UprUprAgeError, UprLwrAgeError : double;
                                    Incl : char;
                                var Ninc : integer);
var
  E76, V76, Fd, Fm,
  Chordlength       :  double;
  Ex, Ey, RRR            :  double;
  AnalType8         :  char;
  X_Uint, X_Lint, Y_Uint,
  Y_Lint            :  double;
  LWt               :  array[1..2] of double;
  J                 : integer;
  IncludeDCUncertainty : boolean;
  AgePlusAgeMinus : string;
begin
  IncludeDCUncertainty := false;
  AgePlusAgeMinus := 'neither';
  if (Incl='Y') then Ninc:=0;
  for J:=1 to NumberOfPoints do begin
   if (RFlg[J]=Incl) then begin
    Age:=Model76_Age(J);
    case ErrTyp[J] of
      '1' : begin
              LWt[1]:=ErrorWt[J,1];
              LWt[2]:=ErrorWt[J,2];
            end;
      '2' : begin
              LWt[1]:=ErrorWt[J,1];
              LWt[2]:=ErrorWt[J,2]*100.0/Ratio[J,2];
            end;
      '3' : begin
              LWt[1]:=ErrorWt[J,1]*100.0/Ratio[J,1];
              LWt[2]:=ErrorWt[J,2];
            end;
      '4' : begin
              LWt[1]:=ErrorWt[J,1]*100.0/Ratio[J,1];
              LWt[2]:=ErrorWt[J,2]*100.0/Ratio[J,2];
            end;
    end;
        X_Uint:=Exp(DecayConst[ord(at235UPb)]*Age)-1;
        Y_Uint:=Exp(DecayConst[ord(at238UPb)]*Age)-1;
        X_Lint:=0.0;
        Y_Lint:=0.0;
        Chordlength:=Sqrt((X_Uint-X_Lint)*(X_Uint-X_Lint)+
                          (Y_Uint-Y_Lint)*(Y_Uint-Y_Lint));
        if (Chordlength=0.0) then Chordlength:=1.0e-6;
        Fd:=Sqrt((X_Uint-Ratio[J,1])*(X_Uint-Ratio[J,1])+
                 (Y_Uint-Ratio[J,2])*(Y_Uint-Ratio[J,2]));
        Fd:=Fd/Chordlength;
        if (Fd<>1.0) then Fm:=Fd/(1.0-Fd)
                     else Fm:=1.0e9;
        Ex:=LWt[1];
        Ey:=LWt[2];
        E76:=Sqrt(Ex*Ex+Ey*Ey-2.0*R[J]*Ex*Ey);
    if (AnalType8='U') then begin
        Ex:=Sqrt(Ex*Ex+Lud_pp*Lud_pp*Fm*Fm);
        Ey:=Sqrt(Ey*Ey+Lud_pp*Lud_pp*Fm*Fm);
        V76:=E76*E76+((Lud_pp*Fm*2.0)*(Lud_pp*Fm*2.0));
        RRR:=(Ex*Ex+Ey*Ey-V76)/(2.0*Ex*Ey);
        E76:=Sqrt(V76);
    end;
    LWt[1]:=Ex;
    LWt[2]:=Ey;
    case ErrTyp[J] of
      '1' : begin
              LWt[1]:=LWt[1];
              LWt[2]:=LWt[2];
            end;
      '2' : begin
              LWt[1]:=LWt[1];
              LWt[2]:=LWt[2]/100.0*Ratio[J,2];
            end;
      '3' : begin
              LWt[1]:=LWt[1]/100.0*Ratio[J,1];
              LWt[2]:=LWt[2];
            end;
      '4' : begin
              LWt[1]:=LWt[1]/100.0*Ratio[J,1];
              LWt[2]:=LWt[2]/100.0*Ratio[J,2];
            end;
    end;
    E76:=E76*Slope/100.0;
    Xtra[J]:=Slope;
    Xtra1[J]:=E76;
    ZPrec[J]:=E76;
    if ((AnalType8 = 'E') and (ZPrec[J] > 0.0) and (Ratio[J,3] > 0.0)) then
    begin
      Xtra1[J]:=ZPrec[J];
      E76:=ZPrec[J];
    end;
    Slope:=Slope+T_Mult*E76;
    IncludeDCUncertainty := false;
    AgePlusAgeMinus := 'neither';
    UprUprAgeError:=PbPbAge(Slope,IncludeDCUncertainty,AgePlusAgeMinus)-Age;
    IncludeDCUncertainty := true;
    AgePlusAgeMinus := UncertaintyPlus;
    UprUprAgeErrorIncl:=PbPbAge(Slope,IncludeDCUncertainty,AgePlusAgeMinus)-Age;
    Slope:=Slope-2.0*T_Mult*E76;
    IncludeDCUncertainty := false;
    AgePlusAgeMinus := 'neither';
    if (Slope<0.0) then
      UprLwrAgeError:=Age
      else UprLwrAgeError:=Age-PbPbAge(Slope,IncludeDCUncertainty,AgePlusAgeMinus);
    IncludeDCUncertainty := true;
    AgePlusAgeMinus := UncertaintyMinus;
    if (Slope<0.0) then
      UprLwrAgeErrorIncl:=Age
      else UprLwrAgeErrorIncl:=Age-PbPbAge(Slope,IncludeDCUncertainty,AgePlusAgeMinus);
    Age:=Age/1.0e6;
    UprUprAgeError:=UprUprAgeError/1.0e6;
    UprLwrAgeError:=UprLwrAgeError/1.0e6;
    UprUprAgeErrorIncl:=UprUprAgeError/1.0e6;
    UprLwrAgeErrorIncl:=UprLwrAgeError/1.0e6;
    if ((RFlg[J]='Y') and (Incl='Y')) then
    begin
      Ninc:=Ninc+1;
    end;
   end;
  end;
end;

procedure Model76_AgeError ( var Age,
                                UprUprAgeError, UprLwrAgeError : double;
                            J : integer);
var
  E76, V76, Fd, Fm,
  Chordlength       :  double;
  Ex, Ey, RRR            :  double;
  AnalType8         :  char;
  X_Uint, X_Lint, Y_Uint,
  Y_Lint            :  double;
  LWt               :  array[1..2] of double;
  IncludeDCUncertainty : boolean;
  AgePlusAgeMinus : string;
begin
  IncludeDCUncertainty := false;
  AgePlusAgeMinus := 'neither';
  if (RFlg[J]='Y') then
  begin
    Age:=Model76_Age(J);
    case ErrTyp[J] of
      '1' : begin
              LWt[1]:=ErrorWt[J,1];
              LWt[2]:=ErrorWt[J,2];
            end;
      '2' : begin
              LWt[1]:=ErrorWt[J,1];
              LWt[2]:=ErrorWt[J,2]*100.0/Ratio[J,2];
            end;
      '3' : begin
              LWt[1]:=ErrorWt[J,1]*100.0/Ratio[J,1];
              LWt[2]:=ErrorWt[J,2];
            end;
      '4' : begin
              LWt[1]:=ErrorWt[J,1]*100.0/Ratio[J,1];
              LWt[2]:=ErrorWt[J,2]*100.0/Ratio[J,2];
            end;
    end;
    X_Uint:=Exp(DecayConst[ord(at235UPb)]*Age)-1;
    Y_Uint:=Exp(DecayConst[ord(at238UPb)]*Age)-1;
    X_Lint:=0.0;
    Y_Lint:=0.0;
    Chordlength:=Sqrt((X_Uint-X_Lint)*(X_Uint-X_Lint)+
                      (Y_Uint-Y_Lint)*(Y_Uint-Y_Lint));
    if (Chordlength=0.0) then Chordlength:=1.0e-6;
    Fd:=Sqrt((X_Uint-Ratio[J,1])*(X_Uint-Ratio[J,1])+
             (Y_Uint-Ratio[J,2])*(Y_Uint-Ratio[J,2]));
    Fd:=Fd/Chordlength;
    if (Fd<>1.0) then Fm:=Fd/(1.0-Fd)
                 else Fm:=1.0e9;
    Ex:=LWt[1];
    Ey:=LWt[2];
    E76:=Sqrt(Ex*Ex+Ey*Ey-2.0*R[J]*Ex*Ey);
    if (AnalType8 = 'N') then
    begin
       RRR := 0.0;
    end;
    if (AnalType8='U') then
    begin
        Ex:=Sqrt(Ex*Ex+Lud_pp*Lud_pp*Fm*Fm);
        Ey:=Sqrt(Ey*Ey+Lud_pp*Lud_pp*Fm*Fm);
        V76:=E76*E76+((Lud_pp*Fm*2.0)*(Lud_pp*Fm*2.0));
        RRR:=(Ex*Ex+Ey*Ey-V76)/(2.0*Ex*Ey);
        E76:=Sqrt(V76);
    end;
    LWt[1]:=Ex;
    LWt[2]:=Ey;
    case ErrTyp[J] of
      '1' : begin
              LWt[1]:=LWt[1];
              LWt[2]:=LWt[2];
            end;
      '2' : begin
              LWt[1]:=LWt[1];
              LWt[2]:=LWt[2]/100.0*Ratio[J,2];
            end;
      '3' : begin
              LWt[1]:=LWt[1]/100.0*Ratio[J,1];
              LWt[2]:=LWt[2];
            end;
      '4' : begin
              LWt[1]:=LWt[1]/100.0*Ratio[J,1];
              LWt[2]:=LWt[2]/100.0*Ratio[J,2];
            end;
    end;
    E76:=E76*Slope/100.0;
    {
    Xtra[J]:=Slope;
    Xtra1[J]:=E76;
    }
    if ((AnalType8 = 'E') and (ZPrec[J] > 0.0) and (Ratio[J,3] > 0.0)) then
    begin
      {
      Xtra1[J]:=ZPrec[J];
      }
      E76:=ZPrec[J];
    end;
    Slope:=Slope+T_Mult*E76;
    IncludeDCUncertainty := false;
    AgePlusAgeMinus := 'neither';
    UprUprAgeError:=PbPbAge(Slope,IncludeDCUncertainty,AgePlusAgeMinus)-Age;
    IncludeDCUncertainty := true;
    AgePlusAgeMinus := UncertaintyPlus;
    UprUprAgeErrorIncl:=PbPbAge(Slope,IncludeDCUncertainty,AgePlusAgeMinus)-Age;
    Slope:=Slope-2.0*T_Mult*E76;
    IncludeDCUncertainty := false;
    AgePlusAgeMinus := 'neither';
    if (Slope<0.0) then
      UprLwrAgeError:=Age
      else UprLwrAgeError:=Age-PbPbAge(Slope,IncludeDCUncertainty,AgePlusAgeMinus);
    IncludeDCUncertainty := true;
    AgePlusAgeMinus := UncertaintyMinus;
    if (Slope<0.0) then
      UprLwrAgeErrorIncl:=Age
      else UprLwrAgeErrorIncl:=Age-PbPbAge(Slope,IncludeDCUncertainty,AgePlusAgeMinus);
    Age:=Age/1.0e6;
    UprUprAgeError:=UprUprAgeError/1.0e6;
    UprLwrAgeError:=UprLwrAgeError/1.0e6;
    UprUprAgeErrorIncl:=UprUprAgeErrorIncl/1.0e6;
    UprLwrAgeErrorIncl:=UprLwrAgeErrorIncl/1.0e6;
  end;
end;

function TeraWasserburgIntercept ( MaxAge : double;
                              Slope, Intercept : double ): double;
var
  ApproxAge, Difference :  double;
  ThisDone         :  Boolean;
  Z1, Z2   : double;

function exp5t(Age : double) : double;
begin
  Result := (exp(DecayConst[ord(at235UPb)]*Age));
end;

function exp8t(Age : double) : double;
begin
  Result := (exp(DecayConst[ord(at238UPb)]*Age));
end;

begin
  if ((Slope<>0.0)) then begin
    ThisDone:=false;
    repeat
      Z1:= Slope*(1/(exp8t(MaxAge)-1))+Intercept-((1.0/U238U235)*(exp5t(MaxAge)-1)/(exp8t(MaxAge)-1));
      Z2 := (-1.0*Slope/((exp8t(MaxAge)-1)*(exp8t(MaxAge)-1)))*DecayConst[ord(at238UPb)]*exp8t(MaxAge);
      Z2 := Z2 - (1.0/U238U235)*DecayConst[ord(at235UPb)]*(exp5t(MaxAge)/(exp8t(MaxAge)-1));
      Z2 := Z2 + (1.0/U238U235)*(exp5t(MaxAge)-1)/((exp8t(MaxAge)-1)*(exp8t(MaxAge)-1))*DecayConst[ord(at238UPb)]*exp8t(MaxAge);
      if (Z2 = 0.0) then
      begin
        MaxAge := 0.0;
        ThisDone := true;
        Result := 0.0;
        Exit;
      end;
      ApproxAge:=MaxAge-Z1/Z2;
      Difference:=Abs(MaxAge-ApproxAge);
      MaxAge:=ApproxAge;
      if Difference<AgeTolerance then ThisDone:=true;
    until ThisDone;
  end
  else MaxAge:=0.0;
  Result:=MaxAge;
end;{TeraWasserburgIntercept}

function TeraWasserburgAgeErr ( LowUp : shortint; MaxAge : double;
                           Slope, SlopeError,
                           Intercept, InterceptError : double;
                           IncludeDCUncertainty : boolean;
                           AgePlusAgeMinus : string ): double;
var
  ApproxAge, Difference
               :  double;
  ThisDone         :  Boolean;
  Z1, Z2, X1,
  Y1, Y2, Y3, Y4   : double;

function exp5t(Age : double) : double;
begin
  Result := (exp(DecayConst[ord(at235UPb)]*Age));
end;

function exp8t(Age : double) : double;
begin
  Result := (exp(DecayConst[ord(at238UPb)]*Age));
end;

begin
  if ((Slope<>0.0)) then begin
    if (not IncludeDCUncertainty) then
    begin
      ThisDone:=false;
      repeat
        X1 := (exp8t(MaxAge)-1);
        if AllowMessages then ShowMessage('MaxAge = '+FormatFloat('####0.000',MaxAge/1.0e6)+'  '+'X1 = '+FormatFloat('##0.0000',X1));
        if (X1 <> 0.0) then
        begin
          X1 := 1.0/(X1);
          Y1 := Sqrt(InterceptError*InterceptError+SlopeError*SlopeError*X1*(X1-2.0*XCentroid));
          Z1 := (exp5t(MaxAge)-1)*(X1/U238U235)-Slope*X1-Intercept;
          Z1 := Z1-(1.0*LowUp)*Y1;
          Z2 := DecayConst[ord(at235UPb)]*exp5t(MaxAge)/((exp8t(MaxAge)-1)*U238U235);
          Z2 := Z2 - (exp5t(MaxAge)-1)*DecayConst[ord(at238UPb)]*exp8t(MaxAge)/((exp8t(MaxAge)-1)*(exp8t(MaxAge)-1)*U238U235);
          Z2 := Z2 + Slope*DecayConst[ord(at238UPb)]*exp8t(MaxAge)/((exp8t(MaxAge)-1)*(exp8t(MaxAge)-1));

          if AllowMessages then ShowMessage('Y1 = '+FormatFloat('####0.00000',Y1)+'  '+'Z1 = '+FormatFloat('####0.00000',Z1)+'  '+'Z2 = '+FormatFloat('####0.0000000',Z2));

          Y2 := (1.0*LowUp)/(2.0*Y1);
          Y3 := (-1.0*SlopeError*SlopeError/((exp8t(MaxAge)-1)*(exp8t(MaxAge)-1)))*((1.0/(exp8t(MaxAge)-1))-2.0*XCentroid)*DecayConst[ord(at238UPb)]*exp8t(MaxAge);
          Y4 := SlopeError*SlopeError/((exp8t(MaxAge)-1)*(exp8t(MaxAge)-1)*(exp8t(MaxAge)-1))*DecayConst[ord(at238UPb)]*exp8t(MaxAge);
          Z2 := Z2+(1.0*LowUp)*Y2*(Y3-Y4);

          if AllowMessages then ShowMessage('Y2 = '+FormatFloat('####0.0000000',Y2)+'   '+'Y3 = '+FormatFloat('####0.0000000',Y3)+'   '+'Y4 = '+FormatFloat('####0.0000000',Y4));

          if (Z2 = 0.0) then
          begin
            ThisDone := true;
            Result := MaxAge;
            Exit;
          end;
          ApproxAge:=MaxAge-Z1/Z2;
          Difference:=Abs(MaxAge-ApproxAge);
          MaxAge:=ApproxAge;
          if Difference<AgeTolerance then ThisDone:=true;
        end else
        begin
            MaxAge := 0.0;
            ThisDone := true;
        end;
      until ThisDone;
    end;
    if (IncludeDCUncertainty) then
    begin
      if (AgePlusAgeMinus = UncertaintyPlus) then
      begin
        ThisDone:=false;
        repeat
          X1 := (exp8tPlus(MaxAge)-1);
          if AllowMessages then ShowMessage('MaxAge = '+FormatFloat('####0.000',MaxAge/1.0e6)+'  '+'X1 = '+FormatFloat('##0.0000',X1));
          if (X1 <> 0.0) then
          begin
            X1 := 1.0/(X1);
            Y1 := Sqrt(InterceptError*InterceptError+SlopeError*SlopeError*X1*(X1-2.0*XCentroid));
            Z1 := (exp5tMinus(MaxAge)-1)*(X1/U238U235)-Slope*X1-Intercept;
            Z1 := Z1-(1.0*LowUp)*Y1;
            Z2 := DecayConst[ord(at235UPb)]*exp5tMinus(MaxAge)/((exp8tPlus(MaxAge)-1)*U238U235);
            Z2 := Z2 - (exp5tMinus(MaxAge)-1)*DecayConst[ord(at238UPb)]*exp8tPlus(MaxAge)/((exp8tPlus(MaxAge)-1)*(exp8tPlus(MaxAge)-1)*U238U235);
            Z2 := Z2 + Slope*DecayConst[ord(at238UPb)]*exp8tPlus(MaxAge)/((exp8tPlus(MaxAge)-1)*(exp8tPlus(MaxAge)-1));

            if AllowMessages then ShowMessage('Y1 = '+FormatFloat('####0.00000',Y1)+'  '+'Z1 = '+FormatFloat('####0.00000',Z1)+'  '+'Z2 = '+FormatFloat('####0.0000000',Z2));

            Y2 := (1.0*LowUp)/(2.0*Y1);
            Y3 := (-1.0*SlopeError*SlopeError/((exp8tPlus(MaxAge)-1)*(exp8tPlus(MaxAge)-1)))*((1.0/(exp8tPlus(MaxAge)-1))-2.0*XCentroid)*DecayConst[ord(at238UPb)]*exp8tPlus(MaxAge);
            Y4 := SlopeError*SlopeError/((exp8tPlus(MaxAge)-1)*(exp8tPlus(MaxAge)-1)*(exp8tPlus(MaxAge)-1))*DecayConst[ord(at238UPb)]*exp8tPlus(MaxAge);
            Z2 := Z2+(1.0*LowUp)*Y2*(Y3-Y4);

            if AllowMessages then ShowMessage('Y2 = '+FormatFloat('####0.0000000',Y2)+'   '+'Y3 = '+FormatFloat('####0.0000000',Y3)+'   '+'Y4 = '+FormatFloat('####0.0000000',Y4));

            if (Z2 = 0.0) then
            begin
              ThisDone := true;
              Result := MaxAge;
              Exit;
            end;
            ApproxAge:=MaxAge-Z1/Z2;
            Difference:=Abs(MaxAge-ApproxAge);
            MaxAge:=ApproxAge;
            if Difference<AgeTolerance then ThisDone:=true;
          end else
          begin
              MaxAge := 0.0;
              ThisDone := true;
          end;
        until ThisDone;
      end;
      if (AgePlusAgeMinus = UncertaintyMinus) then
      begin
        ThisDone:=false;
        repeat
          X1 := (exp8tMinus(MaxAge)-1);
          if AllowMessages then ShowMessage('MaxAge = '+FormatFloat('####0.000',MaxAge/1.0e6)+'  '+'X1 = '+FormatFloat('##0.0000',X1));
          if (X1 <> 0.0) then
          begin
            X1 := 1.0/(X1);
            Y1 := Sqrt(InterceptError*InterceptError+SlopeError*SlopeError*X1*(X1-2.0*XCentroid));
            Z1 := (exp5tPlus(MaxAge)-1)*(X1/U238U235)-Slope*X1-Intercept;
            Z1 := Z1-(1.0*LowUp)*Y1;
            Z2 := DecayConst[ord(at235UPb)]*exp5tPlus(MaxAge)/((exp8tMinus(MaxAge)-1)*U238U235);
            Z2 := Z2 - (exp5tMinus(MaxAge)-1)*DecayConst[ord(at238UPb)]*exp8tMinus(MaxAge)/((exp8tMinus(MaxAge)-1)*(exp8tMinus(MaxAge)-1)*U238U235);
            Z2 := Z2 + Slope*DecayConst[ord(at238UPb)]*exp8tMinus(MaxAge)/((exp8tMinus(MaxAge)-1)*(exp8tMinus(MaxAge)-1));

            if AllowMessages then ShowMessage('Y1 = '+FormatFloat('####0.00000',Y1)+'  '+'Z1 = '+FormatFloat('####0.00000',Z1)+'  '+'Z2 = '+FormatFloat('####0.0000000',Z2));

            Y2 := (1.0*LowUp)/(2.0*Y1);
            Y3 := (-1.0*SlopeError*SlopeError/((exp8tMinus(MaxAge)-1)*(exp8tMinus(MaxAge)-1)))*((1.0/(exp8tMinus(MaxAge)-1))-2.0*XCentroid)*DecayConst[ord(at238UPb)]*exp8tMinus(MaxAge);
            Y4 := SlopeError*SlopeError/((exp8tMinus(MaxAge)-1)*(exp8tMinus(MaxAge)-1)*(exp8tMinus(MaxAge)-1))*DecayConst[ord(at238UPb)]*exp8tMinus(MaxAge);
            Z2 := Z2+(1.0*LowUp)*Y2*(Y3-Y4);

            if AllowMessages then ShowMessage('Y2 = '+FormatFloat('####0.0000000',Y2)+'   '+'Y3 = '+FormatFloat('####0.0000000',Y3)+'   '+'Y4 = '+FormatFloat('####0.0000000',Y4));

            if (Z2 = 0.0) then
            begin
              ThisDone := true;
              Result := MaxAge;
              Exit;
            end;
            ApproxAge:=MaxAge-Z1/Z2;
            Difference:=Abs(MaxAge-ApproxAge);
            MaxAge:=ApproxAge;
            if Difference<AgeTolerance then ThisDone:=true;
          end else
          begin
              MaxAge := 0.0;
              ThisDone := true;
          end;
        until ThisDone;
      end;
    end;
  end
  else MaxAge:=0.0;
  Result:=MaxAge;
end;{TeraWasserburgAgeError}

procedure NewTeraWasserburgErrors;
var
  t1   : double;
  IncludeDCUncertainty : boolean;
  AgePlusAgeMinus : string;
begin
  IncludeDCUncertainty := false;
  AgePlusAgeMinus := 'neither';
  Get_NewSlope;
  AllowMessages := false;
  UprIntercept:=TeraWasserburgIntercept(5.0E9,NewSlope,NewIntercept);
  LwrIntercept:=TeraWasserburgIntercept(1.0E6,NewSlope,NewIntercept);
  UprUprAgeError:=TeraWasserburgAgeErr(1,5.0E9,NewSlope,NewSlopeError*T_Mult,
                   NewIntercept,NewInterceptError*T_Mult,IncludeDCUncertainty,UncertaintyPlus);
  if AllowMessages then ShowMessage('Upr upr error = '+FormatFloat('####0.00',UprUprAgeError/1.0e6));
  UprUprAgeError := UprUprAgeError - UprIntercept;
  UprLwrAgeError:=TeraWasserburgAgeErr(-1,5.0e9,NewSlope,NewSlopeError*T_Mult,
                   NewIntercept,NewInterceptError*T_Mult,IncludeDCUncertainty,UncertaintyMinus);
  if AllowMessages then ShowMessage('Upr lwr error = '+FormatFloat('####0.00',UprLwrAgeError/1.0e6));
  UprLwrAgeError := UprIntercept - UprLwrAgeError;
  UprUprAgeError2:=UprIntercept;

  AllowMessages := false;
  if AllowMessages then ShowMessage('Lwr errors');
  LwrUprAgeError:=TeraWasserburgAgeErr(-1,1.0E6,NewSlope,NewSlopeError*T_Mult,
                   NewIntercept,NewInterceptError*T_Mult,IncludeDCUncertainty,UncertaintyPlus);
  if AllowMessages then ShowMessage('Lwr upr error = '+FormatFloat('####0.00',LwrUprAgeError/1.0e6));
  AllowMessages := false;
  LwrUprAgeError := LwrUprAgeError - LwrIntercept;
  LwrLwrAgeError:= TeraWasserburgAgeErr(1,1.0e6,NewSlope,NewSlopeError*T_Mult,
                    NewIntercept,NewInterceptError*T_Mult,IncludeDCUncertainty,UncertaintyMinus);
  if AllowMessages then ShowMessage('Lwr lwr error = '+FormatFloat('####0.00',LwrLwrAgeError/1.0e6));
  LwrLwrAgeError := LwrIntercept - LwrLwrAgeError;

  AllowMessages := false;
  IncludeDCUncertainty := true;
  //UprIntercept:=TeraWasserburgIntercept(5.0E9,NewSlope,NewIntercept);
  //LwrIntercept:=TeraWasserburgIntercept(1.0E6,NewSlope,NewIntercept);
  UprUprAgeErrorIncl:=TeraWasserburgAgeErr(1,5.0E9,NewSlope,NewSlopeError*T_Mult,
                   NewIntercept,NewInterceptError*T_Mult,IncludeDCUncertainty,UncertaintyPlus);
  if AllowMessages then ShowMessage('Upr upr error = '+FormatFloat('####0.00',UprUprAgeError/1.0e6));
  UprUprAgeErrorIncl := UprUprAgeErrorIncl - UprIntercept;
  UprLwrAgeErrorIncl:=TeraWasserburgAgeErr(-1,5.0e9,NewSlope,NewSlopeError*T_Mult,
                   NewIntercept,NewInterceptError*T_Mult,IncludeDCUncertainty,UncertaintyMinus);
  if AllowMessages then ShowMessage('Upr lwr error = '+FormatFloat('####0.00',UprLwrAgeError/1.0e6));
  UprLwrAgeErrorIncl := UprIntercept - UprLwrAgeErrorIncl;
  UprUprAgeError2:=UprIntercept;

  AllowMessages := false;
  if AllowMessages then ShowMessage('Lwr errors');
  if (AnalType = '8') then AgePlusAgeMinus := UncertaintyPlus;
  if (AnalType = 'A') then AgePlusAgeMinus := UncertaintyMinus;
  LwrUprAgeErrorIncl:=TeraWasserburgAgeErr(-1,1.0E6,NewSlope,NewSlopeError*T_Mult,
                   NewIntercept,NewInterceptError*T_Mult,IncludeDCUncertainty,AgePlusAgeMinus);
  if AllowMessages then ShowMessage('Lwr upr error = '+FormatFloat('####0.00',LwrUprAgeError/1.0e6));
  AllowMessages := false;
  LwrUprAgeErrorIncl := LwrUprAgeErrorIncl - LwrIntercept;
  if (AnalType = '8') then AgePlusAgeMinus := UncertaintyMinus;
  if (AnalType = 'A') then AgePlusAgeMinus := UncertaintyPlus;
  LwrLwrAgeErrorIncl:= TeraWasserburgAgeErr(1,1.0e6,NewSlope,NewSlopeError*T_Mult,
                    NewIntercept,NewInterceptError*T_Mult,IncludeDCUncertainty,AgePlusAgeMinus);
  if AllowMessages then ShowMessage('Lwr lwr error = '+FormatFloat('####0.00',LwrLwrAgeError/1.0e6));
  LwrLwrAgeErrorIncl := LwrIntercept - LwrLwrAgeErrorIncl;
  AllowMessages := false;

  UprIntercept:= UprIntercept/1.0E6;
  UprUprAgeError:= UprUprAgeError/1.0E6;
  UprLwrAgeError:= UprLwrAgeError/1.0E6;
  LwrIntercept:= LwrIntercept/1.0E6;
  LwrUprAgeError:= LwrUprAgeError/1.0E6;
  LwrLwrAgeError:= LwrLwrAgeError/1.0E6;
  UprUprAgeErrorIncl:= UprUprAgeErrorIncl/1.0E6;
  UprLwrAgeErrorIncl:= UprLwrAgeErrorIncl/1.0E6;
  LwrUprAgeErrorIncl:= LwrUprAgeErrorIncl/1.0E6;
  LwrLwrAgeErrorIncl:= LwrLwrAgeErrorIncl/1.0E6;
  if (LwrIntercept < 0.0) then
  begin
    if ((LwrIntercept+LwrUprAgeError) > 0.0) then
    begin
      IncludeDCUncertainty := false;
      AgePlusAgeMinus := 'neither';
      t1:=Xcentroid/Ycentroid/U238U235;
      UprUprAgeError2:=PbPbAge(t1,IncludeDCUncertainty,AgePlusAgeMinus);
    end;
  end;
  UprUprAgeError2:= UprUprAgeError2/1.0E6;
end;{procedure NewTeraWasserburgErrors}

function CalcArInverseError(LowUp : smallint; TrialX : double;
                            Slope, SlopeError,
                            Intercept, InterceptError : double):double;
var
  ApproxX, Difference
               :  double;
  ThisDone         :  Boolean;
  Z1, Z2, X1, X2   : double;
  i : integer;
const
  Tolerance = 0.0000001;
  MaxIterations = 100;
begin
  if ((Slope<0.0)) then begin
    ThisDone:=false;
    i := 0;
    repeat
      X1 := InterceptError*InterceptError+SlopeError*SlopeError*TrialX*(TrialX-2.0*XCentroid);
      X2 := 2.0*SlopeError*SlopeError*TrialX-2.0*SlopeError*SlopeError*XCentroid;
      Z1 := Slope*TrialX + Intercept+ (1.0*LowUp)*Sqrt(X1);
      Z2 := Slope+(1.0*LowUp)*X2/(2.0*Sqrt(X1));
      ApproxX := TrialX-Z1/Z2;
      Difference:=Abs(TrialX-ApproxX);
      TrialX:=ApproxX;
      if Difference<Tolerance then ThisDone:=true;
      i := i + 1;
    until (ThisDone or (i > MaxIterations));
    if (i > MaxIterations) then TrialX := 999.99;
  end
  else TrialX:=0.0;
  Result:=TrialX;
end;

function Q235(Age : double) : extended;
begin
  Result := DecayConst[ord(at235UPb)]*(exp(DecayConst[ord(at235UPb)]*Age));
end;

function Q238(Age : double) : extended;
begin
  Result := DecayConst[ord(at238UPb)]*(exp(DecayConst[ord(at238UPb)]*Age));
end;

function P235(Age : double) : extended;
begin
  Result := Age*(exp(DecayConst[ord(at235UPb)]*Age));
end;

function P238(Age : double) : extended;
begin
  Result := Age*(exp(DecayConst[ord(at238UPb)]*Age));
end;

function E235(Age : double) : extended;
begin
  Result := exp(DecayConst[ord(at235UPb)]*Age) - 1.0;
end;

function E238(Age : double) : extended;
begin
  Result := exp(DecayConst[ord(at238UPb)]*Age) - 1.0;
end;

procedure ConcordiaInterceptDateErrors( t1,t2 : double;
                     SlopeError, InterceptError : double;
                     CovarianceSlopeIntercept : double;
                     Lambda235, SigmaLambda235 : double;
                     Lambda238, SigmaLambda238 : double;
                 var Sigmat1 : double;
                 var Sigmat2 : double;
                 var SigmaCovt1t2 : double);
var
  m, b, Phi51, Phi52, Phi81, Phi82, 
  Delta, E, M1, M2, M3, M4,
  B1, B2, B3, B4 : double;
  Omega, OmegaInverted : RealArrayC;
  YMatrix, SigmaMatrix : RealVector;
begin
  m := (E238(t1) - E238(t2))/(E235(t1) - E235(t2));
  b := E238(t2) - m*E235(t2);
  Phi51 := E235(t1) + 1;
  Phi52 := E235(t2) + 1;
  Delta := Phi51 - Phi52;
  Phi81 := E238(t1) + 1;
  Phi82 := E238(t2) + 1;
  E := Phi52 - 1;
  M1 := (Q238(t1) - m*Q235(t1))/Delta;
  M2 := -1.0*(Q238(t2) - m*Q235(t2))/Delta;
  M3 := m*(t2*Phi52 - t1*Phi51)/Delta;
  M4 := (t1*Phi81 - t2*Phi82)/Delta;
  B1 := -1.0*E*M1;
  B2 := -1.0*(Delta + E)*M2;
  B3 := -1.0*m*t2*Phi52 - E*M3;
  B4 := t2*Phi82 - E*M4;
  Omega[1,1] := M1*M1;
  Omega[1,2] := M2*M2;
  Omega[1,3] := 2.0*M1*m2;
  Omega[2,1] := B1*B1;
  Omega[2,2] := B2*B2;
  Omega[2,3] := 2.0*B1*B2;
  Omega[3,1] := M1*B1;
  Omega[3,2] := M2*B2;
  Omega[3,3] := M1*B2 + M2*B1;
  YMatrix[1] := SlopeError*SlopeError + M3*M3*SigmaLambda235*SigmaLambda235 - M4*M4*SigmaLambda238*SigmaLambda238;
  YMatrix[2] := InterceptError*InterceptError + B3*B3*SigmaLambda235*SigmaLambda235 - B4*B4*SigmaLambda238*SigmaLambda238;
  YMatrix[3] := CovarianceSlopeIntercept + M3*B3*SigmaLambda235*SigmaLambda235 - M4*B4*SigmaLambda238*SigmaLambda238;
  MInv(Omega,OmegaInverted,3,3);
  MmultVR(OmegaInverted,YMatrix,SigmaMatrix,3,3,1);
  Sigmat1 := Sqrt(SigmaMatrix[1]);
  Sigmat2 := Sqrt(SigmaMatrix[2]);
  SigmaCovt1t2 := Sqrt(SigmaMatrix[3]);
end;

function SigmatLambda (temp : double; Lambda : double; SigmaLambda : double) : double;
begin
  Result := (SigmaLambda/Lambda)*((ln(1.0 + temp))/Lambda);
end;

function Sigmat76Lambda (t : double;
           Pb76 : double; SigmaPb76 : double;
           Lambda235 : double; SigmaLambda235 : double;
           Lambda238 : double; SigmaLambda238 : double) : double;
var
  tmp, tmp1 : double;
  r : double;
begin
  r := U238U235*E235(t)/E238(t);
  tmp := (E238(t)*E238(t)*SigmaPb76*SigmaPb76) + (U238U235*P235(t))*(U238U235*P235(t))*SigmaLambda235*SigmaLambda235;
  tmp := tmp + (r*P238(t))*(r*P238(t))*(SigmaLambda238*SigmaLambda238);
  tmp1 := (U238U235*Q235(t) - r*Q238(t))*(U238U235*Q235(t) - r*Q238(t));
  Result := tmp/tmp1;
end;

procedure WtAverConcordia ( NS                  : integer;
                   var EquivalenceXVar  : double;
                   var EquivalenceYVar  : double;
                   var EquivalenceCoVar : double;
                   var N                : integer;
                   var MSWDEquivalence  : double;
                   var df               : integer;
                   var ProbEquivalence  : double;
                   var DateWO           : double;
                   var VarDateWO        : double;
                   var MSWDwo           : double;
                   var Probwo           : double;
                   var DateW            : double;
                   var VarDateW         : double;
                   var MSWDw            : double;
                   var Probw            : double);
// weighted average calculation for concordia ages
// after Ludwig
var
  i, j, k, l   : integer;
  VarCoVar, Omega : array[0..MaxSamp,1..2,1..2] of extended;
  SumOmega12, SumOmega11, SumOmega22 : extended;
  temp, tempX, tempY : extended;
  tA, tB : array[1..2,1..2] of extended;
  D, Det, Ratiot : extended;
  MSWDConcordance : extended;
  IncludeDCUncertainty : boolean;
  AgePlusAgeMinus : string;
begin
  IncludeDCUncertainty := false;
  AgePlusAgeMinus := 'neither';

  {
  Xtra[i] = Ratio[i,1]
  Xtra3[i] = Ratio[i,2]
  Xtra1[i] = 1 sigma uncertainty in Ratio[i,1]
  Xtra2[i] = 1 sigma uncertainty in Ratio[i,2]
  }
  N:=0;
  Sum[1]:=0.0; Sum[2]:=0.0; Sum[3]:=0.0; Sum[4]:=0.0; Sum[5]:=0.0;
  SumOmega22 := 0.0;
  SumOmega11 := 0.0;
  SumOmega12 := 0.0;
  if (NS > 1) then
  begin
    for i:=1 to NS do
    begin
      if ((RFlg[i] = 'Y') and (PFlg[i] = 'Y')) then
      begin
        N:=N+1;
        if ((Xtra1[i] <> 0.0) and (Xtra2[i] <> 0.0)) then
        begin
          VarCoVar[i,1,1] := Xtra1[i]*Xtra1[i];
          VarCoVar[i,2,2] := Xtra2[i]*Xtra2[i];
          VarCoVar[i,1,2] := R[i]*Xtra1[i]*Xtra2[i];
          VarCoVar[i,2,1] := VarCoVar[i,1,2];
          {
ShowMessage(IntToStr(i)+'  CoVar = '+FormatFloat('0.000000e+00',VarCoVar[i,1,2])
   +'   R = '+FormatFloat('#0.000',R[i]));
   }
          tA[1,1] := VarCoVar[i,1,1];
          tA[1,2] := VarCoVar[i,1,2];
          tA[2,1] := VarCoVar[i,2,1];
          tA[2,2] := VarCoVar[i,2,2];
          for l:=1 to 2 do
          begin
            for j:=1 to 2 do
            begin
              tB[l,j]:=0.0;
            end;
            tB[l,l]:=1.0;
          end;
          Det:=1.0;
          for l:=1 to 2 do
          begin
            D:=tA[l,l];
            Det:=Det*D;
            for j:=1 to 2 do
            begin
              tA[l,j]:=tA[l,j]/D;
              tB[l,j]:=tB[l,j]/D;
            end;
            for j:=1 to 2 do
            begin
              if ((l-j) <> 0) then
              begin
                Ratiot:=tA[j,l];
                for k:=1 to 2 do
                begin
                  tA[j,k]:=tA[j,k]-Ratiot*tA[l,k];
                  tB[j,k]:=tB[j,k]-Ratiot*tB[l,k];
                end;
              end;
            end;
          end;
          Omega[i,1,1] := tB[1,1];
          Omega[i,1,2] := tB[1,2];
          Omega[i,2,1] := tB[2,1];
          Omega[i,2,2] := tB[2,2];
          SumOmega11 := SumOmega11 + Omega[i,1,1];
          SumOmega22 := SumOmega22 + Omega[i,2,2];
          SumOmega12 := SumOmega12 + Omega[i,1,2];
          Sum[1] := Sum[1] + Xtra[i]*Omega[i,1,1] + Xtra3[i]*Omega[i,1,2];
          Sum[2] := Sum[2] + Xtra[i]*Omega[i,1,2] + Xtra3[i]*Omega[i,2,2];
        end else
        begin
          Omega[i,1,1] := 1.0;
          Omega[i,2,2] := 1.0;
          Omega[i,1,2] := 1.0;
          Omega[i,2,1] := 1.0;
        end;
      end;
    end;
    if (N > 1) then
    begin
      XCentroid := (SumOmega22*Sum[1]-SumOmega12*Sum[2])/(SumOmega11*SumOmega22-SumOmega12*SumOmega12);
      YCentroid := (SumOmega11*Sum[2]-SumOmega12*Sum[1])/(SumOmega11*SumOmega22-SumOmega12*SumOmega12);
      Sum[3]:=0.0; Sum[4]:=0.0; Sum[5]:=0.0;
      for i:=1 to NS do
      begin
        if ((RFlg[i] = 'Y') and (PFlg[i] = 'Y')) then
        begin
          Sum[3]:=Sum[3] + (Xtra[i]-XCentroid)*(Xtra[i]-XCentroid)*Omega[i,1,1];
          Sum[4]:=Sum[4] + (Xtra3[i]-YCentroid)*(Xtra3[i]-YCentroid)*Omega[i,2,2];
          Sum[5]:=Sum[5] + 2.0*(Xtra[i]-XCentroid)*(Xtra3[i]-YCentroid)*Omega[i,1,2];
        end;
      end;
      df := 2*N-2;
      MSWDEquivalence:=(Sum[3] + Sum[4] + Sum[5]);
      tA[1,1] := SumOmega11;
      tA[1,2] := SumOmega12;
      tA[2,1] := SumOmega12;
      tA[2,2] := SumOmega22;
      for l:=1 to 2 do
      begin
        for j:=1 to 2 do
        begin
          tB[l,j]:=0.0;
        end;
        tB[l,l]:=1.0;
      end;
      Det:=1.0;
      for l:=1 to 2 do
      begin
        D:=tA[l,l];
        Det:=Det*D;
        for j:=1 to 2 do
        begin
          tA[l,j]:=tA[l,j]/D;
          tB[l,j]:=tB[l,j]/D;
        end;
        for j:=1 to 2 do
        begin
          if ((l-j) <> 0) then
          begin
            Ratiot:=tA[j,l];
            for k:=1 to 2 do
            begin
              tA[j,k]:=tA[j,k]-Ratiot*tA[l,k];
              tB[j,k]:=tB[j,k]-Ratiot*tB[l,k];
            end;
          end;
        end;
      end;
      EquivalenceXVar := tB[1,1];
      EquivalenceCoVar := tB[1,2];
      EquivalenceYVar := tB[2,2];
    end
    else begin
      XCentroid := 0.0;
      YCentroid := 0.0;
      MSWDEquivalence := 0.0;
      df := 0;
      EquivalenceXVar := 0.0;
      EquivalenceYVar := 0.0;
      EquivalenceCoVar := 0.0;
      if (N = 1) then
      begin
        XCentroid := Xtra[1];
        YCentroid := Xtra3[1];
        MSWDEquivalence := 0.0;
        df := 1;
        EquivalenceXVar := Xtra1[i]*Xtra1[i];
        EquivalenceYVar := Xtra3[i]*Xtra3[i];
        EquivalenceCoVar := R[i]*Xtra1[i]*Xtra3[i];
      end;
    end;
  end else
  begin
    if (NS = 1) then
    begin
      XCentroid := Xtra[1];
      YCentroid := Xtra3[1];
      MSWDEquivalence := 0.0;
      df := 0;
      EquivalenceXVar := Xtra1[i]*Xtra1[i];
      EquivalenceYVar := Xtra3[i]*Xtra3[i];
      EquivalenceCoVar := R[i]*Xtra1[i]*Xtra3[i];
      N := 0;
    end;
  end;
  IncludeDCUncertainty := false;
  AgePlusAgeMinus := 'neither';
  DateWO := PbPbAge((XCentroid/YCentroid)/U238U235,IncludeDCUncertainty,AgePlusAgeMinus);
  CalculateConcordiaDate(XCentroid,YCentroid,DateWO,20.0e6,20.0e6,
        SumOmega11,SumOmega22,SumOmega12,DateWO);
  VarDateWO := Q235(DateWO)*Q235(DateWO)*SumOmega11 + Q238(DateWO)*Q238(DateWO)*SumOmega22
          + 2.0*Q235(DateWO)*Q238(DateWO)*SumOmega12;
  VarDateWO := 1.0/VarDateWO;
  tempX := exp5t(DateWO) - 1.0;
  tempY := exp8t(DateWO) - 1.0;
  MSWDwo := (Xcentroid-tempX)*(XCentroid-tempX)*SumOmega11+
                     (Ycentroid-tempY)*(YCentroid-tempY)*SumOmega22+
                     2.0*(Xcentroid-tempX)*(YCentroid-tempY)*SumOmega12;
  DateW := DateWO;
  for i:= 1 to 10 do
  begin
    temp := DecayConstUncertainty[ord(at235UPb)]* DecayConst[ord(at235UPb)]/100.0;
    tA[1,1] := EquivalenceXVar + P235(DateW)*P235(DateW)*temp*temp;
    tA[1,2] := EquivalenceCoVar;
    tA[2,1] := EquivalenceCoVar;
    temp := DecayConstUncertainty[ord(at238UPb)]* DecayConst[ord(at238UPb)]/100.0;
    tA[2,2] := EquivalenceYVar + P238(DateW)*P238(DateW)*temp*temp;
    for l:=1 to 2 do
    begin
      for j:=1 to 2 do
      begin
        tB[l,j]:=0.0;
      end;
      tB[l,l]:=1.0;
    end;
    Det:=1.0;
    for l:=1 to 2 do
    begin
      D:=tA[l,l];
      Det:=Det*D;
      for j:=1 to 2 do
      begin
        tA[l,j]:=tA[l,j]/D;
        tB[l,j]:=tB[l,j]/D;
      end;
      for j:=1 to 2 do
      begin
        if ((l-j) <> 0) then
        begin
          Ratiot:=tA[j,l];
          for k:=1 to 2 do
          begin
            tA[j,k]:=tA[j,k]-Ratiot*tA[l,k];
            tB[j,k]:=tB[j,k]-Ratiot*tB[l,k];
          end;
        end;
      end;
    end;
    SumOmega11 := tB[1,1];
    SumOmega12 := tB[1,2];
    SumOmega22 := tB[2,2];
    CalculateConcordiaDate(XCentroid, YCentroid,DateW,20.0e6,20.0e6,
           SumOmega11,SumOmega22,SumOmega12,DateW);
  end;
  VarDateW := Q235(DateW)*Q235(DateW)*SumOmega11 + Q238(DateW)*Q238(DateW)*SumOmega22
          + 2.0*Q235(DateW)*Q238(DateW)*SumOmega12;
  VarDateW := 1.0/VarDateW;
  tempX := exp5t(DateW) - 1.0;
  tempY := exp8t(DateW) - 1.0;
  MSWDw := (Xcentroid-tempX)*(XCentroid-tempX)*SumOmega11+
                     (Ycentroid-tempY)*(YCentroid-tempY)*SumOmega22+
                     2.0*(Xcentroid-tempX)*(YCentroid-tempY)*SumOmega12;
  if (df > 1) then
  begin
    MSWDwo := (MSWDEquivalence+MSWDwo)/(1.0*df+1.0);
    MSWDw := (MSWDEquivalence+MSWDw)/(1.0*df+1.0);
    MSWDEquivalence:=MSWDEquivalence/(1.0*df);
  end else
  begin
    MSWDwo := 0.0;
    MSWDw := 0.0;
    MSWDEquivalence := 0.0;
  end;
end;

procedure ConcordiaAgeSingle ( iRec     : integer;
                   var DateWO           : double;
                   var VarDateWO        : double;
                   var DateW            : double;
                   var VarDateW         : double);
var
  i, j, k, l   : integer;
  //VarCoVar, Omega : array[0..MaxSamp,1..2,1..2] of extended;
  SumOmega12, SumOmega11, SumOmega22 : extended;
  temp, tempX, tempY : extended;
  tA, tB : array[1..2,1..2] of extended;
  D, Det, Ratiot : extended;
  MSWDConcordance : extended;
  MSWDEquivalence,
  EquivalenceXVar, EquivalenceYVar,
  EquivalenceCoVar : double;
  df, N : integer;
  IncludeDCUncertainty : boolean;
  AgePlusAgeMinus : string;
begin
  IncludeDCUncertainty := false;
  AgePlusAgeMinus := 'neither';
  N:=0;
  DateWO := 0.0;
  VarDateWO := 0.0;
  DateW := 0.0;
  VarDateW := 0.0;
  Sum[1]:=0.0; Sum[2]:=0.0; Sum[3]:=0.0; Sum[4]:=0.0; Sum[5]:=0.0;
  SumOmega22 := 0.0;
  SumOmega11 := 0.0;
  SumOmega12 := 0.0;
      XCentroid := Xtra[iRec];
      YCentroid := Xtra3[iRec];
      if (YCentroid <= 0.0) then Exit;
      MSWDEquivalence := 0.0;
      df := 0;
      EquivalenceXVar := Xtra1[iRec]*Xtra1[iRec];
      EquivalenceYVar := Xtra2[iRec]*Xtra2[iRec];
      EquivalenceCoVar := R[iRec]*Xtra1[iRec]*Xtra2[iRec];
      N := 0;
      SumOmega11 := EquivalenceXVar;
      SumOmega22 := EquivalenceYVar;
      SumOmega12 := EquivalenceCoVar;
  IncludeDCUncertainty := false;
  AgePlusAgeMinus := 'neither';
  DateWO := PbPbAge((XCentroid/YCentroid)/U238U235,IncludeDCUncertainty,AgePlusAgeMinus);
  if (DateWO < 1000.0) then DateWO := Age238(YCentroid,IncludeDCUncertainty,AgePlusAgeMinus);
  CalculateConcordiaDate(XCentroid,YCentroid,DateWO,20.0e6,20.0e6,
        SumOmega11,SumOmega22,SumOmega12,DateWO);
  VarDateWO := Q235(DateWO)*Q235(DateWO)*SumOmega11 + Q238(DateWO)*Q238(DateWO)*SumOmega22
          + 2.0*Q235(DateWO)*Q238(DateWO)*SumOmega12;
  if (VarDateWO > 0.0) then VarDateWO := Sqrt(1.0/VarDateWO);
  tempX := exp5t(DateWO) - 1.0;
  tempY := exp8t(DateWO) - 1.0;
  MSWDwo := (Xcentroid-tempX)*(XCentroid-tempX)*SumOmega11+
                     (Ycentroid-tempY)*(YCentroid-tempY)*SumOmega22+
                     2.0*(Xcentroid-tempX)*(YCentroid-tempY)*SumOmega12;
  DateW := DateWO;
  for i:= 1 to 10 do
  begin
    temp := DecayConstUncertainty[ord(at235UPb)]* DecayConst[ord(at235UPb)]/100.0;
    tA[1,1] := EquivalenceXVar + P235(DateW)*P235(DateW)*temp*temp;
    tA[1,2] := EquivalenceCoVar;
    tA[2,1] := EquivalenceCoVar;
    temp := DecayConstUncertainty[ord(at238UPb)]* DecayConst[ord(at238UPb)]/100.0;
    tA[2,2] := EquivalenceYVar + P238(DateW)*P238(DateW)*temp*temp;
    for l:=1 to 2 do
    begin
      for j:=1 to 2 do
      begin
        tB[l,j]:=0.0;
      end;
      tB[l,l]:=1.0;
    end;
    Det:=1.0;
    for l:=1 to 2 do
    begin
      D:=tA[l,l];
      Det:=Det*D;
      for j:=1 to 2 do
      begin
        tA[l,j]:=tA[l,j]/D;
        tB[l,j]:=tB[l,j]/D;
      end;
      for j:=1 to 2 do
      begin
        if ((l-j) <> 0) then
        begin
          Ratiot:=tA[j,l];
          for k:=1 to 2 do
          begin
            tA[j,k]:=tA[j,k]-Ratiot*tA[l,k];
            tB[j,k]:=tB[j,k]-Ratiot*tB[l,k];
          end;
        end;
      end;
    end;
    SumOmega11 := tB[1,1];
    SumOmega12 := tB[1,2];
    SumOmega22 := tB[2,2];
    CalculateConcordiaDate(XCentroid, YCentroid,DateW,20.0e6,20.0e6,
           SumOmega11,SumOmega22,SumOmega12,DateW);
  end;
  VarDateW := Q235(DateW)*Q235(DateW)*SumOmega11 + Q238(DateW)*Q238(DateW)*SumOmega22
          + 2.0*Q235(DateW)*Q238(DateW)*SumOmega12;
  if (VarDateW > 0.0) then VarDateW := Sqrt(1.0/VarDateW);
  tempX := exp5t(DateW) - 1.0;
  tempY := exp8t(DateW) - 1.0;
  MSWDw := (Xcentroid-tempX)*(XCentroid-tempX)*SumOmega11+
                     (Ycentroid-tempY)*(YCentroid-tempY)*SumOmega22+
                     2.0*(Xcentroid-tempX)*(YCentroid-tempY)*SumOmega12;
    MSWDwo := 0.0;
    MSWDw := 0.0;
    MSWDEquivalence := 0.0;
  DateWO := DateWO/1.0e6;
  DateW := DateW/1.0e6;
  VarDateWO := VarDateWO/1.0e6/1.0e6;
  VarDateW := VarDateW/1.0e6;
end;

function exp5t (t : double) : extended;
begin
  Result := exp(DecayConst[ord(at235UPb)] * t);
end;

function exp5tPlus (t : double) : extended;
begin
  Result := exp((DecayConst[ord(at235UPb)] + DecayConst[ord(at235UPb)]*DecayConstUncertainty[ord(at235UPb)]/100.0) * t);
end;

function exp5tMinus (t : double) : extended;
begin
  Result := exp((DecayConst[ord(at235UPb)] - DecayConst[ord(at235UPb)]*DecayConstUncertainty[ord(at235UPb)]/100.0) * t);
end;

function exp8t (t : double) : extended;
begin
  Result := exp(DecayConst[ord(at238UPb)] * t);
end;

function exp8tPlus (t : double) : extended;
begin
  Result := exp((DecayConst[ord(at238UPb)] + DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]/100.0) * t);
end;

function exp8tMinus (t : double) : extended;
begin
  Result := exp((DecayConst[ord(at238UPb)] - DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]/100.0) * t);
end;

function S (X, Y, t, Omega11, Omega22, Omega12 : extended) : extended;
var
  Term1, Term2, Term3 : extended;
begin
  Term1 := (X - exp5t(t) + 1.0) * (X - exp5t(t) + 1.0) * Omega11;
  Term2 := (Y - exp8t(t) + 1.0) * (Y - exp8t(t) + 1.0) * Omega22;
  Term3 := 2.0 * (X - exp5t(t) + 1.0) * (Y - exp8t(t) + 1.0) * Omega12;
  Result := Term1 + Term2 + Term3;
end;

function dSdt (X, Y, t, Omega11, Omega22, Omega12 : extended) : extended;
var
  Term1, Term2, Term3, Term4 : extended;
  DC235, DC238 : extended;
begin
  DC235 := DecayConst[ord(at235UPb)];
  DC238 := DecayConst[ord(at238UPb)];
  Term1 := (X - exp5t(t) + 1.0) * (-1.0*DC235*exp5t(t)) * Omega11;
  Term2 := (Y - exp8t(t) + 1.0) * (-1.0*DC238*exp8t(t)) * Omega22;
  Term3 := (-1.0*DC235*exp5t(t)) * (Y - exp8t(t) + 1.0) * Omega12;
  Term4 := (-1.0*DC238*exp8t(t)) * (X - exp5t(t) + 1.0) * Omega12;
  Result := Term1 + Term2 + Term3 + Term4;
end;

procedure CalculateConcordiaDate ( X, Y, t : double;
                                   lt, ut : double;
                                   Omega11, Omega22, Omega12 :extended;
                               var Date : double);
// Concordia age based on
// X = 207Pb*/235U
// Y = 206Pb*/238U
// t = initial age estimate
// lt = lower age uncertainty
// ut = upper age uncertainty
// Omega11 =
// Omega22 =
// Omega12 =
// Date = the calculated concordia age
const
  Tolerance = 0.001e6;
  MaxIteration = 500;
var
  tnew, Difference : extended;
  fl, fh, dl, dh, th, tl, swap, dt, dtold, temp, f, df : extended;
  rtsafe, frtsafe, drtsafe : extended;
  Iteration : integer;
begin
  iteration := 0;
  Date := 0.0;
  tnew := t-lt;
  th := t+ut;
  f := S(X, Y, tnew, Omega11, Omega22, Omega12);
  temp := f;
  rtsafe := tnew;
  repeat
    f := S(X, Y, tnew, Omega11, Omega22, Omega12);
    if (f < temp) then
    begin
      rtsafe := tnew;
      temp := f;
    end;
    tnew := tnew + 0.5e6;
  until (tnew > th);
  tnew := rtsafe - 0.5e6;
  th := rtsafe + 0.5e6;
  f := S(X, Y, tnew, Omega11, Omega22, Omega12);
  temp := f;
  rtsafe := tnew;
  repeat
    f := S(X, Y, tnew, Omega11, Omega22, Omega12);
    if (f < temp) then
    begin
      rtsafe := tnew;
      temp := f;
    end;
    tnew := tnew + 0.005e6;
  until (tnew > th);
  tnew := rtsafe - 0.02e6;
  th := rtsafe + 0.02e6;
  f := S(X, Y, tnew, Omega11, Omega22, Omega12);
  temp := f;
  rtsafe := tnew;
  repeat
    f := S(X, Y, tnew, Omega11, Omega22, Omega12);
    if (f < temp) then
    begin
      rtsafe := tnew;
      temp := f;
    end;
    tnew := tnew + 0.0001e6;
  until (tnew > th);
  Date := rtsafe;
  (*
  fl := S(X, Y, t-lt, Omega11, Omega22, Omega12);
  fh := S(X, Y, t+ut, Omega11, Omega22, Omega12);
  if (fl*fh = 0.0) then     { should be >= }
  begin
    ShowMessage('Root must be bracketed');
  end else
  begin
    if (fl < 0.0) then
    begin
      tl := t-lt;
      th := t+ut;
    end else
    begin
      th := t-lt;
      tl := t+ut;
      swap := fl;
      fl := fh;
      fh := swap;
    end;
    rtSafe := t;
    dtold := Abs((t+ut)-(t-lt));
    dt := dtold;
    f := S(X, Y, rtsafe, Omega11, Omega22, Omega12);
    df := dSdt(X, Y, rtsafe, Omega11, Omega22, Omega12);
    repeat
      if ((((rtsafe-th)*df-f)*((rtsafe-tl)*df-f) >= 0.0)
        or (Abs(2.0*f) > Abs(dtold-df))) then
      begin
        dtold := dt;
        dt := 0.5*(th-tl);
        rtsafe := tl + dt;
        {
        ShowMessage('tl,th,rts = '+FormatFloat('  ###0.0000',tl/1.0e6)
           +FormatFloat('  ###0.0000',th/1.0e6)+FormatFloat('  ###0.0000',rtsafe/1.0e6));
        }
        if (tl = rtsafe) then exit;
      end else
      begin
        dtold := dt;
        dt := f/df;
        temp := rtsafe;
        rtsafe := rtsafe - dt;
        if (temp = rtsafe) then exit;
      end;
      if (Abs(dt) < Tolerance) then Difference := Abs(dt);
      f := S(X, Y, rtsafe, Omega11, Omega22, Omega12);
      df := dSdt(X, Y, rtsafe, Omega11, Omega22, Omega12);
      if (f < 0.0) then
      begin
        tl := rtsafe;
        fl := f;
      end else
      begin
        th := rtsafe;
        fh := f;
      end;
      Iteration := Iteration + 1;
    until ((Difference < Tolerance) or (Iteration > MaxIteration));
    if (Iteration < MaxIteration) then Date := rtsafe
                                  else Date := 0.0;
  end;
  *)
end;

function Age238(X : double;
                IncludeDCUncertainty : boolean;
                AgePlusAgeMinus : string ) : double;
const
  T_MultDC : double= 1.96;
begin
  if (X > 0.0) then
  begin
    Result := ln(X + 1.0)/DecayConst[ord(at238UPb)];
    if IncludeDCUncertainty then
    begin
      if (AgePlusAgeMinus = UncertaintyPlus) then
      begin
        //Result := ln(X + 1.0)/(DecayConst[ord(at238UPb)]-T_MultDC*DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]/100.0);
        Result := ln(X + 1.0)/(DecayConst[ord(at238UPb)]-DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]/100.0);
      end;
      if (AgePlusAgeMinus = UncertaintyMinus) then
      begin
        //Result := ln(X + 1.0)/(DecayConst[ord(at238UPb)]+T_MultDC*DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]/100.0);
        Result := ln(X + 1.0)/(DecayConst[ord(at238UPb)]+DecayConst[ord(at238UPb)]*DecayConstUncertainty[ord(at238UPb)]/100.0);
      end;
    end else
    begin
      Result := ln(X + 1.0)/DecayConst[ord(at238UPb)];
    end;
  end else
  begin
    Result := 9999.0e6;
  end;
end;

procedure ConvertConcordia2TeraWasserburg;
var
  i : integer;
  tx, ty, tEx, tEy, tr : double;
begin
  for i := 1 to NumberOfPoints do
  begin
    if (Ratio[i,2] > 0.0) then
    begin
      tx := 1.0/Ratio[i,2];
      ty := Ratio[i,1]/(U238U235*Ratio[i,2]);
      case ErrTyp[i] of
        '1' : begin
          Ex := ErrorWt[i,1];
          Ey := ErrorWt[i,2];
        end;
        '2' : begin
          Ex := ErrorWt[i,1];
          Ey := 100.0*ErrorWt[i,2]/Ratio[i,2];
        end;
        '3' : begin
          Ex := 100.0*ErrorWt[i,1]/Ratio[i,1];
          Ey := ErrorWt[i,2];
        end;
        '4' : begin
          Ex := 100.0*ErrorWt[i,1]/Ratio[i,1];
          Ey := 100.0*ErrorWt[i,2]/Ratio[i,2];
        end;
      end;
      tEx := Ey;
      tEy := Sqrt(Ex*Ex+Ey*Ey-2.0*R[i]*Ex*Ey);
      tr := (Ey-tEx*R[i])/tEy;
    end else
    begin
      tx := 0.0;
      ty := 0.0;
      tEx := 0.0;
      tEy := 0.0;
      tr := 0.0;
    end;
    Ratio[i,1] := tx;
    Ratio[i,2] := ty;
    ErrorWt[i,1] := tEx;
    ErrorWt[i,2] := tEy;
    R[i] := tr;
    ErrTyp[i] := '1';
  end;
  iAnalTyp := 10;
  AnalType := 'A';
end;

procedure ConvertTeraWasserburg2Concordia;
var
  i : integer;
  tx, ty, tEx, tEy, tr : double;

function CalcR(TrialX : double):double;
var
  ApproxX, Difference
               :  double;
  ThisDone         :  Boolean;
  X1, X2   : double;
  iCnt : integer;
const
  Tolerance = 0.00000001;
begin
  iCnt := 0;
  if ((TrialX <> 0.0)) then
  begin
    ThisDone:=false;
    repeat
      iCnt := iCnt + 1;
      X1 := TrialX*TrialX - TrialX*(2.0*tr*Ex)-Ey*Ey+Ex*Ex;
      X2 := 2.0*TrialX-2.0*tr*Ey;
      ApproxX := TrialX-X1/X2;
      Difference:=Abs(TrialX-ApproxX);
      TrialX:=ApproxX;
      if Difference<Tolerance then ThisDone:=true;
      if (iCnt > 500) then ThisDone := true;
      {
      ShowMessage('207Pb/235U error = '+FormatFloat('##0.0000000',TrialX));
      }
    until ThisDone;
  end
  else TrialX:=0.0;
  Result:=TrialX;
end;

begin
  for i := 1 to NumberOfPoints do
  begin
    if (Ratio[i,1] > 0.0) then
    begin
      ty := 1.0/Ratio[i,1];
      tx := (Ratio[i,2]*U238U235)*ty;
      case ErrTyp[i] of
        '1' : begin
          Ex := ErrorWt[i,1];
          Ey := ErrorWt[i,2];
        end;
        '2' : begin
          Ex := ErrorWt[i,1];
          Ey := 100.0*ErrorWt[i,2]/Ratio[i,2];
        end;
        '3' : begin
          Ex := 100.0*ErrorWt[i,1]/Ratio[i,1];
          Ey := ErrorWt[i,2];
        end;
        '4' : begin
          Ex := 100.0*ErrorWt[i,1]/Ratio[i,1];
          Ey := 100.0*ErrorWt[i,2]/Ratio[i,2];
        end;
      end;
      tEy := Ex;
      tr := (Ex-Ey*R[i])/Ex;
      tEx := CalcR(10.5);
    end else
    begin
      tx := 0.0;
      ty := 0.0;
      tEx := 0.0;
      tEy := 0.0;
      tr := 0.0;
    end;
    Ratio[i,1] := tx;
    Ratio[i,2] := ty;
    ErrorWt[i,1] := tEx;
    ErrorWt[i,2] := tEy;
    R[i] := tr;
    ErrTyp[i] := '1';
  end;
  iAnalTyp := 8;
  AnalType := '8';
end;

function DM2_Age_From_Epsilon(AgePref, Epsilon : double) : double;
var
  I            : integer;
  temp, temp1  : double;
  AgeMax, ApproxAge
               :  double;
  ThisDone         :  Boolean;
  tx, ty : double;
  tRdm, tKdm, tRsmp, tKc, tRc, tKsmp, tRchur, tKchur : double;
begin
  // Age expected in Ma
  //calculation only works for situation with linear CHUR and DM curves,
  //not for quadratic versions
  if ((DM[iAnalTyp,3] > 0.0) and (CHUR[iAnalTyp,3] > 0.0)) then
  begin
    if ((DM[iAnalTyp,1] = 0.0) and (DM[iAnalTyp,2] > 0.0)
      and (CHUR[iAnalTyp,1] = 0.0) and (CHUR[iAnalTyp,2] > 0.0)) then
    begin
      //ShowMessage('Age = '+FormatFloat('####0.000000',AgePref));
      //ShowMessage('Epsilon = '+FormatFloat('####0.000000',Epsilon));
      tKchur := CHUR[iAnalTyp,2];
      //ShowMessage('tKchur = '+FormatFloat('####0.000000',tKchur));
      tRchur := CHUR[iAnalTyp,3] - tKchur*(exp(DecayConst[iAnalTyp]*AgePref*1.0e6) - 1.0);
      //ShowMessage('tRchur = '+FormatFloat('####0.000000',tRchur));
      tRsmp := (Epsilon/1.0e4 + 1.0) * tRchur;
      //ShowMessage('tRsmp = '+FormatFloat('####0.000000',tRsmp));
      tKc := CC[iAnalTyp,2];
      //ShowMessage('tKc = '+FormatFloat('####0.000000',tKc));
      tRc := tRsmp + tKc*(exp(DecayConst[iAnalTyp]*AgePref*1.0e6) - 1.0);
      //ShowMessage('tRc = '+FormatFloat('####0.000000',tRc));
      tKdm := DM[iAnalTyp,2];
      //ShowMessage('tKdm = '+FormatFloat('####0.000000',tKdm));
      //tRdm := DM[iAnalTyp,3] - tKdm*(exp(DecayConst[iAnalTyp]*AgePref*1.0e6) - 1.0);
      tRdm := DM[iAnalTyp,3];
      //ShowMessage('tRdm = '+FormatFloat('####0.000000',tRdm));
      ty := tKdm - tKc;
      //ShowMessage('ty = '+FormatFloat('####0.000000',ty));
      temp := (tRdm-tRc);
      //ShowMessage('temp = '+FormatFloat('####0.000000',temp));
      //temp := (tRc-tRdm+tKc*(exp(DecayConst[iAnalTyp]*AgePref*1.0e6))-tKc+tKdm);
      //ShowMessage('temp = '+FormatFloat('####0.000000',temp));
      //ty := tKdm;
      //ShowMessage('ty = '+FormatFloat('####0.000000',ty));
      if (ty<>0.0) then
      begin
        if (temp/ty>-1.0) then
        begin
          temp1:=Ln(1.0+temp/ty);
        end else
        begin
          temp1:=0.0;
        end;
      end else
      begin
        temp1:=0.0;
      end;
      //ShowMessage('temp1 = '+FormatFloat('####0.000000',temp1));
      //temp1 := (AgePref + 100.0)*1.0e6*DecayConst[iAnalTyp]; //temporary to provide some result for now
      AgeMax:=(temp1/DecayConst[iAnalTyp]);
    end else
    begin
      AgeMax := -999.99*1.0e6;
    end;
    DM2_Age_From_Epsilon := AgeMax/1.0E6;
  end else
  begin
    DM2_Age_From_Epsilon := 9999.99;
  end;
  //ShowMessage('T2DM = '+FormatFloat('####0.000000',AgeMax/1.0e6));
end;

function Get_IAnal_from_AnalType (AnalType : string) : integer;
var
iAnalTyp : integer;
begin
  if (AnalType = '0') then iAnalTyp := 0;
  if (AnalType = '1') then iAnalTyp := 1;
  if (AnalType = '2') then iAnalTyp := 2;
  if (AnalType = '3') then iAnalTyp := 3;
  if (AnalType = '4') then iAnalTyp := 4;
  if (AnalType = '5') then iAnalTyp := 5;
  if (AnalType = '6') then iAnalTyp := 6;
  if (AnalType = '7') then iAnalTyp := 7;
  if (AnalType = '8') then iAnalTyp := 8;
  if (AnalType = '9') then iAnalTyp := 9;
  if (AnalType = 'A') then iAnalTyp := 10;
  if (AnalType = 'B') then iAnalTyp := 11;
  if (AnalType = 'C') then iAnalTyp := 12;
  if (AnalType = 'D') then iAnalTyp := 13;
  if (AnalType = 'E') then iAnalTyp := 14;
  if (AnalType = 'F') then iAnalTyp := 15;
  if (AnalType = 'G') then iAnalTyp := 16;
  if (AnalType = 'H') then iAnalTyp := 17;
  if (AnalType = 'I') then iAnalTyp := 18;
  if (AnalType = 'J') then iAnalTyp := 19;
  if (AnalType = 'K') then iAnalTyp := 20;
  if (AnalType = 'L') then iAnalTyp := 21;
  if (AnalType = 'M') then iAnalTyp := 22;
  Result := iAnalTyp;
end;

procedure Convert2Inverse;
var
  i : integer;
  tx, ty, tEx, tEy, tr : double;
  sx, sy, rho : double;
begin
  for i := 1 to NumberOfPoints do
  begin
    if (Ratio[i,2] > 0.0) then
    begin
      ty := 1.0/Ratio[i,2];
      tx := Ratio[i,1]/Ratio[i,2];
      case ErrTyp[i] of
        '1' : begin
          Ex := ErrorWt[i,1];
          Ey := ErrorWt[i,2];
        end;
        '2' : begin
          Ex := ErrorWt[i,1];
          Ey := 100.0*ErrorWt[i,2]/Ratio[i,2];
        end;
        '3' : begin
          Ex := 100.0*ErrorWt[i,1]/Ratio[i,1];
          Ey := ErrorWt[i,2];
        end;
        '4' : begin
          Ex := 100.0*ErrorWt[i,1]/Ratio[i,1];
          Ey := 100.0*ErrorWt[i,2]/Ratio[i,2];
        end;
      end;
      {
      tEx := Ey;
      tEy := Sqrt(Ex*Ex+Ey*Ey-2.0*R[i]*Ex*Ey);
      tr := (Ey-tEx*R[i])/tEy;
      }
      // following Li and Vermeesch (2021)
      tEy := Ey;
      tEx := Sqrt(Ex*Ex - 2.0*R[i]*Ex*Ey+Ey*Ey);
      tr :=  1.0/tEx * (Ey-R[i]*Ex);
    end else
    begin
      tx := 0.0;
      ty := 0.0;
      tEx := 0.0;
      tEy := 0.0;
      tr := 0.0;
    end;
    Ratio[i,1] := tx;
    Ratio[i,2] := ty;
    ErrorWt[i,1] := tEx;
    ErrorWt[i,2] := tEy;
    R[i] := tr;
    ErrTyp[i] := '1';
  end;
  case iAnalTyp of
    7 : begin
      iAnalTyp := 20;
      AnalType := 'K';
    end;
    20 : begin
      iAnalTyp := 7;
      AnalType := '7';
    end;
    14 : begin
      iAnalTyp := 21;
      AnalType := 'L';
    end;
    21 : begin
      iAnalTyp := 14;
      AnalType := 'E';
    end;
    15 : begin
      iAnalTyp := 22;
      AnalType := 'M';
    end;
    22 : begin
      iAnalTyp := 15;
      AnalType := 'F';
    end;
  end;
end;

end.


