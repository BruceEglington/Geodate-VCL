unit GDW_reg;

interface

uses
 dialogs,
 allsorts, mathproc,
 gdw_regp,
 gdw_varb;

const
  PrintAll : boolean = true;

procedure CalcTMult (N : double);
procedure CalcWeight;
procedure Regress_Data;

implementation

{----------York regression routines----------}



procedure CalcTMult (N : double);
begin
  T_Mult:=TMultiplier(N);
end;{CalcTMult}

procedure StoreDate;
begin
  MessageDlg('This option is not yet avaiable', mtInformation,[mbOk], 0);
end;

procedure CalcWeight;
var
  J           : shortint;
begin
  FillChar(Weight,SizeOf(Weight),0);
  FillChar(Wt,SizeOf(Weight),0);
  for J:=1 to NumberOfPoints do begin
    case ErrTyp[J] of
      '1' : begin
              Weight[J,1]:=ErrorWt[J,1]*Ratio[J,1]/100.0;
              Weight[J,2]:=ErrorWt[J,2]*Ratio[J,2]/100.0;
            end;
      '2' : begin
              Weight[J,1]:=ErrorWt[J,1]*Ratio[J,1]/100.0;
              Weight[J,2]:=ErrorWt[J,2];
            end;
      '3' : begin
              Weight[J,1]:=ErrorWt[J,1];
              Weight[J,2]:=ErrorWt[J,2]*Ratio[J,2]/100.0;
            end;
      '4' : begin
              Weight[J,1]:=ErrorWt[J,1];
              Weight[J,2]:=ErrorWt[J,2];
            end;
    end;{case}
    if (Weight[J,1]<=0.0) then Weight[J,1]:=1.0e-5;
    if (Weight[J,2]<=0.0) then Weight[J,2]:=1.0e-5;
    Wt[J,1]:=Weight[J,1];
    Wt[J,2]:=Weight[J,2];
    case Model of
      1,4,5,6 : begin
        Weight[J,1]:=1.0/(Weight[J,1]*Weight[J,1]);
        Weight[J,2]:=1.0/(Weight[J,2]*Weight[J,2]);
      end;
      2 : begin
        Weight[J,1]:=1.0;
        Weight[J,2]:=1.0/(Slope*Slope);
      end;
      3 : begin
      end;
    end;
  end;{for}
end;{procedure CalcWeight}

procedure Regress_Data;
procedure Regress( var Lud_pp : double);
var
  Al, D1, D2, D3, Difference,
  temp, temp1,
  tx, ty, C, T1     :  double;
  Iteration_Str     :  string[3];
  Lud_pp_Str        :  string[5];
  Reg_done              :  Boolean;
begin
  Iteration:=0;
  if Model=4 then Iteration:=1;
  Reg_done:=false;
  Converg_done:=false;
  case Model of
    1,5,6 : begin
      for J:=1 to NumberofPoints do begin
        RR[J]:=R[J];
      end;
    end;
    2 : begin
      for J:=1 to NumberofPoints do begin
        RR[J]:=0.0;
      end;
    end;
  end;
  Draw_Block(12,28,14,52,1,NormBld);
  if (AnalType8='U') or (AnalType8='L') then begin
    Draw_Block(14,26,16,54,1,Norm);
    Str(Lud_pp:5:2,Lud_pp_Str);
    At(25,40);
    Centre(15,'Discordance factor '+Lud_pp_Str,Norm);
  end;
  repeat
    Iteration:=Iteration+1;
    TempSlope:=Slope;
    Str(Iteration:3,Iteration_Str);
    At(25,40);
    Centre(13,'Iteration number '+Iteration_Str,Norm);
    if (Model in [2,3,4]) then
    begin
      At(24,33); Write('MSWD = ',Msum:6:2);
    end;
    if (Model in [1]) then
    begin
      At(24,32); Write('Slope = ',Slope:10:6);
    end;
    At(25,40);
    FillChar(Sum,SizeOf(Sum),0);
    FillChar(U,SizeOf(U),0);
    FillChar(V,SizeOf(V),0);
    FillChar(Z,SizeOf(Z),0);
    if (Iteration>1) then begin
      if (AnalType8='U') or (AnalType8='L') then begin
        Get_NewSlope;
        temp:=ConcordiaIntercept(5.0e9,NewSlope,NewIntercept);
        temp1:=ConcordiaIntercept(-1.0e9,NewSlope,NewIntercept);
        X_Uint:=Exp(DecayConst[5]*temp)-1;
        X_Lint:=Exp(DecayConst[5]*temp1)-1;
        Chordlength:=(X_Uint-X_Lint);
        if (Chordlength=0.0) then Chordlength:=1.0e-6;
        CalcWeight;
      end;
    end;
    for J:=1 to NumberOfPoints do begin
      if (AnalType8='U') and (Iteration>1) then begin
        Fd:=(X_Uint-Ratio[J,1]);
      end;
      if (AnalType8='L') and (Iteration>1) then begin
        Fd:=(X_Lint-Ratio[J,1]);
      end;
      if ((AnalType8='U') or (AnalType8='L')) and (Iteration>1) then begin
        Fd:=Fd/Chordlength;
        if (Fd<>1.0) then Fm:=Fd/(1.0-Fd)
                     else Fm:=1.0e9;
        Ex:=Wt[J,1]*100.0/Ratio[J,1];
        Ey:=Wt[J,2]*100.0/Ratio[J,2];
        E76:=Sqrt(Ex*Ex+Ey*Ey-2.0*R[J]*Ex*Ey);
        Ex:=Sqrt(Ex*Ex+Lud_pp*Lud_pp*Fm*Fm);
        Ey:=Sqrt(Ey*Ey+Lud_pp*Lud_pp*Fm*Fm);
        V76:=E76*E76+((Lud_pp*Fm*1.0)*(Lud_pp*Fm*1.0));
        RR[J]:=(Ex*Ex+Ey*Ey-V76)/(2.0*Ex*Ey);
        Wt[J,1]:=Ex*Ratio[J,1]/100.0;
        Wt[J,2]:=Ey*Ratio[J,2]/100.0;
        Weight[J,1]:=1.0/(Wt[J,1]*Wt[J,1]);
        Weight[J,2]:=1.0/(Wt[J,2]*Wt[J,2]);
        NEquivPtsRegressed:=1.0*NumberOfPointsRegressed;
      end;
      if ((Iteration>1) and (Model=2)) then
      begin
        Weight[J,1]:=1.0;
        Weight[J,2]:=1.0/(TempSlope*TempSlope);
      end;
      Al:=Weight[J,1]*Weight[J,2];
      Z[J]:=Al/(TempSlope*TempSlope*Weight[J,2]+Weight[J,1]-2.0*TempSlope*RR[J]*Sqrt(Al));
      if (UpCase(RFlg[J])='Y') then begin
        Sum[1]:=Sum[1]+Z[J]*Ratio[J,1];
        Sum[2]:=Sum[2]+Z[J]*Ratio[J,2];
        Sum[3]:=Sum[3]+Z[J];
      end;{if}
    end;{for}
    Xcentroid:=Sum[1]/Sum[3];
    Ycentroid:=Sum[2]/Sum[3];
    for J:=1 to NumberOfPoints do begin
      if ConstrainFlag then
      begin
        U[J]:=Ratio[J,1]-XConstrain;
        V[J]:=Ratio[J,2]-YConstrain;
      end
      else begin
        U[J]:=Ratio[J,1]-Xcentroid;
        V[J]:=Ratio[J,2]-Ycentroid;
      end;
      if (UpCase(RFlg[J])='Y') then begin
        Al:=Weight[J,1]*Weight[J,2];
        D1:=U[J]/Weight[J,2]+TempSlope*V[J]/Weight[J,1];
        D2:=RR[J]*V[J]/Sqrt(Al);
        D3:=RR[J]*U[J]/Sqrt(Al);
        Sum[4]:=Sum[4]+Z[J]*Z[J]*V[J]*(D1-D2);
        Sum[5]:=Sum[5]+Z[J]*Z[J]*U[J]*(D1-TempSlope*D3);
     end;{if}
   end;{for}
   Slope:=Sum[4]/Sum[5];
   Difference:=Abs(Slope-TempSlope);
   if Iteration>MaxIteration then begin
     Reg_done:=true;
     Converg_done:=false;
   end;
   if Difference<SlopeTolerance then begin
     Reg_done:=true;
     Converg_done:=true;
   end;
   if ConstrainFlag then
   begin
     Intercept:=YConstrain-Slope*XConstrain;
   end
   else begin
     Intercept:=Ycentroid-Slope*Xcentroid;
   end;
  until Reg_done;
  FillChar(Sum,SizeOf(Sum),0);
  FillChar(Residual,SizeOf(Residual),0);
  SST:=0.0;
  SSR:=0.0;
  SSD:=0.0;
  SSLF:=0.0;
  SSPE:=0.0;
  for J:=1 to NumberOfPoints do begin
      Al:=Weight[J,1]*Weight[J,2];
      if Al=0.0 then Al:=1.0E-9;
      C:=RR[J]*Sqrt(Al);
      T1:=Z[J]*(Intercept+Slope*Ratio[J,1]-Ratio[J,2]);
      Residual[J,1]:=T1*(C-Slope*Weight[J,2])/Al;
      Residual[J,2]:=T1*(Weight[J,1]-Slope*C)/Al;
      if (UpCase(RFlg[J])='Y') then begin
        Sum[1]:=Sum[1]+Z[J]*(Ratio[J,2]-Slope*Ratio[J,1]-Intercept)*(Ratio[J,2]-Slope*Ratio[J,1]-Intercept);
        Sum[2]:=Sum[2]+Z[J]*Ratio[J,1]*Ratio[J,1];
        Sum[3]:=Sum[3]+Z[J];
        tx:=Ratio[J,1]+Residual[J,1];
        Sum[4]:=Sum[4]+Z[J]*tx;
        Sum[5]:=Sum[5]+Z[J]*tx*tx;
        SST:=SST+Z[J]*U[J]*U[J];
      end;
      if (ErrTyp[J]<>'4') then begin
        case (ErrTyp[J]) of
          '1' : begin
                  if Ratio[J,1]<>0.0 then
                    Residual[J,1]:=Residual[J,1]*100.0/Ratio[J,1];
                  if Ratio[J,2]<>0.0 then
                    Residual[J,2]:=Residual[J,2]*100.0/Ratio[J,2];
                end;
          '2' : if Ratio[J,1]<>0.0 then
                  Residual[J,1]:=Residual[J,1]*100.0/Ratio[J,1];
          '3' : if Ratio[J,2]<>0.0 then
                  Residual[J,2]:=Residual[J,2]*100.0/Ratio[J,2];
        end;{case}
      end;
  end;{for}
  if NumberOfPointsRegressed>2 then Msum:=Sum[1]/(NumberOfPointsRegressed-2)
                               else Msum:=0.0;
  if (Sum[3]/(Sum[5]*Sum[3]-Sum[4]*Sum[4]))<>0.0 then begin
    SlopeError:=Sqrt(Sum[3]/(Sum[5]*Sum[3]-Sum[4]*Sum[4]));
    InterceptError:=Sqrt(Sum[5]/(Sum[5]*Sum[3]-Sum[4]*Sum[4]));
  end
  else begin
    SlopeError:=0.0;
    InterceptError:=0.0;
  end;
end;{procedure Regress}

procedure RegressModel ( var Lud_pp : double);
var
  ModelOption   : char;
  Msum_Str      : string[4];
  CritF_Str     : string[4];
  temp, temp1,
  temp2,
  tempm, tempse,
  tempie        : double;
  RegCnt        : shortint;
  J             : shortint;
begin
  CalcTMult(1.0*N_Rep);
  Model:=1;
  CalcWeight;
  Regress(Lud_pp);
  if Msum>Msumcutoff then
  begin
   ClrScr;
   ModelOption:='?';
   Centre( 3,StringOf(60,205),NormBld);
   Centre(13,StringOf(60,205),NormBld);
   Centre(3,'    Errorchron Model Menu    ',RevNorm);
   PrintS( 5,12,'[ 1 ]  -  Augment errors by Sqrt(MSWD/Crit-F)',NormBld);
   PrintS( 6,12,'[ 2 ]  -  Recalc. with no assumptions re errors',NormBld);
   if (Analtype in ['1','2','4'..'7','9']) then
     PrintS( 7,12,'[ 3 ]  -  Recalc. assuming variable initial composition',NormBld);
   if (Analtype in ['8']) then
     PrintS( 7,12,'[ 4 ]  -  Recalc. to account for scatter rel. discordance',NormBld);
   PrintS( 8,12,'[ 5 ]  -  Separate anal. and geol. errors  ',NormBld);
   PrintS( 9,12,'[ 6 ]  -  Do not augment errors  ',NormBld);
   PrintS(11,12,'Your choice ?              ',NormBld);
   Str(Msum:4:2,Msum_Str);
   Str(MsumCutoff:4:2,CritF_Str);
   Centre(14,'The data define an errorchron with MSWD='+Msum_Str,Norm);
   Centre(15,'relative to the critical F value of '+CritF_Str,Norm);
   Beep; Beep;
   repeat
      At(11,28); ReadKbd(ModelOption);
      ModelOption:=UpCase(ModelOption);
      if ((AnalType <> '8') and (ModelOption = '4')) then ModelOption := '0';
      At(11,28); Write(ModelOption);
   until (ModelOption in ['1'..'4','5','6']);
   ClrScr;
   case ModelOption of
      '1' : Model := 1;
      '2' : Model := 2;
      '3' : Model := 3;
      '4' : Model := 4;
      '5' : Model := 5;
      '6' : Model := 6;
      else  Model := 1;
   end;
    case Model of
      1 : begin
       CalcTMult(1.0*N_Rep);
       if Msum>MsumCutOff then begin
          SlopeError:=SlopeError*Sqrt(Msum/MsumCutOff);
          InterceptError:=InterceptError*Sqrt(Msum/MsumCutOff);
        end;
      end;
      2 : begin
         if (AnalType8<>'N') then AnalType8:='N';
         CalcTMult(1.0*(NumberOfPointsRegressed-2));
         tempm:=Msum;
         tempse:=SlopeError;
         tempie:=InterceptError;
         Lud_pp:=1.0;
         CalcWeight;
         Regress(Lud_pp);
         if ((Msum < 1.0) and (Msum > 0.0)) then Msum:=1.0/Msum;
         if Msum > 1.0 then
         begin
           SlopeError:=SlopeError/Sqrt(Msum);
           InterceptError:=InterceptError/Sqrt(Msum);
         end;
         Msum:=1.0;
      end;
      3 : begin
         if (AnalType8<>'N') then AnalType8:='N';
         CalcTMult(1.0*(NumberOfPointsRegressed-2));
         RegCnt:=0;
         temp2:=InterceptError;
         CalcWeight;
         repeat
           RegCnt:=RegCnt+1;
           temp1:=temp2*Sqrt(Msum/MsumCutOff);
           temp2:=temp1;
           temp1:=temp1*temp1;
           for J:=1 to NumberofPoints do
           begin
             Weight[J,1]:=Wt[J,1];
             Weight[J,1]:=1.0/(Weight[J,1]*Weight[J,1]);
             temp:=Wt[J,2]*Wt[J,2];
             Weight[J,2]:=1.0/(temp+temp1);
             RR[J]:=R[J]*Sqrt(temp/(temp+temp1));
           end;
           Regress(Lud_pp);
         until (((Msum < (MsumCutOff+0.005)) and (Msum > (MsumCutoff-0.005))) or (RegCnt >15));
         if ((Msum < 1.0) and (Msum > 0.0)) then Msum:=1.0/Msum;
         if Msum>1.0 then
         begin
           SlopeError:=SlopeError*Sqrt(Msum/MsumCutoff);
           InterceptError:=InterceptError*Sqrt(Msum/MsumCutOff);
         end;
         Msum:=MsumCutOff;
      end;
      4 : begin
         CalcTMult(1.0*(NumberOfPointsRegressed-2));
         RegCnt:=0;
         SlopeTolerance:=SlopeTolerance*500.0;
         if (AnalType8='N') then
         begin
           ClrScr;
           OffScr;
           Centre( 4,StringOf(60,205),NormBld);
           Centre(11,StringOf(60,205),NormBld);
           Centre( 4,'  Concordia Weighting Menu  ',NormBld);
           PrintS(6,20,'[ U ] - Weight for upper intercept  ',NormBld);
           PrintS(7,20,'[ L ] - Weight for lower intercept  ',NormBld);
           PrintS(9,20,'Your choice ?           ',NormBld);
           OnScr;
           repeat
             At(9,35); ReadKbd(AnalType8);
             AnalType8:=UpCase(AnalType8);
             At(9,35); Write(AnalType8);
           until (AnalType8 in ['U','L']);
           ClrScr;
           Lud_pp:=0.2;
         end;
         repeat
           RegCnt:=RegCnt+1;
           Lud_pp:=Lud_pp*Sqrt(Msum/MsumCutOff)+0.0001;
           CalcWeight;
           Regress(Lud_pp);
         until ((Abs(Msum-MsumCutoff) < 0.01) or (RegCnt > 20));
         SlopeTolerance:=SlopeTolerance/50.0;
         repeat
           RegCnt:=RegCnt+1;
           Lud_pp:=Lud_pp*Sqrt(Msum/MsumCutOff)+0.0001;
           CalcWeight;
           Regress(Lud_pp);
         until ((Abs(Msum-MsumCutoff) < 0.01) or (RegCnt > 20));
         SlopeTolerance:=SlopeTolerance/10.0;
         repeat
           RegCnt:=RegCnt+1;
           Lud_pp:=Lud_pp*Sqrt(Msum/MsumCutOff)+0.0001;
           CalcWeight;
           Regress(Lud_pp);
         until ((Abs(Msum-MsumCutoff) < 0.01) or (RegCnt > 30));
      end;
      5 : begin
         CalcTMult(1.0*N_Rep);
         {
         SlopeError:=SlopeError;
         InterceptError:=InterceptError;
         }
      end;
      6 : begin
         CalcTMult(1.0*N_Rep);
         SlopeError:=SlopeError;
         InterceptError:=InterceptError;
      end;
    end;
  end;
    if ((AnalType8 in ['U','L']) and (Model in [4])) then
    begin
         NEquivPtsRegressed:=1.0;
         temp:=-9.0e27;
         for J:=1 to NumberofPoints do
         begin
           if (RFlg[J] = 'Y') then
           begin
             if (Z[J] > temp) then temp:=Z[J];
           end;
         end;
         temp1:=0.0;
         for J:=1 to NumberofPoints do
         begin
           if (RFlg[J] = 'Y') then
           begin
             if ((Z[J] > temp1) and (Z[J] < temp)) then temp1:=Z[J];
           end;
         end;
         for J:=1 to NumberofPoints do
         begin
           if (RFlg[J] = 'Y') then
           begin
             if (Z[J] < temp) then
                NEquivPtsRegressed:=NEquivPtsRegressed + Z[J]/temp1;
           end;
         end;
         CalcTMult(1.0*NEquivPtsRegressed-2.0);
    end;
end;{RegressModel}


procedure CalcAgeError;
var
  t1, t2, t3       : double;
begin
    case AnalType of
      '0' : begin
            end;
      '1','2' :
            begin
              if Slope>-1.0 then Age:=Ln(1.0+Slope)/DecayConst[IAnalTyp]
                            else Age:=0.0;
              if Model<>5 then
              begin
                AgeError:=Ln(1.0+T_Mult*SlopeError)/DecayConst[IAnalTyp];
                InitRatioError:=InterceptError*T_Mult;
                t1:=Intercept;
                t2:=CHUR[IAnalTyp,2]-CHUR[IAnalTyp,1]*(Exp(DecayConst[IAnalTyp]*Age)-1.0);
                Epsilon1:=10000.0*(t1/t2-1.0);
                EpError1:=T_Mult*10000.0*InterceptError/Intercept;
              end;
              if Model=5 then
              begin
                t1:=SlopeError*Sqrt(Msum/MsumCutOff-1.0);
                t2:=t1*TMultiplier(NumberOfPointsRegressed-2);
                t2:=t2*t2;
                t1:=SlopeError*TMultiplier(N_Rep);
                t2:=Sqrt(t2+t1*t1);
                AgeError:=Ln(1.0+t2)/DecayConst[IAnalTyp];
                t1:=InterceptError*Sqrt(Msum/MsumCutOff-1.0);
                t2:=t1*TMultiplier(NumberOfPointsRegressed-2);
                t2:=t2*t2;
                t1:=InterceptError*TMultiplier(N_Rep);
                t2:=Sqrt(t2+t1*t1);
                InitRatioError:=t2;
                t1:=Intercept;
                t2:=CHUR[IAnalTyp,2]-CHUR[IAnalTyp,1]*(Exp(DecayConst[IAnalTyp]*Age)-1.0);
                Epsilon1:=10000.0*(t1/t2-1.0);
                EpError1:=10000.0*InitRatioError/Intercept;
              end;
              Age:=Age*0.000001;
              AgeError:=AgeError*0.000001;
            end;
      '3' : begin
              if Slope>0.0 then Age:=PbPbAge(Slope)
                           else Age:=0.0;
              UpperAgeError:=(PbPbAge(Slope+T_Mult*SlopeError)-Age);
              LowerAgeError:=(Age-PbPbAge(Slope-T_Mult*SlopeError));
              if ((Age>MuV[1,1]) and (mu_choice=1)) then begin
                mu_choice:=0;
              end;
              MuErrors;
              Age:=Age*0.000001;
              UpperAgeError:=UpperAgeError*0.000001;
              LowerAgeError:=LowerAgeError*0.000001;
            end;
      '4'..'7','9' :
            begin
              if Slope>-1.0 then Age:=Ln(1.0+Slope)/DecayConst[IAnalTyp]
                            else Age:=0.0;
              AgeError:=Ln(1.0+T_Mult*SlopeError)/DecayConst[IAnalTyp];
              Age:=Age*0.000001;
              AgeError:=AgeError*0.000001;
              InitRatioError:=InterceptError*T_Mult;
            end;
      '8' : begin
               NewConcordiaErrors;
            end;
    end;{case}
    {
    SlopeError:=SlopeError;
    InterceptError:=InterceptError;
    }
end;{CalcAgeError}



procedure Print_Results;
var
  PrintChar     : char;
  Msum_Str      : string[4];
  Lud_pp_Str    : string[5];

procedure PrintData ( RFlgChar : char);
var
  tx,ty          : real;
  tn             : byte;
begin
  tn:=0;
  for J:=1 to NumberOfPoints do begin
   if ((RFlg[J]=RFlgChar) and (PFlg[J]='Y')) then begin
    tn:=tn+1;
    case ErrTyp[J] of
      '1' : begin
              tx:=Wt[J,1]*100.0/Ratio[J,1];
              ty:=Wt[J,2]*100.0/Ratio[J,2];
              Write(ListFile,'       ',SmpNo[J]:10);
              Write(ListFile,Ratio[J,1]:15:6,tx:10:4,' %');
              Write(ListFile,Ratio[J,2]:15:6,ty:10:4,' %',RR[J]:10:3);
              Write(ListFile,Residual[J,1]:13:4,' %',Residual[J,2]:13:4,' %');
              Writeln(ListFile,RFlg[J]:9);
            end;
      '2' : begin
              tx:=Wt[J,1]*100.0/Ratio[J,1];
              ty:=Wt[J,2];
              Write(ListFile,'       ',SmpNo[J]:10);
              Write(ListFile,Ratio[J,1]:15:6,tx:10:4,' %');
              Write(ListFile,Ratio[J,2]:15:6,ty:12:6,RR[J]:10:3);
              Write(ListFile,Residual[J,1]:13:4,' %',Residual[J,2]:15:6);
              Writeln(ListFile,RFlg[J]:9);
            end;
      '3' : begin
              tx:=Wt[J,1];
              ty:=Wt[J,2]*100.0/Ratio[J,2];
              Write(ListFile,'       ',SmpNo[J]:10);
              Write(ListFile,Ratio[J,1]:15:6,tx:12:6);
              Write(ListFile,Ratio[J,2]:15:6,ty:10:4,' %',RR[J]:10:3);
              Write(ListFile,Residual[J,1]:15:6,Residual[J,2]:13:4,' %');
              Writeln(ListFile,RFlg[J]:9);
            end;
      '4' : begin
              tx:=Wt[J,1];
              ty:=Wt[J,2];
              Write(ListFile,'       ',SmpNo[J]:10);
              Write(ListFile,Ratio[J,1]:15:6,tx:12:6);
              Write(ListFile,Ratio[J,2]:15:6,ty:12:6,RR[J]:10:3);
              Write(ListFile,Residual[J,1]:15:6,Residual[J,2]:15:6);
              Writeln(ListFile,RFlg[J]:9);
            end;
    end;
   end;
  end;
  if (tn>0) then Writeln(ListFile);
end;{PrintData}


procedure Lst_Results;
var
  Msum_Str       : string[4];
  N_Rep_Str      : string[4];
  N_Pts_Reg_Str  : string[4];
  Lud_pp_Str     : string[5];
  Scrn_Title     : string[60];
  tx,ty          : real;
begin
  Scrn_Title:=Title+StringOf(60,32);
  CheckPrinter;
  Writeln(ListFile,CondPrn);
  Write(ListFile,'        ',Scrn_Title:60,' '+ProgramName+' v. '+ProgVersion);
  Writeln(ListFile,StringOf(3,32),'Printed on ',DateString);
  Writeln(ListFile);
  if PrintAll then
  begin
    Write(ListFile,'        Sample no.    ');
    Write(ListFile,XRatioStr[IAnalTyp],'   X  Wt        ');
    Write(ListFile,YRatioStr[IAnalTyp],'   Y  Wt        ');
    Write(ListFile,'  R      ');
    Write(ListFile,'  X Error  ','      Y Error ');
    Writeln(ListFile,'      Include');
    Writeln(ListFile);
    PrintData('Y');
    PrintData('N');
  end;
  Write(ListFile,StringOf(15,32),'Sample uncertainties are 1 sigma and based on ',N_Rep:3,' replicates');
  if (1.0*NumberOfPointsRegressed = NEquivPtsRegressed) then
    Writeln(ListFile,'     Students t = ',T_Mult:4:2)
  else Writeln(ListFile);
  Str(MsumCutoff:4:2,Msum_Str);
  Str(N_Rep:3,N_Rep_Str);
  Str((NumberOfPointsRegressed-2):3,N_Pts_Reg_Str);
  case Model of
    1 : begin
      if Msum>MsumCutOff then Augmented:='      Errors augmented by Sqrt(MSWD/'+Msum_Str+')';
      if Msum<=MsumCutOff then
      begin
        Augmented:='  ';
      end;
    end;
    2 : Augmented:='      No assumptions - equal weights, r=0 ';
    3 : Augmented:='      Assuming variable initial ratio     ';
    4 : Augmented:='      Assuming multi-episodic scatter     ';
    5 : Augmented:='      Assuming separate anal. and geol errors';
    6 : Augmented:='      Errors not augmented ';
  end;
  Writeln(ListFile);
  Writeln(ListFile);
  if Converg_done=true then
    Write(ListFile,'       Regression converged after ',Iteration:3,' iterations')
  else
    Write(ListFile,'       Regression had not converged after ',Iteration:3,' iterations');
  Writeln(ListFile,'        F (0.025; '+N_Rep_Str+'; '+N_Pts_Reg_Str+') = '+Msum_Str);
  Writeln(ListFile);
  Write(ListFile,'       Centroid      ',XRatioStr[IAnalTyp],' = ',Xcentroid:12:6);
  Writeln(ListFile,'          ',YRatioStr[IAnalTyp],' = ',Ycentroid:12:6);
  Writeln(ListFile,'       Slope     = ',Slope:12:7,' +/- ',SlopeError:11:7,'    1 sigma');
  Writeln(ListFile,'       Intercept = ',Intercept:12:6,' +/- ',InterceptError:12:6,'   1 sigma');
  Writeln(ListFile);
  if (Model in [1,2,3,5,6]) then
    if (AnalType8 in ['N','U','L']) then
      Write(ListFile,'       MSWD = ',Msum:8:3,' on ',NumberOfPointsRegressed:3,' points ');
  if (AnalType8 in ['U','L']) then
  begin
    if (Model in [4]) then
      Write(ListFile,'       MSWD = ',Msum:6:2,' on ',NEquivPtsRegressed:5:2,' equiv. points ');
  end;
  Writeln(ListFile,Augmented);
  if (Model in [2,3,4]) then
    Writeln(ListFile,'                           '+
                     ' MSWD is forced to the above value by this model.    Students t = ',T_Mult:7:2);
  Writeln(ListFile);
  if (AnalType in ['1','2','4'..'7','9']) then
  begin
   Write(ListFile,'       Ro  = ',Intercept:12:6,' +/- ',InitRatioError:12:6);
   if (AnalType in ['1','2']) then
   begin
    Writeln(ListFile,'       Epsilon = ',Epsilon1:10:3,' +/- ',EpError1:10:3,'   95% conf.')
   end
   else Writeln(ListFile,'   95% conf.');
  end;
  case AnalType of
    '0' : begin
            Writeln(ListFile);
          end;
    '1','2','4'..'7','9' :
          begin
            Writeln(ListFile,'       Age       = ',Age:8:2,'     +/- ',AgeError:8:2,'     95% conf.');
            Writeln(ListFile);
            Writeln(ListFile,'       Decay constant = ',DecayConst[IAnaltyp]:14);
            Writeln(ListFile);
          end;
    '3' : begin
            Write(ListFile,'       Age       = ',Age:8:2,'   + ',UpperAgeError:8:2);
            Write(ListFile,'  - ',LowerAgeError:8:2,'    95% conf.');
            Writeln(ListFile,'       ');
            Writeln(ListFile);
            Write(ListFile,'       Mu = ',Mu:6:3,'  +',UprMuError:6:3,'  -',LwrMuError:6:3);
            Write(ListFile,'     95% conf. ');
            if mu_choice=0 then Writeln(ListFile,'  - Single stage');
            if mu_choice=1 then Writeln(ListFile,'  - S & K two stage');
            if mu_choice=2 then Writeln(ListFile,'  - User defined   ');
            Writeln(ListFile);
            Write(ListFile,'       Decay constants     238U = ',DecayConst[4]:14);
            Writeln(ListFile,'       235U = ',DecayConst[5]:14);
            Writeln(ListFile);
          end;
    '8' : begin
            Str(Lud_pp:5:2,Lud_pp_Str);
            Write(ListFile,'       Upper Age = ',UprIntercept:8:2,'   + ',UprUprAgeError:8:2);
            Write(ListFile,'  - ',UprLwrAgeError:8:2,'    95% conf.');
            if (AnalType8='U') then begin
              Write(ListFile,'       Weighted for upper intercept');
              Writeln(ListFile,'    Discordance factor = '+Lud_pp_Str);
            end
            else Writeln(ListFile);
            if ((UprIntercept <> UprUprAgeError2) and (not ConstrainFlag)) then
            begin
              Write(ListFile,'                   ',UprUprAgeError2:8:2);
              Writeln(ListFile,'             adjusted for zero intercept');
            end;
            Writeln(ListFile);
            Write(ListFile,'       Lower Age = ',LwrIntercept:8:2,'   + ',LwrUprAgeError:8:2);
            Write(ListFile,'  - ',LwrLwrAgeError:8:2,'    95% conf.');
            if (AnalType8='L') then begin
              Write(ListFile,'       Weighted for lower intercept');
              Writeln(ListFile,'    Discordance factor = '+Lud_pp_Str);
            end
            else Writeln(ListFile);
            Writeln(ListFile);
            Write(ListFile,'       Decay constants     238U = ',DecayConst[4]:14);
            Writeln(ListFile,'       235U = ',DecayConst[5]:14);
            Writeln(ListFile);
{
            if (UprIntercept <> UprUprAgeError2) then
            begin
              ConstrainFlag:=true;
              AgeConstrain:=0.0;
              XConstrain:=0.0;
              YConstrain:=0.0;
              RegressModel(Lud_pp);
              Get_NewSlope;
              CalcAgeError;
              Slope2:=Slope;
              Intercept2:=Intercept;
              Str(Lud_pp:5:2,Lud_pp_Str);
              Writeln(ListFile,'                 Line constrained through origin');
              Write(ListFile,'       Upper Age = ',UprIntercept:8:2,'   + ',UprUprAgeError:8:2);
              Write(ListFile,'  - ',UprLwrAgeError:8:2,'    95% conf.');
              if (AnalType8='U') then begin
                Write(ListFile,'       Weighted for upper intercept');
                Writeln(ListFile,'    Discordance factor = '+Lud_pp_Str);
              end
              else Writeln(ListFile);
              ConstrainFlag:=false;
              RegressModel(Lud_pp);
              Get_NewSlope;
              CalcAgeError;
              Slope1:=Slope;
              Intercept1:=Intercept;
            end;
}
          end;
  end;{case}
  if ConstrainFlag then
  begin
    Writeln(ListFile);
    Write(ListFile,'       Line constrained to pass through point  X = ',XConstrain:11:4);
    Write(ListFile,'       Y = ',YConstrain:11:4);
    if (AnalType='8') then
    begin
      Write(ListFile,'    near Age = ',AgeConstrain:7:2,' Ma');
    end;
    Writeln(ListFile);
  end;
  Writeln(ListFile,NormPrn,FF);
end;{Lst_Results}

procedure Scrn_Results;
var
  Msum_Str       : string[4];
  N_Rep_Str      : string[4];
  N_Pts_Reg_Str  : string[4];
  Lud_pp_Str     : string[5];
begin
  Str(MsumCutoff:4:2,Msum_Str);
  Str(N_Rep:3,N_Rep_Str);
  Str((NumberOfPointsRegressed-2):3,N_Pts_Reg_Str);
  case Model of
    1 : begin
      if Msum>MsumCutOff then Augmented:='  Errors augmented by Sqrt(MSWD/'+Msum_Str+')';
      if Msum<=MsumCutOff then
      begin
        Augmented:='  ';
      end;
    end;
    2 : Augmented:='  No assumptions - equal weights, r=0 ';
    3 : Augmented:='  Assuming variable initial ratio     ';
    4 : Augmented:='  Assuming multi-episodic scatter     ';
    5 : Augmented:='  Assuming separate anal. and geol errors';
    6 : Augmented:='  Errors not augmented ';
  end;
  Scrn_Title:=Title;
  repeat
    Title_Len:=Length(Scrn_Title);
    if Scrn_Title[Title_Len]=' ' then Delete(Scrn_Title,Title_Len,1)
  until (Scrn_Title[Title_Len]<>' ');
  Centre(1,Scrn_Title,UndlnBld);
  Writeln;
  Writeln;
  if Converg_done=true then
    Write('Regression converged after ',Iteration:3,' iterations')
  else
    Write('Regression had not converged after ',Iteration:3,' iterations');
  Writeln('        F (0.025; '+N_Rep_Str+'; '+N_Pts_Reg_Str+') = '+Msum_Str);
  Writeln;
  Write('Centroid      ',XRatioStr[IAnalTyp],' = ',Xcentroid:12:6);
  Writeln('     ',YRatioStr[IAnalTyp],' = ',Ycentroid:12:6);
  Writeln('Slope     = ',Slope:12:7,' +/- ',SlopeError:11:7,'     1 sigma');
  Writeln('Intercept = ',Intercept:12:6,' +/- ',InterceptError:12:6,'    1 sigma');
  Writeln;
  Writeln;
  if (Model in [1,2,3,5,6]) then
    if (AnalType8 in ['N','U','L']) then
    begin
      Write('MSWD = ',Msum:8:3,' on ',NumberOfPointsRegressed:3,' points ');
    end;
  if (AnalType8 in ['U','L']) then
  begin
    if (Model in [4]) then
      Write('MSWD = ',Msum:6:2,' on ',NEquivPtsRegressed:5:2,' equiv. points ');
  end;
  Writeln(Augmented);
  if (Model in [2,3,4]) then
    Writeln('     MSWD is forced to the above value by this model.  Students t = ',T_Mult:7:2);
  Writeln;
  Writeln;
  if (AnalType in ['1','2','4'..'7','9']) then
  begin
    Writeln('Ro        = ',Intercept:12:6,' +/- ',InitRatioError:12:6,'   95% conf.');
    if (AnalType in ['1','2']) then
    begin
     Writeln('Epsilon   = ',Epsilon1:10:3,'   +/- ',EpError1:10:3,'     95% conf.');
    end;
  end;
  case AnalType of
    '0' : begin
            Writeln;
          end;
    '1','2','4'..'7','9' :
          begin
            Writeln('Age       = ',Age:8:2,'     +/- ',AgeError:8:2,'       95% conf.');
            Writeln;
            Writeln;
          end;
    '3' : begin
            Write('Age       = ',Age:8:2,'   + ',UpperAgeError:8:2);
            Writeln('  - ',LowerAgeError:8:2,'      95% conf.');
            Writeln;
            Writeln;
            Write('Mu = ',Mu:6:3,'  +',UprMuError:6:3,'  -',LwrMuError:6:3);
            Write('      95% conf. ');
            if mu_choice=0 then Writeln('  - Single stage');
            if mu_choice=1 then Writeln('  - S & K two stage');
            if mu_choice=2 then Writeln('  - User defined   ');
            Writeln;
            Writeln;
          end;
    '8' : begin
            Str(Lud_pp:5:2,Lud_pp_Str);
            if (AnalType8='U') then begin
              Write('Weighted for upper intercept');
              Writeln('      Discordance factor = '+Lud_pp_Str);
            end;
            if (AnalType8='L') then begin
              Write('Weighted for lower intercept');
              Writeln('      Discordance factor = '+Lud_pp_Str);
            end;
            Write('Upper Age = ',UprIntercept:8:2,'   + ',UprUprAgeError:8:2);
            Writeln('  - ',UprLwrAgeError:8:2,'     95% conf.');
            if ((UprIntercept <> UprUprAgeError2) and (not ConstrainFlag)) then
            begin
              Write('            ',UprUprAgeError2:8:2);
              Writeln('               adjusted for zero intercept');
            end;
            Write('Lower Age = ',LwrIntercept:8:2,'   + ',LwrUprAgeError:8:2);
            Writeln('  - ',LwrLwrAgeError:8:2,'     95% conf.');
            Writeln;
          end;
  end;{case}
  if ConstrainFlag then
  begin
    Write('Line constrained through point  X = ',XConstrain:11:4);
    Writeln('    Y = ',YConstrain:11:4);
    if (AnalType='8') then
    begin
      Writeln('                 near Age = ',AgeConstrain:7:2,' Ma');
    end;
  end;
end;{Scrn_Results}

begin
 TCI:=0;
 repeat
  ClrScr;
  Scrn_Results;
  Centre(22,StringOf(78,205),NormBld);
  PrintS(24,2,'Print results   Graphics view   Store results   Any other key to continue',Norm);
  ChangeAtt(24, 2, 1,NormBld);
  ChangeAtt(24,18, 1,NormBld);
  ChangeAtt(24,34, 1,NormBld);
  ChangeAtt(24,50, 1,NormBld);
   At(25,40);
  ReadKbd(PrintChar);
  At(24,1); ClrEOL;
  PrintChar:=UpCase(PrintChar);
  case PrintChar of
    'P' : begin
       Centre(24,'  Printing results  ',NormBldFlik);
       At(25,40);
       CheckPrinter;
       if not Prndone then
       begin
         Centre(24,'  Printing results  ',NormBldFlik);
         At(25,40);
         Lst_Results;
       end;
    end;
    'G' : begin
       Get_Axis_Values;
       if (TCI<>esckey) then begin
         Regr_Graph;
       end;
    end;
    'S' : begin
       SendToRegFile;
    end;
  end;
 until (not (PrintChar in ['P','G','S']));
end;{Print_Results}

procedure CheckConstrain;
const
  LMax : byte = 3;
  LMin : byte = 1;
var
  ConstrainChar : char;
  L             : shortint;
begin
   ClrScr;
   Centre( 4,StringOf(60,205),NormBld);
   Centre(11,StringOf(60,205),NormBld);
   Centre( 4,' Constrain Line Menu ',NormBld);
   if (ProcessOption='C') then ConstrainFlag:=true
                          else ConstrainFlag:=false;
   if ConstrainFlag then
   begin
     PrintS( 7,20,XratioStr[IAnalTyp],NormBld);
     PrintS( 8,20,YRatioStr[IAnalTyp],NormBld);
     PrintS(25, 1,'F10 - Complete',RevNorm);
     ChangeAtt(25,1,79,RevNorm);
     if (AnalType='8') then
     begin
       PrintS( 6,20,'Age (Ma) ',NormBld);
       At( 6,47); Write(AgeConstrain:8:3);
       XConstrain:=exp(DecayConst[5]*AgeConstrain*1.0e6)-1.0;
       YConstrain:=exp(DecayConst[4]*AgeConstrain*1.0e6)-1.0;
     end;
     At( 7,47); Write(XConstrain:11:6);
     At( 8,47); Write(YConstrain:11:6);
   end;
   if (ConstrainFlag) then
   begin
      CPos:=0;
      TCI:=0;
      if (AnalType='8') then
      begin
        L:=1;
        LMin:=1;
      end
      else begin
        L:=2;
        LMin:=2;
      end;
      repeat
        case L of
          1 : begin
           if ((TCI<>f10key) and (AnalType='8')) then
           begin
             TCI:=0;
             CPos:=1;
             ReadReal( 6,47,'@@@@.@@@',NormBld,RevNorm,
                      0.000,4550.0,CPos,TCI,AgeConstrain);
           end;
           if (AnalType='8') then
           begin
             XConstrain:=exp(DecayConst[5]*AgeConstrain*1.0e6)-1.0;
             YConstrain:=exp(DecayConst[4]*AgeConstrain*1.0e6)-1.0;
             At( 7,47); Write(XConstrain:11:6);
             At( 8,47); Write(YConstrain:11:6);
           end;
          end;
          2 : begin
           if (TCI<>f10key) then
           begin
             TCI:=0;
             CPos:=0;
             ReadReal( 7,47,'@@@@.@@@@@@',NormBld,RevNorm,
                      0.00,1000.0,CPos,TCI,XConstrain);
           end;
          end;
          3 : begin
           if (TCI<>f10key) then
           begin
             TCI:=0;
             CPos:=0;
             ReadReal( 8,47,'@@@@.@@@@@@',NormBld,RevNorm,
                      0.00,1000.0,CPos,TCI,YConstrain);
           end;
          end;
        end;
        case TCI of
         tabkey, enterkey : begin
          L:=L+1;
          if L>LMax then L:=LMin;
         end;
         shiftabkey : begin
          L:=L-1;
          if L<LMin then L:=LMax;
         end;
         arrowdnkey : begin
          L:=L+1;
          if L>LMax then L:=LMin;
         end;
         arrowupkey : begin
          L:=L-1;
          if L<LMin then L:=LMax;
         end;
        end;
      until (TCI=f10key);
   end;
end;


begin
   for J:=1 to NumberOfPoints do
   begin
     case AnalType of
       '0' : begin
          if (Ratio[J,1]<-9990.00001) then RFlg[J]:='N';
          if (Ratio[J,2]<-9990.00001) then RFlg[J]:='N';
          if (Ratio[J,1]<-9990.000001) then PFlg[J]:='N';
          if (Ratio[J,2]<-9990.000001) then PFlg[J]:='N';
       end;
       '1' : begin
          if (Ratio[J,1]<0.00001) then RFlg[J]:='N';
          if (Ratio[J,2]<0.690) then RFlg[J]:='N';
          if (Ratio[J,1]<0.00001) then PFlg[J]:='N';
          if (Ratio[J,2]<0.6001) then PFlg[J]:='N';
       end;
       '2' : begin
          if (Ratio[J,1]<0.00001) then RFlg[J]:='N';
          if (Ratio[J,2]<0.490) then RFlg[J]:='N';
          if (Ratio[J,1]<0.000001) then PFlg[J]:='N';
          if (Ratio[J,2]<0.4001) then PFlg[J]:='N';
       end;
       '3' : begin
          if (Ratio[J,1]<8.001) then RFlg[J]:='N';
          if (Ratio[J,2]<8.490) then RFlg[J]:='N';
          if (Ratio[J,1]<5.0001) then PFlg[J]:='N';
          if (Ratio[J,2]<5.4001) then PFlg[J]:='N';
       end;
       '4' : begin
          if (Ratio[J,1]<0.00001) then RFlg[J]:='N';
          if (Ratio[J,2]<8.490) then RFlg[J]:='N';
          if (Ratio[J,1]<0.000001) then PFlg[J]:='N';
          if (Ratio[J,2]<5.4001) then PFlg[J]:='N';
       end;
       '5' : begin
          if (Ratio[J,1]<0.00001) then RFlg[J]:='N';
          if (Ratio[J,2]<8.490) then RFlg[J]:='N';
          if (Ratio[J,1]<0.000001) then PFlg[J]:='N';
          if (Ratio[J,2]<5.4001) then PFlg[J]:='N';
       end;
       '6' : begin
          if (Ratio[J,1]<0.00001) then RFlg[J]:='N';
          if (Ratio[J,2]<8.490) then RFlg[J]:='N';
          if (Ratio[J,1]<0.000001) then PFlg[J]:='N';
          if (Ratio[J,2]<5.4001) then PFlg[J]:='N';
       end;
       '7' : begin
          if (Ratio[J,1]<0.00001) then RFlg[J]:='N';
          if (Ratio[J,2]<0.150) then RFlg[J]:='N';
          if (Ratio[J,1]<0.000001) then PFlg[J]:='N';
          if (Ratio[J,2]<0.1001) then PFlg[J]:='N';
       end;
       '8' : begin
          if (Ratio[J,1]<0.0001) then RFlg[J]:='N';
          if (Ratio[J,2]<0.001) then RFlg[J]:='N';
          if (Ratio[J,1]<0.00001) then PFlg[J]:='N';
          if (Ratio[J,2]<0.00001) then PFlg[J]:='N';
       end;
       '9' : begin
          if (Ratio[J,1]<0.00001) then RFlg[J]:='N';
          if (Ratio[J,2]<0.0001) then RFlg[J]:='N';
          if (Ratio[J,1]<0.000001) then PFlg[J]:='N';
          if (Ratio[J,2]<0.00001) then PFlg[J]:='N';
       end;
     end;
   end;
   CheckConstrain;
   if (AnalType='8') then begin
     ClrScr;
     OffScr;
     Centre( 3,StringOf(60,205),NormBld);
     Centre(11,StringOf(60,205),NormBld);
     Centre( 3,'  Concordia Weighting Menu  ',NormBld);
     PrintS(5,20,'[ N ] - Normal concordia regression ',NormBld);
     PrintS(6,20,'[ U ] - Weight for upper intercept  ',NormBld);
     PrintS(7,20,'[ L ] - Weight for lower intercept  ',NormBld);
     PrintS(9,20,'Your choice ?           ',NormBld);
     OnScr;
     repeat
       At(9,35); ReadKbd(AnalType8);
       AnalType8:=UpCase(AnalType8);
       At(9,35); Write(AnalType8);
     until (AnalType8 in ['N','U','L']);
     ClrScr;
   end;
   NumberOfPointsRegressed:=0;
   for J:=1 to NumberOfPoints do begin
      if UpCase(RFlg[J])='Y' then NumberOfPointsRegressed:=NumberOfPointsRegressed+1;
   end;
   NEquivPtsRegressed:=NumberOfPointsRegressed;
   if NumberOfPointsRegressed>1 then begin
     StartAtX:=Ratio[1,1];
     StartAtY:=Ratio[1,2];
     EndAtX:=StartAtX;
     EndAtY:=StartAtY;
     for J:=1 to NumberOfPointsRegressed do
     begin
       if (Ratio[J,1] < StartAtX) then
       begin
         StartAtX:=Ratio[J,1];
         StartAtY:=Ratio[J,2];
       end;
       if (Ratio[J,1] > EndAtX) then
       begin
         EndAtX:=Ratio[J,1];
         EndAtY:=Ratio[J,2];
       end;
     end;
     if (EndAtX-StartAtX)<>0 then
       Slope:=(EndAtY-StartAtY)/(EndAtX-StartAtX)
     else Slope:=0.015;
     if (NumberOfPointsRegressed=2) then MsumCutOff:=1
       else MsumCutoff:=MSUM_Val[NumberOfPointsRegressed-2];
     if (AnalType8='N') then Lud_pp:=1.0
                        else Lud_pp:=0.2;
     CalcTMult(1.0*N_Rep);
     Model:=1;
     RegressModel(Lud_pp);
     Get_NewSlope;
     CalcAgeError;
     Slope1:=Slope;
     Intercept1:=Intercept;

     if (AnalType='3') or (AnalType='8') then
     begin
       Slope1:=NewSlope;
       Intercept1:=NewIntercept;
       MuAge21:=Age;
       if (AnalType = '3') then Mu1:=Mu
                           else Mu1:=0.0;
     end;

     Print_Results;
   end
   else begin
     ClrScr;
     Draw_Block(12,15,14,65,1,NormBld);
     Centre(13,' Insufficient data for regression ',RevNorm);
     Beep; Beep;
     WaitAnyKey;
   end;
end;{proc Regress_Data}




end.


