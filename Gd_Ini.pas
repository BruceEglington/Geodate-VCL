unit Gd_Ini;

interface
procedure GetIniFile;
procedure SetIniFile;

implementation

procedure GetIniFile;
var
  AppIni   : TIniFile;
  i        : integer;
  tmpStr   : string;
begin
  tmpStr := '1';
  AppIni := TIniFile.Create('GDW.INI');
  try
    RegisteredUser := AppIni.ReadString('Registration','Registered user','not defined');
    AllowMessages := AppIni.ReadBool('Development','Allow Messages',false);
    GdwNetFileDir := AppIni.ReadString('Default directories','NetFileDir','C:\');
    Drive1 := AppIni.ReadString('Default directories','Drive1','C:\GDW');
    Drive2 := AppIni.ReadString('Default directories','Drive2','C:\GDW\DATA');
    Drive3 := AppIni.ReadString('Default directories','Drive3','C:\GDW\TEMP');
    LastCountry := AppIni.ReadString('Last country','LastCountry','SA');
    tmpStr := AppIni.ReadString('Last analysis type','AnalType','1');
    AnalType := tmpStr[1];
    for i := 0 to 17 do
    begin
      Process[i] := AppIni.ReadString(Process[i],'Method',Process[i]);
      XRatioStr[i] := AppIni.ReadString(Process[i],'X Ratio',XRatioStr[i]);
      YRatioStr[i] := AppIni.ReadString(Process[i],'Y Ratio',YRatioStr[i]);
      ZRatioStr[i] := AppIni.ReadString(Process[i],'Z Ratio',ZRatioStr[i]);
      Element[i,1] := AppIni.ReadString(Process[i],'X element',Element[i,1]);
      Element[i,2] := AppIni.ReadString(Process[i],'Y element',Element[i,2]);
      DecayConst[i] := StrToFloat(AppIni.ReadString(Process[i],
        'Decay Constant',FormatFloat('0.00000E+00',DecayConst[i])));
      DecayConstUncertainty[i] := StrToFloat(AppIni.ReadString(Process[i],
        'Decay Constant % Uncertainty',FormatFloat('00.00000',DecayConstUncertainty[i])));

      if (i in [1,2,7,9,14,15,16]) then
      begin
        CHURModelName[i] := AppIni.ReadString(Process[i],'CHUR model name',CHURModelName[i]);
        CHUR[i,0] := StrToFloat(AppIni.ReadString(Process[i],
          'CHUR 1',FormatFloat('0.0000000E+00',CHUR[i,0])));
        CHUR[i,1] := StrToFloat(AppIni.ReadString(Process[i],
          'CHUR 2',FormatFloat('0.0000000E+00',CHUR[i,1])));
        CHUR[i,2] := StrToFloat(AppIni.ReadString(Process[i],
          'CHUR 3',FormatFloat('0.0000000E+00',CHUR[i,2])));
        DMModelName[i] := AppIni.ReadString(Process[i],'DM model name',DMModelName[i]);
        DM[i,1] := StrToFloat(AppIni.ReadString(Process[i],
          'DM 1',FormatFloat('0.00000000E+00',DM[i,1])));
        DM[i,2] := StrToFloat(AppIni.ReadString(Process[i],
          'DM 2',FormatFloat('0.00000000E+00',DM[i,2])));
        DM[i,3] := StrToFloat(AppIni.ReadString(Process[i],
          'DM 3',FormatFloat('0.00000000E+00',DM[i,3])));
      end;
      CalcFac[i,1] := StrToFloat(AppIni.ReadString(Process[i],
        'Calc Fac 1',FormatFloat('0.000000E+00',CalcFac[i,1])));
      CalcFac[i,2] := StrToFloat(AppIni.ReadString(Process[i],
        'Calc Fac 2',FormatFloat('0.000000E+00',CalcFac[i,2])));
      if (i = 3) then
      begin
        MuV[0,1] := StrToFloat(AppIni.ReadString(Process[i],
          'Single stage model Start Date',FormatFloat('0.000E+00',MuV[0,1])));
        MuV[0,2] := StrToFloat(AppIni.ReadString(Process[i],
          'Single stage model Start 206Pb/204Pb',FormatFloat('00.000',MuV[0,2])));
        MuV[0,3] := StrToFloat(AppIni.ReadString(Process[i],
          'Single stage model Start 207Pb/204Pb',FormatFloat('00.000',MuV[0,3])));
        MuV[0,4] := StrToFloat(AppIni.ReadString(Process[i],
          'Single stage model Start 208Pb/204Pb',FormatFloat('00.000',MuV[0,4])));
        MuV[0,5] := StrToFloat(AppIni.ReadString(Process[i],
          'Single stage model Start 238U/204Pb',FormatFloat('00.000',MuV[0,5])));
        MuV[1,1] := StrToFloat(AppIni.ReadString(Process[i],
          'Stacey and Kramers 2nd stage Start Date',FormatFloat('0.000E+00',MuV[1,1])));
        MuV[1,2] := StrToFloat(AppIni.ReadString(Process[i],
          'Stacey and Kramers 2nd stage Start 206Pb/204Pb',FormatFloat('00.000',MuV[1,2])));
        MuV[1,3] := StrToFloat(AppIni.ReadString(Process[i],
          'Stacey and Kramers 2nd stage Start 207Pb/204Pb',FormatFloat('00.000',MuV[1,3])));
        MuV[1,4] := StrToFloat(AppIni.ReadString(Process[i],
          'Stacey and Kramers 2nd stage Start 208Pb/204Pb',FormatFloat('00.000',MuV[1,4])));
        MuV[1,5] := StrToFloat(AppIni.ReadString(Process[i],
          'Stacey and Kramers 2nd stage Start 238U/204Pb',FormatFloat('00.000',MuV[1,5])));
        MuV[2,1] := StrToFloat(AppIni.ReadString(Process[i],
          'User defined model Start Date',FormatFloat('0.000E+00',MuV[2,1])));
        MuV[2,2] := StrToFloat(AppIni.ReadString(Process[i],
          'User defined model Start 206Pb/204Pb',FormatFloat('00.000',MuV[2,2])));
        MuV[2,3] := StrToFloat(AppIni.ReadString(Process[i],
          'User defined model Start 207Pb/204Pb',FormatFloat('00.000',MuV[2,3])));
        MuV[2,4] := StrToFloat(AppIni.ReadString(Process[i],
          'User defined model Start 208Pb/204Pb',FormatFloat('00.000',MuV[2,4])));
        MuV[2,5] := StrToFloat(AppIni.ReadString(Process[i],
          'User defined model Start 238U/204Pb',FormatFloat('00.000',MuV[2,5])));
        mu_choice := Round(StrToFloat(AppIni.ReadString(Process[i],
          'Default model','1')));
        if (mu_choice = 0) then
        begin
          Singlestage.Checked := true;
          SK2Stage.Checked := false;
          UserDefined.Checked := false;
        end;
        if (mu_choice = 1) then
        begin
          Singlestage.Checked := false;
          SK2Stage.Checked := true;
          UserDefined.Checked := false;
        end;
        if (mu_choice = 2) then
        begin
          Singlestage.Checked := false;
          SK2Stage.Checked := false;
          UserDefined.Checked := true;
        end;
      end;
      if (i in [8]) then
      begin
        U238U235 := StrToFloat(AppIni.ReadString(Process[i],
        '238U/235U',FormatFloat('000.000',U238U235)));
        tmpStr := AppIni.ReadString(Process[i],'Default weighting method','N');
        ClearNull(tmpStr);
        if (tmpStr = 'N') then
        begin
          OptionsNormal.Checked := true;
          OptionsDiscordance.Checked := false;
        end;
        if (tmpStr = 'U') then
        begin
          OptionsNormal.Checked := false;
          OptionsDiscordance.Checked := true;
        end;
      end;
      if (i in [17]) then
      begin
        BlanketZErrVal := StrToFloat(AppIni.ReadString(Process[i],
          'Default minimum 207Pb/206Pb','0.001'));
      end;
    end;
  finally
    AppIni.Free;
  end;
end;

procedure SetIniFile;
var
  AppIni   : TIniFile;
  i        : integer;
begin
  AppIni := TIniFile.Create('GDW.INI');
  try
    AppIni.WriteString('Registration','Registered user',RegisteredUser);
    AppIni.WriteBool('Development','Allow Messages',AllowMessages);
    AppIni.WriteString('Default directories','NetFileDir',GdwNetFileDir);
    AppIni.WriteString('Default directories','Drive1',Drive1);
    AppIni.WriteString('Default directories','Drive2',Drive2);
    AppIni.WriteString('Default directories','Drive3',Drive3);
    AppIni.WriteString('Last country','LastCountry',LastCountry);
    AppIni.WriteString('Last analysis type','AnalType',AnalType);
    for i := 0 to 17 do
    begin
      AppIni.WriteString(Process[i],'Method',Process[i]);
      AppIni.WriteString(Process[i],'X Ratio',XRatioStr[i]);
      AppIni.WriteString(Process[i],'Y Ratio',YRatioStr[i]);
      AppIni.WriteString(Process[i],'Z Ratio',ZRatioStr[i]);
      AppIni.WriteString(Process[i],'X element',Element[i,1]);
      AppIni.WriteString(Process[i],'Y element',Element[i,2]);
      if (DecayConst[i] > 0.0) then
      begin
        AppIni.WriteString(Process[i],'Decay Constant',FormatFloat('0.00000E+00',DecayConst[i]));
        AppIni.WriteString(Process[i],'Decay Constant % Uncertainty',FormatFloat('00.00000',DecayConstUncertainty[i]));
      end;
      if (i in [1,2,7,9,14,15,16]) then
      begin
        AppIni.WriteString(Process[i],'CHUR model name',CHURModelName[i]);
        AppIni.WriteString(Process[i],'CHUR 1',FormatFloat('0.0000000E+00',CHUR[i,0]));
        AppIni.WriteString(Process[i],'CHUR 2',FormatFloat('0.0000000E+00',CHUR[i,1]));
        AppIni.WriteString(Process[i],'CHUR 3',FormatFloat('0.0000000E+00',CHUR[i,2]));
        AppIni.WriteString(Process[i],'DM model name',DMModelName[i]);
        AppIni.WriteString(Process[i],'DM 1',FormatFloat('0.00000000E+00',DM[i,1]));
        AppIni.WriteString(Process[i],'DM 2',FormatFloat('0.00000000E+00',DM[i,2]));
        AppIni.WriteString(Process[i],'DM 3',FormatFloat('0.00000000E+00',DM[i,3]));
      end;
      if ((CalcFac[i,1] <> 0.0) and (CalcFac[i,2] <> 0.0)) then
      begin
        AppIni.WriteString(Process[i],'Calc Fac 1',FormatFloat('0.000000E+00',CalcFac[i,1]));
        AppIni.WriteString(Process[i],'Calc Fac 2',FormatFloat('0.000000E+00',CalcFac[i,2]));
      end;
      if (i = 3) then
      begin
        AppIni.WriteString(Process[i],'Single stage model Start Date',FormatFloat('0.000E+00',MuV[0,1]));
        AppIni.WriteString(Process[i],'Single stage model Start 206Pb/204Pb',FormatFloat('00.000',MuV[0,2]));
        AppIni.WriteString(Process[i],'Single stage model Start 207Pb/204Pb',FormatFloat('00.000',MuV[0,3]));
        AppIni.WriteString(Process[i],'Single stage model Start 208Pb/204Pb',FormatFloat('00.000',MuV[0,4]));
        AppIni.WriteString(Process[i],'Single stage model Start 238U/204Pb',FormatFloat('00.000',MuV[0,5]));
        AppIni.WriteString(Process[i],'Stacey and Kramers 2nd stage Start Date',FormatFloat('0.000E+00',MuV[1,1]));
        AppIni.WriteString(Process[i],'Stacey and Kramers 2nd stage Start 206Pb/204Pb',FormatFloat('00.000',MuV[1,2]));
        AppIni.WriteString(Process[i],'Stacey and Kramers 2nd stage Start 207Pb/204Pb',FormatFloat('00.000',MuV[1,3]));
        AppIni.WriteString(Process[i],'Stacey and Kramers 2nd stage Start 208Pb/204Pb',FormatFloat('00.000',MuV[1,4]));
        AppIni.WriteString(Process[i],'Stacey and Kramers 2nd stage Start 238U/204Pb',FormatFloat('00.000',MuV[1,5]));
        AppIni.WriteString(Process[i],'User defined model Start Date',FormatFloat('0.000E+00',MuV[2,1]));
        AppIni.WriteString(Process[i],'User defined model Start 206Pb/204Pb',FormatFloat('00.000',MuV[2,2]));
        AppIni.WriteString(Process[i],'User defined model Start 207Pb/204Pb',FormatFloat('00.000',MuV[2,3]));
        AppIni.WriteString(Process[i],'User defined model Start 208Pb/204Pb',FormatFloat('00.000',MuV[2,4]));
        AppIni.WriteString(Process[i],'User defined model Start 238U/204Pb',FormatFloat('00.000',MuV[2,5]));
        AppIni.WriteString(Process[i],'Default model',FormatFloat('0',1.0*mu_choice));
      end;
      if (i in [8]) then
      begin
        if (OptionsNormal.Checked) then AnalType8 := 'N';
        if (OptionsDiscordance.Checked) then AnalType8 := 'U';
        AppIni.WriteString(Process[i],'Default weighting method',AnalType8);
        AppIni.WriteString(Process[i],'238U/235U',FormatFloat('000.000',U238U235));
      end;
      if (i in [17]) then
      begin
        AppIni.WriteString(Process[i],'Default minimum 207Pb/206Pb',FormatFloat('0.00000',BlanketZErrVal));
      end;
    end;
  finally
    AppIni.Free;
  end;
end;

end.
