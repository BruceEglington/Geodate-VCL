unit GDW_varb;

{$J+}

interface

const
  ProgramName     = 'GDW';
  ProgVersion     = '5.1';
  DVProgVersion   = '5.1';
  GeodateVersionStr = 'Geodate (2025)';
  Steps           =  2500;
  MaxSamp         =  5000;
  MaxType         = 22;
  MaxIteration    =   199;
  NumStatisticsValues = 999;
  AllowMessages   : boolean = true;
  Lud_pp          : double = 0.2;
  MsumCutOff      : double = 2.0;
  SlopeTolerance  : double =    0.000001;
  AgeTolerance    : double =    1000.0;    //used to be 20000.0
  MuTolerance     : double =    0.0002;
  GraphPauseValue  =  600;
  //YorkExt          : string[2] = 'YK';
  YorkExt          : string = 'YK';
  GeodateExt          : string = 'GE';
  DefaultU238U235 : double = 137.818;
  UPbTracerUncertainty_pc : double = 0.015;
  iCurveLineMinus = 15;
  iCurveLinePlus = 14;
  iCurrent = 13;
  iDataConcordia = 12;
  iEllipseConcordia = 11;
  iErrorIncluded = 10;
  iErrorExcluded = 9;
  iDataIncluded = 8;
  iDataExcluded = 7;
  iRegressionLine = 6;
  iCurveLine = 5;
  iCurveTic = 4;
  iEnvelopeUpper = 3;
  iEnvelopeLower = 2;
  iEllipsesIncluded = 1;
  iEllipsesExcluded = 0;
  UncertaintyPlus = 'plus';
  UncertaintyMinus = 'minus';
  UncertaintyNeither = 'neither';

type
  str1            = string[1];
  str5            = string[5];
  str10           = string[10];
  str15           = string[15];
  str25           = string[25];
  str80           = string[80];
  CharSet         = set of char;
  MaskArray       = string[12];
  TErrorNameType           = array [1..4] of string;
  TElementNameType     = array [0..MaxType,1..3] of string;
  TRatioNameType      = array [0..MaxType] of string;
  TProcessNameType     = array [0..MaxType] of string;
  RDataRecord      = packed record
         case shortint of
          1 : (Sano       :  string[10];
              XElemConc, YElemConc, XRatio, XPrecis, XError,
              YRatio, YPrecis, YError, Correl
                         :  Real48;
              AnTypANSI      :  ANSIchar;
              ErTypANSI      :  ANSIchar;
              RFlagANSI      :  ANSIchar;
              PFlagANSI      :  ANSIchar;
              //AnTypANSI      :  string[1];
              //ErTypANSI      :  string[1];
              //RFlagANSI      :  string[1];
              //PFlagANSI      :  string[1];
              ZRatio, ZPrecis, ZError
                         :  Real48);
          2 : (Tit       :  string[80];
               NRep      :  smallint;
              sparefield :  array[1..45] of byte);
         end;
  Yorkfiletype    =  file of RDataRecord;
  RGeodateRecord      = record
         case shortint of
          1 : (Sano       :  string[20];
              XElemConc, YElemConc,
              XRatio, XPrecis, XError,
              YRatio, YPrecis, YError,
              Correl
                         :  double;
              AnTypANSI      :  ANSIchar;
              ErTypANSI      :  ANSIchar;
              RFlagANSI      :  ANSIchar;
              PFlagANSI      :  ANSIchar;
              //AnTyp      :  char;
              //ErTyp      :  char;
              //RFlag      :  char;
              //PFlag      :  char;
              //AnTypANSI      :  string[1];
              //ErTypANSI      :  string[1];
              //RFlagANSI      :  string[1];
              //PFlagANSI      :  string[1];
              ZRatio, ZPrecis, ZError
                         :  double;
              ZElemConc,
              Age, sAge,
              WRatio, WPrecis, WError,
              rho86 : double;
              ErTypZ, ErTypW : char          );
          2 : (Tit       :  string[80];
               NRep      :  smallint;
              sparefield :  array[1..45] of byte);
         end;
  Geodatefiletype    =  file of RGeodateRecord;
  //Prompt          = string[64];
  //AnTypStr        = string[11];
  //ErTypStr        = string[11];
  //Samp            = string[10];
  Prompt          = string;
  AnTypStr        = string;
  ErTypStr        = string;
  ZErTypStr        = string;
  WErTypStr        = string;
  Samp            = string;
  TDecayConstantSourceType = array[0..MaxType] of string;
  TDecayConstantValueType           = array [0..MaxType] of double;
  TDecayUncertaintyValueType = array [0..MaxType] of double;
  TModelNameType = array[0..MaxType] of string;
  //TDMModelNameType = array[0..MaxType] of string;
  //TCCModelNameType = array[0..MaxType] of string;
  TTracerUncertaintyValueType       = array [0..MaxType] of double;
  TModelValueType           = array [0..MaxType,1..3] of double;
  //TCHURvalType         = array [0..MaxType,0..2] of double;
  //TCCvalType           = array [0..MaxType,1..3] of double;
  TMuValueType           = array [0..2,1..5] of double;
  TCalcFacValueType     = array [0..MaxType,1..2] of double;
  TGraphRGBColourType = array [0..9,1..3] of integer;
  FreqArrayType = array[0..21] of byte;
  TCumArrayType  = array[0..Steps+1] of double;
  TTechniqueType    = (atGeneral,atRbSr,atSmNd,atPbPb,
                   at238UPb,at235UPb,atThPb,
                   atLuHf,atConcordia,atLaCe,atTeraWasserburg,atKAr,
                   atArAr,atArInverse,atKCa,atReOs,atLaBa,atEvapPb,
                   atArPlateau,atEps2DM,atLuHfInv,atKCaInv,atReOsInv);


const
  Model           :       shortint = 1;
  SmpNo_Len       =       10;
  MaxAnalType     =       22;      //was 11
  MaxErrType      =        6;
  MaxElem         =        4;
  Bell            :       char = #7;
  N_Rep           :       integer = 999;  // used to be 60
  ArArJ           :       double = 1.0;
  ArArJ1sig       :       double = 0.1;
  ErrTypeStr        :  TErrorNameType
                  = ('% X     % Y  ',
                     '% X     act Y',
                     'act X   % Y  ',
                     'act X   act Y');
  DefaultElement           :  TElementNameType
                  = (('X ','Y ','?'),    { 0}
                     ('Rb','Sr','?'),
                     ('Sm','Nd','?'),    { 2}
                     ('U ','Pb','?'),
                     ('U ','Pb','?'),    { 4}
                     ('U ','Pb','?'),
                     ('Th','Pb','?'),    { 6}
                     ('Lu','Hf','?'),
                     ('U ','Pb','?'),    { 8}
                     ('La','Ce','?'),    { 9}
                     ('U ','Pb','?'),    {10 A}
                     ('K ','Ar','?'),
                     ('Ar','Ar','?'),    {12 C}
                     ('Ar','Ar','?'),
                     ('K ','Ca','?'),    {14 E}
                     ('Re','Os','?'),    {15 F}
                     ('La','Ba','?'),    {16 G}
                     ('T','Pb','?'),    {17 H}
                     ('Ar ','Ar','?'),  {18 I}
                     ('X ','Y ','?'),   {19 J}
                     ('Lu','Hf','?'),    {20 J}
                     ('K ','Ca','?'),    {21 E}
                     ('Re','Os','?'));    {22 F}
  DefaultXRatioStr         :  TRatioNameType
                  = ('X          ',
                     '87Rb/86Sr',
                     '147Sm/144Nd',
                     '206Pb/204Pb',
                     '238U/204Pb ',
                     '235U/204Pb ',
                     '232Th/204Pb',
                     '176Lu/177Hf',
                     '207Pb/235U ',
                     '138La/142Ce',
                     '238U/206Pb ',
                     '40K/36Ar   ',
                     '39Ar/36Ar  ',
                     '39Ar/40Ar  ',
                     '40K/42Ca  ',
                     '187Re/188Os',
                     '138La/137Ba',
                     '204Pb/206Pb',
                     '%39Ar      ',
                     'Age (Ma)   ',
                     '176Lu/176Hf',
                     '40K/40Ca  ',
                     '187Re/187Os');
  DefaultYRatioStr         :  TRatioNameType
                  = ('Y          ',
                     '87Sr/86Sr  ',
                     '143Nd/144Nd',
                     '207Pb/204Pb',
                     '206Pb/204Pb',
                     '207Pb/204Pb',
                     '208Pb/204Pb',
                     '176Hf/177Hf',
                     '206Pb/238U ',
                     '138Ce/142Ce',
                     '207Pb/206Pb',
                     '40Ar/36Ar  ',
                     '40Ar/36Ar  ',
                     '36Ar/40Ar  ',
                     '40Ca/42Ca  ',
                     '187Os/188Os',
                     '138Ba/137Ba',
                     '208Pb/206Pb',
                     '40Ar*/39Ar ',
                     'Epsilon    ',
                     '177Hf/176Hf',
                     '42Ca/40Ca  ',
                     '188Os/187Os');
  DefaultZRatioStr         :  TRatioNameType
                  = ('Extra      ',
                     'Date       ',
                     'Date       ',
                     '207Pb/206Pb',
                     'Extra      ',
                     'Extra      ',
                     'Extra      ',
                     'Date       ',
                     '207Pb/206Pb',
                     'Date       ',
                     'Extra      ',
                     'Extra      ',
                     'J          ',
                     'J          ',
                     'Date       ',
                     'Date       ',
                     'Date       ',
                     '207Pb/206Pb',
                     'J          ',
                     'Extra      ',
                     'Extra      ',
                     'Extra      ',
                     'Extra      ');
  DefaultWRatioStr         :  TRatioNameType
                  = ('Extra      ',
                     'Date       ',
                     'Date       ',
                     '207Pb/206Pb',
                     'Extra      ',
                     'Extra      ',
                     'Extra      ',
                     'Date       ',
                     '207Pb/206Pb',
                     'Date       ',
                     'Extra      ',
                     'Extra      ',
                     'J          ',
                     'J          ',
                     'Date       ',
                     'Date       ',
                     'Date       ',
                     '207Pb/206Pb',
                     'J          ',
                     'Extra      ',
                     'Extra      ',
                     'Extra      ',
                     'Extra      ');
  DefaultGraphXRatioStr         :  TRatioNameType
                  = ('X          ',
                     '<sup>87</sup>Rb/<sup>86</sup>Sr',
                     '<sup>147</sup>Sm/<sup>144</sup>Nd',
                     '<sup>206</sup>Pb/<sup>204</sup>Pb',
                     '<sup>238</sup>U/<sup>204</sup>Pb ',
                     '<sup>235</sup>U/<sup>204</sup>Pb ',
                     '<sup>232</sup>Th/<sup>204</sup>Pb',
                     '<sup>176</sup>Lu/<sup>177</sup>Hf',
                     '<sup>207</sup>Pb/<sup>235</sup>U ',
                     '<sup>138</sup>La/<sup>142</sup>Ce',
                     '<sup>238</sup>U/<sup>206</sup>Pb ',
                     '<sup>40</sup>K/<sup>36</sup>Ar   ',
                     '<sup>39</sup>Ar/<sup>36</sup>Ar  ',
                     '<sup>39</sup>Ar/<sup>40</sup>Ar  ',
                     '<sup>40</sup>Ca/<sup>42</sup>Ca  ',
                     '<sup>187</sup>Re/<sup>188</sup>Os',
                     '<sup>138</sup>La/<sup>137</sup>Ba',
                     '<sup>204</sup>Pb/<sup>206</sup>Pb',
                     '%<sup>39</sup>Ar      ',
                     'Age (Ma)   ',
                     '<sup>176</sup>Lu/<sup>176</sup>Hf',
                     '<sup>40</sup>K/<sup>40</sup>Ca  ',
                     '<sup>187</sup>Re/<sup>187</sup>Os');
  DefaultGraphYRatioStr         :  TRatioNameType
                  = ('Y          ',
                     '<sup>87</sup>Sr/<sup>86</sup>Sr  ',
                     '<sup>143</sup>Nd/<sup>144</sup>Nd',
                     '<sup>207</sup>Pb/<sup>204</sup>Pb',
                     '<sup>206</sup>Pb/<sup>204</sup>Pb',
                     '<sup>207</sup>Pb/<sup>204</sup>Pb',
                     '<sup>208</sup>Pb/<sup>204</sup>Pb',
                     '<sup>176</sup>Hf/<sup>177</sup>Hf',
                     '<sup>206</sup>Pb/<sup>238</sup>U ',
                     '<sup>138</sup>Ce/<sup>142</sup>Ce',
                     '<sup>207</sup>Pb/<sup>206</sup>Pb',
                     '<sup>40</sup>Ar/<sup>36</sup>Ar  ',
                     '<sup>40</sup>Ar/<sup>36</sup>Ar  ',
                     '<sup>36</sup>Ar/<sup>40</sup>Ar  ',
                     '<sup>40</sup>Ca/<sup>42</sup>Ca  ',
                     '<sup>187</sup>Os/<sup>188</sup>Os',
                     '<sup>138</sup>Ba/<sup>137</sup>Ba',
                     '<sup>208</sup>Pb/<sup>206</sup>Pb',
                     '<sup>40</sup>Ar<sup>*/<sup>39</sup>Ar ',
                     'Epsilon    ',
                     '<sup>177</sup>Hf/<sup>176</sup>Hf',
                     '<sup>42</sup>Ca/<sup>40</sup>Ca  ',
                     '<sup>188</sup>Os/<sup>187</sup>Os');
  DefaultGraphZRatioStr         :  TRatioNameType
                  = ('Extra      ',
                     'Date       ',
                     'Date       ',
                     '<sup>207</sup>Pb/<sup>206</sup>Pb',
                     'Extra      ',
                     'Extra      ',
                     'Extra      ',
                     'Date       ',
                     '<sup>207</sup>Pb/<sup>206</sup>Pb',
                     'Date       ',
                     'Extra      ',
                     'Extra      ',
                     'J          ',
                     'J          ',
                     'Date       ',
                     'Date       ',
                     'Date       ',
                     '<sup>207</sup>Pb/<sup>206</sup>Pb',
                     'J          ',
                     'Extra      ',
                     'Extra      ',
                     'Extra      ',
                     'Extra      ');
  DefaultProcess           :  TProcessNameType
                  = ('X-Y general',
                     'Rb-Sr      ',
                     'Sm-Nd      ',
                     'Pb-Pb      ',
                     '238U-Pb    ',
                     '235U-Pb    ',
                     'Th-Pb      ',
                     'Lu-Hf      ',
                     'Wetherill  ',
                     'La-Ce      ',
                     'TeraWass   ',
                     'K-Ar       ',
                     'Ar-Ar      ',
                     'Ar Inverse ',
                     'K-Ca       ',
                     'Re-Os      ',
                     'La-Ba      ',
                     'Evap Pb    ',
                     'Ar Plateau ',
                     'Age-Ep-T2DM',
                     'Lu-Hf Inverse',
                     'K-Ca Inverse',
                     'Re-Os Inverse');
  DefaultProcessAbr        :  TProcessNameType
                  = ('X-Y general',
                     'Rb-Sr      ',
                     'Sm-Nd      ',
                     'Pb-Pb      ',
                     '238U-Pb    ',
                     '235U-Pb    ',
                     'Th-Pb      ',
                     'Lu-Hf      ',
                     'Wetherill  ',
                     'La-Ce      ',
                     'TeraWass   ',
                     'K-Ar       ',
                     'Ar-Ar      ',
                     'Ar Inverse ',
                     'K-Ca       ',
                     'Re-Os      ',
                     'La-Ba      ',
                     'Evap Pb    ',
                     'Ar Plateau ',
                     'Age-Ep-T2DM',
                     'Lu-Hf Inverse',
                     'K-Ca Inverse',
                     'Re-Os Inverse');
  DefaultDecayConstantSource       :  TDecayConstantSourceType
                  = ('none         ',
                     'Steiger and Jager (1977)',
                     'Lugmair and Marti (1978)',
                     'Steiger and Jager (1977)',
                     'Steiger and Jager (1977)',
                     'Steiger and Jager (1977)',
                     'Steiger and Jager (1977)',
                     'Soderlund et al (2004)',
                     'Steiger and Jager (1977)',
                     'Steiger and Jager (1977)',
                     'Steiger and Jager (1977)',
                     'Steiger and Jager (1977)',
                     'Steiger and Jager (1977)',
                     'Steiger and Jager (1977)',
                     'Steiger and Jager (1977)',
                     'Steiger and Jager (1977)',
                     'Steiger and Jager (1977)',
                     'Steiger and Jager (1977)',
                     'Steiger and Jager (1977)',
                     'Steiger and Jager (1977)',
                     'Steiger and Jager (1977)',
                     'Steiger and Jager (1977)',
                     'Steiger and Jager (1977)');
  DefaultDecayConst        :  TDecayConstantValueType
                  = (1,
                     1.42E-11,
                     6.54E-12,
                     1,
                     1.55125E-10,
                     9.8485E-10,
                     4.9475E-11,
                     1.867E-11,
                     1,
                     2.33E-12,
                     1,
                     0.581E-10,
                     0.581E-10,
                     0.581E-10,
                     4.962E-10,
                     1.66602E-11,
                     4.44E-12,
                     1,
                     1,
                     1,
                     1,
                     1,
                     1);
  DefaultDecayConstUncertainty        :  TDecayUncertaintyValueType
                  = (0.000001,
                     0.000001,
                     0.000001,
                     0.000001,
                     0.08,
                     0.10,
                     0.000001,
                     0.000001,
                     0.000001,
                     0.000001,
                     0.000001,
                     0.000001,
                     0.000001,
                     0.000001,
                     0.000001,
                     0.31,
                     0.000001,
                     0.000001,
                     0.000001,
                     0.000001,
                     0.000001,
                     0.000001,
                     0.000001);
  DefaultTracerUncertainty        :  TTracerUncertaintyValueType
                  = (0,
                     0.0,
                     0.0,
                     0.0,
                     0.0075,
                     0.0075,
                     0.0,
                     0.0,
                     0.0,
                     0.0,
                     0.0,
                     0.0,
                     0.0,
                     0.0,
                     0.0,
                     0.0,
                     0.0,
                     0.0,
                     0.0,
                     0.0,
                     0.0,
                     0.0,
                     0.0);
  DefaultCHURModelName     :  TModelNameType
                  = ('UR         ',
                     'UR         ',
                     'CHUR       ',
                     'UR         ',
                     'UR         ',
                     'UR         ',
                     'UR         ',
                     'CHUR       ',
                     'UR         ',
                     'CHUR       ',
                     'UR         ',
                     'UR         ',
                     'UR         ',
                     'UR         ',
                     'CHUR       ',
                     'CHUR       ',
                     'CHUR       ',
                     'UR         ',
                     'nd         ',
                     'nd         ',
                     'nd         ',
                     'nd         ',
                     'nd         ');
  DefaultCHUR              :  TModelValueType
                  = ((0, 0      ,0        ),
                     (0, 0.0847 ,0.7047   ),
                     (0, 0.1967 ,0.51264  ),
                     (0, 0      ,0        ),
                     (0, 0      ,0        ),
                     (0, 0      ,0        ),
                     (0, 0      ,0        ),
                     (0, 0.0336 ,0.282785  ),
                     (0, 0      ,0        ),
                     (0, 0.00306,0.0228527),
                     (0, 0      ,0        ),
                     (0, 0      ,0        ),
                     (0, 0      ,0        ),
                     (0, 0      ,0        ),
                     (0, 0      ,0        ),
                     (0, 0      ,0        ),
                     (0, 0      ,0        ),
                     (0, 0      ,0        ),
                     (0, 0      ,0        ),
                     (0, 0      ,0        ),
                     (0, 0      ,0        ),
                     (0, 0      ,0        ),
                     (0, 0      ,0        ));
  DefaultDMModelName       :  TModelNameType
                  = ('none         ',
                     'Ben Othman et al. (1984)',
                     'De Paolo (1981)',
                     'DM         ',
                     'DM         ',
                     'DM         ',
                     'DM         ',
                     'Chauvel and Blichert-Toft (2001)',
                     'DM         ',
                     'DM         ',
                     'DM         ',
                     'DM         ',
                     'DM         ',
                     'DM         ',
                     'DM         ',
                     'DM         ',
                     'DM         ',
                     'DM         ',
                     'DM         ',
                     'DM         ',
                     'DM         ',
                     'DM         ',
                     'DM         ');
  DefaultDM                :  TModelValueType
                  = ((0      ,0        ,0),
                     (-1.54985776E-22 ,-1.6007234E-13,0.70273029),
                     ( 1.53077E-23    ,-1.4435742E-12,0.513078),
                     (0      ,0        ,0),
                     (0      ,0        ,0),
                     (0      ,0        ,0),
                     (0      ,0        ,0),
                     (0      ,0.0384   ,0.283250),
                     (0      ,0        ,0),
                     (0      ,0        ,0),
                     (0      ,0        ,0),
                     (0      ,0        ,0),
                     (0      ,0        ,0),
                     (0      ,0        ,0),
                     (0      ,0        ,0),
                     (0      ,0        ,0),
                     (0      ,0        ,0),
                     (0      ,0        ,0),
                     (0      ,0        ,0),
                     (0      ,0        ,0),
                     (0      ,0        ,0),
                     (0      ,0        ,0),
                     (0      ,0        ,0));
  DefaultCCModelName       :  TModelNameType
                  = ('none         ',
                     'CC         ',
                     'CC         ',
                     'CC         ',
                     'CC         ',
                     'CC         ',
                     'CC         ',
                     'Belousova et al (2010)',
                     'CC         ',
                     'CC         ',
                     'CC         ',
                     'CC         ',
                     'CC         ',
                     'CC         ',
                     'CC         ',
                     'CC         ',
                     'CC         ',
                     'CC         ',
                     'CC         ',
                     'CC         ',
                     'CC         ',
                     'CC         ',
                     'CC         ');
  DefaultCC              :  TModelValueType
                  = ((0, 0      ,0    ),
                     (0, 0.10   ,0.0  ),
                     (0, 0.11   ,0.0  ),
                     (0, 0      ,0    ),
                     (0, 0      ,0    ),
                     (0, 0      ,0    ),
                     (0, 0      ,0    ),
                     (0, 0.03   ,0.0  ),
                     (0, 0      ,0    ),
                     (0, 0.015  ,0.0  ),
                     (0, 0      ,0    ),
                     (0, 0      ,0    ),
                     (0, 0      ,0    ),
                     (0, 0      ,0    ),
                     (0, 0      ,0    ),
                     (0, 0      ,0    ),
                     (0, 0      ,0    ),
                     (0, 0      ,0    ),
                     (0, 0      ,0    ),
                     (0, 0      ,0    ),
                     (0, 0      ,0    ),
                     (0, 0      ,0    ),
                     (0, 0      ,0    ));
  DefaultMuV               :  TMuValueType
                  = ((4.57E9 , 9.307 ,10.294 ,29.487,8.00),
                     (3.70E9 ,11.152 ,12.998 ,31.230,9.74),
                     (3.70E9 ,11.152 ,12.998 ,31.230,9.74));
  Mu_Char           : char    = '1';
  mu_choice         : shortint = 1;
  mu_age            : double    = 3.7e9;
  mu_206            : double    = 11.152;
  mu_207            : double    = 12.998;
  BlanketZErrVal    :  double   = 0.001;
  DefaultCalcFac           :  TCalcFacValueType
                  = ((0.0      ,0.0      ),
                     (2.692948 ,0.283040 ),
                     (0.531497 ,0.142521 ),
                     (0.0      ,0.0      ),
                     (0.0      ,0.0      ),
                     (0.0      ,0.0      ),
                     (0.0      ,0.0      ),
                     (0.134399 ,0.025983 ),
                     (0.0      ,0.0      ),
                     (0.00811  ,0.0      ),
                     (0.0      ,0.0      ),
                     (0.0      ,0.0      ),
                     (0.0      ,0.0      ),
                     (0.0      ,0.0      ),
                     (0.000001 ,0.000001 ),
                     (4.739172 ,0.628518 ),
                     (0.000001 ,0.000001 ),
                     (0.000001 ,0.000001 ),
                     (0.0      ,0.0      ),
                     (0.0      ,0.0      ),
                     (0.0      ,0.0      ),
                     (0.0      ,0.0      ),
                     (0.0      ,0.0      ));
  DefaultGraphColour      :  TGraphRGBColourType     {Red, Blue, Green}
                  = ((000, 000, 000),     {0 Spare}
                     (255, 000, 000),     {1 Points included}
                     (000, 255, 000),     {2 Points excluded}
                     (255, 255, 000),     {3 Ellipses included}
                     (000, 255, 000),     {4 Ellipses excluded}
                     (000, 000, 255),     {5 Regression line}
                     (000, 000, 200),     {6 Error envelope}
                     (000, 000, 000),     {7 Concordia and model curves}
                     (000, 000, 000),     {8 Concordia date ellipse}
                     (255, 255, 000));    {9 Cumulative histogram}

var
  GlobalChosenStyle : string;
  AdjustForNegativeIntercept : boolean;
  //N_Rep             : integer;  // used to be 60 but now needs to be set to 999
  //temporaryAnalType :  string[1];
  U238U235 : double;
  temporaryAnalType :  string;
  ItemsHaveChanged  :  boolean;
  iTechnique        :  set of TTechniqueType;
  iAnalType         :  TTechniqueType;
  iAnalTyp          :  integer;
  AnyChar           :  char;
  //RegisteredUser    :  string[30];
  RegisteredUser    :  string;
  AcceptableUser    :  boolean;
  //DateString        :  string[10];
  DateString        :  string;
  FAlpha            :  double;
  T_Mult            :  double;
  FileExistChar     :  char;
  //ProjectName       :  string[40];
  ProjectName       :  string;
  AnalType, AnalType8
                    :  char;
  GdwNetFileDir, Drive1, Drive2, Drive3 : string;
  Ch, TC            :  char;
  Mask              :  MaskArray;
  Is_OK             :  boolean;
  Msg,Title         :  Prompt;
  York_file         :  YorkFileType;
  TempDataRec       :  RDataRecord;
  Geodate_file         :  GeodateFileType;
  GeodateDataRec       :  RGeodateRecord;
  SmpNo             :  array [1..MaxSamp] of Samp;
  RecordNo          :  array [1..MaxSamp] of integer;
  Latitude,
  Longitude         :  array [1..MaxSamp] of single;
  R, Rho2,
  YPrec, XPrec,
  ZPrec, WPrec, AgePrec,
  Xtra, Xtra1,
  Xtra2, Xtra3      :  array [0..MaxSamp] of double;
  Residual          :  array [1..MaxSamp,1..2] of double;
  RFlg              :  array [0..MaxSamp] of char;
  PFlg              :  array [0..MaxSamp] of char;
  Conc, ErrorWt     :  array [1..MaxSamp,0..4] of double;
  Ratio             :  array [0..MaxSamp,0..4] of double;
  AnalTyp           :  array [1..MaxSamp] of char;
  RR, R2                :  array [0..MaxSamp] of double;
  ErrTyp            :  array [1..MaxSamp] of char;
  U, V, Z           :  array [1..MaxSamp] of double;
  Weight,
  Wt                :  array [0..MaxSamp,1..2] of double;
  Sum               :  array [1..8] of double;
  AnalTypeStr       :  AnTypStr;
  IError            :  shortint;
  NumberOfPoints, I, J, K
                    :  integer;
  Iteration         :  integer;
  Age, Slope, Intercept, Mu, InitRatio,
  Xcentroid, Ycentroid,
  SlopeError, InterceptError,
  UprIntercept, LwrIntercept,
  Slope1, Intercept1,
  Slope2, Intercept2
                    :  double;
  NewSlope, NewSlopeError,
  NewIntercept, NewInterceptError,
  ThetaError        : double;
  InputMethod, InputMethod2
                    :  char;
  Drive, DefDrive   :  char;
  SPath, TTPath      :  string;
  //Direc             :  string[40];
  Direc             :  string;
  //YorkFilename      :  string[12];
  YorkFilename      :  string;
  GeodateFilename      :  string;
  done,Processdone,Methoddone
                    :  boolean;
  EditDone          :  boolean;
  MethodOption, ProcessOption, EditOption,
  CheckRegress, DeviceChar
                    :  char;
  //PrintFile         : string[40];
  PrintFile         : string;
  Device            : text;
  {
  MSUM_Num          : array[1..999] of shortint;
  }
  MSUM_Val          : array[1..999] of double;
  {
  MSUM_Rec      : MSUM_Record;
  MSUM_File     : file of MSUM_Record;
  }
  BlanketErrType    :  char;
  BlanketXErrVal, BlanketYErrVal, BlanketRVal,
  BlanketXErr, BlanketYErr, BlanketR, BlanketZErr,
  CalcIsoRat, BlanketErr
                    :  char;
  MuAge11,MuAge12,MuAge13,
  MuAge21,MuAge22,MuAge23,
  MuX01,MuX02,MuX03,
  MuY01,MuY02,MuY03,
  Mu1,Mu2,Mu3,
  MuTic1,MuTic2,MuTic3      : double;
  PlotErrorEnvelope       : string[1];
  StartAtX,EndAtX,TicsAtX,StartAtY,EndAtY,TicsAtY : double;
  Graphdone : boolean;
  //Augmented         :  string[60];
  Augmented         :  string;
  NumberOfPointsRegressed
                    :  integer;
  NEquivPtsRegressed : double;
  ProbabilityOfFit,
  ProbabilityOfFitWO,
  ProbabilityOfFitW   : double;
  TempSlope         :  double;
  Epsilon1, EpError1,
  tempratio1        :  double;
  Ex, Ey, E76, V76, Fd, Fm,
  Chordlength       :  double;
  X_Uint, X_Lint, Y_Uint,
  Y_Lint            :  double;
  UprMuError, LwrMuError
                    :  double;
  AgeError, AgeErrorPlusIncl, AgeErrorMinusIncl, Msum    :  double;
  UpperAgeErrorIncl, LowerAgeErrorIncl,
  UpperAgeError, LowerAgeError
                    :  double;
  UprUprAgeErrorIncl,
  UprLwrAgeErrorIncl,
  UprUprAgeError, UprLwrAgeError,
  UprUprAgeError2, UprUprAgeError2Incl   :  double;
  LwrUprAgeErrorIncl,
  LwrLwrAgeErrorIncl,
  LwrUprAgeError, LwrLwrAgeError
                    :  double;
  Factor            :  double;
  Converg_done      :  boolean;
  SST, SSR, SSD, SSLF, SSPE  : double;
  MST, MSR, MSD, MSLF, MSPE  : double;
  Eps, Ro            :  array [1..MaxSamp] of double;
  Icnt               : integer;
  //Scrn_Title      :  string[60];
  Scrn_Title      :  string;
  Title_Len       :  shortint;
  InitRatioError  : double;
  XWide, YWide    : integer;
  SessionCode     : integer;
  ChooseEllipse   : char;
  EllipseMagnif   : double;
  ConstrainFlag   : boolean;
  XConstrain, YConstrain,
  AgeConstrain    : double;
  AllSame         : boolean;
  HistArray       : FreqArrayType;
  //NameString      : string[20];
  NameString      : string;
  Spectrum        : TCumArrayType;
  //SuiteName       : string[50];
  SuiteName       : string;
  //LitholName      : string[50];
  LitholName      : string;
  XItcpt, XItcptError
                  : double;
  MSWDe, MSWDwo, MSWDw,
  Probe, ProbWO, ProbW,
  DateWO, DateW, VarDateWO, VarDateW  : double;
  LastCountry : string;
  cdsPath : string;
  CommonFilePath : string;
  TicksEvery : double;
  MaxAgeConcordia : double;
  FormationAge,
  RequiredAge : double;
  DefaultErrorType : string;

  Element               :  TElementNameType;
  XRatioStr             :  TRatioNameType;
  YRatioStr             :  TRatioNameType;
  ZRatioStr             :  TRatioNameType;
  WRatioStr             :  TRatioNameType;
  GraphXRatioStr        :  TRatioNameType;
  GraphYRatioStr        :  TRatioNameType;
  GraphZRatioStr        :  TRatioNameType;
  Process               : TProcessNameType;
  ProcessAbr            :  TProcessNameType;
  DecayConstantSource   :  TDecayConstantSourceType;
  DecayConst            :  TDecayConstantValueType;
  DecayConstUncertainty :  TDecayUncertaintyValueType;
  TracerUncertainty     :  TTracerUncertaintyValueType;
  CHURModelName         :  TModelNameType;
  CHUR                  :  TModelValueType;
  DMModelName           :  TModelNameType;
  DM                    :  TModelValueType;
  CCModelName           :  TModelNameType;
  CC                    :  TModelValueType;
  MuV                   :  TMuValueType;
  CalcFac               :  TCalcFacValueType;
  GraphColour           :  TGraphRGBColourType;


implementation

end.
