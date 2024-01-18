{ **********************************************************************
  *                         Unit PASTRING.PAS                          *
  *                           Version 1.9d                             *
  *                     (c) J. Debord, July 2002                       *
  **********************************************************************
                         Pascal string routines
  ********************************************************************** }

unit pastring;

interface

uses
  fmath, matrices;

{ *** Global variables controlling the appearance of a numeric string ** }

var
  NumLength  : Integer;  { Length of a numeric field }
  MaxDec     : Integer;  { Max. number of decimal places }
  FloatPoint : Boolean;  { Floating point notation }
  NSZero     : Boolean;  { Write non significant zero's }

{ ************************** String routines *************************** }

function LTrim(S : String) : String;
{ ----------------------------------------------------------------------
  Removes leading blanks
  ---------------------------------------------------------------------- }

function RTrim(S : String) : String;
{ ----------------------------------------------------------------------
  Removes trailing blanks
  ---------------------------------------------------------------------- }

function Trim(S : String) : String;
{ ----------------------------------------------------------------------
  Removes leading and trailing blanks
  ---------------------------------------------------------------------- }

function StrChar(N : Integer; C : Char) : String;
{ ----------------------------------------------------------------------
  Returns a string made of character C repeated N times
  ---------------------------------------------------------------------- }

function RFill(S : String; L : Integer) : String;
{ ----------------------------------------------------------------------
  Completes string S with trailing blanks for a total length L
  ---------------------------------------------------------------------- }

function LFill(S : String; L : Integer) : String;
{ ----------------------------------------------------------------------
  Completes string S with leading blanks for a total length L
  ---------------------------------------------------------------------- }

function CFill(S : String; L : Integer) : String;
{ ----------------------------------------------------------------------
  Completes string S with leading blanks
  to center the string on a total length L
  ---------------------------------------------------------------------- }

function Replace(S : String; C1, C2 : Char) : String;
{ ----------------------------------------------------------------------
  Replaces in string S all the occurences
  of character C1 by character C2
  ---------------------------------------------------------------------- }

function Extract(S : String; var Index : Integer; Delim : Char) : String;
{ ----------------------------------------------------------------------
  Extracts a field from a string. Index is the position of the first
  character of the field. Delim is the character used to separate
  fields (e.g. blank, comma or tabulation). Blanks immediately
  following Delim are ignored. Index is updated to the position of
  the next field.
  ---------------------------------------------------------------------- }

procedure Parse(S : String; Delim : Char; Field : TStrVector; var N : Integer);
{ ----------------------------------------------------------------------
  Parses a string into its constitutive fields. Delim is the field
  separator. The number of fields is returned in N. The fields are
  returned in Field[0]..Field[N - 1]. Field must be dimensioned in
  the calling program.
  ---------------------------------------------------------------------- }

function Float2Str(X : Float) : String;
{ ----------------------------------------------------------------------
  Converts a real to a string according to the values of the global
  variables NumLength, MaxDec, FloatPoint and NSZero
  ---------------------------------------------------------------------- }

function Int2Str(N : LongInt) : String;
{ ----------------------------------------------------------------------
  Converts an integer to a string according to the values of the global
  variables NumLength and MaxDec.
  ---------------------------------------------------------------------- }

function Comp2Str(Z : Complex) : String;
{ ----------------------------------------------------------------------
  Converts a complex number to a string.
  ---------------------------------------------------------------------- }

implementation

  function LTrim(S : String) : String;
  begin
    if S <> '' then
      repeat
        if S[1] = ' ' then Delete(S, 1, 1);
      until S[1] <> ' ';
    LTrim := S;
  end;

  function RTrim(S : String) : String;
  var
    L1 : Integer;
  begin
    if S <> '' then
      repeat
        L1 := Length(S);
        if S[L1] = ' ' then Delete(S, L1, 1);
      until S[L1] <> ' ';
    RTrim := S;
  end;

  function Trim(S : String) : String;
  begin
    Trim := LTrim(RTrim(S));
  end;

  function StrChar(N : Integer; C : Char) : String;
  var
    I : Integer;
    S : String;
  begin
    S := '';
    for I := 1 to N do
      S := S + C;
    StrChar := S;
  end;

  function RFill(S : String; L : Integer) : String;
  var
    L1 : Integer;
  begin
    L1 := Length(S);
    if L1 >= L then
      RFill := S
    else
      RFill := S + StrChar(L - L1, ' ');
  end;

  function LFill(S : String; L : Integer) : String;
  var
    L1 : Integer;
  begin
    L1 := Length(S);
    if L1 >= L then
      LFill := S
    else
      LFill := StrChar(L - L1, ' ') + S;
  end;

  function CFill(S : String; L : Integer) : String;
  var
    L1 : Integer;
  begin
    L1 := Length(S);
    if L1 >= L then
      CFill := S
    else
      CFill := StrChar((L - L1) div 2, ' ') + S;
  end;

  function Replace(S : String; C1, C2 : Char) : String;
  var
    S1 : String;
    K : Integer;
  begin
    S1 := S;
    K := Pos(C1, S1);
    while K > 0 do
      begin
        S1[K] := C2;
        K := Pos(C1, S1);
      end;
    Replace := S1;
  end;

  function Extract(S : String; var Index : Integer; Delim : Char) : String;
  var
    I, L : Integer;
  begin
    I := Index;
    L := Length(S);

    { Search for Delim }
    while (I <= L) and (S[I] <> Delim) do
      Inc(I);

    { Extract field }
    if I = Index then
      Extract := ''
    else
      Extract := Copy(S, Index, I - Index);

    { Skip blanks after Delim }
    repeat
      Inc(I);
    until (I > L) or (S[I] <> ' ');

    { Update Index }
    Index := I;
  end;

  procedure Parse(S : String; Delim : Char; Field : TStrVector; var N : Integer);
  var
    I, Index, L : Integer;
  begin
    I := 0;
    Index := 1;
    L := Length(S);
    repeat
      Field[I] := Extract(S, Index, Delim);
      Inc(I);
    until (Index > L) or (I > High(Field));
    N := I;
  end;

  function Float2Str(X : Float) : String;
  var
    S : String;
    C : Char;
    L : Integer;
  begin
    if FloatPoint then
      begin
        Str(X:Pred(NumLength), S);
        S := ' ' + S;
      end
    else
      begin
        Str(X:NumLength:MaxDec, S);
        if not NSZero then
          repeat
            L := Length(S);
            C := S[L];
            if (C = '0') or (C = '.') then Delete(S, L, 1);
          until C <> '0';
      end;
    Float2Str := S;
  end;

  function Int2Str(N : LongInt) : String;
  var
    S : String;
  begin
    Str(N:(NumLength - MaxDec - 1), S);
    Int2Str := S;
  end;

  function Comp2Str(Z : Complex) : String;
  var
    S : String;
  begin
    if Z.Y >= 0.0 then S := ' + ' else S := ' - ';
    Comp2Str := Float2Str(Z.X) + S + Float2Str(Abs(Z.Y)) + ' * i';
  end;

begin
  NumLength  := 10;
  MaxDec     := 4;
  FloatPoint := False;
  NSZero     := True;
end.

