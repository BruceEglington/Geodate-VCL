{ **********************************************************************
  *                         Unit PLOTVAR.PAS                           *
  *                           Version 1.0d                             *
  *                     (c) J. Debord, May 2002                        *
  **********************************************************************
           Variables and procedures common to WINPLOT and TEXPLOT
  ********************************************************************** }

unit plotvar;

interface

uses
  fmath, matrices;

{ ************************* Constants and types ************************ }

const
  MAXSYMBOL = 9;        { Max. number of symbols for plotting curves }
  EPS       = 1.0E-10;  { Lower limit for an axis label }

type
  TScale = (LIN_SCALE,         { Scale }
            LOG_SCALE);

  TGrid = (NO_GRID,            { Grid }
           HORIZ_GRID,
           VERTIC_GRID,
           BOTH_GRID);

  TAxis = record               { Coordinate axis }
    Scale          : TScale;
    Min, Max, Step : Float;
    Title          : String;
  end;

{ ******** Global variables defining the appearance of the graph ******* }

var
  Xwin1, Ywin1,
  Xwin2, Ywin2 : Integer;  { Window coordinates in % of maximum }

  GraphBorder : Boolean;   { For plotting a rectangle around the graph }

  XAxis : TAxis;           { Horizontal axis }

  YAxis : TAxis;           { Vertical axis }

  Grid : TGrid;

  GraphTitle : String;

{ ***************************** Procedures ***************************** }

procedure Interval(X1, X2             : Float;
                   MinDiv, MaxDiv     : Integer;
                   var Min, Max, Step : Float);
{ ----------------------------------------------------------------------
  Determines an interval [Min, Max] including the values from X1 to X2,
  and a subdivision Step of this interval
  ----------------------------------------------------------------------
  Input parameters  : X1, X2 = min. & max. values to be included
                      MinDiv = minimum nb of subdivisions
                      MaxDiv = maximum nb of subdivisions
  ----------------------------------------------------------------------
  Output parameters : Min, Max, Step
  ---------------------------------------------------------------------- }

procedure AutoScale(Z              : TVector;
                    Lbound, Ubound : Integer;
                    var Axis       : TAxis);
{ ----------------------------------------------------------------------
  Determines the scale of an axis
  ----------------------------------------------------------------------
  Input parameters  : Z      = array of values to be plotted
                      Lbound,
                      Ubound = indices of first and last elements of Z
  ----------------------------------------------------------------------
  Output parameters : Axis
  ---------------------------------------------------------------------- }

implementation

  procedure Interval(X1, X2 : Float; MinDiv, MaxDiv : Integer;
                     var Min, Max, Step : Float);
  var
    H, R, K : Float;
  begin
    if X1 >= X2 then Exit;
    H := X2 - X1;
    R := Int(Log10(H));
    if H < 1.0 then R := R - 1.0;
    Step := Exp10(R);

    repeat
      K := Int(H / Step);
      if K < MinDiv then Step := 0.5 * Step;
      if K > MaxDiv then Step := 2.0 * Step;
    until (K >= MinDiv) and (K <= MaxDiv);

    Min := Step * Int(X1 / Step);
    Max := Step * Int(X2 / Step);
    while Min > X1 do Min := Min - Step;
    while Max < X2 do Max := Max + Step;
  end;

  procedure AutoScale(Z : TVector; Lbound, Ubound : Integer;
                      var Axis : TAxis);
  var
    I                  : Integer;
    Zmin, Zmax, Z1, Z2 : Float;
  begin
    if Axis.Scale = LIN_SCALE then
      Interval(Min(Z, Lbound, Ubound), Max(Z, Lbound, Ubound),
               2, 6, Axis.Min, Axis.Max, Axis.Step)
    else
      begin
        Zmin := MAXNUM; Zmax := 0.0;
        for I := Lbound to Ubound do
          if Z[I] > 0.0 then
            if Z[I] < Zmin then
              Zmin := Z[I]
            else if Z[I] > Zmax then
              Zmax := Z[I];
        Z1 := Int(Log10(Zmin));
        Z2 := Int(Log10(Zmax));
        if Zmin < 1.0 then Z1 := Z1 - 1.0;
        if Zmax > 1.0 then Z2 := Z2 + 1.0;
        Axis.Min := Z1;
        Axis.Max := Z2;
        Axis.Step := 1.0;
      end;
  end;

begin
  Xwin1 := 15;
  Ywin1 := 15;
  Xwin2 := 75;
  Ywin2 := 75;

  GraphBorder := True;

  XAxis.Scale := LIN_SCALE;
  XAxis.Min   := 0.0;
  XAxis.Max   := 1.0;
  XAxis.Step  := 0.2;
  XAxis.Title := 'X';

  YAxis.Scale := LIN_SCALE;
  YAxis.Min   := 0.0;
  YAxis.Max   := 1.0;
  YAxis.Step  := 0.2;
  YAxis.Title := 'Y';

  Grid := BOTH_GRID;

  GraphTitle := '';
end.
