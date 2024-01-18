unit uMain;

{ **********************************************************************
  *                         Program MINFUNC.PAS                        *
  *                             Version 1.8                            *
  *                      (c) J. Debord, June 2001                      *
  **********************************************************************
  This program demonstrates the use of simulated annealing and BFGS
  methods for minimizing a function of several variables.

  BFGS (like Marquardt or Simplex methods) is a local optimization
  method which works only at the vicinity of a minimum. By contrast,
  simulated annealing can escape from such a local minimum and find
  the global one. This is obtained by accepting that the function
  increases sometimes during the process. The probability of acceptation
  is controlled by a parameter called temperature: a higher temperature
  means a higher probability of acceptation. At each temperature, the
  algorithm generates a random point from the current one (using a
  uniform distribution in our implementation). The new point is always
  accepted if the function decreases, otherwise the probability of
  acceptation depends on the temperature. The process is repeated for
  a given number of points, then the temperature is decreased. The
  algorithm stops when the parameters do not vary by more than a
  user-defined limit.

  Simulated annealing is used here to determine an approximation
  to the global minimum, which is then refined by BFGS.

  Notes:

  1) For best performance, the program should be compiled in extended
     precision ($DEFINE EXTENDEDREAL). Rebuild all units if necessary.

  2) It may be necessary to restart the program if the algorithm does
     not produce the expected results, since the random number generator
     is re-initialized at each start of the program.
  ********************************************************************** }

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls,
  FMath, Matrices, Optim, SimOpt, PaString;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    MainMenu1: TMainMenu;
    menFunc: TMenuItem;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure AddFuncMenuItems;
    procedure WriteLn(S: String);
    procedure WriteResult(Method  : String;
                          Nvar    : Integer;
                          Hessian : Boolean;
                          X       : TVector;
                          H_inv   : TMatrix;
                          F_min   : Float);
  public
    { Public declarations }
    procedure FitFunction(I: Word);
  end;

  TFuncMenuItem = class ( TMenuItem )
  public
    procedure Click; override ;
  end ;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  RandNum,
  uFunc;

const
  NFUNC   = 10;  { Number of functions }
  MAXNVAR = 4;   { Maximum number of variables }

type
  TFuncIndex = 1..NFUNC ;

const
  FuncName : array[TFuncIndex] of String[70] =
  ('Numerical Recipes Example 1: Minimum at (-2.0, +/-0.89442719), F = 0 ',
   'Numerical Recipes Example 2: Minimum at (0, 0, 0, 0), F = 1          ',
   'Rosenbrock function: Minimum at (1, 1), F = 0                        ',
   'Powell function: Minimum at (0, 0, 0, 0), F = 0  (Singular Hessian)  ',
   'Another Powell function: Minimum at x1=x2=x3= +/- Sqrt(4*n+1), F = -3',
   'Fletcher & Powell function: Minimum at (1, 0, 0), F = 0              ',
   'Colville function: Minimum at (1, 1, 1, 1), F = 0                    ',
   'Griewank function: Minimum at (0, 0), F = 0                          ',
   'Chichinadze function: Minimum at (5.90133, 0.5), F = -43.3159        ',
   'Rastrigin function: Minimum at (0, 0), F = -2                        ');
  Func      : array[TFuncIndex] of TFuncNVar=  { Functions }
  (Func1, Func2, Func3, Func4, Func5, Func6, Func7, Func8, Func9, Func10);


const
  Nvar : array[TFuncIndex] of Integer =
         (2, 4, 2, 4, 3, 3, 4, 2, 2, 2);  { Number of variables }

{ TForm1 }

procedure TForm1.AddFuncMenuItems;
var
  Index : Word ;
  IFunc : TFuncIndex ;
  NewFuncItem : TFuncMenuItem ;
begin
// Func option on main menu
  with menFunc do
    begin
      // Remove old Func menu items
      for Index := Count downto 1 do
        Delete ( Index - 1 ) ;
      // Add new Func menu items
      Index := 0 ;
      for IFunc := 1 to NFUNC do
        begin
          Inc ( Index ) ;
          NewFuncItem := TFuncMenuItem.Create ( Self ) ;
          with NewFuncItem do
            begin
              Caption := FuncName[IFunc] ;
              Hint := 'Fit ' + LowerCase ( Caption ) ;
              if ( Index - 1 ) mod 20 = 0 then
                Break := mbBarBreak ;
              if ( Index - 1 ) mod 20 = 0 then
                NewFuncItem.Break := mbBarBreak ;
            end ;
          Add ( NewFuncItem ) ;
        end ;
    end ;
end;

procedure TForm1.FitFunction(I: Word);
var
  X, Xmin, Xmax : TVector;                       { Variables and limit values }
  H_inv         : TMatrix;                       { Inverse hessian matrix }
  F_min         : Float;                         { Function value at minimum }
  ErrCode       : Integer;                       { Error code }
  J             : Integer;                       { Loop variable }
begin
  Memo1.Lines.Clear;

  { Allocate arrays }
  DimVector(X, Nvar[I]);
  DimVector(Xmin, Nvar[I]);
  DimVector(Xmax, Nvar[I]);
  DimMatrix(H_inv, Nvar[I], Nvar[I]);

  { Initialize limits and pick starting point }
  for J := 1 to Nvar[I] do
    begin
      Xmin[J] := -10.0;
      Xmax[J] := 10.0;
      X[J] := Xmin[J] + RanMar * (Xmax[J] - Xmin[J]);
    end;

      Writeln(FuncName[I]);

      { Approximate global minimum with simulated annealing }
      ErrCode := SimAnn(Func[I], X, Xmin, Xmax, 1, Nvar[I], 1000, 1.0E-4, F_min);

      if ErrCode = OPT_OK then
        WriteResult('Simulated Annealing', Nvar[I], False, X, H_inv, F_min);

      { Refine minimum with BFGS }
      ErrCode := BFGS(Func[I], NumGradient,
                      X, 1, Nvar[I], 1000, 1.0E-10, F_min, H_inv);

      if ErrCode = OPT_OK then
        WriteResult('BFGS', Nvar[I], True, X, H_inv, F_min);

  X := nil ;
  Xmin := nil ;
  Xmax := nil ;
  H_inv := nil ;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Lines.Clear;
  AddFuncMenuItems;
end;

procedure TForm1.WriteLn(S: String);
begin
  Memo1.Lines.Add(S);
end;

procedure TForm1.WriteResult(Method: String; Nvar: Integer;
  Hessian: Boolean; X: TVector; H_inv: TMatrix; F_min: Float);
{ --------------------------------------------------------------------
  Outputs results to screen.
  Hessian indicates if the Hessian matrix has been evaluated
  -------------------------------------------------------------------- }
  var
    I, J : Integer;
    S : String ;
  begin
    WriteLn('');
    WriteLn(Method+' '+StrChar(44 - Length(Method), '*'));
    WriteLn('Coordinates of minimum         Function value');
    WriteLn('');
    S := '' ;
    for I := 1 to Nvar do
      begin
        S := FloatToStr(X[I]);
        if I = 1 then
          S := S + StrChar(17, ' ')+ FloatToStr(F_Min);
        WriteLn(S);
      end;

    if Hessian then
      begin
        WriteLn('');
        WriteLn('Inverse Hessian matrix :');
        WriteLn('');
        for I := 1 to Nvar do
          begin
            S := '' ;
            for J := 1 to Nvar do
              S := S + FloatToStr(H_inv[I,J])+' ';
            WriteLn(S);
          end;
      end;
end;

{ TFuncMenuItem }

procedure TFuncMenuItem.Click;
begin
  inherited;
  Form1.FitFunction(MenuIndex+1);
end;

end.
