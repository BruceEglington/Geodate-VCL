(*==========================================================================

    fourier.pas  -  Don Cross <dcross@intersrv.com>

    Modified by Jean Debord <JDebord@compuserve.com> for use with TP Math.

    This is a Turbo Pascal Unit for calculating the Fast Fourier Transform
    (FFT) and the Inverse Fast Fourier Transform (IFFT).
    Visit the following URL for the latest version of this code.
    This page also has a C/C++ version, and a brief discussion of the
    theory behind the FFT algorithm.

       http://www.intersrv.com/~dcross/fft.html#pascal

    Revision history [most recent first]:

2002 January 20 [Jean Debord]
     Modified for use with Delphi 6:
    1. Renamed MaxPower as MaxPowerOfTwo. Interfaced this variable.
       Set its default value to 20.
    2. Changed vector type PVector to dynamic array type TVector.
    3. Used overloaded procedure FFT for both real and integer inputs.
    4. Removed procedure FFT_Integer_Cleanup which was no longer necessary.
    5. Slightly modified function NumberOfBitsNeeded so that Delphi does
       not complain that its value may be undefined.

1998 November 27 [Jean Debord]
     Replaced the constant MAXPOWER by a variable which is initialized
     according to the value of MAX_FLT defined in MATRICES.PAS

1997 March 1 [Jean Debord]
     Modifications for use with the TP Math library:
    1. Added a USES clause for the TP Math units.
    2. Set real type to Float (defined in FMATH.PAS)
    3. Added a constant MAXPOWER to define the maximum number of points.
       Modified functions IsPowerOfTwo and NumberOfBitsNeeded accordingly.
    4. Changed array types to those defined in TP Math. Modified array
       allocation, deallocation and reference accordingly.
    5. Removed compiler directives, which were no longer necessary.
    6. Modified some typographical and formatting options so that the
       code looks like the other TP Math units.
    No modification was made to the original algorithm.

1996 December 11 [Don Cross]
    Improved documentation of the procedure CalcFrequency.
    Fixed some messed up comments in procedure IFFT.

1996 December 6 [Don Cross]
    Made procedure 'fft_integer' more efficient when buffer size changes
    in successive calls:  the buffer is now only resized when the input
    has more samples, not a differing number of samples.
    Also changed the way 'fft_integer_cleanup' works so that it is
    more "bullet-proof".

1996 December 4 [Don Cross]
    Adding the procedure 'CalcFrequency', which calculates the FFT
    at a specific frequency index p=0..n-1, instead of the whole
    FFT.  This is O(n^2) instead of O(n*log(n)).

1996 November 30 [Don Cross]
    Adding a routine to allow FFT of an input array of integers.
    It is called 'fft_integer'.

1996 November 18 [Don Cross]
    Added some comments.

1996 November 17 [Don Cross]
    Wrote and debugged first version.

==========================================================================*)

unit fourier;

interface

uses
  fmath, matrices;

var
  MaxPowerOfTwo : Integer;

(*---------------------------------------------------------------------------
  procedure FFT

  Calculates the Fast Fourier Transform of the array of complex numbers
  represented by 'RealIn' and 'ImagIn' to produce the output complex
  numbers in 'RealOut' and 'ImagOut'.
---------------------------------------------------------------------------*)
procedure FFT(NumSamples                       : Integer;
              RealIn, ImagIn, RealOut, ImagOut : TVector); overload;

(*---------------------------------------------------------------------------
  procedure FFT for integer input arrays.
---------------------------------------------------------------------------*)
procedure FFT(NumSamples       : Integer;
              RealIn, ImagIn   : TIntVector;
              RealOut, ImagOut : TVector); overload;

(*---------------------------------------------------------------------------
  procedure IFFT

  Calculates the Inverse Fast Fourier Transform of the array of complex
  numbers represented by 'RealIn' and 'ImagIn' to produce the output complex
  numbers in 'RealOut' and 'ImagOut'.
---------------------------------------------------------------------------*)
procedure IFFT(NumSamples                       : Integer;
               RealIn, ImagIn, RealOut, ImagOut : TVector);

(*--------------------------------------------------------------------------
   procedure CalcFrequency

   This procedure calculates the complex frequency sample at a given
   index directly.  Use this instead of 'FFT' when you only need one
   or two frequency samples, not the whole spectrum.

   It is also useful for calculating the Discrete Fourier Transform (DFT)
   of a number of data which is not an integer power of 2. For example,
   you could calculate the DFT of 100 points instead of rounding up to
   128 and padding the extra 28 array slots with zeroes.
--------------------------------------------------------------------------*)
procedure CalcFrequency(NumSamples, FrequencyIndex : Integer;
                        RealIn, ImagIn             : TVector;
                        var RealOut, ImagOut       : Float);

implementation

  function IsPowerOfTwo(X : Integer) : Boolean;
  var
    I, Y : Integer;
  begin
    Y := 2;
    for I := 1 to Pred(MaxPowerOfTwo) do
      begin
        if X = Y then
          begin
            IsPowerOfTwo := True;
            Exit;
          end;
        Y := Y shl 1;
      end;
    IsPowerOfTwo := False;
  end;

  function NumberOfBitsNeeded(PowerOfTwo : Integer) : Integer;
  var
    I : Integer;
  begin
    for I := 0 to MaxPowerOfTwo do
      begin
        if (PowerOfTwo and (1 shl I)) <> 0 then
          begin
            NumberOfBitsNeeded := I;
            Exit;
          end;
      end;
    NumberOfBitsNeeded := 0;
  end;

  function ReverseBits(Index, NumBits : Integer) : Integer;
  var
    I, Rev : Integer;
  begin
    Rev := 0;
    for I := 0 to NumBits - 1 do
      begin
        Rev := (Rev shl 1) or (Index and 1);
        Index := Index shr 1;
      end;
    ReverseBits := Rev;
  end;

  procedure FourierTransform(AngleNumerator                   : Float;
                             NumSamples                       : Integer;
                             RealIn, ImagIn, RealOut, ImagOut : TVector);
  var
    NumBits, I, J, K, N, BlockSize, BlockEnd : Integer;
    Delta_angle, Delta_ar                    : Float;
    Alpha, Beta                              : Float;
    Tr, Ti, Ar, Ai                           : Float;
  begin
    if not IsPowerOfTwo(NumSamples) or (NumSamples < 2) then
      begin
        Write('Error in procedure Fourier:  NumSamples=', NumSamples);
        WriteLn(' is not a positive integer power of 2.');
        Halt;
      end;

    NumBits := NumberOfBitsNeeded(NumSamples);
    for I := 0 to NumSamples - 1 do
      begin
        J := ReverseBits(I, NumBits);
        RealOut[J] := RealIn[I];
        ImagOut[J] := ImagIn[I];
      end;

    BlockEnd := 1;
    BlockSize := 2;
    while BlockSize <= NumSamples do
      begin
        Delta_angle := AngleNumerator / BlockSize;
        Alpha := Sin(0.5 * Delta_angle);
        Alpha := 2.0 * Alpha * Alpha;
        Beta := Sin(Delta_angle);

        I := 0;
        while I < NumSamples do 
          begin
            Ar := 1.0;    (* cos(0) *)
            Ai := 0.0;    (* sin(0) *)

            J := I;
            for N := 0 to BlockEnd - 1 do
              begin
                K := J + BlockEnd;
                Tr := Ar * RealOut[K] - Ai * ImagOut[K];
                Ti := Ar * ImagOut[K] + Ai * RealOut[K];
                RealOut[K] := RealOut[J] - Tr;
                ImagOut[K] := ImagOut[J] - Ti;
                RealOut[J] := RealOut[J] + Tr;
                ImagOut[J] := ImagOut[J] + Ti;
                Delta_ar := Alpha * Ar + Beta * Ai;
                Ai := Ai - (Alpha * Ai - Beta * Ar);
                Ar := Ar - Delta_ar;
                Inc(J);
              end;

            I := I + BlockSize;
          end;

        BlockEnd := BlockSize;
        BlockSize := BlockSize shl 1;
      end;
  end;

  procedure FFT(NumSamples                       : Integer;
                RealIn, ImagIn, RealOut, ImagOut : TVector);
  begin
    FourierTransform(2 * PI, NumSamples, RealIn, ImagIn, RealOut, ImagOut);
  end;

  procedure IFFT(NumSamples                       : Integer;
                 RealIn, ImagIn, RealOut, ImagOut : TVector);
  var
    I : Integer;
  begin
    FourierTransform(- 2 * PI, NumSamples, RealIn, ImagIn, RealOut, ImagOut);

    { Normalize the resulting time samples }
    for I := 0 to NumSamples - 1 do
      begin
        RealOut[I] := RealOut[I] / NumSamples;
        ImagOut[I] := ImagOut[I] / NumSamples;
      end;
  end;

var
  RealTemp, ImagTemp : TVector;
  TempArraySize      : Integer;

  procedure FFT(NumSamples       : Integer;
                RealIn, ImagIn   : TIntVector;
                RealOut, ImagOut : TVector); overload;
  var
    I : Integer;
  begin
    if NumSamples > TempArraySize then
      begin
        DimVector(RealTemp, NumSamples);
        DimVector(ImagTemp, NumSamples);
        TempArraySize := NumSamples;
      end;

    for I := 0 to NumSamples - 1 do
      begin
        RealTemp[I] := RealIn[I];
        ImagTemp[I] := ImagIn[I];
      end;

    FourierTransform(2 * PI, NumSamples, RealTemp, ImagTemp, RealOut, ImagOut);
  end;

  procedure CalcFrequency(NumSamples, FrequencyIndex : Integer;
                          RealIn, ImagIn             : TVector;
                          var RealOut, ImagOut       : Float);
  var
    K                             : Integer;
    Cos1, Cos2, Cos3, Theta, Beta : Float;
    Sin1, Sin2, Sin3              : Float;
  begin
    RealOut := 0.0;
    ImagOut := 0.0;
    Theta := 2 * PI * FrequencyIndex / NumSamples;
    Sin1 := Sin(- 2 * Theta);
    Sin2 := Sin(- Theta);
    Cos1 := Cos(- 2 * Theta);
    Cos2 := Cos(- Theta);
    Beta := 2 * Cos2;
    for K := 0 to NumSamples - 1 do
      begin
        { Update trig values }
        Sin3 := Beta * Sin2 - Sin1;
        Sin1 := Sin2;
        Sin2 := Sin3;

        Cos3 := Beta * Cos2 - Cos1;
        Cos1 := Cos2;
        Cos2 := Cos3;

        RealOut := RealOut + RealIn[K] * Cos3 - ImagIn[K] * Sin3;
        ImagOut := ImagOut + ImagIn[K] * Cos3 + RealIn[K] * Sin3;
      end;
  end;

begin
  MaxPowerOfTwo := 20;
  TempArraySize := 0;   { Flag that buffers RealTemp, ImagTemp not allocated }
end.
