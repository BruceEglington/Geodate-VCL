{====
    from the 1966(?) issue of the U.S. Standard Atmosphere;
    Z = geometric altitude, in kilometers; density is in
    units of kg/m^3.
{====}

Unit TestData;

INTERFACE

uses Matrices;

Const
  NumDataPTS = 114;

Var
  Density, Altitude : TVector;

Procedure GetTestData;
{====
    the procedure GetTestData returns two PVectors (see the
    TPMath Matrices unit), one containing altitudes, and the
    other containing the natural log of the associated density
    data
{====}

IMPLEMENTATION


Procedure GetTestData;
  Var
    i : integer;
  BEGIN
    DimVector(Density,  NumDataPTS);
    DimVector(Altitude, NumDataPTS);
    Altitude[  0] :=  0.00;     Density[  0] := 1.167e+00;
    Altitude[  1] :=  0.25;     Density[  1] := 1.140e+00;
    Altitude[  2] :=  0.50;     Density[  2] := 1.115e+00;
    Altitude[  3] :=  0.75;     Density[  3] := 1.089e+00;
    Altitude[  4] :=  1.00;     Density[  4] := 1.064e+00;
    Altitude[  5] :=  1.25;     Density[  5] := 1.040e+00;
    Altitude[  6] :=  1.50;     Density[  6] := 1.016e+00;
    Altitude[  7] :=  1.75;     Density[  7] := 9.922e-01;
    Altitude[  8] :=  2.00;     Density[  8] := 9.698e-01;
    Altitude[  9] :=  2.25;     Density[  9] := 9.460e-01;
    Altitude[ 10] :=  2.50;     Density[ 10] := 9.185e-01;
    Altitude[ 11] :=  2.75;     Density[ 11] := 8.968e-01;
    Altitude[ 12] :=  3.00;     Density[ 12] := 8.757e-01;
    Altitude[ 13] :=  3.25;     Density[ 13] := 8.550e-01;
    Altitude[ 14] :=  3.50;     Density[ 14] := 8.347e-01;
    Altitude[ 15] :=  3.75;     Density[ 15] := 8.147e-01;
    Altitude[ 16] :=  4.00;     Density[ 16] := 7.951e-01;
    Altitude[ 17] :=  4.25;     Density[ 17] := 7.757e-01;
    Altitude[ 18] :=  4.50;     Density[ 18] := 7.567e-01;
    Altitude[ 19] :=  4.75;     Density[ 19] := 7.381e-01;
    Altitude[ 20] :=  5.00;     Density[ 20] := 7.198e-01;
    Altitude[ 21] :=  5.25;     Density[ 21] := 7.019e-01;
    Altitude[ 22] :=  5.50;     Density[ 22] := 6.843e-01;
    Altitude[ 23] :=  5.75;     Density[ 23] := 6.671e-01;
    Altitude[ 24] :=  6.00;     Density[ 24] := 6.501e-01;
    Altitude[ 25] :=  6.25;     Density[ 25] := 6.335e-01;
    Altitude[ 26] :=  6.50;     Density[ 26] := 6.172e-01;
    Altitude[ 27] :=  6.75;     Density[ 27] := 6.012e-01;
    Altitude[ 28] :=  7.00;     Density[ 28] := 5.855e-01;
    Altitude[ 29] :=  7.25;     Density[ 29] := 5.701e-01;
    Altitude[ 30] :=  7.50;     Density[ 30] := 5.551e-01;
    Altitude[ 31] :=  7.75;     Density[ 31] := 5.403e-01;
    Altitude[ 32] :=  8.00;     Density[ 32] := 5.258e-01;
    Altitude[ 33] :=  8.25;     Density[ 33] := 5.116e-01;
    Altitude[ 34] :=  8.50;     Density[ 34] := 4.977e-01;
    Altitude[ 35] :=  8.75;     Density[ 35] := 4.841e-01;
    Altitude[ 36] :=  9.00;     Density[ 36] := 4.708e-01;
    Altitude[ 37] :=  9.25;     Density[ 37] := 4.578e-01;
    Altitude[ 38] :=  9.50;     Density[ 38] := 4.450e-01;
    Altitude[ 39] :=  9.75;     Density[ 39] := 4.325e-01;
    Altitude[ 40] := 10.00;     Density[ 40] := 4.203e-01;
    Altitude[ 41] := 10.25;     Density[ 41] := 4.083e-01;
    Altitude[ 42] := 10.50;     Density[ 42] := 3.966e-01;
    Altitude[ 43] := 10.75;     Density[ 43] := 3.851e-01;
    Altitude[ 44] := 11.00;     Density[ 44] := 3.739e-01;
    Altitude[ 45] := 11.50;     Density[ 45] := 3.523e-01;
    Altitude[ 46] := 12.00;     Density[ 46] := 3.316e-01;
    Altitude[ 47] := 12.50;     Density[ 47] := 3.118e-01;
    Altitude[ 48] := 13.00;     Density[ 48] := 2.929e-01;
    Altitude[ 49] := 13.50;     Density[ 49] := 2.749e-01;
    Altitude[ 50] := 14.00;     Density[ 50] := 2.578e-01;
    Altitude[ 51] := 14.50;     Density[ 51] := 2.415e-01;
    Altitude[ 52] := 15.00;     Density[ 52] := 2.260e-01;
    Altitude[ 53] := 15.50;     Density[ 53] := 2.112e-01;
    Altitude[ 54] := 16.00;     Density[ 54] := 1.972e-01;
    Altitude[ 55] := 16.50;     Density[ 55] := 1.839e-01;
    Altitude[ 56] := 17.00;     Density[ 56] := 1.676e-01;
    Altitude[ 57] := 17.50;     Density[ 57] := 1.521e-01;
    Altitude[ 58] := 18.00;     Density[ 58] := 1.382e-01;
    Altitude[ 59] := 18.50;     Density[ 59] := 1.257e-01;
    Altitude[ 60] := 19.00;     Density[ 60] := 1.145e-01;
    Altitude[ 61] := 19.50;     Density[ 61] := 1.043e-01;
    Altitude[ 62] := 20.00;     Density[ 62] := 9.516e-02;
    Altitude[ 63] := 20.50;     Density[ 63] := 8.688e-02;
    Altitude[ 64] := 21.00;     Density[ 64] := 7.938e-02;
    Altitude[ 65] := 21.50;     Density[ 65] := 7.260e-02;
    Altitude[ 66] := 22.00;     Density[ 66] := 6.645e-02;
    Altitude[ 67] := 22.50;     Density[ 67] := 6.105e-02;
    Altitude[ 68] := 23.00;     Density[ 68] := 5.618e-02;
    Altitude[ 69] := 23.50;     Density[ 69] := 5.172e-02;
    Altitude[ 70] := 24.00;     Density[ 70] := 4.763e-02;
    Altitude[ 71] := 24.50;     Density[ 71] := 4.389e-02;
    Altitude[ 72] := 25.00;     Density[ 72] := 4.045e-02;
    Altitude[ 73] := 25.50;     Density[ 73] := 3.730e-02;
    Altitude[ 74] := 26.00;     Density[ 74] := 3.441e-02;
    Altitude[ 75] := 26.50;     Density[ 75] := 3.176e-02;
    Altitude[ 76] := 27.00;     Density[ 76] := 2.932e-02;
    Altitude[ 77] := 27.50;     Density[ 77] := 2.708e-02;
    Altitude[ 78] := 28.00;     Density[ 78] := 2.502e-02;
    Altitude[ 79] := 28.50;     Density[ 79] := 2.313e-02;
    Altitude[ 80] := 29.00;     Density[ 80] := 2.138e-02;
    Altitude[ 81] := 29.50;     Density[ 81] := 1.978e-02;
    Altitude[ 82] := 30.00;     Density[ 82] := 1.831e-02;
    Altitude[ 83] := 30.50;     Density[ 83] := 1.695e-02;
    Altitude[ 84] := 31.00;     Density[ 84] := 1.569e-02;
    Altitude[ 85] := 31.50;     Density[ 85] := 1.454e-02;
    Altitude[ 86] := 32.00;     Density[ 86] := 1.347e-02;
    Altitude[ 87] := 33.00;     Density[ 87] := 1.158e-02;
    Altitude[ 88] := 34.00;     Density[ 88] := 9.974e-03;
    Altitude[ 89] := 35.00;     Density[ 89] := 8.600e-03;
    Altitude[ 90] := 36.00;     Density[ 90] := 7.425e-03;
    Altitude[ 91] := 37.00;     Density[ 91] := 6.419e-03;
    Altitude[ 92] := 38.00;     Density[ 92] := 5.557e-03;
    Altitude[ 93] := 39.00;     Density[ 93] := 4.817e-03;
    Altitude[ 94] := 40.00;     Density[ 94] := 4.181e-03;
    Altitude[ 95] := 41.00;     Density[ 95] := 3.633e-03;
    Altitude[ 96] := 42.00;     Density[ 96] := 3.161e-03;
    Altitude[ 97] := 43.00;     Density[ 97] := 2.753e-03;
    Altitude[ 98] := 44.00;     Density[ 98] := 2.401e-03;
    Altitude[ 99] := 45.00;     Density[ 99] := 2.097e-03;
    Altitude[100] := 46.00;     Density[100] := 1.833e-03;
    Altitude[101] := 47.00;     Density[101] := 1.604e-03;
    Altitude[102] := 48.00;     Density[102] := 1.411e-03;
    Altitude[103] := 49.00;     Density[103] := 1.246e-03;
    Altitude[104] := 50.00;     Density[104] := 1.101e-03;
    Altitude[105] := 51.00;     Density[105] := 9.722e-04;
    Altitude[106] := 52.00;     Density[106] := 8.615e-04;
    Altitude[107] := 53.00;     Density[107] := 7.659e-04;
    Altitude[108] := 54.00;     Density[108] := 6.803e-04;
    Altitude[109] := 55.00;     Density[109] := 6.038e-04;
    Altitude[110] := 56.00;     Density[110] := 5.354e-04;
    Altitude[111] := 57.00;     Density[111] := 4.743e-04;
    Altitude[112] := 58.00;     Density[112] := 4.199e-04;
    Altitude[113] := 59.00;     Density[113] := 3.714e-04;
    Altitude[114] := 60.00;     Density[114] := 3.287e-04;
  {= take the natural log of the density data =}
    for i := 0 to NumDataPTS do
      Density[i] := ln(Density[i]);
  END;
END.


