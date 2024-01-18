{ **********************************************************************
  *                           Unit EIGEN.PAS                           *
  *                            Version 2.2d                            *
  *                      (c) J. Debord, July 2004                      *
  **********************************************************************
          Procedures for computing eigenvalues and eigenvectors
  **********************************************************************
  References:
  1) 'Mathematiques et Statistiques' by H. Haut (PSI ed.) : Jacobi
  2) EISPACK (http://www.netlib.org/eispack) : EigenVals, EigenVect
  ********************************************************************** }

unit eigen;

interface

uses
  fmath, matrices, stat;

function Jacobi(A : TMatrix; Lbound, Ubound, MaxIter : Integer;
                Tol : Float; V : TMatrix; Lambda : TVector) : Integer;
{ ----------------------------------------------------------------------
  Eigenvalues and eigenvectors of a symmetric matrix by the iterative
  method of Jacobi
  ----------------------------------------------------------------------
  Input parameters  : A       = matrix
                      Lbound  = index of first matrix element
                      Ubound  = index of last matrix element
                      MaxIter = maximum number of iterations
                      Tol     = required precision
  ----------------------------------------------------------------------
  Output parameters : V      = matrix of eigenvectors (stored by columns)
                      Lambda = eigenvalues in decreasing order
  ----------------------------------------------------------------------
  Possible results  : MAT_OK
                      MAT_NON_CONV
  ----------------------------------------------------------------------
  NB : 1. The eigenvectors are normalized, with their first component > 0
       2. This procedure destroys the original matrix A
  ---------------------------------------------------------------------- }

function EigenVals(A : TMatrix; Lbound, Ubound : Integer;
                   Lambda_Re, Lambda_Im : TVector) : Integer;
{ ----------------------------------------------------------------------
  Eigenvalues of a general square matrix
  ----------------------------------------------------------------------
  Input parameters  : A      = matrix
                      Lbound = index of first matrix element
                      Ubound = index of last matrix element
  ----------------------------------------------------------------------
  Output parameters : Lambda_Re = real part of eigenvalues
                      Lambda_Im = imaginary part of eigenvalues

                      The eigenvalues are unordered, except that complex
                      conjugate pairs appear consecutively with the
                      value having the positive imaginary part first.
  ----------------------------------------------------------------------
  Possible results  :  0 : No error
                      -i : Non-convergence has been encountered during
                           the search for the i-th eigenvalue. The
                           eigenvalues should be correct for indices
                           (i+1)..Ubound.
  ----------------------------------------------------------------------
  NB : This procedure destroys the original matrix A
  ---------------------------------------------------------------------- }

function EigenVect(A : TMatrix; Lbound, Ubound : Integer;
                   Lambda_Re, Lambda_Im : TVector; V : TMatrix) : Integer;
{ ----------------------------------------------------------------------
  Eigenvalues and eigenvectors of a general square matrix
  ----------------------------------------------------------------------
  Input parameters  : A      = matrix
                      Lbound = index of first matrix element
                      Ubound = index of last matrix element
  ----------------------------------------------------------------------
  Output parameters : Lambda_Re = real part of eigenvalues
                      Lambda_Im = imaginary part of eigenvalues
                      V         = matrix of eigenvectors

  If the i-th eigenvalue is real, the i-th column of V contains its
  eigenvector. If the i-th eigenvalue is complex with positive imaginary
  part, the i-th and (i+1)-th columns of V contain the real and imaginary
  parts of its eigenvector. The eigenvectors are unnormalized.
  ----------------------------------------------------------------------
  Possible results  :  0 : No error
                      -i : Non-convergence has been encountered during
                           the search for the i-th eigenvalue. None of
                           the eigenvectors has been found. The
                           eigenvalues should be correct for indices
                           (i+1)..Ubound.
  ----------------------------------------------------------------------
  NB : This procedure destroys the original matrix A
  ---------------------------------------------------------------------- }

procedure DivLargest(V : TVector; Lbound, Ubound : Integer;
                     var Largest : Float);
{ ----------------------------------------------------------------------
  Normalizes an eigenvector V by dividing by the element with the
  largest absolute value
  ---------------------------------------------------------------------- }

function RootPol(Coef : TVector; Deg : Integer;
                 Xr, Xi : TVector) : Integer;
{ ----------------------------------------------------------------------
  Real and complex roots of a real polynomial by the method of the
  companion matrix
  ----------------------------------------------------------------------
  Input parameters  : Coef = coefficients of polynomial
                      Deg  = degree of polynomial
  ----------------------------------------------------------------------
  Output parameters : Xr = real parts of root
                      Xi = imaginary parts of root
  ----------------------------------------------------------------------
  If no error occurred, the function returns the number of real roots.
  The N real roots are returned in Xr[1..N] in increasing order. The
  complex roots are returned in Xr[N+1..Deg], Xi[N+1..Deg] and are
  unordered.

  If an error occurred during the search for the i-th root, the function
  returns (-i). The roots should be correct for indices (i+1)..Deg. The
  roots are unordered.
  ---------------------------------------------------------------------- }

implementation

  function Jacobi(A : TMatrix; Lbound, Ubound, MaxIter : Integer;
                  Tol : Float; V : TMatrix; Lambda : TVector) : Integer;
  var
    I, J, K, Im, Jm, Iter : Integer;
    B, C, C2, Na, Nd, P, Q, S, S2, R, T : Float;
  begin
    Iter := 0;
    Na := 0.0;
    Nd := 0.0;
    R := 0.0;

    for I := Lbound to Ubound do
      begin
        V[I,I] := 1.0;
        Nd := Nd + Sqr(A[I,I]);
        if I <> Ubound then
          for J := Succ(I) to Ubound do
            begin
              R := R + Sqr(A[I,J]);
              V[I,J] := 0.0;
              V[J,I] := 0.0;
            end;
      end;

    Na := Nd + 2.0 * R;

    repeat
      R := 0.0;
      for I := Lbound to Pred(Ubound) do
        for J := Succ(I) to Ubound do
          begin
            T := Abs(A[I,J]);
            if T > R then
              begin
                R := T;
                Im := I;
                Jm := J;
              end;
          end;

      B := A[Im,Im] - A[Jm,Jm];

      if B = 0 then
        begin
          C := SQRT2DIV2;
          S := C * Sgn(A[Im,Jm]);
        end
      else
        begin
          P := 2.0 * A[Im,Jm] * Sgn(B);
          Q := Abs(B);
          R := Pythag(P, Q);
          C := Sqrt(0.5 * (1.0 + Q / R));
          S := 0.5 * P / (R * C);
        end;

      for K := Lbound to Ubound do
        begin
          R := V[K,Im];
          V[K,Im] := C * R + S * V[K,Jm];
          V[K,Jm] := C * V[K,Jm] - S * R;
        end;

      if Im <> Lbound then
        for K := Lbound to Pred(Im) do
          begin
            R := A[K,Im];
            A[K,Im] := C * R + S * A[K,Jm];
            A[K,Jm] := C * A[K,Jm] - S * R;
          end;

      if Jm <> Succ(Im) then
        for K := Succ(Im) to Pred(Jm) do
          begin
            R := A[Im,K];
            A[Im,K] := C * R + S * A[K,Jm];
            A[K,Jm] := C * A[K,Jm] - S * R;
          end;

      if Jm <> Ubound then
        for K := Succ(Jm) to Ubound do
          begin
            R := A[Im,K];
            A[Im,K] := C * R + S * A[Jm,K];
            A[Jm,K] := C * A[Jm,K] - S * R;
          end;

      Nd := Nd + 2.0 * Sqr(A[Im,Jm]);

      C2 := Sqr(C);
      S2 := Sqr(S);
      P := 2.0 * S * C * A[Im,Jm];
      R := A[Im,Im];
      A[Im,Im] := C2 * R + S2 * A[Jm,Jm] + P;
      A[Jm,Jm] := S2 * R + C2 * A[Jm,Jm] - P;
      A[Im,Jm] := 0.0;

      Inc(Iter);
      if Iter > MaxIter then
        begin
          Jacobi := MAT_NON_CONV;
          Exit;
        end;
    until Abs(1.0 - Na / Nd) < Tol;

    { The diagonal terms of the transformed matrix are the eigenvalues }
    for I := Lbound to Ubound do
      Lambda[I] := A[I,I];

    { Sort eigenvalues and eigenvectors }
    for I := Lbound to Pred(Ubound) do
      begin
        K := I;
        R := Lambda[I];
        for J := Succ(I) to Ubound do
          if Lambda[J] > R then
            begin
              K := J;
              R := Lambda[J];
            end;
        Swap(Lambda[I], Lambda[K]);
        SwapCols(I, K, V, Lbound, Ubound);
      end;

    { Make sure that the first component of each eigenvector is > 0 }
    for J := Lbound to Ubound do
      if V[Lbound, J] < 0.0 then
        for I := Lbound to Ubound do
          V[I,J] := - V[I,J];

    Jacobi := MAT_OK;
  end;

  procedure Balance(A : TMatrix; Lbound, Ubound : Integer;
                    var I_low, I_igh : Integer; Scale : TVector);
{ ----------------------------------------------------------------------
  This procedure is a translation of the EISPACK procedure Balanc.

  This procedure balances a real matrix and isolates eigenvalues
  whenever possible.

  On input:

    A contains the input matrix to be balanced.

    Lbound, Ubound are the lowest and highest indices
    of the elements of A.

  On output:

    A contains the balanced matrix.

    I_low and I_igh are two integers such that A[i,j]
    is equal to zero if
      (1) i is greater than j and
      (2) j=Lbound,...,I_low-1 or i=I_igh+1,...,Ubound.

    Scale contains information determining the permutations
    and scaling factors used.

    Suppose that the principal submatrix in rows I_low through I_igh
    has been balanced, that P[j] denotes the index interchanged
    with j during the permutation step, and that the elements
    of the diagonal matrix used are denoted by D[i,j].  then
        Scale[j] = P[j],    for j = Lbound,...,I_low-1
                 = D[j,j],      j = I_low,...,I_igh
                 = P[j]         j = I_igh+1,...,Ubound.
    the order in which the interchanges are made is
    Ubound to I_igh+1, then Lbound to I_low-1.

    Note that Lbound is returned for I_igh if I_igh is < Lbound formally
  ---------------------------------------------------------------------- }
  const
    RADIX = 2;  { Base used in floating number representation }

  var
    I, J, M : Integer;
    C, F, G, R, S, B2 : Float;
    Flag, Found, Conv : Boolean;

    procedure Exchange;
    { Row and column exchange }
    var
      I : Integer;
    begin
      Scale[M] := J;
      if J = M then Exit;

      for I := Lbound to I_igh do
        Swap(A[I,J], A[I,M]);

      for I := I_low to Ubound do
        Swap(A[J,I], A[M,I]);
    end;

  begin
    B2 := RADIX * RADIX;
    I_low := Lbound;
    I_igh := Ubound;

    { Search for rows isolating an eigenvalue and push them down }
    repeat
      J := I_igh;
      repeat
        I := Lbound;
        repeat
          Flag := (I <> J) and (A[J,I] <> 0.0);
          I := I + 1;
        until Flag or (I > I_igh);
        Found := not Flag;
        if Found then
          begin
            M := I_igh;
            Exchange;
            I_igh := I_igh - 1;
          end;
        J := J - 1;
      until Found or (J < Lbound);
    until (not Found) or (I_igh < Lbound);

    if I_igh < Lbound then I_igh := Lbound;
    if I_igh = Lbound then Exit;

    { Search for columns isolating an eigenvalue and push them left }
    repeat
      J := I_low;
      repeat
        I := I_low;
        repeat
          Flag := (I <> J) and (A[I,J] <> 0.0);
          I := I + 1;
        until Flag or (I > I_igh);
        Found := not Flag;
        if Found then
          begin
            M := I_low;
            Exchange;
            I_low := I_low + 1;
          end;
        J := J + 1;
      until Found or (J > I_igh);
    until (not Found);

    { Now balance the submatrix in rows I_low to I_igh }
    for I := I_low to I_igh do
      Scale[I] := 1.0;

    { Iterative loop for norm reduction }
    repeat
      Conv := True;

      for I := I_low to I_igh do
        begin
          C := 0.0;
          R := 0.0;

          for J := I_low to I_igh do
            if J <> I then
              begin
                C := C + Abs(A[J,I]);
                R := R + Abs(A[I,J]);
              end;

          { Guard against zero C or R due to underflow }
          if (C <> 0.0) and (R <> 0.0) then
            begin
              G := R / RADIX;
              F := 1.0;
              S := C + R;

              while C < G do
                begin
                  F := F * RADIX;
                  C := C * B2;
                end;

              G := R * RADIX;

              while C >= G do
                begin
                  F := F / RADIX;
                  C := C / B2;
                end;

              { Now balance }
              if (C + R) / F < 0.95 * S then
                begin
                  G := 1.0 / F;
                  Scale[I] := Scale[I] * F;
                  Conv := False;
                  for J := I_low to Ubound do A[I,J] := A[I,J] * G;
                  for J := Lbound to I_igh do A[J,I] := A[J,I] * F;
                end;
            end;
        end;
    until Conv;
  end;

  procedure ElmHes(A : TMatrix; Lbound, Ubound, I_low, I_igh : Integer;
                   I_int : TIntVector);
{ ----------------------------------------------------------------------
  This procedure is a translation of the EISPACK subroutine Elmhes

  Given a real general matrix, this procedure reduces a submatrix
  situated in rows and columns I_low through I_igh to upper Hessenberg
  form by stabilized elementary similarity transformations.

  On input:

    A contains the input matrix.

    Lbound, Ubound are the lowest and highest indices
    of the elements of A.

    I_low and I_igh are integers determined by the balancing procedure
    Balance. If Balance has not been used, set I_low=Lbound, I_igh=Ubound.

  On output:

    A contains the Hessenberg matrix. The multipliers which were used
    in the reduction are stored in the remaining triangle under the
    Hessenberg matrix.

    I_int contains information on the rows and columns interchanged in
    the reduction. Only elements I_low through I_igh are used.
  ---------------------------------------------------------------------- }
  var
    I, J, M, La, Kp1, Mm1, Mp1 : Integer;
    X, Y : Float;
  begin
    La := I_igh - 1;
    Kp1 := I_low + 1;
    if La < Kp1 then Exit;

    for M := Kp1 to La do
      begin
        Mm1 := M - 1;
        X := 0.0;
        I := M;

        for J := M to I_igh do
          if Abs(A[J,Mm1]) > Abs(X) then
            begin
              X := A[J,Mm1];
              I := J;
            end;

        I_int[M] := I;

        { Interchange rows and columns of A }
        if I <> M then
          begin
            for J := Mm1 to Ubound do
              Swap(A[I,J], A[M,J]);

            for J := Lbound to I_igh do
              Swap(A[J,I], A[J,M]);
          end;

        if X <> 0.0 then
          begin
            Mp1 := M + 1;
            for I := Mp1 to I_igh do
              begin
                Y := A[I,Mm1];
                if Y <> 0.0 then
                  begin
                    Y := Y / X;
                    A[I,Mm1] := Y;
                    for J := M to Ubound do
                      A[I,J] := A[I,J] - Y * A[M,J];
                    for J := Lbound to I_igh do
                      A[J,M] := A[J,M] + Y * A[J,I];
                  end;
              end;
          end;
      end;
  end;

  procedure Eltran(A : TMatrix; Lbound, Ubound, I_low, I_igh : Integer;
                   I_int : TIntVector; Z : TMatrix);
{ ----------------------------------------------------------------------
  This procedure is a translation of the EISPACK subroutine Eltran.

  This procedure accumulates the stabilized elementary similarity
  transformations used in the reduction of a real general matrix
  to upper Hessenberg form by Elmhes.

  On input:

    A contains the multipliers which were used in the reduction
    by Elmhes in its lower triangle below the subdiagonal.

    Lbound, Ubound are the lowest and highest indices
    of the elements of A

    I_low and I_igh are integers determined by the balancing procedure
    Balance. If Balance has not been used, set I_low=Lbound, I_igh=Ubound.

    I_int contains information on the rows and columns interchanged in
    the reduction by Elmhes. Only elements I_low through I_igh are used.

  On output:

    Z contains the transformation matrix produced in the reduction by
    Elmhes.
  ---------------------------------------------------------------------- }
  var
    I, J, Mp, Mp1 : Integer;
  begin
    { Initialize Z to identity matrix }
    for I := Lbound to Ubound do
      for J := Lbound to Ubound do
        if I = J then Z[I,J] := 1.0 else Z[I,J] := 0.0;

    if I_igh < I_low then Exit;

    for Mp := I_igh - 1 downto I_low + 1 do
      begin
        Mp1 := Mp + 1;
        for I := Mp1 to I_igh do
          Z[I,Mp] := A[I,Mp - 1];
        I := I_int[Mp];
        if I <> Mp then
          begin
            for J := Mp to I_igh do
              begin
                Z[Mp,J] := Z[I,J];
                Z[I,J] := 0.0;
              end;
            Z[I,Mp] := 1.0;
          end;
      end;
  end;

  function Hqr(H : TMatrix; Lbound, Ubound, I_low, I_igh : Integer;
               Wr, Wi : TVector) : Integer;
{ ----------------------------------------------------------------------
  This function is a translation of the EISPACK subroutine hqr.

  This function finds the eigenvalues of a real upper Hessenberg
  matrix by the QR method.

  On input:

    H contains the upper Hessenberg matrix.

    Lbound, Ubound are the lowest and highest indices
    of the elements of H

    I_low and I_igh are integers determined by the balancing subroutine
    Balance. If Balance has not been used, set I_low=Lbound, I_igh=Ubound

  On output:

    H has been destroyed.

    Wr and Wi contain the real and imaginary parts, respectively, of
    the eigenvalues. The eigenvalues are unordered except that complex
    conjugate pairs of values appear consecutively with the eigenvalue
    having the positive imaginary part first.

    The function returns an error code:
       zero       for normal return,
       -j         if the limit of 30*N iterations is exhausted
                  while the j-th eigenvalue is being sought.
                  (N being the size of the matrix). The eigenvalues
                  should be correct for indices j+1,...,Ubound.
  ----------------------------------------------------------------------
  Note: This is a crude translation. Many of the original goto's have
  been kept !
  ---------------------------------------------------------------------- }
  var
    I, J, K, L, M, N, En, Na, Itn, Its, Mp2, Enm2 : Integer;
    P, Q, R, S, T, W, X, Y, Zz, Norm, Tst1, Tst2 : Float;
    NotLas : Boolean;

  label
    60, 70, 100, 130, 150, 170, 225, 260, 270, 280, 320, 330;

  begin
    { Store roots isolated by Balance and compute matrix norm }
    K := Lbound;
    Norm := 0.0;
    for I := Lbound to Ubound do
      begin
        for J := K to Ubound do
          Norm := Norm + Abs(H[I,J]);
        K := I;
        if (I < I_low) or (I > I_igh) then
          begin
            Wr[I] := H[I,I];
            Wi[I] := 0.0;
          end;
      end;

    N := Ubound - Lbound + 1;
    Itn := 30 * N;
    En := I_igh;
    T := 0.0;

60: { Search for next eigenvalues }
    if En < I_low then
      begin
        Hqr := 0;
        Exit;
      end;

    Its := 0;
    Na := En - 1;
    Enm2 := Na - 1;

70: { Look for single small sub-diagonal element }
    for L := En downto I_low do
      begin
        if L = I_low then goto 100;
        S := Abs(H[L - 1,L - 1]) + Abs(H[L,L]);
        if S = 0.0 then S := Norm;
        Tst1 := S;
        Tst2 := Tst1 + Abs(H[L,L - 1]);
        if Tst2 = Tst1 then goto 100;
      end;

100: { Form shift }
    X := H[En,En];
    if L = En then goto 270;
    Y := H[Na,Na];
    W := H[En,Na] * H[Na,En];
    if L = Na then goto 280;

    if Itn = 0 then
      begin
        { Set error -- all eigenvalues have not
          converged after 30*N iterations }
        Hqr := - En;
        Exit;
      end;

    if (Its <> 10) and (Its <> 20) then goto 130;

    { Form exceptional shift }
    T := T + X;

    for I := I_low to En do
      H[I,I] := H[I,I] - X;

    S := Abs(H[En,Na]) + Abs(H[Na,Enm2]);
    X := 0.75 * S;
    Y := X;
    W := - 0.4375 * S * S;

130:
    Its := Its + 1;
    Itn := Itn - 1;

    { Look for two consecutive small sub-diagonal elements }
    for M := Enm2 downto L do
      begin
        Zz := H[M,M];
        R := X - Zz;
        S := Y - Zz;
        P := (R * S - W) / H[M + 1,M] + H[M,M + 1];
        Q := H[M + 1,M + 1] - Zz - R - S;
        R := H[M + 2,M + 1];
        S := Abs(P) + Abs(Q) + Abs(R);
        P := P / S;
        Q := Q / S;
        R := R / S;
        if M = L then goto 150;
        Tst1 := Abs(P) * (Abs(H[M - 1,M - 1]) + Abs(Zz) + Abs(H[M + 1,M + 1]));
        Tst2 := Tst1 + Abs(H[M,M - 1]) * (Abs(Q) + Abs(R));
        if Tst2 = Tst1 then goto 150;
      end;

150:
    Mp2 := M + 2;

    for I := Mp2 to En do
      begin
        H[I,I - 2] := 0.0;
        if I <> Mp2 then H[I,I - 3] := 0.0;
      end;

    { Double QR step involving rows L to En and columns M to En }
    for K := M to Na do
      begin
        NotLas := (K <> Na);
        if (K = M) then goto 170;
        P := H[K,K - 1];
        Q := H[K + 1,K - 1];
        R := 0.0;
        if NotLas then R := H[K + 2,K - 1];
        X := Abs(P) + Abs(Q) + Abs(R);
        if X = 0.0 then goto 260;
        P := P / X;
        Q := Q / X;
        R := R / X;
170:    S := DSgn(Sqrt(P * P + Q * Q + R * R), P);
        if K <> M then
          H[K,K - 1] := - S * X
        else if L <> M then
          H[K,K - 1] := - H[K,K - 1];
        P := P + S;
        X := P / S;
        Y := Q / S;
        Zz := R / S;
        Q := Q / P;
        R := R / P;
        if NotLas then goto 225;

        { Row modification }
        for J := K to En do
          begin
            P := H[K,J] + Q * H[K + 1,J];
            H[K,J] := H[K,J] - P * X;
            H[K + 1,J] := H[K + 1,J] - P * Y;
          end;

        J := Min(En, K + 3);

        { Column modification }
        for I := L to J do
          begin
            P := X * H[I,K] + Y * H[I,K + 1];
            H[I,K] := H[I,K] - P;
            H[I,K + 1] := H[I,K + 1] - P * Q;
          end;
        goto 260;

225:
        { Row modification }
        for J := K to En do
          begin
            P := H[K,J] + Q * H[K + 1,J] + R * H[K + 2,J];
            H[K,J] := H[K,J] - P * X;
            H[K + 1,J] := H[K + 1,J] - P * Y;
            H[K + 2,J] := H[K + 2,J] - P * Zz;
          end;

        J := Min(En, K + 3);

        { Column modification }
        for I := L to J do
          begin
            P := X * H[I,K] + Y * H[I,K + 1] + Zz * H[I,K + 2];
            H[I,K] := H[I,K] - P;
            H[I,K + 1] := H[I,K + 1] - P * Q;
            H[I,K + 2] := H[I,K + 2] - P * R;
          end;

260:  end;

    goto 70;

270: { One root found }
    Wr[En] := X + T;
    Wi[En] := 0.0;
    En := Na;
    goto 60;

280: { Two roots found }
    P := 0.5 * (Y - X);
    Q := P * P + W;
    Zz := Sqrt(Abs(Q));
    X := X + T;
    if Q < 0.0 then goto 320;

    { Real pair }
    Zz := P + DSgn(Zz, P);
    Wr[Na] := X + Zz;
    Wr[En] := Wr[Na];
    if Zz <> 0.0 then Wr[En] := X - W / Zz;
    Wi[Na] := 0.0;
    Wi[En] := 0.0;
    goto 330;

320: { Complex pair }
    Wr[Na] := X + P;
    Wr[En] := X + P;
    Wi[Na] := Zz;
    Wi[En] := - Zz;

330:
    En := Enm2;
    goto 60;
  end;

  function Hqr2(H : TMatrix; Lbound, Ubound, I_low, I_igh : Integer;
                Wr, Wi : TVector; Z : TMatrix) : Integer;
{ ----------------------------------------------------------------------
  This function is a translation of the EISPACK subroutine hqr2

  This procedure finds the eigenvalues and eigenvectors of a real
  upper Hessenberg matrix by the QR method.

  On input:

    H contains the upper Hessenberg matrix.

    Lbound, Ubound are the lowest and highest indices
    of the elements of H

    I_low and I_igh are integers determined by the balancing subroutine
    Balance. If Balance has not been used, set I_low=Lbound, I_igh=Ubound

    Z contains the transformation matrix produced by Eltran after the
    reduction by Elmhes, or by Ortran after the reduction by Orthes, if
    performed. If the eigenvectors of the Hessenberg matrix are desired,
    Z must contain the identity matrix.

  On output:

    H has been destroyed.

    Wr and Wi contain the real and imaginary parts, respectively, of
    the eigenvalues. The eigenvalues are unordered except that complex
    conjugate pairs of values appear consecutively with the eigenvalue
    having the positive imaginary part first.

    Z contains the real and imaginary parts of the eigenvectors. If the
    i-th eigenvalue is real, the i-th column of Z contains its eigenvector.
    If the i-th eigenvalue is complex with positive imaginary part, the i-th
    and (i+1)-th columns of Z contain the real and imaginary parts of its
    eigenvector. The eigenvectors are unnormalized. If an error exit is made,
    none of the eigenvectors has been found.

    The function returns an error code:
       zero       for normal return,
       -j         if the limit of 30*N iterations is exhausted
                  while the j-th eigenvalue is being sought
                  (N being the size of the matrix). The eigenvalues
                  should be correct for indices j+1,...,Ubound.
  ----------------------------------------------------------------------
  Note: This is a crude translation. Many of the original goto's have
  been kept !
  ---------------------------------------------------------------------- }

    procedure Cdiv(Ar, Ai, Br, Bi : Float; var Cr, Ci : Float);
    { Complex division, (Cr,Ci) = (Ar,Ai)/(Br,Bi) }
    var
      S, Ars, Ais, Brs, Bis : Float;
    begin
      S := Abs(Br) + Abs(Bi);
      Ars := Ar / S;
      Ais := Ai / S;
      Brs := Br / S;
      Bis := Bi / S;
      S := Sqr(Brs) + Sqr(Bis);
      Cr := (Ars * Brs + Ais * Bis) / S;
      Ci := (Ais * Brs - Ars * Bis) / S;
    end;

  var
    I, J, K, L, M, N, En, Na, Itn, Its, Mp2, Enm2 : Integer;
    P, Q, R, S, T, W, X, Y, Ra, Sa, Vi, Vr, Zz, Norm, Tst1, Tst2 : Float;
    NotLas : Boolean;

  label
    60, 70, 100, 130, 150, 170, 225, 260, 270, 280, 320, 330, 340,
    600, 630, 635, 640, 680, 700, 710, 770, 780, 790, 795, 800;

  begin
    { Store roots isolated by Balance and compute matrix norm }
    K := Lbound;
    Norm := 0.0;
    for I := Lbound to Ubound do
      begin
        for J := K to Ubound do
          Norm := Norm + Abs(H[I,J]);
        K := I;
        if (I < I_low) or (I > I_igh) then
          begin
            Wr[I] := H[I,I];
            Wi[I] := 0.0;
          end;
      end;

    N := Ubound - Lbound + 1;
    Itn := 30 * N;
    En := I_igh;
    T := 0.0;

60: { Search for next eigenvalues }
    if En < I_low then goto 340;
    Its := 0;
    Na := En - 1;
    Enm2 := Na - 1;

70: { Look for single small sub-diagonal element }
    for L := En downto I_low do
      begin
        if L = I_low then goto 100;
        S := Abs(H[L - 1,L - 1]) + Abs(H[L,L]);
        if S = 0.0 then S := Norm;
        Tst1 := S;
        Tst2 := Tst1 + Abs(H[L,L - 1]);
        if Tst2 = Tst1 then goto 100;
      end;

100: { Form shift }
    X := H[En,En];
    if L = En then goto 270;
    Y := H[Na,Na];
    W := H[En,Na] * H[Na,En];
    if L = Na then goto 280;

    if Itn = 0 then
      begin
        { Set error -- all eigenvalues have not
          converged after 30*N iterations }
        Hqr2 := - En;
        Exit;
      end;

    if (Its <> 10) and (Its <> 20) then goto 130;

    { Form exceptional shift }
    T := T + X;

    for I := I_low to En do
      H[I,I] := H[I,I] - X;

    S := Abs(H[En,Na]) + Abs(H[Na,Enm2]);
    X := 0.75 * S;
    Y := X;
    W := - 0.4375 * S * S;

130:
    Its := Its + 1;
    Itn := Itn - 1;

    { Look for two consecutive small sub-diagonal elements }
    for M := Enm2 downto L do
      begin
        Zz := H[M,M];
        R := X - Zz;
        S := Y - Zz;
        P := (R * S - W) / H[M + 1,M] + H[M,M + 1];
        Q := H[M + 1,M + 1] - Zz - R - S;
        R := H[M + 2,M + 1];
        S := Abs(P) + Abs(Q) + Abs(R);
        P := P / S;
        Q := Q / S;
        R := R / S;
        if M = L then goto 150;
        Tst1 := Abs(P) * (Abs(H[M - 1,M - 1]) + Abs(Zz) + Abs(H[M + 1,M + 1]));
        Tst2 := Tst1 + Abs(H[M,M - 1]) * (Abs(Q) + Abs(R));
        if Tst2 = Tst1 then goto 150;
      end;

150:
    Mp2 := M + 2;

    for I := Mp2 to En do
      begin
        H[I,I - 2] := 0.0;
        if I <> Mp2 then H[I,I - 3] := 0.0;
      end;

    { Double QR step involving rows L to En and columns M to En }
    for K := M to Na do
      begin
        NotLas := (K <> Na);
        if (K = M) then goto 170;
        P := H[K,K - 1];
        Q := H[K + 1,K - 1];
        R := 0.0;
        if NotLas then R := H[K + 2,K - 1];
        X := Abs(P) + Abs(Q) + Abs(R);
        if X = 0.0 then goto 260;
        P := P / X;
        Q := Q / X;
        R := R / X;
170:    S := DSgn(Sqrt(P * P + Q * Q + R * R), P);
        if K <> M then
          H[K,K - 1] := - S * X
        else if L <> M then
          H[K,K - 1] := - H[K,K - 1];
        P := P + S;
        X := P / S;
        Y := Q / S;
        Zz := R / S;
        Q := Q / P;
        R := R / P;
        if NotLas then goto 225;

        { Row modification }
        for J := K to Ubound do
          begin
            P := H[K,J] + Q * H[K + 1,J];
            H[K,J] := H[K,J] - P * X;
            H[K + 1,J] := H[K + 1,J] - P * Y;
          end;

        J := Min(En, K + 3);

        { Column modification }
        for I := Lbound to J do
          begin
            P := X * H[I,K] + Y * H[I,K + 1];
            H[I,K] := H[I,K] - P;
            H[I,K + 1] := H[I,K + 1] - P * Q;
          end;

        { Accumulate transformations }
        for I := I_low to I_igh do
          begin
            P := X * Z[I,K] + Y * Z[I,K + 1];
            Z[I,K] := Z[I,K] - P;
            Z[I,K + 1] := Z[I,K + 1] - P * Q;
          end;
        goto 260;

225:
        { Row modification }
        for J := K to Ubound do
          begin
            P := H[K,J] + Q * H[K + 1,J] + R * H[K + 2,J];
            H[K,J] := H[K,J] - P * X;
            H[K + 1,J] := H[K + 1,J] - P * Y;
            H[K + 2,J] := H[K + 2,J] - P * Zz;
          end;

        J := Min(En, K + 3);

        { Column modification }
        for I := Lbound to J do
          begin
            P := X * H[I,K] + Y * H[I,K + 1] + Zz * H[I,K + 2];
            H[I,K] := H[I,K] - P;
            H[I,K + 1] := H[I,K + 1] - P * Q;
            H[I,K + 2] := H[I,K + 2] - P * R;
          end;

        { Accumulate transformations }
        for I := I_low to I_igh do
          begin
            P := X * Z[I,K] + Y * Z[I,K + 1] + Zz * Z[I,K + 2];
            Z[I,K] := Z[I,K] - P;
            Z[I,K + 1] := Z[I,K + 1] - P * Q;
            Z[I,K + 2] := Z[I,K + 2] - P * R;
          end;

260:  end;

    goto 70;

270: { One root found }
    H[En,En] := X + T;
    Wr[En] := H[En,En];
    Wi[En] := 0.0;
    En := Na;
    goto 60;

280: { Two roots found }
    P := 0.5 * (Y - X);
    Q := P * P + W;
    Zz := Sqrt(Abs(Q));
    H[En,En] := X + T;
    X := H[En,En];
    H[Na,Na] := Y + T;
    if Q < 0.0 then goto 320;

    { Real pair }
    Zz := P + DSgn(Zz, P);
    Wr[Na] := X + Zz;
    Wr[En] := Wr[Na];
    if Zz <> 0.0 then Wr[En] := X - W / Zz;
    Wi[Na] := 0.0;
    Wi[En] := 0.0;
    X := H[En,Na];
    S := Abs(X) + Abs(Zz);
    P := X / S;
    Q := Zz / S;
    R := Sqrt(P * P + Q * Q);
    P := P / R;
    Q := Q / R;

    { Row modification }
    for J := Na to Ubound do
      begin
        Zz := H[Na,J];
        H[Na,J] := Q * Zz + P * H[En,J];
        H[En,J] := Q * H[En,J] - P * Zz;
      end;

    { Column modification }
    for I := Lbound to En do
      begin
        Zz := H[I,Na];
        H[I,Na] := Q * Zz + P * H[I,En];
        H[I,En] := Q * H[I,En] - P * Zz;
      end;

    { Accumulate transformations }
    for I := I_low to I_igh do
      begin
        Zz := Z[I,Na];
        Z[I,Na] := Q * Zz + P * Z[I,En];
        Z[I,En] := Q * Z[I,En] - P * Zz;
      end;

    goto 330;

320: { Complex pair }
    Wr[Na] := X + P;
    Wr[En] := Wr[Na];
    Wi[Na] := Zz;
    Wi[En] := - Zz;

330:
    En := Enm2;
    goto 60;

340:
    if Norm = 0.0 then Exit;

    { All roots found. Backsubstitute to find
      vectors of upper triangular form }
    for En := Ubound downto Lbound do
      begin
        P := Wr[En];
        Q := Wi[En];
        Na := En - 1;
        if Q < 0.0 then
          goto 710
        else if Q = 0.0 then
          goto 600
        else
          goto 800;

600:    { Real vector }
        M := En;
        H[En,En] := 1.0;
        if Na < Lbound then goto 800;

        for I := Na downto Lbound do
          begin
            W := H[I,I] - P;
            R := 0.0;

            for J := M to En do
              R := R + H[I,J] * H[J,En];

            if Wi[I] >= 0.0 then goto 630;
            Zz := W;
            S := R;
            goto 700;
630:        M := I;
            if Wi[I] <> 0.0 then goto 640;
            T := W;
            if T <> 0.0 then goto 635;
            Tst1 := Norm;
            T := Tst1;
            repeat
              T := 0.01 * T;
              Tst2 := Norm + T;
            until Tst2 <= Tst1;
635:        H[I,En] := - R / T;
            goto 680;

640:        { Solve real equations }
            X := H[I,I + 1];
            Y := H[I + 1,I];
            Q := (Wr[I] - P) * (Wr[I] - P) + Wi[I] * Wi[I];
            T := (X * S - Zz * R) / Q;
            H[I,En] := T;
            if Abs(X) > Abs(Zz) then
              H[I + 1,En] := (- R - W * T) / X
            else
              H[I + 1,En] := (- S - Y * T) / Zz;

680:        { Overflow control }
            T := Abs(H[I,En]);
            if T = 0.0 then goto 700;
            Tst1 := T;
            Tst2 := Tst1 + 1.0 / Tst1;
            if Tst2 > Tst1 then goto 700;
            for J := I to En do
              H[J,En] := H[J,En] / T;
700:      end;
        { End real vector }
        goto 800;

        { Complex vector }
710:    M := Na;

        { Last vector component chosen imaginary so that
          eigenvector matrix is triangular }
        if Abs(H[En,Na]) > Abs(H[Na,En]) then
          begin
            H[Na,Na] := Q / H[En,Na];
            H[Na,En] := - (H[En,En] - P) / H[En,Na];
          end
        else
          Cdiv(0.0, - H[Na,En], H[Na,Na] - P, Q, H[Na,Na], H[Na,En]);

        H[En,Na] := 0.0;
        H[En,En] := 1.0;
        Enm2 := Na - 1;
        if Enm2 < Lbound then goto 800;

        for I := Enm2 downto Lbound do
          begin
            W := H[I,I] - P;
            Ra := 0.0;
            Sa := 0.0;

            for J := M to En do
              begin
                Ra := Ra + H[I,J] * H[J,Na];
                Sa := Sa + H[I,J] * H[J,En];
              end;

            if Wi[I] >= 0.0 then goto 770;
            Zz := W;
            R := Ra;
            S := Sa;
            goto 795;
770:        M := I;
            if Wi[I] <> 0.0 then goto 780;
            Cdiv(- Ra, - Sa, W, Q, H[I,Na], H[I,En]);
            goto 790;

            { Solve complex equations }
780:        X := H[I,I + 1];
            Y := H[I + 1,I];
            Vr := (Wr[I] - P) * (Wr[I] - P) + Wi[I] * Wi[I] - Q * Q;
            Vi := (Wr[I] - P) * 2.0 * Q;
            if (Vr = 0.0) and (Vi = 0.0) then
              begin
                Tst1 := Norm * (Abs(W) + Abs(Q) + Abs(X) + Abs(Y) + Abs(Zz));
                Vr := Tst1;
                repeat
                  Vr := 0.01 * Vr;
                  Tst2 := Tst1 + Vr;
                until Tst2 <= Tst1;
              end;
            Cdiv(X * R - Zz * Ra + Q * Sa, X * S - Zz * Sa - Q * Ra, Vr, Vi, H[I,Na], H[I,En]);
            if Abs(X) > Abs(Zz) + Abs(Q) then
              begin
                H[I + 1,Na] := (- Ra - W * H[I,Na] + Q * H[I,En]) / X;
                H[I + 1,En] := (- Sa - W * H[I,En] - Q * H[I,Na]) / X;
              end
            else
              Cdiv(- R - Y * H[I,Na], - S - Y * H[I,En], Zz, Q, H[I + 1,Na], H[I + 1,En]);

790:        { Overflow control }
            T := Max(Abs(H[I,Na]), Abs(H[I,En]));
            if T = 0.0 then goto 795;
            Tst1 := T;
            Tst2 := Tst1 + 1.0 / Tst1;
            if Tst2 > Tst1 then goto 795;
            for J := I to En do
              begin
                H[J,Na] := H[J,Na] / T;
                H[J,En] := H[J,En] / T;
              end;

795:      end;
      { End complex vector }
800:  end;

    { End back substitution.
      Vectors of isolated roots }
    for I := Lbound to Ubound do
      if (I < I_low) or (I > I_igh) then
        for J := I to Ubound do
          Z[I,J] := H[I,J];

    { Multiply by transformation matrix to give
      vectors of original full matrix. }
    for J := Ubound downto I_low do
      begin
        M := Min(J, I_igh);
        for I := I_low to I_igh do
          begin
            Zz := 0.0;
            for K := I_low to M do
              Zz := Zz + Z[I,K] * H[K,J];
            Z[I,J] := Zz;
          end;
      end;

    Hqr2 := 0;
  end;

  procedure BalBak(Z : TMatrix; Lbound, Ubound, I_low, I_igh : Integer;
                   Scale : TVector; M : Integer);
{ ----------------------------------------------------------------------
  This procedure is a translation of the EISPACK subroutine Balbak

  This procedure forms the eigenvectors of a real general matrix
  by back transforming those of the corresponding balanced matrix
  determined by Balance.

  On input:

    Z contains the real and imaginary parts of the eigenvectors
    to be back transformed.

    Lbound, Ubound are the lowest and highest indices
    of the elements of Z

    I_low and I_igh are integers determined by Balance.

    Scale contains information determining the permutations
    and scaling factors used by Balance.

    M is the index of the latest column of Z to be back transformed.

  On output:

    Z contains the real and imaginary parts of the transformed
    eigenvectors in its columns Lbound..M
  ---------------------------------------------------------------------- }
  var
    I, J, K : Integer;
    S : Float;
  begin
    if M < Lbound then Exit;

    if I_igh <> I_low then
      for I := I_low to I_igh do
        begin
          S := Scale[I];
          { Left hand eigenvectors are back transformed if the
            foregoing statement is replaced by S := 1.0 / Scale[I] }
          for J := Lbound to M do
            Z[I,J] := Z[I,J] * S;
        end;

    for I := (I_low - 1) downto Lbound do
      begin
        K := Round(Scale[I]);
        if K <> I then
          for J := Lbound to M do
            Swap(Z[I,J], Z[K,J]);
      end;

    for I := (I_igh + 1) to Ubound do
      begin
        K := Round(Scale[I]);
        if K <> I then
          for J := Lbound to M do
            Swap(Z[I,J], Z[K,J]);
      end;
  end;

  function EigenVals(A : TMatrix; Lbound, Ubound : Integer;
                     Lambda_Re, Lambda_Im : TVector) : Integer;
  var
    I_low, I_igh : Integer;
    Scale : TVector;
    I_int : TIntVector;
  begin
    DimVector(Scale, Ubound);
    DimVector(I_Int, Ubound);

    Balance(A, Lbound, Ubound, I_low, I_igh, Scale);
    ElmHes(A, Lbound, Ubound, I_low, I_igh, I_int);
    EigenVals := Hqr(A, Lbound, Ubound, I_low, I_igh, Lambda_Re, Lambda_Im);
  end;

  function EigenVect(A : TMatrix; Lbound, Ubound : Integer;
                     Lambda_Re, Lambda_Im : TVector; V : TMatrix) : Integer;
  var
    I_low, I_igh, ErrCode : Integer;
    Scale : TVector;
    I_Int : TIntVector;
  begin
    DimVector(Scale, Ubound);
    DimVector(I_Int, Ubound);

    Balance(A, Lbound, Ubound, I_low, I_igh, Scale);
    ElmHes(A, Lbound, Ubound, I_low, I_igh, I_int);
    Eltran(A, Lbound, Ubound, I_low, I_igh, I_int, V);
    ErrCode := Hqr2(A, Lbound, Ubound, I_low, I_igh, Lambda_Re, Lambda_Im, V);
    if ErrCode = 0 then BalBak(V, Lbound, Ubound, I_low, I_igh, Scale, Ubound);

    EigenVect := ErrCode;
  end;

  procedure DivLargest(V : TVector; Lbound, Ubound : Integer;
                       var Largest : Float);
  var
    I : Integer;
  begin
    Largest := V[Lbound];
    for I := Succ(Lbound) to Ubound do
      if Abs(V[I]) > Abs(Largest) then
        Largest := V[I];
    for I := Lbound to Ubound do
      V[I] := V[I] / Largest;
  end;

  function RootPol(Coef : TVector; Deg : Integer;
                   Xr, Xi : TVector) : Integer;
  var
    A            : TMatrix;  { Companion matrix }
    N            : Integer;  { Size of matrix }
    I_low, I_igh : Integer;  { Used by Balance }
    Scale        : TVector;  { Used by Balance }
    Nr           : Integer;  { Number of real roots }
    I, J         : Integer;  { Loop variables }
    ErrCode      : Integer;  { Error code }
  begin
    N := Pred(Deg);
    DimMatrix(A, N, N);
    DimVector(Scale, N);

    { Set up the companion matrix (to save space, begin at index 0) }
    for J := 0 to N do
      A[0,J] := - Coef[Deg - J - 1] / Coef[Deg];
    for J := 0 to Pred(N) do
      A[J + 1,J] := 1.0;

    { The roots of the polynomial are the eigenvalues of the companion matrix }
    Balance(A, 0, N, I_low, I_igh, Scale);
    ErrCode := Hqr(A, 0, N, I_low, I_igh, Xr, Xi);

    if ErrCode <> 0 then
      begin
        RootPol := ErrCode;
        Exit;
      end;

    { Count real roots }
    Nr := 0;
    for I := 0 to N do
      if Xi[I] = 0.0 then
        Inc(Nr);

    { Transfer roots from 0..(Deg - 1) to 1..Deg }
    for I := N downto 0 do
      begin
        J := I + 1;
        Xr[J] := Xr[I];
        Xi[J] := Xi[I];
      end;

    RootPol := Nr;
  end;

end.
