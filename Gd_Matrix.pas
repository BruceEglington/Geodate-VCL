unit Gd_Matrix;

interface

type
  RealArrayC = array[1..3,1..3] of double;
  RealVector = array[1..3] of double;

procedure Transp (     A          : RealArrayC;
                   var B          : RealArrayC;
                       N, M       : integer);
procedure MInv   (     A          : RealArrayC;
                   var B          : RealArrayC;
                      N, M        : integer);
procedure MmultVR (     A         : RealArrayC;
                        B         : RealVector;
                    var C         : RealVector;
                        L, N, M   : integer);


implementation

procedure Transp (     A          : RealArrayC;
                   var B          : RealArrayC;
                       N, M       : integer);
var
  I, J   :  integer;
begin
  for I:=1 to N do
  begin
    for J:=1 to M do
    begin
      B[J,I]:=A[I,J];
    end;
  end;
end;


procedure MInv   (    A   : RealArrayC;
                  var B   : RealArrayC;
                   N, M : integer);
var
  i, j, k    : integer;
  Ratio,
  D, Det     : double;
begin
  for i:=1 to N do
  begin
    for j:=1 to N do
    begin
      B[i,j]:=0.0;
    end;
    B[i,i]:=1.0;
  end;
  Det:=1.0;
  for i:=1 to N do
  begin
    D:=A[i,i];
    Det:=Det*D;
    for j:=1 to N do
    begin
      A[i,j]:=A[i,j]/D;
      B[i,j]:=B[i,j]/D;
    end;
    for j:=1 to N do
    begin
      if ((i-j) <> 0) then
      begin
        Ratio:=A[j,i];
        for k:=1 to N do
        begin
          A[j,k]:=A[j,k]-Ratio*A[i,k];
          B[j,k]:=B[j,k]-Ratio*B[i,k];
        end;
      end;
    end;
  end;
end;


procedure MmultVR (     A          : RealArrayC;
                        B          : RealVector;
                    var C          : RealVector;
                        L, N, M    : integer);
var
  I, K   :  integer;
begin
  {A = LxN
   B = Nx1
   C = Lx1}
  for I:=1 to L do
  begin
    C[I]:=0.0;
    for K:=1 to N do
    begin
      C[I]:=C[I]+A[I,K]*B[K];
    end;
  end;
end;

end.
