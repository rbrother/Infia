unit imslmath;

interface

type
  THugeArray = array[0..10000000] of double; // Never allocate this directly
  PHugeArray = ^THugeArray;

// Kokeile, että ottavat varrien sijasta pointtereita (PhugeArray)

// Returns error code: 0 = no error
function DiagSymMatrix(var MatrixSize : longint;
  M, EigVals, EigVecs : PHugeArray) : longint; stdcall;
  external 'imslmath.dll' name '_DIAGONALIZE_SYM_MATRIX@16';

// Returns error code: 0 = no error
function InvMatrix(var MatrixSize : longint;
  M, Inv : PHugeArray) : longint; stdcall;
  external 'imslmath.dll' name '_INVERT_MATRIX@12';

implementation

end.
