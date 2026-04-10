unit HashFunctions;

{$mode objfpc}{$H+}{$J-}

{
  XXHash32
  - Best for strings > 64 bytes
  - Uses SIMD-friendly operations
  - Good avalanche effect
  
  FNV1aHash
  - Good for short strings
  - Simple and fast
  - Good distribution
  
  MultiplicativeHash
  - Optimal for integers
  - Uses golden ratio for better distribution
  - Very fast single multiplication
  
  DefaultHash
  - Fallback for other types
  - Uses FNV-1a algorithm on raw bytes
  - Reasonable performance for unknown types
}

interface

// Hash function declarations
function XXHash32(const Key: string): Cardinal;
function FNV1aHash(const Key: string): Cardinal;
function MultiplicativeHash(Key: Cardinal): Cardinal;
function DefaultHash(const Key): Cardinal;

implementation

// All hash functions use intentional modular (wrap-around) 32-bit arithmetic.
// Disable range checking for this entire unit so FPC does not raise ERangeError
// on overflow, which is expected and correct behaviour for hash mixing.
{$PUSH}
{$R-}

const
  // XXHash constants
  PRIME32_1: Cardinal = 2654435761;
  PRIME32_2: Cardinal = 2246822519;
  PRIME32_3: Cardinal = 3266489917;
  PRIME32_4: Cardinal = 668265263;
  PRIME32_5: Cardinal = 374761393;

  // FNV constants
  FNV_PRIME:         Cardinal = 16777619;
  FNV_OFFSET_BASIS:  Cardinal = 2166136261;

function XXHash32(const Key: string): Cardinal;
var
  Len: Integer;
  H32: Cardinal;
  Data: PByte;
begin
  Len := Length(Key);
  H32 := PRIME32_5;
  if Len = 0 then
  begin
    // Finalization mix for empty string
    H32 := H32 xor (H32 shr 15);
    H32 := H32 * PRIME32_2;
    H32 := H32 xor (H32 shr 13);
    H32 := H32 * PRIME32_3;
    H32 := H32 xor (H32 shr 16);
    Result := H32;
    Exit;
  end;
  Data := @Key[1];

  // Process 4 bytes at a time
  while Len >= 4 do
  begin
    H32 := H32 + PLongWord(Data)^ * PRIME32_3;
    H32 := (H32 shl 17) or (H32 shr 15);
    H32 := H32 * PRIME32_4;
    Inc(Data, 4);
    Dec(Len, 4);
  end;

  // Process remaining bytes
  while Len > 0 do
  begin
    H32 := H32 + Data^ * PRIME32_5;
    H32 := (H32 shl 11) or (H32 shr 21);
    H32 := H32 * PRIME32_1;
    Inc(Data);
    Dec(Len);
  end;

  // Finalization
  H32 := H32 xor (H32 shr 15);
  H32 := H32 * PRIME32_2;
  H32 := H32 xor (H32 shr 13);
  H32 := H32 * PRIME32_3;
  H32 := H32 xor (H32 shr 16);

  Result := H32;
end;

function FNV1aHash(const Key: string): Cardinal;
var
  I: Integer;
begin
  Result := FNV_OFFSET_BASIS;
  for I := 1 to Length(Key) do
  begin
    Result := Result xor Ord(Key[I]);
    Result := Result * FNV_PRIME;
  end;
end;

function MultiplicativeHash(Key: Cardinal): Cardinal;
const
  GOLDEN_RATIO: Cardinal = 2654435769;  // 2^32 * (sqrt(5)-1)/2
begin
  Result := Key * GOLDEN_RATIO;
end;

function DefaultHash(const Key): Cardinal;
var
  Data: PByte;
  Size: Integer;
  I: Integer;
begin
  Result := FNV_OFFSET_BASIS;
  Data := @Key;
  Size := SizeOf(Key);

  for I := 0 to Size - 1 do
  begin
    Result := Result xor Data[I];
    Result := Result * FNV_PRIME;
  end;
end;

{$POP}

end.