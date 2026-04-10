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
  Len, OriginalLen: Integer;
  H32, V1, V2, V3, V4: Cardinal;
  Data: PByte;
begin
  OriginalLen := Length(Key);
  Len := OriginalLen;

  if Len = 0 then
  begin
    H32 := PRIME32_5;
    H32 := H32 xor (H32 shr 15);
    H32 := H32 * PRIME32_2;
    H32 := H32 xor (H32 shr 13);
    H32 := H32 * PRIME32_3;
    H32 := H32 xor (H32 shr 16);
    Result := H32;
    Exit;
  end;

  Data := @Key[1];

  if Len >= 16 then
  begin
    // 4-lane main loop: processes 16 bytes per iteration across four independent
    // accumulators, allowing the CPU to pipeline all four multiply+rotate chains.
    V1 := PRIME32_1 + PRIME32_2;
    V2 := PRIME32_2;
    V3 := 0;
    V4 := Cardinal(0) - PRIME32_1;

    repeat
      V1 := V1 + PLongWord(Data)^ * PRIME32_2;
      V1 := (V1 shl 13) or (V1 shr 19);
      V1 := V1 * PRIME32_1;
      Inc(Data, 4);

      V2 := V2 + PLongWord(Data)^ * PRIME32_2;
      V2 := (V2 shl 13) or (V2 shr 19);
      V2 := V2 * PRIME32_1;
      Inc(Data, 4);

      V3 := V3 + PLongWord(Data)^ * PRIME32_2;
      V3 := (V3 shl 13) or (V3 shr 19);
      V3 := V3 * PRIME32_1;
      Inc(Data, 4);

      V4 := V4 + PLongWord(Data)^ * PRIME32_2;
      V4 := (V4 shl 13) or (V4 shr 19);
      V4 := V4 * PRIME32_1;
      Inc(Data, 4);

      Dec(Len, 16);
    until Len < 16;

    H32 := ((V1 shl 1)  or (V1 shr 31)) +
           ((V2 shl 7)  or (V2 shr 25)) +
           ((V3 shl 12) or (V3 shr 20)) +
           ((V4 shl 18) or (V4 shr 14));

    H32 := H32 + Cardinal(OriginalLen);

    // Process remaining 4-byte chunks after the main loop
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
  end
  else
  begin
    // Short strings (< 16 bytes): single-lane path.
    // Uses the original pre-4-lane algorithm which has proven good distribution
    // for short sequential keys (e.g. "key00001"…"key01000").
    H32 := PRIME32_5;

    while Len >= 4 do
    begin
      H32 := H32 + PLongWord(Data)^ * PRIME32_3;
      H32 := (H32 shl 17) or (H32 shr 15);
      H32 := H32 * PRIME32_4;
      Inc(Data, 4);
      Dec(Len, 4);
    end;

    while Len > 0 do
    begin
      H32 := H32 + Data^ * PRIME32_5;
      H32 := (H32 shl 11) or (H32 shr 21);
      H32 := H32 * PRIME32_1;
      Inc(Data);
      Dec(Len);
    end;
  end;

  // Finalization avalanche
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