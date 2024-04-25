unit Olf.RTL.CryptDecrypt;

interface

uses
  System.Classes,
  System.Types;

type
  /// <summary>
  /// Simple class to crypt and decrypt buffers.
  /// </summary>
  /// <remarks>
  /// This unit contains very simple cryptographic algorithms.
  /// DON't USE THEM FOR SENSIBLE DATAS !!!
  ///
  /// Check those projects if you need a more robust algorithm :
  ///
  /// - Delphi Encryption Compendium
  /// https://github.com/MHumm/DelphiEncryptionCompendium
  ///
  /// - TMS Cryptographic Pack
  /// https://www.tmssoftware.com/site/tmscrypto.asp
  ///
  /// </remarks>
  TOlfCryptDecrypt = class
  private
    FIntegerKeys: TIntegerDynArray;
    FByteKeys: TByteDynArray;
    procedure SetByteKeys(const Value: TByteDynArray);
    procedure SetIntegerKeys(const Value: TIntegerDynArray);
  protected
  public
    /// <summary>
    /// buffer of bytes used as a key by Crypt/Decrypt functions
    /// </summary>
    property ByteKeys: TByteDynArray read FByteKeys write SetByteKeys;

    /// <summary>
    /// buffer of integers used as a key by Crypt/Decrypt functions
    /// </summary>
    property IntegerKeys: TIntegerDynArray read FIntegerKeys
      write SetIntegerKeys;

    /// <summary>
    /// use XOR operand to crypt a buffer with the keys buffer property
    /// </summary>
    /// <remarks>
    /// You can use the program at https://xorkeysgenerator.olfsoftware.fr/ to
    /// generate a compatible XOR keys buffer.
    /// </remarks>
    function XORCrypt(Const AStream: TStream): TStream; overload;

    /// <summary>
    /// use XOR operand to crypt a buffer with a keys buffer
    /// </summary>
    /// <remarks>
    /// You can use the program at https://xorkeysgenerator.olfsoftware.fr/ to
    /// generate a compatible XOR keys buffer.
    /// </remarks>
    class function XORCrypt(Const AStream: TStream; Const AKeys: TByteDynArray)
      : TStream; overload;

    /// <summary>
    /// use XOR operand to decrypt a buffer with the keys buffer property
    /// </summary>
    /// <remarks>
    /// You can use the program at https://xorkeysgenerator.olfsoftware.fr/ to
    /// generate a compatible XOR keys buffer.
    /// </remarks>
    function XORDecrypt(Const AStream: TStream): TStream; overload;

    /// <summary>
    /// use XOR operand to decrypt a buffer with a keys buffer
    /// </summary>
    /// <remarks>
    /// You can use the program at https://xorkeysgenerator.olfsoftware.fr/ to
    /// generate a compatible XOR keys buffer.
    /// </remarks>
    class function XORDecrypt(Const AStream: TStream;
      Const AKeys: TByteDynArray): TStream; overload;

    /// <summary>
    /// Export a key as an array of random bytes
    /// </summary>
    class function GenXORKey(Const Size: word): TByteDynArray;

    /// <summary>
    /// exchange bytes between a buffer to crypt and the property key
    /// </summary>
    /// <remarks>
    /// You can use the program at https://swapkeysgenerator.olfsoftware.fr/ to
    /// generate a compatible Swap keys buffer.
    /// </remarks>
    function SwapCrypt(Const AStream: TStream): TStream; overload;

    /// <summary>
    /// exchange bytes between a buffer to crypt and the key parameter
    /// </summary>
    /// <remarks>
    /// You can use the program at https://swapkeysgenerator.olfsoftware.fr/ to
    /// generate a compatible Swap keys buffer.
    /// </remarks>
    class function SwapCrypt(Const AStream: TStream; Const AKeys: TByteDynArray)
      : TStream; overload;

    /// <summary>
    /// exchange bytes between a buffer to uncrypt and the property key
    /// </summary>
    /// <remarks>
    /// You can use the program at https://swapkeysgenerator.olfsoftware.fr/ to
    /// generate a compatible Swap keys buffer.
    /// </remarks>
    function SwapDecrypt(Const AStream: TStream): TStream; overload;

    /// <summary>
    /// exchange bytes between a buffer to uncrypt and the key parameter
    /// </summary>
    /// <remarks>
    /// You can use the program at https://swapkeysgenerator.olfsoftware.fr/ to
    /// generate a compatible Swap keys buffer.
    /// </remarks>
    class function SwapDecrypt(Const AStream: TStream;
      Const AKeys: TByteDynArray): TStream; overload;

    /// <summary>
    /// Export a key as an array of 256 random bytes
    /// </summary>
    class function GenSwapKey: TByteDynArray;

    function ShiftCrypt(Const AStream: TStream): TStream; overload;
    class function ShiftCrypt(Const AStream: TStream;
      const AKeys: TIntegerDynArray): TStream; overload;
    function ShiftDecrypt(Const AStream: TStream): TStream; overload;
    class function ShiftDecrypt(Const AStream: TStream;
      const AKeys: TIntegerDynArray): TStream; overload;
    class function GenShiftKey(Const Size: word): TIntegerDynArray;

    function IDBCrypt(Const AStream: TStream): TStream; overload;
    class function IDBCrypt(Const AStream: TStream;
      const AKeys: TIntegerDynArray): TStream; overload;
    function IDBDecrypt(Const AStream: TStream): TStream; overload;
    class function IDBDecrypt(Const AStream: TStream;
      const AKeys: TIntegerDynArray): TStream; overload;
    class function GenIDBKey(Const Size: word): TIntegerDynArray;

    /// <summary>
    /// Create an instance of TOlfCryptDecrypt class
    /// </summary>
    constructor Create; overload;

    /// <summary>
    /// Create an instance of TOlfCryptDecrypt class and fill its key buffer as bytes
    /// </summary>
    constructor Create(Const AKeys: TByteDynArray); overload;

    /// <summary>
    /// Create an instance of TOlfCryptDecrypt class and fill its key buffer as integers
    /// </summary>
    constructor Create(Const AKeys: TIntegerDynArray); overload;

    /// <summary>
    /// DEPRECATED - internally use XORCrypt
    /// </summary>
    function Crypt(Const AStream: TStream): TStream; overload;
      deprecated 'Use XORCrypt()';

    /// <summary>
    /// DEPRECATED - internally use XORCrypt
    /// </summary>
    class function Crypt(Const AStream: TStream; Const AKeys: TByteDynArray)
      : TStream; overload; deprecated 'Use XORCrypt()';

    /// <summary>
    /// DEPRECATED - internally use XORDecrypt
    /// </summary>
    function Decrypt(Const AStream: TStream): TStream; overload;
      deprecated 'Use XORDecrypt()';

    /// <summary>
    /// DEPRECATED - internally use XORDecrypt
    /// </summary>
    class function Decrypt(Const AStream: TStream; Const AKeys: TByteDynArray)
      : TStream; overload; deprecated 'Use XORDecrypt()';
  end;

implementation

uses
  System.Generics.Collections,
  System.SysUtils;

{ TOlfCryptDecrypt }

constructor TOlfCryptDecrypt.Create(const AKeys: TByteDynArray);
var
  i: uint64;
begin
  Create;
  setlength(FByteKeys, length(AKeys));
  for i := 0 to length(AKeys) - 1 do
    FByteKeys[i] := AKeys[i];
end;

function TOlfCryptDecrypt.Crypt(const AStream: TStream): TStream;
begin
  result := XORCrypt(AStream);
end;

constructor TOlfCryptDecrypt.Create(const AKeys: TIntegerDynArray);
var
  i: uint64;
begin
  Create;
  setlength(FIntegerKeys, length(AKeys));
  for i := 0 to length(AKeys) - 1 do
    FIntegerKeys[i] := AKeys[i];
end;

class function TOlfCryptDecrypt.Crypt(const AStream: TStream;
  const AKeys: TByteDynArray): TStream;
begin
  result := XORCrypt(AStream, AKeys);
end;

function TOlfCryptDecrypt.Decrypt(const AStream: TStream): TStream;
begin
  result := XORDecrypt(AStream)
end;

class function TOlfCryptDecrypt.Decrypt(const AStream: TStream;
  const AKeys: TByteDynArray): TStream;
begin
  result := XORDecrypt(AStream, AKeys);
end;

class function TOlfCryptDecrypt.GenIDBKey(const Size: word): TIntegerDynArray;
var
  i: word;
begin
  if Size < 1 then
    raise exception.Create('The size must be greater than 0.');

  setlength(result, Size);
  for i := 0 to Size - 1 do
    repeat
      result[i] := random(255 + 255 + 1) - 255;
    until result[i] <> 0;
end;

class function TOlfCryptDecrypt.GenShiftKey(const Size: word): TIntegerDynArray;
var
  i: word;
begin
  if Size < 1 then
    raise exception.Create('The size must be greater than 0.');

  setlength(result, Size);
  for i := 0 to Size - 1 do
    repeat
      result[i] := random(7 + 7 + 1) - 7;
    until result[i] <> 0;
end;

class function TOlfCryptDecrypt.GenSwapKey: TByteDynArray;
var
  i: integer;
  nb: integer;
  Bytes: TList<byte>;
begin
  setlength(result, 256);
  Bytes := TList<byte>.Create;
  try
    for i := 0 to 255 do
      Bytes.add(i);
    for i := 0 to 255 do
    begin
      nb := random(Bytes.count);
      result[i] := Bytes[nb];
      Bytes.Delete(nb);
    end;
  finally
    Bytes.free;
  end;
end;

class function TOlfCryptDecrypt.GenXORKey(const Size: word): TByteDynArray;
var
  i: word;
begin
  if Size < 1 then
    raise exception.Create('The size must be greater than 0.');

  setlength(result, Size);
  for i := 0 to Size - 1 do
  begin
    result[i] := random(256);
    if (i = 0) then
      while (result[0] in [0, 255]) do
        result[0] := random(255);
  end;
end;

class function TOlfCryptDecrypt.IDBCrypt(const AStream: TStream;
  const AKeys: TIntegerDynArray): TStream;
var
  KeyIndex: uint64;
  KeyLength: uint64;
  oc, od: byte;
  IncDecValue: integer;
begin
  KeyLength := length(AKeys);

  if (KeyLength = 0) then
    raise exception.Create('Need a private key to crypt !');

  if not assigned(AStream) then
    result := nil
  else
  begin
    result := tmemorystream.Create;
    KeyIndex := 0;
    AStream.position := 0;
    while (AStream.position < AStream.Size) do
    begin
      if (1 <> AStream.Read(od, 1)) then
        raise exception.Create('Can''t read a new byte.');

      IncDecValue := AKeys[KeyIndex] + od;

      while IncDecValue > 255 do
        dec(IncDecValue, 255);

      while IncDecValue < 0 do
        inc(IncDecValue, 255);

      oc := IncDecValue;

      if (1 <> result.write(oc, 1)) then
        raise exception.Create('Can''t write encrypted byte.');

      if (KeyIndex + 1 < KeyLength) then
        inc(KeyIndex)
      else
        KeyIndex := 0;
    end;
  end;
end;

function TOlfCryptDecrypt.IDBCrypt(const AStream: TStream): TStream;
begin
  result := IDBCrypt(AStream, FIntegerKeys);
end;

function TOlfCryptDecrypt.IDBDecrypt(const AStream: TStream): TStream;
begin
  result := IDBDecrypt(AStream, FIntegerKeys);
end;

class function TOlfCryptDecrypt.IDBDecrypt(const AStream: TStream;
  const AKeys: TIntegerDynArray): TStream;
var
  KeyIndex: uint64;
  KeyLength: uint64;
  oc, od: byte;
  IncDecValue: integer;
begin
  KeyLength := length(AKeys);

  if (KeyLength = 0) then
    raise exception.Create('Need a private key to crypt !');

  if not assigned(AStream) then
    result := nil
  else
  begin
    result := tmemorystream.Create;
    KeyIndex := 0;
    AStream.position := 0;
    while (AStream.position < AStream.Size) do
    begin
      if (1 <> AStream.Read(od, 1)) then
        raise exception.Create('Can''t read a new byte.');

      IncDecValue := od - AKeys[KeyIndex];

      while IncDecValue > 255 do
        dec(IncDecValue, 255);

      while IncDecValue < 0 do
        inc(IncDecValue, 255);

      oc := IncDecValue;

      if (1 <> result.write(oc, 1)) then
        raise exception.Create('Can''t write encrypted byte.');

      if (KeyIndex + 1 < KeyLength) then
        inc(KeyIndex)
      else
        KeyIndex := 0;
    end;
  end;
end;

class function TOlfCryptDecrypt.XORCrypt(const AStream: TStream;
  const AKeys: TByteDynArray): TStream;
var
  Key1, Key2: byte;
  KeyIndex: uint64;
  KeyLength: uint64;
  oc, od: byte;
begin
  KeyLength := length(AKeys);

  if (KeyLength = 0) then
    raise exception.Create('Need a private key to crypt !');

  if not assigned(AStream) then
    result := nil
  else
  begin
    result := tmemorystream.Create;
    Key1 := 0;
    KeyIndex := 0;
    Key2 := AKeys[KeyIndex];
    AStream.position := 0;
    while (AStream.position < AStream.Size) do
    begin
      if (1 <> AStream.Read(od, 1)) then
        raise exception.Create('Can''t read a new byte.');

      oc := (od xor Key1) xor Key2;

      if (1 <> result.write(oc, 1)) then
        raise exception.Create('Can''t write encrypted byte.');

      if (KeyIndex + 1 < KeyLength) then
        inc(KeyIndex)
      else
        KeyIndex := 0;

      Key1 := od;
      Key2 := AKeys[KeyIndex];
    end;
  end;
end;

constructor TOlfCryptDecrypt.Create;
begin
  inherited;
  setlength(FIntegerKeys, 0);
  setlength(FByteKeys, 0);
end;

function TOlfCryptDecrypt.XORCrypt(const AStream: TStream): TStream;
begin
  result := XORCrypt(AStream, FByteKeys);
end;

function TOlfCryptDecrypt.XORDecrypt(const AStream: TStream): TStream;
begin
  result := XORDecrypt(AStream, FByteKeys);
end;

procedure TOlfCryptDecrypt.SetByteKeys(const Value: TByteDynArray);
begin
  FByteKeys := Value;
end;

procedure TOlfCryptDecrypt.SetIntegerKeys(const Value: TIntegerDynArray);
begin
  FIntegerKeys := Value;
end;

class function TOlfCryptDecrypt.ShiftCrypt(const AStream: TStream;
  const AKeys: TIntegerDynArray): TStream;
var
  KeyIndex: uint64;
  KeyLength: uint64;
  oc, od: byte;
  w, wo: word;
  ShiftValue: byte;
begin
  KeyLength := length(AKeys);

  if (KeyLength = 0) then
    raise exception.Create('Need a private key to crypt !');

  if not assigned(AStream) then
    result := nil
  else
  begin
    result := tmemorystream.Create;
    KeyIndex := 0;
    AStream.position := 0;
    while (AStream.position < AStream.Size) do
    begin
      if (1 <> AStream.Read(od, 1)) then
        raise exception.Create('Can''t read a new byte.');

      ShiftValue := abs(AKeys[KeyIndex]) mod 8;
      wo := od;
      if AKeys[KeyIndex] = 0 then
        w := od
      else if AKeys[KeyIndex] > 0 then
        w := wo shl ShiftValue
      else
        w := wo shl (8 - ShiftValue); // SHR inversé
      oc := (w mod 256) + (w div 256);

      if (1 <> result.write(oc, 1)) then
        raise exception.Create('Can''t write encrypted byte.');

      if (KeyIndex + 1 < KeyLength) then
        inc(KeyIndex)
      else
        KeyIndex := 0;
    end;
  end;
end;

function TOlfCryptDecrypt.ShiftCrypt(const AStream: TStream): TStream;
begin
  result := ShiftCrypt(AStream, FIntegerKeys);
end;

function TOlfCryptDecrypt.ShiftDecrypt(const AStream: TStream): TStream;
begin
  result := ShiftDecrypt(AStream, FIntegerKeys);
end;

class function TOlfCryptDecrypt.ShiftDecrypt(const AStream: TStream;
  const AKeys: TIntegerDynArray): TStream;
var
  KeyIndex: uint64;
  KeyLength: uint64;
  oc, od: byte;
  w, wo: word;
  ShiftValue: byte;
begin
  KeyLength := length(AKeys);

  if (KeyLength = 0) then
    raise exception.Create('Need a private key to crypt !');

  if not assigned(AStream) then
    result := nil
  else
  begin
    result := tmemorystream.Create;
    KeyIndex := 0;
    AStream.position := 0;
    while (AStream.position < AStream.Size) do
    begin
      if (1 <> AStream.Read(od, 1)) then
        raise exception.Create('Can''t read a new byte.');

      ShiftValue := abs(AKeys[KeyIndex]) mod 8;
      wo := od;
      if AKeys[KeyIndex] = 0 then
        w := wo
      else if AKeys[KeyIndex] < 0 then
        w := wo shl ShiftValue
      else
        w := wo shl (8 - ShiftValue); // SHR inversé
      oc := (w mod 256) + (w div 256);

      if (1 <> result.write(oc, 1)) then
        raise exception.Create('Can''t write encrypted byte.');

      if (KeyIndex + 1 < KeyLength) then
        inc(KeyIndex)
      else
        KeyIndex := 0;
    end;
  end;
end;

function TOlfCryptDecrypt.SwapCrypt(const AStream: TStream): TStream;
begin
  result := SwapCrypt(AStream, FByteKeys);
end;

class function TOlfCryptDecrypt.SwapCrypt(const AStream: TStream;
  const AKeys: TByteDynArray): TStream;
var
  oc, od: byte;
begin
  if (length(AKeys) <> 256) then
    raise exception.Create('Need a 256 bytes private key to crypt !');

  if not assigned(AStream) then
    result := nil
  else
  begin
    result := tmemorystream.Create;
    AStream.position := 0;
    while (AStream.position < AStream.Size) do
    begin
      if (1 <> AStream.Read(od, 1)) then
        raise exception.Create('Can''t read a new byte.');

      oc := AKeys[ord(od)];

      if (1 <> result.write(oc, 1)) then
        raise exception.Create('Can''t write encrypted byte.');
    end;
  end;
end;

function TOlfCryptDecrypt.SwapDecrypt(const AStream: TStream): TStream;
begin
  result := SwapDecrypt(AStream, FByteKeys);
end;

class function TOlfCryptDecrypt.SwapDecrypt(const AStream: TStream;
  const AKeys: TByteDynArray): TStream;
var
  oc, od: byte;
  ReverseKey: TByteDynArray;
  i: integer;
begin
  if (length(AKeys) <> 256) then
    raise exception.Create('Need a 256 bytes private key to decrypt !');

  if not assigned(AStream) then
    result := nil
  else
  begin
    setlength(ReverseKey, length(AKeys));
    for i := 0 to length(AKeys) - 1 do
      ReverseKey[AKeys[i]] := i;

    result := tmemorystream.Create;
    AStream.position := 0;
    while (AStream.position < AStream.Size) do
    begin
      if (1 <> AStream.Read(oc, 1)) then
        raise exception.Create('Can''t read a new byte.');

      od := ReverseKey[ord(oc)];

      if (1 <> result.write(od, 1)) then
        raise exception.Create('Can''t write encrypted byte.');
    end;
  end;
end;

class function TOlfCryptDecrypt.XORDecrypt(const AStream: TStream;
  const AKeys: TByteDynArray): TStream;
var
  Key1, Key2: byte;
  KeyIndex: uint64;
  KeyLength: uint64;
  oc, od: byte;
begin
  KeyLength := length(AKeys);

  if (KeyLength = 0) then
    raise exception.Create('Need a private key to crypt !');

  if not assigned(AStream) then
    result := nil
  else
  begin
    result := tmemorystream.Create;
    Key1 := 0;
    KeyIndex := 0;
    Key2 := AKeys[KeyIndex];
    AStream.position := 0;
    while (AStream.position < AStream.Size) do
    begin
      if (1 <> AStream.Read(oc, 1)) then
        raise exception.Create('Can''t read a new byte.');

      od := (oc xor Key1) xor Key2;

      if (1 <> result.write(od, 1)) then
        raise exception.Create('Can''t write decrypted byte.');

      if (KeyIndex + 1 < KeyLength) then
        inc(KeyIndex)
      else
        KeyIndex := 0;

      Key1 := od;
      Key2 := AKeys[KeyIndex];
    end;
  end;
end;

initialization

randomize;

end.
