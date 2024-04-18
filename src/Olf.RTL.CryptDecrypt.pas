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
    FKeys: TByteDynArray;
    procedure SetKeys(const Value: TByteDynArray);
  protected
  public
    /// <summary>
    /// buffer of bytes used as a key by Crypt/Decrypt functions
    /// </summary>
    property Keys: TByteDynArray read FKeys write SetKeys;

    /// <summary>
    /// use XOR operand to crypt a buffer with the keys buffer property
    /// </summary>
    function XORCrypt(Const AStream: TStream): TStream; overload;

    /// <summary>
    /// use XOR operand to crypt a buffer with a keys buffer
    /// </summary>
    class function XORCrypt(Const AStream: TStream; Const AKeys: TByteDynArray)
      : TStream; overload;

    /// <summary>
    /// use XOR operand to decrypt a buffer with the keys buffer property
    /// </summary>
    function XORDecrypt(Const AStream: TStream): TStream; overload;

    /// <summary>
    /// use XOR operand to decrypt a buffer with a keys buffer
    /// </summary>
    class function XORDecrypt(Const AStream: TStream;
      Const AKeys: TByteDynArray): TStream; overload;

    /// <summary>
    /// exchange bytes between a buffer to crypt and the property key
    /// </summary>
    function SwapCrypt(Const AStream: TStream): TStream; overload;

    /// <summary>
    /// exchange bytes between a buffer to crypt and the key parameter
    /// </summary>
    class function SwapCrypt(Const AStream: TStream; Const AKeys: TByteDynArray)
      : TStream; overload;

    /// <summary>
    /// exchange bytes between a buffer to uncrypt and the property key
    /// </summary>
    function SwapDecrypt(Const AStream: TStream): TStream; overload;

    /// <summary>
    /// exchange bytes between a buffer to uncrypt and the key parameter
    /// </summary>
    class function SwapDecrypt(Const AStream: TStream;
      Const AKeys: TByteDynArray): TStream; overload;

    /// <summary>
    /// Create an instance of TOlfCryptDecrypt class
    /// </summary>
    constructor Create; overload;

    /// <summary>
    /// Create an instance of TOlfCryptDecrypt class and fill its key buffer
    /// </summary>
    constructor Create(Const AKeys: TByteDynArray); overload;

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
  System.SysUtils;

{ TOlfCryptDecrypt }

constructor TOlfCryptDecrypt.Create(const AKeys: TByteDynArray);
var
  i: uint64;
begin
  Create;
  setlength(FKeys, length(AKeys));
  for i := 0 to length(AKeys) - 1 do
    FKeys[i] := AKeys[i];
end;

function TOlfCryptDecrypt.Crypt(const AStream: TStream): TStream;
begin
  result := XORCrypt(AStream);
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
  setlength(FKeys, 0);
end;

function TOlfCryptDecrypt.XORCrypt(const AStream: TStream): TStream;
begin
  result := XORCrypt(AStream, FKeys);
end;

function TOlfCryptDecrypt.XORDecrypt(const AStream: TStream): TStream;
begin
  result := XORDecrypt(AStream, FKeys);
end;

procedure TOlfCryptDecrypt.SetKeys(const Value: TByteDynArray);
begin
  FKeys := Value;
end;

function TOlfCryptDecrypt.SwapCrypt(const AStream: TStream): TStream;
begin
  result := SwapCrypt(AStream, FKeys);
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
  result := SwapDecrypt(AStream, FKeys);
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

end.
