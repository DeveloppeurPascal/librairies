unit Olf.RTL.CryptDecrypt;

interface

// This unit contains very simple cryptographic algorithms.
// DON't USE THEM FOR SENSIBLE DATAS !!!
//
// Check those projects if you need a more robust algorithm :
//
// - Delphi Encryption Compendium
// https://github.com/MHumm/DelphiEncryptionCompendium
//
// - TMS Cryptographic Pack
// https://www.tmssoftware.com/site/tmscrypto.asp

uses
  System.Classes,
  System.Types;

type
  TOlfCryptDecrypt = class
  private
    FKeys: TByteDynArray;
    procedure SetKeys(const Value: TByteDynArray);
  protected
  public
    property Keys: TByteDynArray read FKeys write SetKeys;
    function Crypt(Const AStream: TStream): TStream;
    function Decrypt(Const AStream: TStream): TStream;
    constructor Create; overload;
    constructor Create(Const AKeys: TByteDynArray); overload;
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

constructor TOlfCryptDecrypt.Create;
begin
  inherited;
  setlength(FKeys, 0);
end;

function TOlfCryptDecrypt.Crypt(const AStream: TStream): TStream;
var
  Key1, Key2: byte;
  KeyIndex: uint64;
  KeyLength: uint64;
  oc, od: byte;
begin
  KeyLength := length(FKeys);

  if (KeyLength = 0) then
    raise exception.Create('Need a private key to crypt !');

  if not assigned(AStream) then
    result := nil
  else
  begin
    result := tmemorystream.Create;
    Key1 := 0;
    KeyIndex := 0;
    Key2 := FKeys[KeyIndex];
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
      Key2 := FKeys[KeyIndex];
    end;
  end;
end;

function TOlfCryptDecrypt.Decrypt(const AStream: TStream): TStream;
var
  Key1, Key2: byte;
  KeyIndex: uint64;
  KeyLength: uint64;
  oc, od: byte;
begin
  KeyLength := length(FKeys);

  if (KeyLength = 0) then
    raise exception.Create('Need a private key to crypt !');

  if not assigned(AStream) then
    result := nil
  else
  begin
    result := tmemorystream.Create;
    Key1 := 0;
    KeyIndex := 0;
    Key2 := FKeys[KeyIndex];
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
      Key2 := FKeys[KeyIndex];
    end;
  end;
end;

procedure TOlfCryptDecrypt.SetKeys(const Value: TByteDynArray);
begin
  FKeys := Value;
end;

end.
