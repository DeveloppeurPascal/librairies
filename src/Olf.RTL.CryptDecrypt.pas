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
  idxKey: uint64;
begin
  if (length(FKeys) = 0) then
    raise exception.Create('Need a private key to crypt !');

  if not assigned(AStream) then
    result := nil
  else
  begin
    result := tmemorystream.Create;
    // TODO : à compléter
  end;
end;

function TOlfCryptDecrypt.Decrypt(const AStream: TStream): TStream;
begin
  // TODO : à compléter
end;

procedure TOlfCryptDecrypt.SetKeys(const Value: TByteDynArray);
begin
  FKeys := Value;
end;

end.
