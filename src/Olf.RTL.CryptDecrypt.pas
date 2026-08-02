(* C2PP
  ***************************************************************************

  My libraries for Delphi
  Copyright (c) 1990-2026 Patrick PREMARTIN

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

  ***************************************************************************

  This repository contains functions, procedures and classes to use in
  Delphi projects (console, VCL, FireMonkey and others). It's my "everything reuseable things" toolbox.

  The units to be used in your projects can be found in the "src" folder.
  Some features are explained on my blog or have been coded live on Twitch.

  Examples of use in the form of VCL or FireMonkey projects are available in
  the "samples" subfolder.

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://librairies.developpeur-pascal.fr

  Project site :
  https://github.com/DeveloppeurPascal/librairies

  ***************************************************************************
  File last update : 2026-08-02T21:08:04.000+02:00
  Signature : dc13ac2c7122de1db74462a1ee72d1a54eb3fad9
  ***************************************************************************
*)

unit Olf.RTL.CryptDecrypt;

interface

// TODO : add missing XMLDoc comments

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
    function XORCrypt(const AStream: TStream): TMemoryStream; overload;

    /// <summary>
    /// use XOR operand to crypt a buffer with a keys buffer
    /// </summary>
    /// <remarks>
    /// You can use the program at https://xorkeysgenerator.olfsoftware.fr/ to
    /// generate a compatible XOR keys buffer.
    /// </remarks>
    class function XORCrypt(const AStream: TStream; const AKeys: TByteDynArray)
      : TMemoryStream; overload;

    class function XORCrypt(const ASource: string; const AKeys: TByteDynArray):
      string; overload;

    /// <summary>
    /// use XOR operand to decrypt a buffer with the keys buffer property
    /// </summary>
    /// <remarks>
    /// You can use the program at https://xorkeysgenerator.olfsoftware.fr/ to
    /// generate a compatible XOR keys buffer.
    /// </remarks>
    function XORDecrypt(const AStream: TStream): TMemoryStream; overload;

    /// <summary>
    /// use XOR operand to decrypt a buffer with a keys buffer
    /// </summary>
    /// <remarks>
    /// You can use the program at https://xorkeysgenerator.olfsoftware.fr/ to
    /// generate a compatible XOR keys buffer.
    /// </remarks>
    class function XORDecrypt(const AStream: TStream;
      const AKeys: TByteDynArray): TMemoryStream; overload;

    class function XORDecrypt(const ASource: string; const AKeys:
      TByteDynArray): string; overload;

    /// <summary>
    /// Export a key as an array of random bytes
    /// </summary>
    class function GenXORKey(const Size: word): TByteDynArray;

    /// <summary>
    /// exchange bytes between a buffer to crypt and the property key
    /// </summary>
    /// <remarks>
    /// You can use the program at https://swapkeysgenerator.olfsoftware.fr/ to
    /// generate a compatible Swap keys buffer.
    /// </remarks>
    function SwapCrypt(const AStream: TStream): TMemoryStream; overload;

    /// <summary>
    /// exchange bytes between a buffer to crypt and the key parameter
    /// </summary>
    /// <remarks>
    /// You can use the program at https://swapkeysgenerator.olfsoftware.fr/ to
    /// generate a compatible Swap keys buffer.
    /// </remarks>
    class function SwapCrypt(const AStream: TStream; const AKeys: TByteDynArray)
      : TMemoryStream; overload;

    class function SwapCrypt(const ASource: string; const AKeys: TByteDynArray):
      string; overload;

    /// <summary>
    /// exchange bytes between a buffer to uncrypt and the property key
    /// </summary>
    /// <remarks>
    /// You can use the program at https://swapkeysgenerator.olfsoftware.fr/ to
    /// generate a compatible Swap keys buffer.
    /// </remarks>
    function SwapDecrypt(const AStream: TStream): TMemoryStream; overload;

    /// <summary>
    /// exchange bytes between a buffer to uncrypt and the key parameter
    /// </summary>
    /// <remarks>
    /// You can use the program at https://swapkeysgenerator.olfsoftware.fr/ to
    /// generate a compatible Swap keys buffer.
    /// </remarks>
    class function SwapDecrypt(const AStream: TStream;
      const AKeys: TByteDynArray): TMemoryStream; overload;

    class function SwapDecrypt(const ASource: string; const AKeys:
      TByteDynArray): string; overload;

    /// <summary>
    /// Export a key as an array of 256 random bytes
    /// </summary>
    class function GenSwapKey: TByteDynArray;

    function ShiftCrypt(const AStream: TStream): TMemoryStream; overload;

    class function ShiftCrypt(const AStream: TStream;
      const AKeys: TIntegerDynArray): TMemoryStream; overload;

    class function ShiftCrypt(const ASource: string; const AKeys:
      TIntegerDynArray): string; overload;

    function ShiftDecrypt(const AStream: TStream): TMemoryStream; overload;

    class function ShiftDecrypt(const AStream: TStream;
      const AKeys: TIntegerDynArray): TMemoryStream; overload;

    class function ShiftDecrypt(const ASource: string; const AKeys:
      TIntegerDynArray): string; overload;

    class function GenShiftKey(const Size: word): TIntegerDynArray;

    function IDBCrypt(const AStream: TStream): TMemoryStream; overload;

    class function IDBCrypt(const AStream: TStream;
      const AKeys: TIntegerDynArray): TMemoryStream; overload;

    class function IDBCrypt(const ASource: string; const AKeys:
      TIntegerDynArray): string; overload;

    function IDBDecrypt(const AStream: TStream): TMemoryStream; overload;

    class function IDBDecrypt(const AStream: TStream;
      const AKeys: TIntegerDynArray): TMemoryStream; overload;

    class function IDBDecrypt(const ASource: string; const AKeys:
      TIntegerDynArray): string; overload;

    class function GenIDBKey(const Size: word): TIntegerDynArray;

    /// <summary>
    /// Create an instance of TOlfCryptDecrypt class
    /// </summary>
    constructor Create; overload;

    /// <summary>
    /// Create an instance of TOlfCryptDecrypt class and fill its key buffer as bytes
    /// </summary>
    constructor Create(const AKeys: TByteDynArray); overload;

    /// <summary>
    /// Create an instance of TOlfCryptDecrypt class and fill its key buffer as integers
    /// </summary>
    constructor Create(const AKeys: TIntegerDynArray); overload;

    /// <summary>
    /// DEPRECATED - internally use XORCrypt
    /// </summary>
    function Crypt(const AStream: TStream): TStream; overload;
      deprecated 'Use XORCrypt()';

    /// <summary>
    /// DEPRECATED - internally use XORCrypt
    /// </summary>
    class function Crypt(const AStream: TStream; const AKeys: TByteDynArray)
      : TStream; overload; deprecated 'Use XORCrypt()';

    /// <summary>
    /// DEPRECATED - internally use XORDecrypt
    /// </summary>
    function Decrypt(const AStream: TStream): TStream; overload;
      deprecated 'Use XORDecrypt()';

    /// <summary>
    /// DEPRECATED - internally use XORDecrypt
    /// </summary>
    class function Decrypt(const AStream: TStream; const AKeys: TByteDynArray)
      : TStream; overload; deprecated 'Use XORDecrypt()';
  end;

implementation

uses
  System.Generics.Collections,
  System.SysUtils,
  System.NetEncoding;

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
  const AKeys: TIntegerDynArray): TMemoryStream;
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
    result := TMemoryStream.Create;
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

function TOlfCryptDecrypt.IDBCrypt(const AStream: TStream): TMemoryStream;
begin
  result := IDBCrypt(AStream, FIntegerKeys);
end;

function TOlfCryptDecrypt.IDBDecrypt(const AStream: TStream): TMemoryStream;
begin
  result := IDBDecrypt(AStream, FIntegerKeys);
end;

class function TOlfCryptDecrypt.IDBDecrypt(const AStream: TStream;
  const AKeys: TIntegerDynArray): TMemoryStream;
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
    result := TMemoryStream.Create;
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
  const AKeys: TByteDynArray): TMemoryStream;
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
    result := TMemoryStream.Create;
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

function TOlfCryptDecrypt.XORCrypt(const AStream: TStream): TMemoryStream;
begin
  result := XORCrypt(AStream, FByteKeys);
end;

function TOlfCryptDecrypt.XORDecrypt(const AStream: TStream): TMemoryStream;
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
  const AKeys: TIntegerDynArray): TMemoryStream;
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
    result := TMemoryStream.Create;
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

function TOlfCryptDecrypt.ShiftCrypt(const AStream: TStream): TMemoryStream;
begin
  result := ShiftCrypt(AStream, FIntegerKeys);
end;

function TOlfCryptDecrypt.ShiftDecrypt(const AStream: TStream): TMemoryStream;
begin
  result := ShiftDecrypt(AStream, FIntegerKeys);
end;

class function TOlfCryptDecrypt.ShiftDecrypt(const AStream: TStream;
  const AKeys: TIntegerDynArray): TMemoryStream;
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
    result := TMemoryStream.Create;
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

function TOlfCryptDecrypt.SwapCrypt(const AStream: TStream): TMemoryStream;
begin
  result := SwapCrypt(AStream, FByteKeys);
end;

class function TOlfCryptDecrypt.SwapCrypt(const AStream: TStream;
  const AKeys: TByteDynArray): TMemoryStream;
var
  oc, od: byte;
begin
  if (length(AKeys) <> 256) then
    raise exception.Create('Need a 256 bytes private key to crypt !');

  if not assigned(AStream) then
    result := nil
  else
  begin
    result := TMemoryStream.Create;
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

function TOlfCryptDecrypt.SwapDecrypt(const AStream: TStream): TMemoryStream;
begin
  result := SwapDecrypt(AStream, FByteKeys);
end;

class function TOlfCryptDecrypt.SwapDecrypt(const AStream: TStream;
  const AKeys: TByteDynArray): TMemoryStream;
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

    result := TMemoryStream.Create;
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
  const AKeys: TByteDynArray): TMemoryStream;
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
    result := TMemoryStream.Create;
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

class function TOlfCryptDecrypt.IDBCrypt(const ASource: string;
  const AKeys: TIntegerDynArray): string;
var
  ss: TStringStream;
  ms: TMemoryStream;
begin
  result := '';
  ss := TStringStream.Create(ASource);
  try
    ms := IDBCrypt(ss, AKeys);
    try
      result := TNetEncoding.Base64.EncodeBytesToString(ms.Memory, ms.Size);
    finally
      ms.free;
    end;
  finally
    ss.free;
  end;
end;

class function TOlfCryptDecrypt.IDBDecrypt(const ASource: string;
  const AKeys: TIntegerDynArray): string;
var
  ss: TStringStream;
  ms, ms2: TMemoryStream;
  b: TBytes;
begin
  result := '';
  ms := TMemoryStream.Create;
  try
    b := TNetEncoding.Base64.DecodeStringToBytes(ASource);
    ms.write(b, length(b));
    ms.position := 0;
    ms2 := IDBDecrypt(ms, AKeys);
    try
      ss := TStringStream.Create;
      try
        ss.CopyFrom(ms2);
        result := ss.DataString;
      finally
        ss.free;
      end;
    finally
      ms2.free;
    end;
  finally
    ms.free;
  end;
end;

class function TOlfCryptDecrypt.ShiftCrypt(const ASource: string;
  const AKeys: TIntegerDynArray): string;
var
  ss: TStringStream;
  ms: TMemoryStream;
begin
  result := '';
  ss := TStringStream.Create(ASource);
  try
    ms := ShiftCrypt(ss, AKeys);
    try
      result := TNetEncoding.Base64.EncodeBytesToString(ms.Memory, ms.Size);
    finally
      ms.free;
    end;
  finally
    ss.free;
  end;
end;

class function TOlfCryptDecrypt.ShiftDecrypt(const ASource: string;
  const AKeys: TIntegerDynArray): string;
var
  ss: TStringStream;
  ms, ms2: TMemoryStream;
  b: TBytes;
begin
  result := '';
  ms := TMemoryStream.Create;
  try
    b := TNetEncoding.Base64.DecodeStringToBytes(ASource);
    ms.write(b, length(b));
    ms.position := 0;
    ms2 := ShiftDecrypt(ms, AKeys);
    try
      ss := TStringStream.Create;
      try
        ss.CopyFrom(ms2);
        result := ss.DataString;
      finally
        ss.free;
      end;
    finally
      ms2.free;
    end;
  finally
    ms.free;
  end;
end;

class function TOlfCryptDecrypt.SwapCrypt(const ASource: string;
  const AKeys: TByteDynArray): string;
var
  ss: TStringStream;
  ms: TMemoryStream;
begin
  result := '';
  ss := TStringStream.Create(ASource);
  try
    ms := SwapCrypt(ss, AKeys);
    try
      result := TNetEncoding.Base64.EncodeBytesToString(ms.Memory, ms.Size);
    finally
      ms.free;
    end;
  finally
    ss.free;
  end;
end;

class function TOlfCryptDecrypt.SwapDecrypt(const ASource: string;
  const AKeys: TByteDynArray): string;
var
  ss: TStringStream;
  ms, ms2: TMemoryStream;
  b: TBytes;
begin
  result := '';
  ms := TMemoryStream.Create;
  try
    b := TNetEncoding.Base64.DecodeStringToBytes(ASource);
    ms.write(b, length(b));
    ms.position := 0;
    ms2 := SwapDecrypt(ms, AKeys);
    try
      ss := TStringStream.Create;
      try
        ss.CopyFrom(ms2);
        result := ss.DataString;
      finally
        ss.free;
      end;
    finally
      ms2.free;
    end;
  finally
    ms.free;
  end;
end;

class function TOlfCryptDecrypt.XORCrypt(const ASource: string;
  const AKeys: TByteDynArray): string;
var
  ss: TStringStream;
  ms: TMemoryStream;
begin
  result := '';
  ss := TStringStream.Create(ASource);
  try
    ms := XORCrypt(ss, AKeys);
    try
      result := TNetEncoding.Base64.EncodeBytesToString(ms.Memory, ms.Size);
    finally
      ms.free;
    end;
  finally
    ss.free;
  end;
end;

class function TOlfCryptDecrypt.XORDecrypt(const ASource: string;
  const AKeys: TByteDynArray): string;
var
  ss: TStringStream;
  ms, ms2: TMemoryStream;
  b: TBytes;
begin
  result := '';
  ms := TMemoryStream.Create;
  try
    b := TNetEncoding.Base64.DecodeStringToBytes(ASource);
    ms.write(b, length(b));
    ms.position := 0;
    ms2 := XORDecrypt(ms, AKeys);
    try
      ss := TStringStream.Create;
      try
        ss.CopyFrom(ms2);
        result := ss.DataString;
      finally
        ss.free;
      end;
    finally
      ms2.free;
    end;
  finally
    ms.free;
  end;
end;

initialization

  randomize;

end.

