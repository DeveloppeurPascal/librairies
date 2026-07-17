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
  File last update : 2026-07-17T19:09:00.000+02:00
  Signature : 7a092d70d3ac51724a6d8d93ec9584f8e8012a15
  ***************************************************************************
*)

unit Olf.RTL.GenRandomID;

interface

type
  TOlfRandomIDGenerator = class
  public
    /// <summary>
    /// Generate a string composed by a random list of chars (0..9 -> numbers, 0..35 -> numbers + lowercased letters ('a'-'z'), 0 ..61 -> numbers and letters ('a'-'z' and 'A'-'Z')
    /// </summary>
    class function getID(const Base: Byte; const ASize: Cardinal = 0): string;
    /// <summary>
    /// Generate a string with random numbers between '0' and '1'
    /// </summary>
    class function getIDBase2(const Size: Cardinal = 0): string;
    /// <summary>
    /// Generate a string with random numbers between '0' and '9'
    /// </summary>
    class function getIDBase10(const Size: Cardinal = 0): string;
    /// <summary>
    /// Generate a string with random numbers between '0' and '9' or letters between 'a' and 'z'
    /// </summary>
    class function getIDBase36(const Size: Cardinal = 0): string;
    /// <summary>
    /// Generate a string with random numbers between '0' and '9' or letters between 'a' and 'z' or 'A' and 'Z'
    /// </summary>
    class function getIDBase62(const Size: Cardinal = 0): string;
    /// <summary>
    ///   Generate a string like a password with different elements in it
    /// </summary>
    class function getPassword(const Numbers, LowerCaseLetters, UpperCaseLetters: Boolean; const Symbols: TArray < Char >= []; const ASize: Cardinal = 0): string; overload;
    /// <summary>
    ///   Generate a string like a password with different elements in it
    /// </summary>
    class function getPassword(const Numbers, LowerCaseLetters, UpperCaseLetters: Boolean; const Symbols: string = ''; const ASize: Cardinal = 0): string; overload;
    /// <summary>
    /// Set the default size used by other methods of this class when no size is given when generating an ID.
    /// </summary>
    class procedure SetDefaultSize(const Size: Cardinal);
  end;

implementation

uses
  System.SysUtils;

var
  DefaultSize: Cardinal;

  { TOlfRandomIDGenerator }

  class function TOlfRandomIDGenerator.getID(const Base: Byte;
    const ASize: Cardinal): string;
var
  i,
  n: Byte;
  LSize: Cardinal;
begin
  result := '';

  if ASize > 0
  then
    LSize := ASize
  else
    LSize := DefaultSize;

  for i := 1 to LSize do
    begin
      n := random(Base);
      case n of
        0 .. 9:
          result := result + chr(ord('0') + n);
        10 .. 35:
          result := result + chr(ord('a') + n - 10);
        36 .. 61:
          result := result + chr(ord('A') + n - 36);
      end;
    end;
end;

class function TOlfRandomIDGenerator.getIDBase10(const Size: Cardinal): string;
begin
  result := getID(10, Size);
end;

class function TOlfRandomIDGenerator.getIDBase2(const Size: Cardinal): string;
begin
  result := getID(2, Size);
end;

class function TOlfRandomIDGenerator.getIDBase36(const Size: Cardinal): string;
begin
  result := getID(36, Size);
end;

class function TOlfRandomIDGenerator.getIDBase62(const Size: Cardinal): string;
begin
  result := getID(62, Size);
end;

class function TOlfRandomIDGenerator.getPassword(const Numbers,
  LowerCaseLetters, UpperCaseLetters: Boolean; const Symbols: string;
  const ASize: Cardinal): string;
  var
    SymbolsTab: TArray<Char>; i: Integer;
begin
  setlength(symbolstab, symbols.Length);
  for i := 0 to symbols.length - 1 do
    symbolstab[i] := symbols.Chars[i];
  result := getPassword(Numbers, LowerCaseLetters, UpperCaseLetters, SymbolsTab, ASize);
end;

class function TOlfRandomIDGenerator.getPassword(const Numbers, LowerCaseLetters,
  UpperCaseLetters: Boolean; const Symbols: TArray<Char>;
  const ASize: Cardinal): string;
  var
    Items: TArray<Char>;
    i: Integer;
    LSize: Cardinal;
begin
  result := '';

  if ASize > 0
  then
    LSize := ASize
  else
    LSize := DefaultSize;

  setLength(Items, 0);

  if numbers
  then
    begin
      setLength(Items, Length(Items) + 10);
      for i := 0 to 9 do
        items[Length(Items) - i - 1] := chr(ord('0') + i);
    end;

  if LowerCaseLetters
  then
    begin
      setLength(Items, Length(Items) + 26);
      for i := 0 to 25 do
        items[Length(Items) - i - 1] := chr(ord('a') + i);
    end;

  if UpperCaseLetters
  then
    begin
      setLength(Items, Length(Items) + 26);
      for i := 0 to 25 do
        items[Length(Items) - i - 1] := chr(ord('A') + i);
    end;

  setLength(Items, Length(Items) + length(symbols));
  for i := 0 to                    length(symbols) - 1 do
    items[Length(Items) - i - 1] := symbols[i];

  if length(items) > 0
  then
    begin
      result := '';
      for i := 1 to LSize do
        result := result + Items[random(length(items))];
    end else
      result := '';
end;

class procedure TOlfRandomIDGenerator.SetDefaultSize(const Size: Cardinal);
begin
  if (Size < 1)
  then
    raise exception.create('Size must be greater then 0.');

  DefaultSize := Size;
end;

initialization

randomize;
DefaultSize := 10;

end.


