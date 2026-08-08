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
  File last update : 2026-08-08T15:57:18.000+02:00
  Signature : 009ac2b69c112f7086968cc9ade430c8e80a7c67
  ***************************************************************************
*)

unit Olf.RTL.Checksum;

interface

// This unit calculates checksums.
//
// If you work between Delphi and Delphi (or Pascal) projects, you can use this
// file. It has been explained at
// https://developpeur-pascal.fr/calculer-et-verifier-un-checksum-pour-dialoguer-avec-l-exterieur.html
//
// If you send things to a web server with PHP, you can generate a checksum or
// test it with the code explained at
// https://trucs-de-developpeur-web.fr/calculer-et-verifier-un-checksum-pour-dialoguer-avec-l-exterieur.html

uses
  System.Classes,
  System.Types;

type
  TOlfChecksumVerifParamList = class(TStringList)
    function addParam(ch: string): TOlfChecksumVerifParamList;
  end;

{$IF Not Declared(TStringDynArray)}
  TStringDynArray = array of string;
{$ENDIF}

  TOlfChecksumVerif = class
  private
    class function get(const params: TStringDynArray; const isPublic: boolean):
      string; overload;
  public
    class function get(const params: TStringDynArray): string; overload;
    class function get(const param: TOlfChecksumVerifParamList; const key1:
      string = ''; const key2: string = ''; const key3: string = ''; const key4:
      string = ''; const key5: string = ''; const FreeParam: boolean = true):
      string; overload;
    class function get(const param: string; const key1: string = ''; const key2:
      string = ''; const key3: string = ''; const key4: string = ''; const key5:
      string = ''): string; overload;
    class function check(const verif: string; const param:
      TOlfChecksumVerifParamList; const key1: string = ''; const key2: string =
      ''; const key3: string = ''; const key4: string = ''; const key5: string =
      ''; const FreeParam: boolean = true): boolean; overload;
    class function check(const verif: string; const param: string; const key1:
      string = ''; const key2: string = ''; const key3: string = ''; const key4:
      string = ''; const key5: string = ''): boolean; overload;
    class function check(const verif: string; const params: TStringDynArray):
      boolean; overload;
  end;

  // TODO -oDeveloppeurPascal : add XMLDoc comments

  /// <summary>
  /// For compatibility with existing code only. Use "TOlfChecksumVerifParamList" instead.
  /// </summary>
  TChecksumVerifParamList = TOlfChecksumVerifParamList;
  /// <summary>
  /// For compatibility with existing code only. Use "TOlfChecksumVerif" instead.
  /// </summary>
  ChecksumVerif = TOlfChecksumVerif;

implementation

{$IF CompilerVersion>=30.0}
uses
  System.SysUtils,
  System.Hash;

{$ELSE}
uses
  u_md5;
{$ENDIF}

class function TOlfChecksumVerif.get(const params: TStringDynArray; const
  isPublic: boolean): string;
var
  verif: string;
  i: integer;
  key: string;
begin
  key := '';
  for i := 0 to length(params) - 1 do
    key := key + params[i];

{$IF CompilerVersion>=30.0}
  verif := THashMD5.GetHashString(key);
{$ELSE}
  verif := MD5(key);
{$ENDIF}

  if isPublic then
    result := copy(verif, 1 + random(length(verif) - 10), 10)
  else
    result := verif;
end;

class function TOlfChecksumVerif.get(const param: TOlfChecksumVerifParamList;
  const key1, key2, key3, key4, key5: string; const FreeParam: boolean): string;
var
  i: integer;
  ch: string;
begin
  ch := '';
  for i := 0 to param.Count - 1 do
  begin
    ch := ch + param[i];
  end;
  result := get([ch, key1, key2, key3, key4, key5], true);
  if FreeParam then
    param.Free;
end;

class function TOlfChecksumVerif.check(const verif: string;
  const params: TStringDynArray): boolean;
var
  verif_: string;
begin
  if '' = verif then
    result := false
  else
  begin
    verif_ := get(params, false);
    result := 0 < pos(verif, verif_);
  end;
end;

class function TOlfChecksumVerif.get(const param: string; const key1, key2,
  key3, key4, key5: string): string;
begin
  result := get([param, key1, key2, key3, key4, key5], true);
end;

class function TOlfChecksumVerif.get(const params: TStringDynArray): string;
begin
  result := get(params, true);
end;

class function TOlfChecksumVerif.check(const verif: string; const param:
  TOlfChecksumVerifParamList; const key1, key2, key3, key4, key5: string; const
  FreeParam: boolean): boolean;
var
  i: integer;
  ch: string;
begin
  ch := '';
  for i := 0 to param.Count - 1 do
  begin
    ch := ch + param[i];
  end;
  result := check(verif, ch, key1, key2, key3, key4, key5);
  if FreeParam then
    param.Free;
end;

class function TOlfChecksumVerif.check(const verif: string; const param, key1,
  key2, key3, key4, key5: string): boolean;
var
  verif_: string;
begin
  if '' = verif then
    result := false
  else
  begin
    verif_ := get([param, key1, key2, key3, key4, key5], false);
    result := 0 < pos(verif, verif_);
  end;
end;

{ TOlfChecksumVerifParamList }

function TOlfChecksumVerifParamList.addParam(ch: string)
  : TOlfChecksumVerifParamList;
begin
  add(ch);
  result := self;
end;

initialization

  randomize;

end.

