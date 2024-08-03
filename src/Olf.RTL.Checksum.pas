/// <summary>
/// ***************************************************************************
///
/// Librairies pour Delphi
///
/// Copyright 1990-2024 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// This repository contains functions, procedures and classes to use in
/// Delphi projects (console, VCL, FireMonkey and others). It's my "everything reuseable things" toolbox.
///
/// The units to be used in your projects can be found in the "src" folder.
/// Some features are explained on my blog or have been coded live on Twitch.
///
/// Examples of use in the form of VCL or FireMonkey projects are available in
/// the "samples" subfolder.
///
/// ***************************************************************************
///
/// Author(s) :
///      Patrick PREMARTIN
///
/// Site :
///      https://developpeur-pascal.fr/librairies-publiques.html
///
/// Project site :
///      https://github.com/DeveloppeurPascal/librairies
///
/// ***************************************************************************
/// File last update : 28/05/2024 12:19:15
/// Signature : ca5df1f07be9d1ff819c95b8e5be8a21fddc369c
/// ***************************************************************************
/// </summary>

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
  system.classes;

type
  TOlfChecksumVerifParamList = class(TStrings)
    function addParam(ch: string): TOlfChecksumVerifParamList;
  end;

  TOlfChecksumVerif = class
  private
    class function get(param: string; key1: string; key2: string; key3: string;
      key4: string; key5: string; isPublic: boolean): string; overload;
  public
    class function get(param: TOlfChecksumVerifParamList; key1: string = '';
      key2: string = ''; key3: string = ''; key4: string = '';
      key5: string = ''): string; overload;
    class function get(param: string; key1: string = ''; key2: string = '';
      key3: string = ''; key4: string = ''; key5: string = ''): string;
      overload;
    class function check(verif: string; param: TOlfChecksumVerifParamList;
      key1: string = ''; key2: string = ''; key3: string = '';
      key4: string = ''; key5: string = ''): boolean; overload;
    class function check(verif: string; param: string; key1: string = '';
      key2: string = ''; key3: string = ''; key4: string = '';
      key5: string = ''): boolean; overload;
  end;

  /// <summary>
  /// For compatibility with existing code only. Use "TOlfChecksumVerifParamList" instead.
  /// </summary>
  TChecksumVerifParamList = TOlfChecksumVerifParamList;
  /// <summary>
  /// For compatibility with existing code only. Use "TOlfChecksumVerif" instead.
  /// </summary>
  ChecksumVerif = TOlfChecksumVerif;

implementation

uses
  u_md5;

class function TOlfChecksumVerif.get(param: string; key1: string; key2: string;
  key3: string; key4: string; key5: string; isPublic: boolean): string;
var
  verif: string;
begin
  verif := MD5(param + key1 + key2 + key3 + key4 + key5);
  if isPublic then
    result := copy(verif, 1 + random(Length(verif) - 10), 10)
  else
    result := verif;
end;

class function TOlfChecksumVerif.get(param: TOlfChecksumVerifParamList;
  key1: string = ''; key2: string = ''; key3: string = ''; key4: string = '';
  key5: string = ''): string;
var
  i: integer;
  ch: string;
begin
  ch := '';
  for i := 0 to param.Count - 1 do
  begin
    ch := ch + param[i];
  end;
  result := get(ch, key1, key2, key3, key4, key5, true);
  param.Free;
end;

class function TOlfChecksumVerif.get(param: string; key1: string = '';
  key2: string = ''; key3: string = ''; key4: string = '';
  key5: string = ''): string;
begin
  result := get(param, key1, key2, key3, key4, key5, true);
end;

class function TOlfChecksumVerif.check(verif: string;
  param: TOlfChecksumVerifParamList; key1: string = ''; key2: string = '';
  key3: string = ''; key4: string = ''; key5: string = ''): boolean;
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
end;

class function TOlfChecksumVerif.check(verif: string; param: string;
  key1: string = ''; key2: string = ''; key3: string = ''; key4: string = '';
  key5: string = ''): boolean;
var
  verif_: string;
begin
  if '' = verif then
    result := false
  else
  begin
    verif_ := get(param, key1, key2, key3, key4, key5, false);
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
