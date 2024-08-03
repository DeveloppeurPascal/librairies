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
/// Signature : 3aaaeae7c50bc4ae2001484818301408d24548bd
/// ***************************************************************************
/// </summary>

unit Olf.RTL.GenRandomID;

interface

type
  TOlfRandomIDGenerator = class
  public
    /// <summary>
    /// Generate a string composed by a random list of chars (0..9 -> numbers, 0..35 -> numbers + lowercased letters ('a'-'z'), 0 ..61 -> numbers and letters ('a'-'z' and 'A'-'Z')
    /// </summary>
    class function getID(Const Base: Byte; Const ASize: cardinal = 0): string;
    /// <summary>
    /// Generate a string with random numbers between '0' and '1'
    /// </summary>
    class function getIDBase2(Const Size: cardinal = 0): string;
    /// <summary>
    /// Generate a string with random numbers between '0' and '9'
    /// </summary>
    class function getIDBase10(Const Size: cardinal = 0): string;
    /// <summary>
    /// Generate a string with random numbers between '0' and '9' or letters between 'a' and 'z'
    /// </summary>
    class function getIDBase36(Const Size: cardinal = 0): string;
    /// <summary>
    /// Generate a string with random numbers between '0' and '9' or letters between 'a' and 'z' or 'A' and 'Z'
    /// </summary>
    class function getIDBase62(Const Size: cardinal = 0): string;
    /// <summary>
    /// Set the default size used by other methods of this class when no size is given when generating an ID.
    /// </summary>
    class procedure SetDefaultSize(Const Size: cardinal);
  end;

implementation

uses
  System.SysUtils;

var
  DefaultSize: cardinal;

  { TOlfRandomIDGenerator }

class function TOlfRandomIDGenerator.getID(const Base: Byte;
  const ASize: cardinal): string;
var
  i, n: Byte;
  LSize: cardinal;
begin
  result := '';

  if ASize > 0 then
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

class function TOlfRandomIDGenerator.getIDBase10(const Size: cardinal): string;
begin
  result := getID(10, Size);
end;

class function TOlfRandomIDGenerator.getIDBase2(const Size: cardinal): string;
begin
  result := getID(2, Size);
end;

class function TOlfRandomIDGenerator.getIDBase36(const Size: cardinal): string;
begin
  result := getID(36, Size);
end;

class function TOlfRandomIDGenerator.getIDBase62(const Size: cardinal): string;
begin
  result := getID(62, Size);
end;

class procedure TOlfRandomIDGenerator.SetDefaultSize(const Size: cardinal);
begin
  if (Size < 1) then
    raise exception.create('Size must be greater then 0.');

  DefaultSize := Size;
end;

initialization

randomize;
DefaultSize := 10;

end.
