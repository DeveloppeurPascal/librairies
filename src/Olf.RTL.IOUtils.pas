(* C2PP
  ***************************************************************************

  My libraries for Delphi

  Copyright 1990-2025 Patrick Prémartin under AGPL 3.0 license.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

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
  File last update : 2025-09-19T18:46:48.000+02:00
  Signature : 722da0ae7590f3702cb4d913ca9b64d4bd38d7ed
  ***************************************************************************
*)

unit Olf.RTL.IOUtils;

interface

type
  TOlfPath = class
    /// <summary>
    ///   Return relative path of ForPath depending on CurrentPath
    /// </summary>
    class function GetRelativePath(const ForPath, CurrentPath: string): string;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils;

class function TOlfPath.GetRelativePath(const ForPath, CurrentPath: string):
string;
var
  i, j: integer;
begin
  i := 0;
  j := 0;
  while (i < length(ForPath)) and (i < length(CurrentPath)) and
  (ForPath.Chars[i] = CurrentPath.Chars[i]) do
  begin
    if ForPath.Chars[i] = tpath.DirectorySeparatorChar then
      j := i;
    inc(i);
  end;
  if (i < length(ForPath)) or ((i < length(CurrentPath)) and
    (CurrentPath.Chars[i] <> tpath.DirectorySeparatorChar)) then
  begin
    result := ForPath.Substring(j + 1);
    i := j + 1;
  end
  else
    result := '';
  while (i < length(CurrentPath)) do
  begin
    if CurrentPath.Chars[i] = tpath.DirectorySeparatorChar then
      if result.IsEmpty then
        result := '.' + tpath.DirectorySeparatorChar
      else if result = '.' + tpath.DirectorySeparatorChar then
        result := '.' + result
      else
        result := '..' + tpath.DirectorySeparatorChar + result;
    inc(i);
  end;
end;

end.

