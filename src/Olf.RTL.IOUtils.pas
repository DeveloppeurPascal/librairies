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
  File last update : 2026-03-30T16:35:19.662+02:00
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

