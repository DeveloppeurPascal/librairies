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
  File last update : 2026-03-30T16:35:19.678+02:00
  Signature : c8d6acff10a8095fefd5237e292b79075d171813
  ***************************************************************************
*)

unit u_md5;

interface

/// <summary>Get MD5 code from a string</summary>
/// <param name="AString">String to encode</param>
/// <returns>The function return the MD5 of the AString string.</returns>
/// <remarks>
/// Before Delphi 10 Seattle this function uses IdHashMessageDigest from Iny.
/// Since Delphi 10 Seattle it uses System.Hash.THashMD5 from Embarcadero.
/// </remarks>
function MD5(const AString: String): String;

implementation

uses
{$IF CompilerVersion>=30.0}
  System.Hash,
{$ELSE}
  IdHashMessageDigest,
{$ENDIF}
  System.SysUtils;

function MD5(const AString: String): String;
{$IF CompilerVersion>=30.0}
{$ELSE}
var
  ch: string;
{$ENDIF}
begin
{$IF CompilerVersion>=30.0}
  result := THashMD5.GetHashString(AString).ToLower;
{$ELSE}
  with TIdHashMessageDigest5.Create do
  begin
    ch := HashStringAsHex(AString);
    Free;
  end;
  result := ch.ToLower;
{$ENDIF}
end;

end.
