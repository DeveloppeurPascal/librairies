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
  Signature : 30f3273864574b39e3dd943651ba84b4b21c19f5
  ***************************************************************************
*)

unit u_version;

interface

function GetApplicationVersion: string;
function GetFileVersion(aFileName : String; var Found : Boolean) : String;

implementation

uses
    SysUtils, windows;

function GetApplicationVersion: string;
var
   trouve : boolean;
begin
  result := GetFileVersion (ParamStr (0), trouve);
end;

function GetFileVersion(aFileName : String; var Found : Boolean) : String;
var
  woMajorVersion, woMinorVersion, woSubVersion, woBuildNumber : Word;
  doInfoSize, doVersionSize : LongWord;
{$IFDEF VER100}
  Wnd : Integer;
{$ELSE}
  Wnd : Cardinal;
{$ENDIF}
  ptVersion : Pointer;
  ptFixedFileInfo : PVSFixedFileInfo;
  s : String;
begin
  s := '';
  aFileName := ExtractFileName(aFileName);
  doInfoSize := GetFileVersionInfoSize(PChar(aFileName), Wnd);
  if (doInfoSize <> 0) then begin
    GetMem(ptVersion, doInfoSize);
    try
      if GetFileVersionInfo(PChar(aFileName), Wnd, doInfoSize, ptVersion) then
        if VerQueryValue(ptVersion, '\', Pointer(ptFixedFileInfo), doVersionSize) then begin
          woMajorVersion := (ptFixedFileInfo^.dwFileVersionMS div $FFFF);
          woMinorVersion := (ptFixedFileInfo^.dwFileVersionMS and $0000FFFF);
          woSubVersion := (ptFixedFileInfo^.dwFileVersionLS div $FFFF);
          woBuildNumber := (ptFixedFileInfo^.dwFileVersionLS and $0000FFFF);
          s := Format('(v %d.%d.%d.%d)', [woMajorVersion, woMinorVersion, woSubVersion, woBuildNumber]);
          Found := True;
        end;
      {endif}
    finally
      FreeMem(ptVersion);
    end;
  end else begin
    s := '';
    Found := True;
  end;
  if s = '' then
    s := '(v inconnue)';
  {endif}
  result := s;
end;

end.
 
