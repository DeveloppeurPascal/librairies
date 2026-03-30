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
  File last update : 2026-03-30T16:35:19.664+02:00
  Signature : 8d312ae231f6fa1767b08a6dd812bb953f083862
  ***************************************************************************
*)

unit Olf.RTL.SystemAppearance;

interface

function isSystemThemeInLightMode: boolean;
function isSystemThemeInDarkMode: boolean;

implementation

{$IF NOT Defined(MSWINDOWS)}

uses
  FMX.Platform;
{$ELSE}
// TODO : intercepter le changement de couleur du système et le signaler sous forme de message à l'application en cours

uses
  winapi.Windows,
  System.Win.Registry;

const
  CRegKey = 'Software\Microsoft\Windows\CurrentVersion\Themes\Personalize';
  CRegValueApps = 'AppsUseLightTheme';
  CRegValueSystem = 'SystemUsesLightTheme';
{$ENDIF}

function isSystemThemeInLightMode: boolean;
{$IF NOT Defined(MSWINDOWS)}
var
  InfoScreen: IFMXSystemAppearanceService;
{$ELSE}
var
  reg: TRegistry;
{$ENDIF}
begin
{$IF NOT Defined(MSWINDOWS)}
  if TPlatformServices.Current.SupportsPlatformService
    (IFMXSystemAppearanceService, InfoScreen) then
    result := (InfoScreen.ThemeKind = TSystemThemeKind.Light)
  else
    result := true;
{$ELSE}
  // inspired by unit WindowsDarkMode.pas from Ian Barker at https://github.com/checkdigits/delphidarkmode
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.KeyExists(CRegKey) and reg.OpenKey(CRegKey, False) then
      try
        if reg.ValueExists(CRegValueApps) then
          result := reg.ReadInteger(CRegValueApps) = 1
        else if reg.ValueExists(CRegValueSystem) then
          result := reg.ReadInteger(CRegValueApps) = 1
        else
          result := true;
      finally
        reg.CloseKey;
      end
    else
      result := true;
  finally
    reg.Free;
  end;
{$ENDIF}
end;

function isSystemThemeInDarkMode: boolean;
{$IF NOT Defined(MSWINDOWS)}
var
  InfoScreen: IFMXSystemAppearanceService;
{$ELSE}
var
  reg: TRegistry;
{$ENDIF}
begin
{$IF NOT Defined(MSWINDOWS)}
  result := TPlatformServices.Current.SupportsPlatformService
    (IFMXSystemAppearanceService, InfoScreen) and
    (InfoScreen.ThemeKind = TSystemThemeKind.Dark);
{$ELSE}
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.KeyExists(CRegKey) and reg.OpenKey(CRegKey, False) then
      try
        if reg.ValueExists(CRegValueApps) then
          result := reg.ReadInteger(CRegValueApps) = 0
        else if reg.ValueExists(CRegValueSystem) then
          result := reg.ReadInteger(CRegValueApps) = 0
        else
          result := False;
      finally
        reg.CloseKey;
      end
    else
      result := False;
  finally
    reg.Free;
  end;
{$ENDIF}
end;

end.
