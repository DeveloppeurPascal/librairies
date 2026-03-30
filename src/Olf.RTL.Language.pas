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
  Signature : 58918830aeabf3c420c90a4be3f496c637c68efe
  ***************************************************************************
*)

unit Olf.RTL.Language;

interface

/// <summary>
/// Get current language code (like "fr-fr" or "en-gb")
/// The result is lower cased.
/// </summary>
function GetCurrentLanguageCode: String;

/// <summary>
/// Get current language ISO code (like "fr" or "en")
/// This function returns first 2 letters of GetcurrentLanguageCode.
/// The result is lower cased.
/// </summary>
function GetCurrentLanguageISOCode: String;

implementation

// Created from this forum posts :
// https://forums.embarcadero.com/thread.jspa?threadID=108333
// http://www.synaptica.info/en/2015/12/21/delphi-10seattle-get-current-device-language/
// http://codeverge.com/embarcadero.delphi.firemonkey/detect-current-language-on-andr/2001235#sthash.zjLIi2KY.dpuf

{$IF Defined(MSWINDOWS)}

uses Winapi.Windows, System.SysUtils;
{$ELSEIF Defined(IOS)}

uses MacAPI.ObjectiveC, iOSapi.Foundation, System.SysUtils;
{$ELSEIF Defined(MACOS)}

uses MacAPI.ObjectiveC, MacAPI.Foundation, System.SysUtils;
{$ELSE}

// Android + Linux
uses FMX.Platform, System.SysUtils;
{$ENDIF}

function GetCurrentLanguageCode: String;
{$IF Defined(MSWINDOWS)}
var
  buffer: PWideChar;
  UserLCID: LCID;
  BufLen: Integer;
begin
  // defaults
  UserLCID := GetUserDefaultLCID;
  BufLen := GetLocaleInfo(UserLCID, LOCALE_SISO639LANGNAME, nil, 0);
  buffer := StrAlloc(BufLen);
  if GetLocaleInfo(UserLCID, LOCALE_SISO639LANGNAME, buffer, BufLen) <> 0 then
    Result := lowercase(buffer)
  else
    Result := 'en';
  StrDispose(buffer);
end;
{$ELSEIF Defined(MACOS) or Defined(IOS)}

var
  Languages: NSArray;
begin
  Languages := TNSLocale.OCClass.preferredLanguages;
  Result := String(TNSString.Wrap(Languages.objectAtIndex(0)).lowercaseString.UTF8String);
end;
{$ELSE}

var
  LocServ: IFMXLocaleService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService,
    IInterface(LocServ)) then
    Result := LocServ.GetCurrentLangID;
end;
{$ENDIF}

function GetCurrentLanguageISOCode: String;
begin
  Result := GetCurrentLanguageCode.Substring(0, 2);
end;

end.
