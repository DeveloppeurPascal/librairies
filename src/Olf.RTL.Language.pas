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
/// File last update : 03/08/2024 22:01:25
/// Signature : 2c5550e3b008903c964c02a8627018617f65dbd7
/// ***************************************************************************
/// </summary>

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
  Result := lowercase(TNSString.Wrap(Languages.objectAtIndex(0)).UTF8String);
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
