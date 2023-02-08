unit Olf.RTL.Language;

// (c) Patrick Prémartin 02/2023
//
// This file is distributed under AGPL license.
//
// Get the latest version at
// https://github.com/DeveloppeurPascal/librairies

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
