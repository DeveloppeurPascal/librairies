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
