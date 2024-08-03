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
/// Signature : 146e81cdb3fe752051e37a8ab7e0cbeb1ab84ccb
/// ***************************************************************************
/// </summary>

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
