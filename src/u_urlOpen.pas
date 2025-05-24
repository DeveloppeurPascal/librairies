/// <summary>
/// ***************************************************************************
///
/// My libraries for Delphi
///
/// Copyright 1990-2025 Patrick Prémartin under AGPL 3.0 license.
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
/// Patrick PREMARTIN
///
/// Site :
/// https://librairies.developpeur-pascal.fr
///
/// Project site :
/// https://github.com/DeveloppeurPascal/librairies
///
/// ***************************************************************************
/// File last update : 2025-05-24T12:25:22.000+02:00
/// Signature : d44d0958b693360fbd7599448e3e12415aaa8278
/// ***************************************************************************
/// </summary>

unit u_urlOpen;

interface

/// <summary>Launch URL website on the default browser</summary>
/// <param name="URL">Absolute address of the website to open in the web browser</param>
procedure url_Open_In_Browser(URL: string);

implementation

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants
{$IF Defined(IOS)}
    ,
  macapi.helpers,
  iOSapi.Foundation,
  FMX.helpers.iOS
{$ELSEIF Defined(ANDROID)}
    ,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.helpers
{$ELSEIF Defined(MACOS)}
    ,
  Posix.Stdlib
{$ELSEIF Defined(MSWINDOWS)}
    ,
  Winapi.ShellAPI,
  Winapi.Windows
{$ENDIF};

procedure url_Open_In_Browser(URL: string);
{$IF Defined(ANDROID)}
var
  Intent: JIntent;
{$ENDIF}
begin
{$IF Defined(ANDROID)}
  Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
  Intent.setData(StrToJURI(URL));
  // SharedActivity.startActivity(Intent);
  tandroidhelper.Activity.startActivity(Intent);
{$ELSEIF Defined(MSWINDOWS)}
  ShellExecute(0, 'OPEN', PWideChar(URL), nil, nil, SW_SHOWNORMAL);
{$ELSEIF Defined(IOS)}
  SharedApplication.OpenURL(StrToNSUrl(URL));
{$ELSEIF Defined(MACOS)}
  _system(PAnsiChar('open ' + AnsiString(URL)));
{$ENDIF}
end;

end.
