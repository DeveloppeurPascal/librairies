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
/// Signature : ddaa6cfc6c6779944c8d61a955443ae02a7e38be
/// ***************************************************************************
/// </summary>

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
 
