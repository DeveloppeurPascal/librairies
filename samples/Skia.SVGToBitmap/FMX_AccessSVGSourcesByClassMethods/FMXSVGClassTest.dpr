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
/// File last update : 24/07/2024 11:17:06
/// Signature : 9e8519f6a81159d7433f7b3121c743e3a65150ab
/// ***************************************************************************
/// </summary>

program FMXSVGClassTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  fMain in 'fMain.pas' {Form5},
  Olf.Skia.SVGToBitmap in '..\..\..\src\Olf.Skia.SVGToBitmap.pas',
  USVGCursorSVGSamples in '..\_CursorSVGSamples\USVGCursorSVGSamples.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
