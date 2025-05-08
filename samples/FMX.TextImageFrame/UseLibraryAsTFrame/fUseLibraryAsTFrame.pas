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
/// File last update : 2025-05-08T16:59:08.000+02:00
/// Signature : 17a89a586b1c2e65b91b1fd53dc94a4b19693623
/// ***************************************************************************
/// </summary>

unit fUseLibraryAsTFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  Olf.FMX.TextImageFrame;

type
  TForm1 = class(TForm)
    OlfFMXTextImageFrame1: TOlfFMXTextImageFrame;
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    function ConvertUnknownChars(Sender: TOlfFMXTextImageFrame;
      AChar: char): integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  udm_CharacterImages;

function TForm1.ConvertUnknownChars(Sender: TOlfFMXTextImageFrame;
  AChar: char): integer;
begin
  result := -1;
  if charinset(AChar, ['a' .. 'z']) then // _a, _b, _c in source images
    result := Sender.getImageIndexOfChar('_' + AChar);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OlfFMXTextImageFrame1.OnGetImageIndexOfUnknowChar := ConvertUnknownChars;
  OlfFMXTextImageFrame1.Font := dm_CharacterImages.ImageList;
  OlfFMXTextImageFrame1.Text := '123 abc ABC';
end;

end.
