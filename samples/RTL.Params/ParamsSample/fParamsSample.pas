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
/// Signature : b6d2025c94629398cadd6b68d4995d0ecb55c77b
/// ***************************************************************************
/// </summary>

unit fParamsSample;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses System.IOUtils, Olf.RTL.Params;

procedure TForm1.Button1Click(Sender: TObject);
begin
  tParams.setValue('Edit1Text', Edit1.Text);
  Memo1.Lines.Add('Edit1Text=' + Edit1.Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit1.Text := tParams.getValue('Edit1Text', 'Default Value');
  Memo1.Lines.Add('Edit1Text=' + Edit1.Text);
end;

procedure initParamsFile;
var
  NewFolder, Oldfolder, FileName: string;
begin
  Oldfolder := tpath.Combine(tpath.GetPicturesPath, 'temp');
  NewFolder := tpath.Combine(tpath.GetDocumentsPath, 'temp');
  FileName := tpath.GetFileName(tParams.getFilePath);

  // Move actual Pictures/Temp parameter file to new Documents/Temp parameter file
  if tfile.Exists(tpath.Combine(Oldfolder, FileName)) then
  begin
    tParams.setFolderName(Oldfolder);
    tParams.MoveToFilePath(tpath.Combine(NewFolder, FileName), true, true);
  end
  else
    tParams.setFolderName(NewFolder);
end;

initialization

initParamsFile;

end.
