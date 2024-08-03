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
/// Signature : 78c52d79c1bf2833d16001f1b0ef7127a3e1c25d
/// ***************************************************************************
/// </summary>

unit fMain;

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
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.Layouts;

type
  TfrmMain = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    btnDefaultFolder: TButton;
    btnDefaultFolderV2: TButton;
    Memo1: TMemo;
    GridPanelLayout1: TGridPanelLayout;
    btnCreateParamFile: TButton;
    btnDeleteParamFile: TButton;
    btnClear: TButton;
    procedure btnDefaultFolderClick(Sender: TObject);
    procedure btnDefaultFolderV2Click(Sender: TObject);
    procedure btnCreateParamFileClick(Sender: TObject);
    procedure btnDeleteParamFileClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  Olf.RTL.Params;

procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  TParams.Clear;
  TParams.save;
  Memo1.lines.add(TParams.ToJSON);
end;

procedure TfrmMain.btnCreateParamFileClick(Sender: TObject);
begin
  TParams.setValue(random(500).tostring, random(500));
  TParams.save;
  if tfile.exists(TParams.getFilePath) then
    Memo1.lines.add('params saved in ' + TParams.getFilePath)
  else
    Memo1.lines.add(TParams.getFilePath + ' doesn''t exist');
  Memo1.lines.add(TParams.ToJSON);
end;

procedure TfrmMain.btnDefaultFolderClick(Sender: TObject);
begin
  TParams.setFilePath('');
  Memo1.lines.add(TParams.getFilePath);
end;

procedure TfrmMain.btnDefaultFolderV2Click(Sender: TObject);
begin
  TParams.InitDefaultFileNameV2(Edit1.Text, Edit2.Text);
  Memo1.lines.add(TParams.getFilePath);
end;

procedure TfrmMain.btnDeleteParamFileClick(Sender: TObject);
begin
  if tfile.exists(TParams.getFilePath) then
  begin
    Memo1.lines.add(TParams.getFilePath + ' exists');
    TParams.Delete;
    if tfile.exists(TParams.getFilePath) then
      Memo1.lines.add('can''t delete ' + TParams.getFilePath)
    else
      Memo1.lines.add(TParams.getFilePath + ' deleted');
  end
  else
    Memo1.lines.add(TParams.getFilePath + ' doesn''t exist');
  Memo1.lines.add(TParams.ToJSON);
end;

end.
