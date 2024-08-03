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
/// Signature : 045021bb989bb75df38ef8a4d871dd60d970f531
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
  FMX.Layouts,
  FMX.Edit,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  Olf.RTL.Params;

type
  TForm1 = class(TForm)
    btnLoad: TButton;
    btnSave: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    GridPanelLayout1: TGridPanelLayout;
    procedure FormCreate(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure BeforeLoad(Const AParamsFile: TParamsFile);
    procedure AfterLoad(Const AParamsFile: TParamsFile);
    procedure BeforeSave(Const AParamsFile: TParamsFile);
    procedure AfterSave(Const AParamsFile: TParamsFile);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.AfterLoad(const AParamsFile: TParamsFile);
begin
  Memo1.lines.add('AfterLoadEvent : ' + AParamsFile.ToJSON);
end;

procedure TForm1.AfterSave(const AParamsFile: TParamsFile);
begin
  if not assigned(self.owner) then
    exit;
  Memo1.lines.add('AfterSaveEvent : ' + AParamsFile.ToJSON);
end;

procedure TForm1.BeforeLoad(const AParamsFile: TParamsFile);
begin
  Memo1.lines.add('BeforeLoadEvent : ' + AParamsFile.ToJSON);
end;

procedure TForm1.BeforeSave(const AParamsFile: TParamsFile);
begin
  if not assigned(self.owner) then
    exit;
  Memo1.lines.add('BeforeSaveEvent : ' + AParamsFile.ToJSON);
end;

procedure TForm1.btnLoadClick(Sender: TObject);
begin
  tparams.load;
  Memo1.lines.add(tparams.ToJSON);
end;

procedure TForm1.btnSaveClick(Sender: TObject);
begin
  tparams.setValue('val' + random(20).ToString, random(maxint));
  tparams.save;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit1.Text := tparams.getFilePath;
  tparams.onBeforeLoadEvent := BeforeLoad;
  tparams.onAfterLoadEvent := AfterLoad;
  tparams.onBeforeSaveEvent := BeforeSave;
  tparams.onAfterSaveEvent := AfterSave;
end;

initialization

finalization

tparams.onbeforesaveproc := procedure(const AParamsFile: TParamsFile)
  begin
    AParamsFile.onBeforeSaveEvent := nil;
    AParamsFile.onAfterSaveEvent := nil;
  end;

end.
