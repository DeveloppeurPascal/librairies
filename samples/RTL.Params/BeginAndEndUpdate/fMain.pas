/// <summary>
/// ***************************************************************************
///
/// Librairies pour Delphi
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
/// File last update : 2025-01-13T19:08:18.000+01:00
/// Signature : 5ec5841f1a91a785dbe818f1293fcb300c9166a1
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
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo;

type
  TfrmMain = class(TForm)
    Memo1: TMemo;
    GridPanelLayout1: TGridPanelLayout;
    btnBeginUpdate: TButton;
    btnEndUpdate: TButton;
    btnCreateParams: TButton;
    btnLoadParams: TButton;
    btnSaveParams: TButton;
    btnClearParams: TButton;
    btnDeleteParams: TButton;
    btnCancel: TButton;
    procedure btnBeginUpdateClick(Sender: TObject);
    procedure btnEndUpdateClick(Sender: TObject);
    procedure btnCreateParamsClick(Sender: TObject);
    procedure btnLoadParamsClick(Sender: TObject);
    procedure btnSaveParamsClick(Sender: TObject);
    procedure btnClearParamsClick(Sender: TObject);
    procedure btnDeleteParamsClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
  public
    procedure AddLog(const txt: string);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  Olf.RTL.Params;

procedure TfrmMain.AddLog(const txt: string);
begin
  if not txt.isempty then
    Memo1.lines.add(txt);
  Memo1.lines.add('HasChanged=' + tparams.HasChanged.tostring);
  Memo1.lines.add(tparams.ToJSON);
  Memo1.GoToTextEnd;
end;

procedure TfrmMain.btnBeginUpdateClick(Sender: TObject);
begin
  AddLog('BeginUpdate');
  tparams.BeginUpdate;
end;

procedure TfrmMain.btnCancelClick(Sender: TObject);
begin
  AddLog('Cancel (= reload)');
  tparams.Cancel;
  AddLog('');
end;

procedure TfrmMain.btnClearParamsClick(Sender: TObject);
begin
  AddLog('Clear');
  tparams.Clear;
  AddLog('');
end;

procedure TfrmMain.btnCreateParamsClick(Sender: TObject);
var
  i: integer;
begin
  AddLog('Create some params');
  for i := 1 to 10 do
    tparams.setValue('key' + random(50).tostring, random(512));
  AddLog('');
end;

procedure TfrmMain.btnDeleteParamsClick(Sender: TObject);
begin
  AddLog('Deleted the file and cleared the memory');
  tparams.Delete;
  AddLog('');
end;

procedure TfrmMain.btnEndUpdateClick(Sender: TObject);
begin
  AddLog('EndUpdate');
  tparams.EndUpdate;
  AddLog('');
end;

procedure TfrmMain.btnLoadParamsClick(Sender: TObject);
begin
  AddLog('Load');
  tparams.Load;
  AddLog('');
end;

procedure TfrmMain.btnSaveParamsClick(Sender: TObject);
begin
  AddLog('Save');
  tparams.save;
  AddLog('');
end;

end.
