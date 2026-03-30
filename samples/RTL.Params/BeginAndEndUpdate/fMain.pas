(* C2PP
  ***************************************************************************

  My libraries for Delphi
  Copyright (c) 1990-2026 Patrick PREMARTIN

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

  ***************************************************************************

  This repository contains functions, procedures and classes to use in
  Delphi projects (console, VCL, FireMonkey and others). It's my "everything reuseable things" toolbox.

  The units to be used in your projects can be found in the "src" folder.
  Some features are explained on my blog or have been coded live on Twitch.

  Examples of use in the form of VCL or FireMonkey projects are available in
  the "samples" subfolder.

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://librairies.developpeur-pascal.fr

  Project site :
  https://github.com/DeveloppeurPascal/librairies

  ***************************************************************************
  File last update : 2026-03-30T16:35:19.581+02:00
  Signature : 2aef5db5c5507ed777b88f7f49986d0b30269d78
  ***************************************************************************
*)

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
