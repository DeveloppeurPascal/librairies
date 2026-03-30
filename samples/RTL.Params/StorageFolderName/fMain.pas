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
  File last update : 2026-03-30T16:35:19.590+02:00
  Signature : f401283c4c193c3abaab26bf1733092a606c87b5
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
