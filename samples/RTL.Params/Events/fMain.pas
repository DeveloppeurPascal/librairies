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
  File last update : 2026-03-30T16:35:19.584+02:00
  Signature : b9892c64e29065a903791fb3bc6b4d994ea0e56a
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
