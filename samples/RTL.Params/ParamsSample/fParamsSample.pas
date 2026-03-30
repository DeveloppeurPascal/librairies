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
  Signature : 705a78dfe2817103308e4a358101429576376222
  ***************************************************************************
*)

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
