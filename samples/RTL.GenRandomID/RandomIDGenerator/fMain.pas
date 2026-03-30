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
  File last update : 2026-03-30T16:35:19.567+02:00
  Signature : 0a2d60cb7c821e99fdc2c4b8af3e7ace2dacc025
  ***************************************************************************
*)

unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Layouts;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    GridPanelLayout1: TGridPanelLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses Olf.RTL.GenRandomID;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.lines.add('Base 2 => ' + TOlfRandomIDGenerator.getidbase2
    (random(20) + 5));
  Memo1.GoToTextEnd;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.lines.add('Base 10 => ' + TOlfRandomIDGenerator.getidbase10
    (random(20) + 5));
  Memo1.GoToTextEnd;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Memo1.lines.add('Base 36 => ' + TOlfRandomIDGenerator.getIDBase36
    (random(20) + 5));
  Memo1.GoToTextEnd;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Memo1.lines.add('Base 62 => ' + TOlfRandomIDGenerator.getIDBase62
    (random(20) + 5));
  Memo1.GoToTextEnd;
end;

end.
