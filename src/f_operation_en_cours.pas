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
  File last update : 2026-03-30T16:35:19.666+02:00
  Signature : 55ab952b30570b9cd118eda55cc6a9e4ddcd1a67
  ***************************************************************************
*)

unit f_operation_en_cours;

interface

uses System.Classes, Vcl.Controls, Vcl.ComCtrls;

Procedure oec_Ouverture(nb_operation: cardinal);
Procedure oec_Operation_Suivante;
Procedure oec_Fermeture;

implementation

{$R *.DFM}

uses
  Windows, Messages, SysUtils, Graphics, Forms, Dialogs;

type
  Tfrm = class(TForm)
    ProgressBar1: TProgressBar;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  frm: Tfrm;

Procedure oec_Ouverture(nb_operation: cardinal);
begin
  if (frm = nil) then
    frm := Tfrm.Create(Nil);
  try
    frm.ProgressBar1.Min := 0;
    frm.ProgressBar1.Max := nb_operation;
    frm.ProgressBar1.Position := 0;
    frm.Show;
  except
    try
      frm.Free;
    finally
      frm := nil;
    end;
  end;
end;

Procedure oec_Operation_Suivante;
begin
  if (frm = nil) then
    oec_Ouverture(0);
  try
    frm.ProgressBar1.StepIt;
  except
    try
      frm.Free;
    finally
      frm := nil;
    end;
  end;
end;

Procedure oec_Fermeture;
begin
  if (frm = nil) then
    oec_Ouverture(0);
  try
    frm.Hide;
  except
    try
      frm.Free;
    finally
      frm := nil;
    end;
  end;
end;

initialization

frm := nil;

finalization

if (frm <> nil) then
begin
  frm.Free;
  frm := nil;
end;

{ endif }
end.
