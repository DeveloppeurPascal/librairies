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
  Signature : e3a2534017ed647d4dac76c16ac9d7157eaaa921
  ***************************************************************************
*)

unit uDivers;

// (c) Patrick Prémartin / Olf Software 05/2016

interface

uses
  FMX.Forms;

procedure intervertir_affichage(fiche_actuelle, fiche_a_activer: TForm);

implementation

procedure intervertir_affichage(fiche_actuelle, fiche_a_activer: TForm);
begin
  fiche_a_activer.Caption := fiche_actuelle.Caption;
  fiche_a_activer.Left := fiche_actuelle.Left;
  fiche_a_activer.Top := fiche_actuelle.Top;
  fiche_a_activer.Width := fiche_actuelle.Width;
  fiche_a_activer.Height := fiche_actuelle.Height;
  fiche_a_activer.Show;
  fiche_actuelle.Hide;
end;

end.
