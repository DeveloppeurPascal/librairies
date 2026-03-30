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
  Signature : 4de3e8d02afecbacc3e28d5a3ad983caaeb12fc8
  ***************************************************************************
*)

unit uAjaxAnimation;

// (c) Patrick Prémartin / Olf Software 02/2017

interface

uses
  fmx.objects;

procedure ajax_animation_set(zone: TRectangle);
procedure ajax_animation_on;
procedure ajax_animation_off(reinitialiser: boolean = false);

implementation

var
  compteur: integer;
  zoneAjax: TRectangle;

procedure ajax_animation_set(zone: TRectangle);
begin
  zoneAjax := zone;
end;

procedure ajax_animation_on;
begin
  if Assigned(zoneAjax) then
  begin
    if (compteur < 1) then
    begin
      compteur := 1;
      zoneAjax.visible := true;
      zoneAjax.BringToFront;
    end
    else
      inc(compteur);
  end;
end;

procedure ajax_animation_off(reinitialiser: boolean = false);
begin
  if Assigned(zoneAjax) then
  begin
    if reinitialiser then
    begin
      compteur := 0;
      zoneAjax.visible := false;
    end
    else if (compteur > 1) then
      dec(compteur)
    else
    begin
      compteur := 0;
      zoneAjax.visible := false;
    end;
  end;
end;

initialization

compteur := 0;
zoneAjax := nil;

end.
