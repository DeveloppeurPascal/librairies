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
