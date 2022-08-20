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
