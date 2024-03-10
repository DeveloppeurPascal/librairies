unit u_niveau_bdd;
{*******************************************************************************
Unit: U_Niveau_Bdd
Author: Patrick Prémartin (pprem@lenet.net or pprem@infonie.fr)
Date: 15 May 1998
Comment: This unit copy all Paradox files (*.db) from a folder to an other.
*******************************************************************************}
interface

procedure Verifier_Niveau_Base_de_Donnees (dossier1, dossier2: string);

implementation

uses
    Windows, Dialogs, SysUtils, FMXUtils, FileCtrl;

Function Nom_Fichier_Generique (s : string): string;
var
   n : integer;
begin
  n := pos ('.', s);
  if (n > 0)
  then
    Result := copy (s, 1, n)+Nom_Fichier_Generique (copy (s, Succ(n), length (s)-n))
  else
    Result := '*';
  {endif}
end; { Nom_Fichier_Generique }

procedure Verifier_Niveau_Base_de_Donnees (dossier1, dossier2: string);
var
   f, f2 : TSearchRec;
   res: integer;
begin
  { On vire le '\' en fin de dossier ! }
  while (dossier1 [length (dossier1)] = '\') do delete (dossier1, length (Dossier1), 1);
  while (dossier2 [length (dossier2)] = '\') do delete (dossier2, length (Dossier2), 1);

  { Si le dossier de départ n'existe pas, on laisse tomber ! }
  if not DirectoryExists (Dossier1)
  then
    exit;
  {endif}

  { Si le dossier d'arrivée n'existe pas, on le crée ! }
  if not DirectoryExists (Dossier2)
  then
    ForceDirectories (dossier2);
  {endif}

  res := FindFirst (dossier1+'\*.db', faReadOnly+faArchive, f);
  while (res = 0) do
    begin
      if (not FileExists (dossier2+'\'+f.Name))
      then
        begin
          res := FindFirst (dossier1+'\'+Nom_Fichier_Generique (ExtractFileName (f.Name)), faReadOnly+faArchive, f2);
          while (res = 0) do
            begin
              CopyFile (dossier1+'\'+f2.Name, Dossier2);
              res := FindNext (f2);
            end;
          {endwhile}
          FindClose (f2);
        end;
      {endif}
      res := FindNext (f);
    end;
  {endwhile}
  FindClose (f);
end; { Verifier_Niveau_Base_de_Donnees }

end.
