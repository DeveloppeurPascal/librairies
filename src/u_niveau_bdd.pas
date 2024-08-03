/// <summary>
/// ***************************************************************************
///
/// Librairies pour Delphi
///
/// Copyright 1990-2024 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// This repository contains functions, procedures and classes to use in
/// Delphi projects (console, VCL, FireMonkey and others). It's my "everything reuseable things" toolbox.
///
/// The units to be used in your projects can be found in the "src" folder.
/// Some features are explained on my blog or have been coded live on Twitch.
///
/// Examples of use in the form of VCL or FireMonkey projects are available in
/// the "samples" subfolder.
///
/// ***************************************************************************
///
/// Author(s) :
///      Patrick PREMARTIN
///
/// Site :
///      https://developpeur-pascal.fr/librairies-publiques.html
///
/// Project site :
///      https://github.com/DeveloppeurPascal/librairies
///
/// ***************************************************************************
/// File last update : 03/08/2024 22:03:21
/// Signature : 011b288101c0b895bd26f987495b45733d6f4634
/// ***************************************************************************
/// </summary>

unit u_niveau_bdd;
{*******************************************************************************
Unit: U_Niveau_Bdd
Author: Patrick Prémartin 
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
