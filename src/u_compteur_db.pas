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
/// File last update : 03/08/2024 22:02:10
/// Signature : 4e7cf073c0343d65c9d78fa93fe3479e174ea0b8
/// ***************************************************************************
/// </summary>

unit u_compteur_db;
{*******************************************************************************
Unit: U_Compteur_Reg
Author: Patrick Prémartin 
Date: 29 March 1998 / modified on the 9th april 1998
Comment: This unit use COMPTEUR.DB (a Paradox Database) for counters. If you use
         it, think about the BDE (Borland Database Engine).
*******************************************************************************}
interface

type
    TString80 = String [80];

function compteur_add (Application, Code: TString80): Integer;

implementation

uses
    db, dbTables, FileCtrl, SysUtils;

var
   tab_compteur: TTable;
   First_Use : boolean;

procedure initialisation;
begin
  Tab_Compteur := TTable.Create (nil);
  Tab_Compteur.Databasename := copy (ExtractFilePath (ParamStr (0)), 1, pred (length (ExtractFilePath (ParamStr (0)))))+'\Donnees';
  if (not DirectoryExists (Tab_Compteur.DatabaseName))
  then
    ForceDirectories (Tab_Compteur.DatabaseName);
  {endif}
  Tab_Compteur.TableName := 'compteur.db';
  if not FileExists (Tab_Compteur.Databasename+'\'+Tab_Compteur.TableName)
  then
    begin
      Tab_Compteur.TableType := ttParadox;
      Tab_Compteur.FieldDefs.Clear;
      Tab_Compteur.FieldDefs.Add ('Cpt_application', ftString, 80, True);
      Tab_Compteur.FieldDefs.Add ('Cpt_code', ftString, 80, True);
      Tab_Compteur.FieldDefs.Add ('Cpt_valeur', ftInteger, 0, True);
      Tab_Compteur.IndexDefs.Clear;
      Tab_Compteur.IndexDefs.Add ('', 'Cpt_application;Cpt_code', [ixPrimary, ixUnique]);
      Tab_Compteur.CreateTable;
    end;
  {endif}
  Tab_Compteur.Open;
  First_Use := false;
end; { Initialisation }

function compteur_add (Application, Code: TString80): Integer;
begin
  if First_Use
  then
    Initialisation;
  {endif}
  if Tab_Compteur.Findkey ([Application, Code])
  then
    begin
      Tab_Compteur.Edit;
      Result := Succ (Tab_Compteur.FieldByName ('Cpt_valeur').AsInteger);
      Tab_Compteur.FieldByName ('Cpt_valeur').AsInteger := Result;
      Tab_Compteur.Post;
    end
  else
    begin
      Tab_Compteur.Insert;
      Tab_Compteur.FieldByName ('Cpt_application').AsString := Application;
      Tab_Compteur.FieldByName ('Cpt_code').AsString := Code;
      Tab_Compteur.FieldByName ('Cpt_valeur').AsInteger := 1;
      Tab_Compteur.Post;
      Result := 1;
    end
  {endif}
end; { Compteur_Add }

initialization
  First_Use := True;

finalization
  tab_compteur.free;

end.
