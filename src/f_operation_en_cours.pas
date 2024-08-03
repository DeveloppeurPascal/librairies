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
/// File last update : 28/05/2024 12:19:15
/// Signature : 5c3a7d21c68c74b4fb0e135d068bc06d8bd70349
/// ***************************************************************************
/// </summary>

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
