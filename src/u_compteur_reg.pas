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
/// File last update : 03/08/2024 22:02:17
/// Signature : 0ce23dbaa00d8468e9c52603a97a0fdf0c13602b
/// ***************************************************************************
/// </summary>

unit u_compteur_reg;
{*******************************************************************************
Unit: U_Compteur_Reg
Author: Patrick Prémartin 
Date: 29 March 1998
Comment: This unit use Windows registry for counters.
*******************************************************************************}
interface

type
    TString80 = String [80];

function compteur_add (Application, Code: TString80): Integer;

implementation

uses
    Windows,
    Registry;

const
     NomDeLaCle : string = '\Software\Olf Software\Counter';

var
   BaseDeRegistres: TRegistry;

function compteur_add (Application, Code: TString80): Integer;
var
   ch : string;
begin
  ch := Application+' * '+Code;
  try
    Result := BaseDeRegistres.ReadInteger (ch);
  except
    Result := 0;
  end;
  inc (Result);
  BaseDeRegistres.WriteInteger (ch, Result);
end; { Compteur_Add }

initialization
  BaseDeRegistres := TRegistry.Create;
  BaseDeRegistres.RootKey := HKEY_Local_Machine;
  BaseDeRegistres.OpenKey (NomDeLaCle, not BaseDeRegistres.KeyExists (NomDeLaCle));

finalization
  BaseDeRegistres.Free;

end.
