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
  File last update : 2026-03-30T16:35:19.678+02:00
  Signature : 2c10818f39919599fe50dcc3b3f64e5f6f871bf0
  ***************************************************************************
*)

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
