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
  Signature : 524f37726d81397e661c831fde42e5f7848b3bd8
  ***************************************************************************
*)

Unit U_Matrices;             
{ mise … jour : 29 - 12 - 1990 }

{
Cette unit‚ contient diverses proc‚dures et fonctions de calcul matriciel.
Elle utilise l'unit‚ 'Var_Matrices' qui contient les d‚clarations de types.
}

Interface

Uses Var_Matrices;

Procedure Mat_Addition (Var Mat1, Mat2, Mat3 : T_Matrice);
          { Mat3 := Mat1 + Mat2 }
Procedure Mat_Soustrait (Var Mat1, Mat2, Mat3 : T_Matrice);
          { Mat3 := Mat1 - Mat2 }

Implementation
Procedure Mat_Addition (Var Mat1, Mat2, Mat3 : T_Matrice);
Var I, J : Integer;
Begin { mat_addition }
  For I := 1 To Nb_Max_Lig Do
    For J := 1 To Nb_Max_Col Do
      Mat3 [I, J] := Mat1 [I, J] + Mat2 [I, J]
    {endfor}
  {endfor}
End; { mat_addition }

Procedure Mat_Soustrait (Var Mat1, Mat2, Mat3 : T_Matrice);
Var I, J : Integer;
Begin { mat_soustrait }
  For I := 1 To Nb_Max_Lig Do
    For J := 1 To Nb_Max_Col Do
      Mat3 [I, J] := Mat1 [I, J] - Mat2 [I, J]
    {endfor}
  {endfor}
End; { mat_soustrait }

End. { Fin de l'unit‚ 'Matrices'. }

