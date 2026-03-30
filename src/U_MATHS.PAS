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
  Signature : 5b41d14e166eac517f353fe51d06b47af23a22c6
  ***************************************************************************
*)

Unit U_Maths;               
 { mise … jour : 16 - 12 - 1990. }
Interface
Function Puissance_Ent (X : Real; N : Integer): Real;
         { calcul de x^n avec x r‚Šl et n entier }
Function Sin_Trapeze (A, B : Real; N : Integer): Real;
         { calcul l'int‚grale du sinus entre A et B avec N divisions par la
         m‚thode des trapŠzes. }

{ Fin de l'interface. }

Implementation
Function Puissance_Ent (X : Real; N : Integer): Real;
Var Y : Real;
Begin { puissance_ent }
  Y := 1.0;
  While Not (N=0) Do
    Begin
      If Odd(N)
      Then
        Begin
          Y := Y * X;
          Dec (N);
        End
      Else
        Begin
          X := Sqr (X);
          N := N Div 2
        End
      {endif}
    End;
  Puissance_Ent := Y
End; { puissance_ent }

Function Sin_Trapeze (A, B : Real; N : Integer): Real;
Var I    : Integer;
    P, Y : Real;
Begin { sin_trapeze }
  P := (B-A)/N;
  Y := (Sin (A)+Sin (B))/2.0;
  For I := 1 To (N-1) Do
    Begin
      A := A+P;
      Y := Y+Sin (A)
    End;
  {endfor}
  Sin_Trapeze := P*Y
End; { sin_trapeze }

End. { Maths }

