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
  Signature : 7889ffb1988e19ee014af3d17f4abdd1eca53640
  ***************************************************************************
*)

Unit U_Tris;                 

{
Auteur: Pr‚martin Patrick
Date: 28/12/1990.
      19/05/1991.

Cette unit‚ contient diverses proc‚dures de tris. Elle utilise l'unit‚
'Var_Tris' qui contient quand … elle les d‚clarations de type (modifiables) …
utiliser ici.
}

Interface

Uses Var_Tris;

{
Ces deux proc‚dures echangent les valeurs de Elem_1 et de Elem_2.
}
Procedure Swap_1 (Var Elem_1, Elem_2 : T_Variable_1);
Procedure Swap_2 (Var Elem_1, Elem_2 : T_Variable_2);

{
Methode du tri par permutation:
   dŠs que l'on trouve un element plus petit (ou plus grand) que celui sur lequel
   on est, on fait une permutation de ces deux elements.
}
{ tri croissant }
Procedure Tri_Permutation_Up_1 (N : Integer; Var Vecteur : T_Tabl_Variable_1);
Procedure Tri_Permutation_Up_2 (N : Integer; Var Vecteur : T_Tabl_Variable_2);
{ tri d‚croissant }
Procedure Tri_Permutation_Down_1 (N : Integer; Var Vecteur : T_Tabl_Variable_1);
Procedure Tri_Permutation_Down_2 (N : Integer; Var Vecteur : T_Tabl_Variable_2);

{
Methode du tri echage (ou tri bulle):
   balayage du vecteur, si deux elements consecutifs sont mal ranges, on les
   inverse.
}
{ tri croissant }
Procedure Tri_Echange_Up_1 (N : Integer; Var Vecteur : T_Tabl_Variable_1);
Procedure Tri_Echange_Up_2 (N : Integer; Var Vecteur : T_Tabl_Variable_2);
{ tri decroissant }
Procedure Tri_Echange_Down_1 (N : Integer; Var Vecteur : T_Tabl_Variable_1);
Procedure Tri_Echange_Down_2 (N : Integer; Var Vecteur : T_Tabl_Variable_2);

{
Methode de Quick Sort.
}
{ tri croissant }
Procedure Tri_Quicksort_Up_1 (N : Integer; Var Vecteur : T_Tabl_Variable_1);
Procedure Tri_Quicksort_Up_2 (N : Integer; Var Vecteur : T_Tabl_Variable_2);
{ tri decroissant }
Procedure Tri_Quicksort_Down_1 (N : Integer; Var Vecteur : T_Tabl_Variable_1);
Procedure Tri_Quicksort_Down_2 (N : Integer; Var Vecteur : T_Tabl_Variable_2);

Implementation
Procedure Swap_1 (Var Elem_1, Elem_2 : T_Variable_1);
Var Elem_3 : T_Variable_1;
Begin { swap_1 }
  Elem_3 := Elem_1;
  Elem_1 := Elem_2;
  Elem_2 := Elem_3
End; { swap_1 }

Procedure Swap_2 (Var Elem_1, Elem_2 : T_Variable_2);
Var Elem_3 : T_Variable_2;
Begin { swap_2 }
  Elem_3 := Elem_1;
  Elem_1 := Elem_2;
  Elem_2 := Elem_3
End; { swap_2 }

Procedure Tri_Permutation_Up_1 (N : Integer; Var Vecteur : T_Tabl_Variable_1);
Var I, J : Integer;
    Ax   : T_Variable_1;
Begin { tri_permutation_up_1 }
  I := 1;
  While Not (I = N) Do
    Begin
      J := Succ (I);
      While Not (J > N) Do
        Begin
          If (Vecteur [J] < Vecteur [I])
          Then
            Swap_1 (Vecteur [I], Vecteur [J])
          {endif};
          Inc (J)
        End
      {endwhile};
      Inc (I)
    End
  {endwhile};
End; { tri_permutation_up_1 }

Procedure Tri_Permutation_Up_2 (N : Integer; Var Vecteur : T_Tabl_Variable_2);
Var I, J : Integer;
    Ax   : T_Variable_2;
Begin { tri_permutation_up_2 }
  I := 1;
  While Not (I = N) Do
    Begin
      J := Succ (I);
      While Not (J > N) Do
        Begin
          If (Vecteur [J] < Vecteur [I])
          Then
            Swap_2 (Vecteur [J], Vecteur [I])
          {endif};
          Inc (J)
        End
      {endwhile};
      Inc (I)
    End
  {endwhile};
End; { tri_permutation_up_2 }

Procedure Tri_Permutation_Down_1 (N : Integer; Var Vecteur : T_Tabl_Variable_1);
Var I, J : Integer;
    Ax   : T_Variable_1;
Begin { tri_permutation_down_1 }
  I := 1;
  While Not (I = N) Do
    Begin
      J := Succ (I);
      While Not (J > N) Do
        Begin
          If (Vecteur [J] > Vecteur [I])
          Then
            Swap_1 (Vecteur [I], Vecteur [J])
          {endif};
          Inc (J)
        End
      {endwhile};
      Inc (I)
    End
  {endwhile};
End; { tri_permutation_down_1 }

Procedure Tri_Permutation_Down_2 (N : Integer; Var Vecteur : T_Tabl_Variable_2);
Var I, J : Integer;
    Ax   : T_Variable_2;
Begin { tri_permutation_down_2 }
  I := 1;
  While Not (I = N) Do
    Begin
      J := Succ (I);
      While Not (J > N) Do
        Begin
          If (Vecteur [J] > Vecteur [I])
          Then
            Swap_2 (Vecteur [I], Vecteur [J])
          {endif};
          Inc (J)
        End
      {endwhile};
      Inc (I)
    End
  {endwhile};
End; { tri_permutation_down_2 }

Procedure Tri_Echange_Up_1 (N : Integer; Var Vecteur : T_Tabl_Variable_1);
Var Inversion : Boolean;
    I, J      : Integer;
Begin { tri_echange_up_1 }
  I := 1;
  Inversion := True;
  While (Inversion) Do
    Begin
      J := N;
      Inversion := False;
      While Not (J = I) Do
        Begin
          If (Vecteur [J] < Vecteur [Pred (J)])
          Then
            Begin
              Inversion := True;
              Swap_1 (Vecteur [J], Vecteur [Pred (J)])
            End
          {endif};
          Dec (J)
        End
      {endwhile};
      Inc (I)
    End
  {endwhile}
End; { tri_echange_up_1 }

Procedure Tri_Echange_Up_2 (N : Integer; Var Vecteur : T_Tabl_Variable_2);
Var Inversion : Boolean;
    I, J      : Integer;
Begin { tri_echange_up_2 }
  I := 1;
  Inversion := True;
  While (Inversion) Do
    Begin
      J := N;
      Inversion := False;
      While Not (J = I) Do
        Begin
          If (Vecteur [J] < Vecteur [Pred (J)])
          Then
            Begin
              Inversion := True;
              Swap_2 (Vecteur [J], Vecteur [Pred (J)])
            End
          {endif};
          Dec (J)
        End
      {endwhile};
      Inc (I)
    End
  {endwhile}
End; { tri_echange_up_2 }

Procedure Tri_Echange_Down_1 (N : Integer; Var Vecteur : T_Tabl_Variable_1);
Var Inversion : Boolean;
    I, J      : Integer;
Begin { tri_echange_down_1 }
  I := 1;
  Inversion := True;
  While (Inversion) Do
    Begin
      J := N;
      Inversion := False;
      While Not (J = I) Do
        Begin
          If (Vecteur [J] > Vecteur [Pred (J)])
          Then
            Begin
              Inversion := True;
              Swap_1 (Vecteur [J], Vecteur [Pred (J)])
            End
          {endif};
          Dec (J)
        End
      {endwhile};
      Inc (I)
    End
  {endwhile}
End; { tri_echange_down_1 }

Procedure Tri_Echange_Down_2 (N : Integer; Var Vecteur : T_Tabl_Variable_2);
Var Inversion : Boolean;
    I, J      : Integer;
Begin { tri_echange_down_2 }
  I := 1;
  Inversion := True;
  While (Inversion) Do
    Begin
      J := N;
      Inversion := False;
      While Not (J = I) Do
        Begin
          If (Vecteur [J] > Vecteur [Pred (J)])
          Then
            Begin
              Inversion := True;
              Swap_2 (Vecteur [J], Vecteur [Pred (J)])
            End
          {endif};
          Dec (J)
        End
      {endwhile};
      Inc (I)
    End
  {endwhile}
End; { tri_echange_down_2 }

Procedure Tri_Quicksort_Up_1 (N : Integer; Var Vecteur : T_Tabl_Variable_1);
Var X, Y : T_Variable_1;
  Procedure Qsort (L, R : Integer);
  Var I, J : Integer;
  Begin { qsort }
    I := L;
    J := R;
    X := Vecteur [(L+R) Div 2];
    Repeat
      While (Vecteur [I] < X) Do
        Inc (I)
      {endwhile};
      While (X < Vecteur [J]) Do
        Dec (J)
      {endwhile};
      If (I <= J)
      Then
        Begin
          Y := Vecteur [I];
          Vecteur [I] := Vecteur [J];
          Vecteur [J] := Y;
          Inc (I);
          Dec (J)
        End
      {endif}
    Until (I > J);
    If (L < J)
    Then
      Qsort (L, J)
    {endif};
    If (I < R)
    Then
      Qsort (I, R)
    {endif}
  End; { qsort }
Begin { tri_quicksort_up_1 }
  Qsort (1, N)
End; { tri_quicksort_up_1 }

Procedure Tri_Quicksort_Up_2 (N : Integer; Var Vecteur : T_Tabl_Variable_2);
Var X, Y : T_Variable_2;
  Procedure Qsort (L, R : Integer);
  Var I, J : Integer;
  Begin { qsort }
    I := L;
    J := R;
    X := Vecteur [(L+R) Div 2];
    Repeat
      While (Vecteur [I] < X) Do
        Inc (I)
      {endwhile};
      While (X < Vecteur [J]) Do
        Dec (J)
      {endwhile};
      If (I <= J)
      Then
        Begin
          Y := Vecteur [I];
          Vecteur [I] := Vecteur [J];
          Vecteur [J] := Y;
          Inc (I);
          Dec (J)
        End
      {endif}
    Until (I > J);
    If (L < J)
    Then
      Qsort (L, J)
    {endif};
    If (I < R)
    Then
      Qsort (I, R)
    {endif}
  End; { qsort }
Begin { tri_quicksort_up_2 }
  Qsort (1, N)
End; { tri_quicksort_up_2 }

Procedure Tri_Quicksort_Down_1 (N : Integer; Var Vecteur : T_Tabl_Variable_1);
Var X, Y : T_Variable_1;
  Procedure Qsort (L, R : Integer);
  Var I, J : Integer;
  Begin { qsort }
    I := L;
    J := R;
    X := Vecteur [(L+R) Div 2];
    Repeat
      While (X > Vecteur [J]) Do
        Dec (J)
      {endwhile};
      While (Vecteur [I] > X) Do
        Inc (I)
      {endwhile};
      If (I <= J)
      Then
        Begin
          Y := Vecteur [I];
          Vecteur [I] := Vecteur [J];
          Vecteur [J] := Y;
          Inc (I);
          Dec (J)
        End
      {endif}
    Until (I > J);
    If (L < J)
    Then
      Qsort (L, J)
    {endif};
    If (I < R)
    Then
      Qsort (I, R)
    {endif}
  End; { qsort }
Begin { tri_quicksort_up_1 }
  Qsort (1, N)
End; { tri_quicksort_up_1 }

Procedure Tri_Quicksort_Down_2 (N : Integer; Var Vecteur : T_Tabl_Variable_2);
Var X, Y : T_Variable_2;
  Procedure Qsort (L, R : Integer);
  Var I, J : Integer;
  Begin { qsort }
    I := L;
    J := R;
    X := Vecteur [(L+R) Div 2];
    Repeat
      While (X > Vecteur [J]) Do
        Dec (J)
      {endwhile};
      While (Vecteur [I] > X) Do
        Inc (I)
      {endwhile};
      If (I <= J)
      Then
        Begin
          Y := Vecteur [I];
          Vecteur [I] := Vecteur [J];
          Vecteur [J] := Y;
          Inc (I);
          Dec (J)
        End
      {endif}
    Until (I > J);
    If (L < J)
    Then
      Qsort (L, J)
    {endif};
    If (I < R)
    Then
      Qsort (I, R)
    {endif}
  End; { qsort }
Begin { tri_quicksort_up_2 }
  Qsort (1, N)
End; { tri_quicksort_up_2 }

End. { Fin de l'unit‚ 'U_Tris'. }

