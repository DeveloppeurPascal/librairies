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
/// File last update : 03/08/2024 22:03:49
/// Signature : 54daa21d904d4ccebd696e6b23251602c232e3d9
/// ***************************************************************************
/// </summary>

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
