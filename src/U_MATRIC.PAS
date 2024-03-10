Unit U_Matrices;             { mise … jour : 29 - 12 - 1990 }

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