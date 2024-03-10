Unit U_Maths;                { mise … jour : 16 - 12 - 1990. }
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