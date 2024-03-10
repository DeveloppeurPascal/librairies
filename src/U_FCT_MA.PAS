Unit U_Fct_Math;             { mise … jour : 16 - 12 - 1990 }
{
Cette UNIT regroupe diverses fonctions math‚matiques non pr‚sentes en Turbo
Pascal 4.0 mais qui peuvent s'av‚rer int‚ressantes un jour ou l'autre.

Voici les fonctions accessibles:
                       - s‚cante                             SEC
                       - cos‚cante                           COSEC
                       - cotangente                          COTAN
                       - sinus hyperbolique                  SINH
                       - cosinus hyperbolique                COSH
                       - tangente hyperbolique               TANH
                       - s‚cante hyperbolique                SECH
                       - cos‚cante hyperbolique              COSECH
                       - cotangente hyperbolique             COTANH
                       - sinus hyperbolique inverse          ARCSINH
                       - cosinus hyperbolique inverse        ARCCOSH
                       - tangente hyperbolique inverse       ARCTANH
                       - s‚cante hyperbolique inverse        ARCSECH
                       - cotangente hyperbolique inverse     ARCCOTANH
                       - tangente                            TAN
  Ainsi que les fonctions normales (du Turbo Pascal 4.0):
                       - sinus:                              SIN    (Turbo)
                       - cosinus:                            COS    (Turbo)
                       - logarithme n‚p‚rien                 LN     (Turbo)
                       - exponentielle                       EXP    (Turbo)
                       - racine carr‚                        SQRT   (Turbo)
                       - carr‚                               SQR    (Turbo)
}
Interface
Function Sec (X: Real): Real;
Function Cosec (X: Real): Real;
Function Cotan (X: Real): Real;
Function Sinh (X: Real): Real;
Function Cosh (X: Real): Real;
Function Tanh (X: Real): Real;
Function Sech (X: Real): Real;
Function Cosech (X: Real): Real;
Function Cotanh (X: Real): Real;
Function Arcsinh (X: Real): Real;
Function Arccosh (X: Real): Real;
Function Arctanh (X: Real): Real;
Function Arcsech (X: Real): Real;
Function Arccotanh (X: Real): Real;
Function Tan (X: Real): Real;

{ Fin de l'interface. }

Implementation
Function Sinh (X: Real): Real;
Begin { sinh }
     Sinh := ((Exp (X)-Exp (-X)) / 2.0)
End; { sinh }

Function Tan (X: Real): Real;
Begin { tan }
      Tan := ((Sin (X)) / (Cos (X)))
End; { tan }

Function Cotan (X: Real): Real;
Begin { cotan }
      Cotan := ((Cos (X)) / (Sin (X)))
End; { cotan }

Function Sec (X: Real): Real;
Begin { sec }
      Sec := (1 / Cos (X))
End; { sec }

Function Cosec (X: Real): Real;
Begin { cosec }
      Cosec := (1 / Sin (X))
End; { cosec }

Function Cosh (X: Real): Real;
Begin { cosh }
      Cosh := ((Exp (X)+Exp (-X)) / 2.0)
End; { cosh }

Function Tanh (X: Real): Real;
Begin { tanh }
      Tanh := ((Exp (X)-Exp (-X)) / (Exp (X)+Exp (-X)))
End; { tanh }

Function Sech (X: Real): Real;
Begin { sech }
      Sech := (2.0 / (Exp (X)+Exp (-X)))
End; { sech }

Function Cosech (X: Real): Real;
Begin { cosech }
      Cosech := (2.0 / (Exp(X)-Exp (-X)))
End; { cosech }

Function Cotanh (X: Real): Real;
Begin { cotanh }
      Cotanh := ((Exp (X)+Exp (-X)) / (Exp (X)-Exp (-X)))
End; { cotanh }

Function Arcsinh (X: Real): Real;
Begin { arcsinh }
      Arcsinh := Ln (X+Sqrt ((Sqr (X))+1))
End; { arcsinh }

Function Arccosh (X: Real): Real;
Begin { arccosh }
      Arccosh := Ln (X+Sqrt ((Sqr (X))-1))
End; { arccosh }

Function Arctanh (X: Real): Real;
Begin { arctanh }
      Arctanh := (Ln ((1+X) / (1-X)))/2.0
End; { arctanh }

Function Arcsech (X: Real): Real;
Begin { arcsech }
      Arcsech := Ln ((Sqrt (-Sqr (X)+1)+1)/X)
End; { arcsech }

Function Arccotanh (X: Real): Real;
Begin { arccotanh }
      Arccotanh := Ln ((X+1)/(X-1))/2.0
End; { arccotanh }

End. { Fonctions-Math }
