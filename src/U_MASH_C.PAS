Unit U_Mash_Code;            { mise … jour le 17 - 02 - 1991 }
{
Auteur: Pr‚martin Patrick.
Date: 17/02/1991.

But: Cette unit‚ contient une fonction qui renvoie le Mash-Code d'une chaine
     pass‚e en paramŠtre. Le Mash-Code est une sorte de concatenation de la
     chaine permettant de la comparer ais‚ment … d'autres chaines ayant une
     orthographe approximativement identique.
}
Interface
Function Set_Maj_Chaine (Chaine : String) : String;
         { Renvoie la chaine en majuscules. }
Function Kill_Space (Chaine : String) : String;
         { Renvoie la chaine sans les espaces qu'elle contenait. }
Function Mash_Code (Chaine : String) : String;
         { Renvoie le Mash-Code de la chaine pass‚e. }

Implementation
Function Set_Maj_Chaine (Chaine : String) : String;
Var I : Integer;
Begin { set_maj_chaine }
  For I := 1 To Length (Chaine) Do
    Chaine [I] := Upcase (Chaine [I])
  {endfor};
  Set_Maj_Chaine := Chaine
End; { set_maj_chaine }

Function Kill_Space (Chaine : String) : String;
Var I : Integer;
Begin { kill_space }
  I := Pos (' ', Chaine);
  While Not (I = 0) Do
    Begin
      Delete (Chaine, I, 1);
      I := Pos (' ', Chaine)
    End
  {endwhile};
  Kill_Space := Chaine
End; { kill_space }

Function Compression_2 (Chaine : String) : String;
Var I, J : Integer;
Begin { compression_2 }
  I := 1;
  For J := 2 To Length (Chaine) Do
    If Not (Chaine [J] In ['A', 'E', 'I', 'O', 'U', 'Y', 'H'])
    Then
      Begin
        Inc (I);
        Chaine [I] := Chaine [J]
      End
    {endif}
  {endfor};
  Chaine [0] := Chr (I);
  Compression_2 := Chaine
End; { compression_2 }

Function Compression_3 (Chaine : String) : String;
Var I, J : Integer;
Begin { compression_3 }
  I := 1;
  For J := 2 To Length (Chaine) Do
    If (Chaine [J] <> Chaine [I])
    Then
      Begin
        Inc (I);
        Chaine [I] := Chaine [J]
      End
    {endif}
  {endfor};
  Chaine [0] := Chr (I);
  Compression_3 := Chaine
End; { compression_3 }

Function Mash_Code (Chaine : String) : String;
Var I : Integer;
Begin { mash_code }
  { On met la chaine en majuscules. }
  Chaine := Set_Maj_Chaine (Chaine);
  { On enlŠve les espaces de la chaine. }
  Chaine := Kill_Space (Chaine);
  { On vire les voyelles et les 'H'. }
  Chaine := Compression_2 (Chaine);
  { On vire les lettres faisant double emploi. }
  Chaine := Compression_3 (Chaine);
  { On transforme les lettres suivantes:
       - 'J' ===> 'G'
       - 'N' ===> 'M'
       - 'S' ===> 'C'
       - 'T' ===> 'D'
       - 'W' ===> 'V'
  afin de diminuer les risques d'erreurs d'orthographe. }
  For I := 2 To Length (Chaine) Do
    Case Chaine [I] Of
      'J' : Chaine [I] := 'G';
      'N' : Chaine [I] := 'M';
      'S' : Chaine [I] := 'C';
      'T' : Chaine [I] := 'D';
      'W' : Chaine [I] := 'V'
    End
  {endfor};
  Mash_Code := Chaine
End; { mash_code }

End. { Fin de l'unit‚: 'U_Mash_Code'. }
