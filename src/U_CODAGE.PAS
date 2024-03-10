Unit U_Codage;
{
Auteur: Pr‚martin Patrick.
Date: 17/02/1991.
      13/07/1991.
      28/12/1991.

But: Cette unit‚ contient des fonctions permettant de coder, et de d‚coder des
     donnees sous forme de caractŠres, ou de chaines de caractŠres.
}
Interface

{ Voici les deux variables utilis‚es lors du codage. Vous pouvez changer leur
valeur par defaut. }
Const C_Codage_1 : byte = $2a; { 0010 1010 }
      C_Codage_2 : byte = $f3; { 1111 0011 }

{ Voici les fonctions r‚alisant les codages, et d‚codages. }
Function Car_Coder (Carac : Char; masq1, masq2 : byte) : Char;
         { Renvoie le caractŠre cod‚. }
Function Car_Decoder (Carac : Char; masq1, masq2 : byte) : Char;
         { Renvoie le caractŠre d‚cod‚. }
Function Ch_Coder (Chaine : String) : String;
         { Renvoie la chaine cod‚e. }
Function Ch_Decoder (Chaine : String) : String;
         { Renvoie la chaine d‚cod‚e. }
procedure ptr_coder (adr : pointer; nb_adr : word);
procedure ptr_decoder (adr : pointer; nb_adr : word);

Implementation
type
    prec = ^trec;
    trec = array [1..65535] of byte;

Function Car_Coder (Carac : Char; masq1, masq2 : byte) : Char;
Begin
  Car_Coder := Chr ((Ord (Carac) Xor masq1) Xor masq2);
End; { car_coder }

Function Car_Decoder (Carac : Char; masq1, masq2 : byte) : Char;
Begin
  Car_Decoder := Chr ((Ord (Carac) Xor masq2) Xor masq1);
End; { car_decoder }

Function Ch_Coder (Chaine : String) : String;
Var I : Integer;
    msq1, msq2, msq3 : byte;
Begin
     msq1 := c_codage_1;
     msq2 := c_codage_2;
     for i := 1 to length (chaine) do
         begin
              msq3 := ord (chaine [i]);
              chaine [i] := car_coder (chaine [i], msq1, msq2);
              msq1 := msq2;
              msq2 := msq3;
         end;
     {endfor}
     ch_coder := chaine;
End; { ch_coder }

Function Ch_Decoder (Chaine : String) : String;
Var I : Integer;
    msq1, msq2 : byte;
Begin
     msq1 := c_codage_1;
     msq2 := c_codage_2;
     for i := 1 to length (chaine) do
         begin
              chaine [i] := car_decoder (chaine [i], msq1, msq2);
              msq1 := msq2;
              msq2 := ord (chaine [i]);
         end;
     {endfor}
     ch_decoder := chaine;
End; { ch_decoder }

procedure ptr_coder (adr : pointer; nb_adr : word);
var
   i : word;
   res : prec;
   msq1, msq2, msq3 : byte;
begin
     res := adr;
     msq1 := c_codage_1;
     msq2 := c_codage_2;
     for i := 1 to nb_adr do
         begin
              msq3 := res^ [i];
              res^ [i] := msq3 xor msq1 xor msq2;
              msq1 := msq2;
              msq2 := msq3;
         end;
     {endif}
end; { Ptr_Coder }

procedure ptr_decoder (adr : pointer; nb_adr : word);
var
   i : word;
   res : prec;
   msq1, msq2 : byte;
begin
     res := adr;
     msq1 := c_codage_1;
     msq2 := c_codage_2;
     for i := 1 to nb_adr do
         begin
              res ^ [i] := res^ [i] xor msq2 xor msq1;
              msq1 := msq2;
              msq2 := res^ [i];
         end;
     {endif}
end; { Ptr_Decoder }

End. { u_codage }
