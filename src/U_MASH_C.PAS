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
/// File last update : 03/08/2024 22:02:58
/// Signature : 7f3bc65ae020a8f8abe58fdee7d01a8d46b7d69c
/// ***************************************************************************
/// </summary>

Unit U_Mash_Code;
{
Auteur: Prémartin Patrick.
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
