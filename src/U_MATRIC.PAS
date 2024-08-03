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
/// File last update : 03/08/2024 22:03:12
/// Signature : 7a90c14335c44073e33aa1ae24f38aaa94976ad3
/// ***************************************************************************
/// </summary>

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
