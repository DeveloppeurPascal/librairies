unit uParam;
{
  Gestion automatisée d'un fichier de stockage sous forme de dictionnaire de
  données (clé / valeur) pour les logiciels développés sous Delphi.

  Logiciel open source distribué sous licence MIT.
  Open source software distributed under the MIT license

  Copyright Patrick Prémartin / Olf Software

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

  Find the original source code on
  https://github.com/DeveloppeurPascal/librairies

  Find explanations on
  https://www.developpeur-pascal.fr/p/_5007-plutot-ini-ou-json-pour-stocker-vos-parametres.html

  Liste des mises à jour :
  => 11/05/2020, Patrick Prémartin :
  *     ajout de la notice de copyright et licence d'utilisation
  *     gestion de données de types Syngle et TDataTime
  *     correction d'une perte de mémoire au niveau du remplacement d'une valeur sur un paramètre existant
  => 22/04/2022, Patrick PRémartin :
  *     ajout de la procédure setFolderName() pour permettre la modification du dossier de stockage du fichier de paramètres
  => 19/05/2022, Patrick Prémartin :
  *     depréciation de cette unité au profit de l'unité Olf.RTL.Params
}

interface

uses
  Olf.RTL.Params;

type
  TParams = Olf.RTL.Params.TParams;

implementation

{$MESSAGE WARN 'uParam unit is deprecated. Use Olf.RTL.Params unit instead.'}

end.
