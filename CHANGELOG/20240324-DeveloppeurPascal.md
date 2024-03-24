# 20240324 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

## Sur l'unité Olf.RTL.CryptDecrypt

* renommage de TOlfCryptDecrypt.Crypt en TOlfCryptDecrypt.XORCrypt
* renommage de TOlfCryptDecrypt.Decrypt en TOlfCryptDecrypt.XORDecrypt
* mise à jour du projet de démo utilisant ces fonctions de chiffrement
* mise à jour de l'ordre de tabulation dans l'écran du projet de test de XORCrypt/XORDecrypt

## Sur l'unité u_MD5

* mise à niveau de l'algorithme de calcul en passant par celui fourni par Embarcadero depuis Delphi 10 Seattle plutôt que celui de Indy
* ajout d'un exemple pour afficher le résultat d'un MD5
* remplacement de DisposeOf (dépréciée) par Free sur le calcul du MD5

## Sur les unités Olf.VCL.Streams et Olf.FMX.Streams

* ajout d'un projet de test pour la sauvegarde d'un bitmap nil et d'un bitmap vide en VCL
* ajout d'un projet de test pour la sauvegarde d'un bitmap nil et d'un bitmap vide en FMX
* correction de la sauvegarde en stream pour la version FMX car un bitmap vide enregistre quand même quelque chose mais ne se recharge pas

## divers

* ajout du "deprecated" sur u_langue.pas remplacée par Olf.RTL.Language.pas
* ajout d'une fonction ReverseString() à l'unité u_GenerationUtilitaire
