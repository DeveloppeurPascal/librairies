# 20240310 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

## Sur l'unité Olf.RTL.Maths.Conversions

* ajout d'un programme de test des conversions Base10 depuis et vers Base2, Base8, Base16, Base34 et Base62
* création de l'unité Olf.RTL.Maths.Conversions et de sa classe TOlfNumberConversion permettant des conversions d'un nombre entier en base 2 à 62
* publication du programme de test

## Sur l'unité Olf.RTL.Checksum.pas

* renommage de l'unité uChecksumVerif.pas et ajout d'une redirection pour péremption de l'ancienne unité
* ajout des liens vers les blogs Delphi et PHP expliquant le fonctionnement du calcul de checksum, de sa vérification et donnant des exemples d'utilisation
* renommage des classes ChecksumVerif et TChecksumVerifParamList
* correction de la potentielle anomalie liée au checksum vide lors de son contrôle
* publication du programme de test

## Sur l'unité Olf.RTL.Streams.pas

* ajout de l'interface IOlfLoadSaveStreamWithSize
* création et publication d'un projet d'exemple utilisant une chaine et un stream en mémoire

## Sur l'unité Olf.RTL.CryptDecrypt

* suppression de la librairie u_codage.pas historique (datée de 1991, provenant de ma boite à outils Turbo Pascal)
* création de l'unité Olf.RTL.CryptDecrypt et de sa classe de chiffrement/déchiffrement par XOR
* création d'un projet de démo
