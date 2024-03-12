# 20240312 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

## Sur l'unité Olf.RTL.CryptDecrypt

* finalisation de l'algorithme de base de chiffrement/déchiffrement par XOR simple (avec ou sans instance de TOlfCryptDecrypt)
* publication du programme de test

## Sur l'unité Olf.RTL.Params

* implémentation des événements onCrypt et onDecrypt
* modification du choix du nom de fichier (".par" pour le json en texte, ".parc" pour sa version binaire chiffrée)
* création d'un programme de test permettant de s'assurer que les paramètres non chiffrés fonctionnent toujours et chiffrés fonctionnent aussi

* ajout d'un PortableMode booléen pour activer (par défaut) ou pas les créations de chemins et de fichier spour stocker les informations sur le disque
* création d'un programme pour tester et montrer le fonctionnement de cette fonctionnalité

* ajout d'une méthode permettant de retirer un paramètre
* ajout d'un programme pour tester et montrer son utilisation

## Divers

* mise à jour du lien vers le dépôt Delphi Game Engine dans l'unité src/u_score.pas
