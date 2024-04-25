# 20240425 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

* mise à jour du dépôt local et des dépendances

* ajout d'une fonction GenXORKey sur la classe TOlfCryptDecrypt de Olf.RTL.CryptDecrypt
* adaptation du programme d'exemple pour utiliser cette nouvelle fonction lors de la génération de sa clé de chiffrement

* ajout du RANDOMIZE dans l'unité Olf.RTL.CryptDecrypt pour initialiser systématiquemt la fonction random et ne pas repartir par défaut sur une série connue

* ajout d'une fonction GenSwapKey sur la classe TOlfCryptDecrypt de Olf.RTL.CryptDecrypt
* adaptation du programme d'exemple pour utiliser cette nouvelle fonction lors de la génération de sa clé de chiffrement

* modification des types de stockage de clés pour avoir du byte ou de l'integer selon les besoins (valeurs de 0 à 255 ou autorisation de valeurs négatives)

* ajout de méthodes de chiffrements et déchiffrement en utilisant les opérations binaires SHL / SHR
* ajout d'un exemple utilisant ces fonctions

* correction  d'une anomalie dans les fonctions de conversion qui ne traitaient pas le cas du "0"
* ajout d'un exemple d'utilisation de SHL/SHR sur des valeurs binaires saisies à l'écran

* ajout d'un algorithme de chiffrement par ajout/retrait d'une valeur à chaque octet (fonctions IDBxxx pour IncDecByte)
* ajout d'un exemple d'utilisation de cet algorithme
