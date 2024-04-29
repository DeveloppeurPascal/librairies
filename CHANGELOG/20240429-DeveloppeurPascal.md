# 20240429 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

* modification de Olf.RTL.CryptDecrypt pour retourner des TMemoryStream au lieu de TStream et permettre leur manipulation en direct si besoin
* correction d'un bogue sur Olf.RTL.Params en RELEASE (nom de variable incorrect utilisé dans le nom de fichier)
* ajout d'un dump décimal/hexa d'un flux dans Olf.RTL.Stream
* modification de la conversion vers les hexadécimaux dans Olf.RTL.Maths.Conversions pour avoir un zéro à gauche en cas de nombre de lettres impair ('0' donne '00', 'F' donne '0F')
* ajout de la méthode Cancel() sur la classe TParams (elle existait sur l'instance)
* ajout d'une méthode Delete() sur TParams et TParamsFile pour supprimer le fichier de stockage des paramètres
* ajout d'une méthode Clear() sur TParams et TParamsFile pour vider la liste des paramètres en mémoire
