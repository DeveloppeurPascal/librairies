# 20240310 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

* déplacement des fichiers de la librairie vers le sous-dossier "./src" pour homogénéiser mes dépôts de code
* suppression des fichiers qui ne feront pas l'objet d'une redirection (dof, fmx, dfm, ...)
* application du programme "RedirectionPasDPK" provenant du dépôt "DeveloppeurPascal/One-Shot-Tools" pour rediriger les fichiers de la librairie vers leur version dans "/src"
* copie dans "/samples" des exemples provenant du dépôt DeveloppeurPascal/librairies-samples-and-tests
* mise à jour des dépendances de ces programmes
* mise à jour du .gitignore depuis celui de mon template de projets Delphi
* ajout de quelques unités venant historiquement de ma boite à outils pour Turbo Pascal, Borland Pascal et Delphi 1 (à trier ultérieurement)

## Modifications liées à Olf.RTL.Params

* création d'un projet pour traiter les tickets concernant les événements à mettre sur TParams et TParamsFile
* ajout de la doc XMLDoc sur les classes TParams et TParamsFile
* réorganisation des unités pour diminuer les changements inutiles dans git lors de l'ajout de nouvelles unités
* ajout des évenements BeforeLoad, AfterLoad, BeforeSave et AfterSave sur TParams et TParamsFile

* création d'un projet d'exemple pour manipuler les chemins de stockage des paramètres par défaut (ancienne ou nouvelle version)
* ajout d'une méthode InitDefaultFileNameV2() pour créer un chemin sur la nouvelle arborescence (DEBUG/RELEASE, DocumentsPath/HomePath selon la plateforme)
