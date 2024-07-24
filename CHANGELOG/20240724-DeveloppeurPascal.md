# 20240724 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

## Travail sur Olf.Skia.SVGToBitmap 

* mise à jour des docs sur les exemples SVG provenant du "Cursor Pack" de Kenney (et renommage du dossier)
* ajout de SVG "tuyaux" provenant du pack "Puzzle Parck 2" de Kenney (pour démos de recadrage des SVG transformés en bitmap)
* (re)génération par SVGFolder2DelphiUnit des unités contenant les codes sources des fichiers SVG 
* changement du doc lié aux exemples de lettres en graphiques
* correction du lien vers la licence AGPL dans le fichier LISEZMOI.md
* correction de la liste des unités du programme samples\Skia.SVGToBitmap\VCL\VCLSVGToBitmapSample suite au renommage du dossier des curseurs en SVG
* correction de la liste des unités du programme samples\Skia.SVGToBitmap\FMX\FMXSVGToBitmapSample suite au renommage du dossier des curseurs en SVG
* correction de la liste des unités du programme samples\Skia.SVGToBitmap\VCL_With_SVGList\VCLShowSVGFromBitmapSample suite au renommage du dossier des curseurs en SVG
* correction de la liste des unités du programme samples\Skia.SVGToBitmap\FMX_With_SVGList\FMXShowSVGFromBitmapSample suite au renommage du dossier des curseurs en SVG
* ajout d'un projet de démo (FMX_AccessSVGSourcesByClassMethods) utilisant la classe TSVGxxx exportée par SVGFolder2DelphiUnit depuis la version 20240723
* ajout d'un projet de démo (FMXAutoRegisterAndSVGMargins) utilisant l'auto enregistrement dans la classe d'export des bitmaps et des images avec marge autour des SVG (repris depuis l'affichage des tuyaux provenant du jeu vidéo Ploumtris)
* ajout du décompte du nombre d'éléments présents dans une liste de SVG sur la classe TOlfSVGBitmapList
* modification de l'exemple FMX_AccessSVGSourcesByClassMethods pour afficher le dcompte des éléments dans la liste
* ajout d'un référencement du tableau de SVG plutôt que 1 par 1 dans TOlfSVGBitmapList
* prise en charge de cette modification dans FMXAutoRegisterAndSVGMargins pour utiliser la même liste avec curseurs et éléments de tuyaux
* ajout de marges (en pourcentage) entre le dessin du SVG et la taille de l'image à générer
* modification de l'exemple affichant curseurs et tuyaux pour s'assurer que ça fonctionne.
