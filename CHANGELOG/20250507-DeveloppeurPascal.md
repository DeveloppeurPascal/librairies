# 20250507 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

## Olf.Skia.SVGToBitmap

* added TMonitor on instance methods for Skia.SVGToBitmap

## Olf.FMX.TextImageFrame

* replaced Adobe Stock images for the letters used in FMX.TextImageFrame samples 
* fixed an error of string used in a sample project ('012' instead of '123')
* added an optional call to OnGetImageIndexOfUnknowChar in TOlfFMXTextImageFrame.getImageIndexOfChar 
* added a DefaultOnGetImageIndexOfUnknowChar called when OnGetImageIndexOfUnknowChar returns nothing
* used Video Title Page Generator replacement method in the DefaultOnGetImageIndexOfUnknowChar list
* added other "non latin" characters from different countries
* added XML doc comments on TextImageFrame class
* published the Refresh() method after renaming RefreshTexte()
* removed unused internal constants (tests from first release of the library with hard coded indexes of characters)
* added current character in the TGlyph TagString for each image created for the text
* added a refresh operation after changing each property
* managed BeginUpdate/EndUpdate methods and delay the calls for Refresh operations
* added an AutoSize property to draw the text in its parent client zone or totally free (previous and still the default value)
* added a sample to test and show the AutoSize feature

## general changes

* updated developer docs
