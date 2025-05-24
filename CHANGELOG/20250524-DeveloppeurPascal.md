# 20250524 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

## Olf.RTL.Language

* fixed a warning message when compiling on macOS or iOs (implicite string conversion)

## u_urlOpen

* added a new FMX sample to show the use of this unit
* reordered units list in the implementation
* fixed a warning message when compiling on Android (missing two units)

## Olf.FMX.TextImageFrame

* added missing replacement letters/words in TOlfFMXTextImageFrame.DefaultOnGetImageIndexOfUnknowChar
* added ' ' as a space character replacement and to show a space in a text if the font has an image for it
* replaced CharInSet() function by IsInArray() method to work in UTF-16 and don't loose special chars
* used Isdigit and IsLetter for full UTF-16 compatibility
* updated and closed some TODOs

## Other

* updated developer doc
