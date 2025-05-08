/// <summary>
/// ***************************************************************************
///
/// My libraries for Delphi
///
/// Copyright 1990-2025 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// This repository contains functions, procedures and classes to use in
/// Delphi projects (console, VCL, FireMonkey and others). It's my "everything reuseable things" toolbox.
///
/// The units to be used in your projects can be found in the "src" folder.
/// Some features are explained on my blog or have been coded live on Twitch.
///
/// Examples of use in the form of VCL or FireMonkey projects are available in
/// the "samples" subfolder.
///
/// ***************************************************************************
///
/// Author(s) :
/// Patrick PREMARTIN
///
/// Site :
/// https://librairies.developpeur-pascal.fr
///
/// Project site :
/// https://github.com/DeveloppeurPascal/librairies
///
/// ***************************************************************************
/// File last update : 2025-05-08T14:53:36.514+02:00
/// Signature : e5eccf052db26389315c85c5e3288475f062e145
/// ***************************************************************************
/// </summary>

unit USVGCharacterImages;

// ****************************************
// * SVG from folder :
// * C:\Dev\___lib\librairies\samples\FMX.TextImageFrame\_CharacterImages\uSVGCharacterImages.pas
// ****************************************
//
// This file contains a list of contants and 
// an enumeration to access to SVG source codes 
// from the generated array of strings.
//
// ****************************************
// File generator : SVG Folder to Delphi Unit v1.0
// Website : https://svgfolder2delphiunit.olfsoftware.fr/
// Generation date : 2025-05-08T14:53:36.500Z
//
// Don't do any change on this file.
// They will be erased by next generation !
// ****************************************

interface

const
  CSVGA = 0;
  CSVGB = 1;
  CSVGC = 2;
  CSVG1 = 3;
  CSVG2 = 4;
  CSVG3 = 5;
  CSVGA = 6;
  CSVGB = 7;
  CSVGC = 8;

type
{$SCOPEDENUMS ON}
  TSVGSVGIndex = (
    A = CSVGA,
    B = CSVGB,
    C = CSVGC,
    1 = CSVG1,
    2 = CSVG2,
    3 = CSVG3,
    A = CSVGA,
    B = CSVGB,
    C = CSVGC);

  TSVGSVG = class
  private
  class var
    FTag: integer;
    FTagBool: Boolean;
    FTagFloat: Single;
    FTagObject: TObject;
    FTagString: string;
    class procedure SetTag(const Value: integer); static;
    class procedure SetTagBool(const Value: Boolean); static;
    class procedure SetTagFloat(const Value: Single); static;
    class procedure SetTagObject(const Value: TObject); static;
    class procedure SetTagString(const Value: string); static;
  public const
    A = CSVGA;
    B = CSVGB;
    C = CSVGC;
    1 = CSVG1;
    2 = CSVG2;
    3 = CSVG3;
    A = CSVGA;
    B = CSVGB;
    C = CSVGC;
    class property Tag: integer read FTag write SetTag;
    class property TagBool: Boolean read FTagBool write SetTagBool;
    class property TagFloat: Single read FTagFloat write SetTagFloat;
    class property TagObject: TObject read FTagObject write SetTagObject;
    class property TagString: string read FTagString write SetTagString;
    class function SVG(const Index: Integer): string; overload;
    class function SVG(const Index: TSVGSVGIndex) : string; overload;
    class function Count : Integer;
    class constructor Create;
  end;

var
  SVGSVG : array of String;

implementation

uses
  System.SysUtils;

{ TSVGSVG }

class constructor TSVGSVG.Create;
begin
  inherited;
  FTag := 0;
  FTagBool := false;
  FTagFloat := 0;
  FTagObject := nil;
  FTagString := '';
end;

class procedure TSVGSVG.SetTag(const Value: integer);
begin
  FTag := Value;
end;

class procedure TSVGSVG.SetTagBool(const Value: Boolean);
begin
  FTagBool := Value;
end;

class procedure TSVGSVG.SetTagFloat(const Value: Single);
begin
  FTagFloat := Value;
end;

class procedure TSVGSVG.SetTagObject(const Value: TObject);
begin
  FTagObject := Value;
end;

class procedure TSVGSVG.SetTagString(const Value: string);
begin
  FTagString := Value;
end;

class function TSVGSVG.SVG(const Index: Integer): string;
begin
  if (index < Count) then
    result := SVGSVG[index]
  else
    raise Exception.Create('SVG not found. Index out of range.');
end;

class function TSVGSVG.SVG(const Index : TSVGSVGIndex): string;
begin
  result := SVG(ord(index));
end;

class function TSVGSVG.Count: Integer;
begin
  result := length(SVGSVG);
end;

initialization

SetLength(SVGSVG, 9);

{$TEXTBLOCK NATIVE XML}
SVGSVG[CSVGA] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40.01 84.47"><g id="Calque_1-2"><text transform="translate(0 62.64)" font-family="ComicSansMS-Bold, &apos;Comic Sans MS&apos;" font-size="72" font-weight="700"><tspan x="0" y="0">a</tspan></text></g></svg>
''';
SVGSVG[CSVGB] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 42.71 84.47"><g id="Calque_1-2"><text transform="translate(0 62.64)" font-family="ComicSansMS-Bold, &apos;Comic Sans MS&apos;" font-size="72" font-weight="700"><tspan x="0" y="0">b</tspan></text></g></svg>
''';
SVGSVG[CSVGC] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 36.98 84.47"><g id="Calque_1-2"><text transform="translate(0 62.64)" font-family="ComicSansMS-Bold, &apos;Comic Sans MS&apos;" font-size="72" font-weight="700"><tspan x="0" y="0">c</tspan></text></g></svg>
''';
SVGSVG[CSVG1] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 43.95 84.47"><g id="Calque_1-2"><text transform="translate(0 62.64)" font-family="ComicSansMS-Bold, &apos;Comic Sans MS&apos;" font-size="72" font-weight="700"><tspan x="0" y="0">1</tspan></text></g></svg>
''';
SVGSVG[CSVG2] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 43.95 84.47"><g id="Calque_1-2"><text transform="translate(0 62.64)" font-family="ComicSansMS-Bold, &apos;Comic Sans MS&apos;" font-size="72" font-weight="700"><tspan x="0" y="0">2</tspan></text></g></svg>
''';
SVGSVG[CSVG3] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 43.95 84.47"><g id="Calque_1-2"><text transform="translate(0 62.64)" font-family="ComicSansMS-Bold, &apos;Comic Sans MS&apos;" font-size="72" font-weight="700"><tspan x="0" y="0">3</tspan></text></g></svg>
''';
SVGSVG[CSVGA] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 52.66 84.47"><g id="Calque_1-2"><text transform="translate(0 62.64)" font-family="ComicSansMS-Bold, &apos;Comic Sans MS&apos;" font-size="72" font-weight="700"><tspan x="0" y="0">A</tspan></text></g></svg>
''';
SVGSVG[CSVGB] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 45.39 84.47"><g id="Calque_1-2"><text transform="translate(0 62.64)" font-family="ComicSansMS-Bold, &apos;Comic Sans MS&apos;" font-size="72" font-weight="700"><tspan x="0" y="0">B</tspan></text></g></svg>
''';
SVGSVG[CSVGC] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 44.54 84.47"><g id="Calque_1-2"><text transform="translate(0 62.64)" font-family="ComicSansMS-Bold, &apos;Comic Sans MS&apos;" font-size="72" font-weight="700"><tspan x="0" y="0">C</tspan></text></g></svg>
''';

end.
