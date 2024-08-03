/// <summary>
/// ***************************************************************************
///
/// Librairies pour Delphi
///
/// Copyright 1990-2024 Patrick Prémartin under AGPL 3.0 license.
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
///      Patrick PREMARTIN
///
/// Site :
///      https://developpeur-pascal.fr/librairies-publiques.html
///
/// Project site :
///      https://github.com/DeveloppeurPascal/librairies
///
/// ***************************************************************************
/// File last update : 28/07/2024 14:31:46
/// Signature : 0e03fb67babaa93495ad8ee9acad2b311a961ca5
/// ***************************************************************************
/// </summary>

unit USVGCursorSVGSamples;

// ****************************************
// * SVG from folder :
// * C:\Users\patrickpremartin\Documents\Embarcadero\Studio\Projets\___librairies-et-composants\librairies\samples\Skia.SVGToBitmap\_CursorSVGSamples\USVGCursorSVGSamples.pas
// ****************************************
//
// This file contains a list of contants and 
// an enumeration to access to SVG source codes 
// from the generated array of strings.
//
// ****************************************
// File generator : SVG Folder to Delphi Unit (1.0)
// Website : https://svgfolder2delphiunit.olfsoftware.fr/
// Generation date : 28/07/2024 14:31:46
//
// Don't do any change on this file.
// They will be erased by next generation !
// ****************************************

interface

const
  CSVGArrowE = 0;
  CSVGArrowN = 1;
  CSVGArrowNe = 2;
  CSVGArrowNw = 3;
  CSVGArrowS = 4;
  CSVGArrowSe = 5;
  CSVGArrowSw = 6;
  CSVGArrowW = 7;

type
{$SCOPEDENUMS ON}
  TSVGCursorSVGSamplesIndex = (
    ArrowE = CSVGArrowE,
    ArrowN = CSVGArrowN,
    ArrowNe = CSVGArrowNe,
    ArrowNw = CSVGArrowNw,
    ArrowS = CSVGArrowS,
    ArrowSe = CSVGArrowSe,
    ArrowSw = CSVGArrowSw,
    ArrowW = CSVGArrowW);

  TSVGCursorSVGSamples = class
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
    ArrowE = CSVGArrowE;
    ArrowN = CSVGArrowN;
    ArrowNe = CSVGArrowNe;
    ArrowNw = CSVGArrowNw;
    ArrowS = CSVGArrowS;
    ArrowSe = CSVGArrowSe;
    ArrowSw = CSVGArrowSw;
    ArrowW = CSVGArrowW;
    class property Tag: integer read FTag write SetTag;
    class property TagBool: Boolean read FTagBool write SetTagBool;
    class property TagFloat: Single read FTagFloat write SetTagFloat;
    class property TagObject: TObject read FTagObject write SetTagObject;
    class property TagString: string read FTagString write SetTagString;
    class function SVG(const Index: Integer): string; overload;
    class function SVG(const Index: TSVGCursorSVGSamplesIndex) : string; overload;
    class function Count : Integer;
    class constructor Create;
  end;

var
  SVGCursorSVGSamples : array of String;

implementation

uses
  System.SysUtils;

{ TSVGCursorSVGSamples }

class constructor TSVGCursorSVGSamples.Create;
begin
  inherited;
  FTag := 0;
  FTagBool := false;
  FTagFloat := 0;
  FTagObject := nil;
  FTagString := '';
end;

class procedure TSVGCursorSVGSamples.SetTag(const Value: integer);
begin
  FTag := Value;
end;

class procedure TSVGCursorSVGSamples.SetTagBool(const Value: Boolean);
begin
  FTagBool := Value;
end;

class procedure TSVGCursorSVGSamples.SetTagFloat(const Value: Single);
begin
  FTagFloat := Value;
end;

class procedure TSVGCursorSVGSamples.SetTagObject(const Value: TObject);
begin
  FTagObject := Value;
end;

class procedure TSVGCursorSVGSamples.SetTagString(const Value: string);
begin
  FTagString := Value;
end;

class function TSVGCursorSVGSamples.SVG(const Index: Integer): string;
begin
  if (index < Count) then
    result := SVGCursorSVGSamples[index]
  else
    raise Exception.Create('SVG not found. Index out of range.');
end;

class function TSVGCursorSVGSamples.SVG(const Index : TSVGCursorSVGSamplesIndex): string;
begin
  result := SVG(ord(index));
end;

class function TSVGCursorSVGSamples.Count: Integer;
begin
  result := length(SVGCursorSVGSamples);
end;

initialization

SetLength(SVGCursorSVGSamples, 8);

{$TEXTBLOCK NATIVE XML}
SVGCursorSVGSamples[CSVGArrowE] := '''
<svg width="32" height="32" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#000000" d="M7 11 L16 11 16 10 Q16 8.8 16.9 7.9 17.8 7 19 7 20.2 7 21.15 7.9 L27.15 13.9 Q28 14.8 28 16 28 17.2 27.15 18.15 L21.15 24.15 Q20.2 25 19 25 17.8 25 16.9 24.15 16 23.2 16 22 L16 21 7 21 Q5.8 21 4.9 20.15 4 19.2 4 18 L4 14 Q4 12.8 4.9 11.9 5.8 11 7 11 M6 18 Q6 18.4 6.3 18.7 6.6 19 7 19 L18 19 18 22 Q18 22.4 18.3 22.7 18.6 23 19 23 L19.7 22.7 25.7 16.7 26 16 25.7 15.3 19.7 9.3 Q19.4 9 19 9 18.6 9 18.3 9.3 18 9.6 18 10 L18 13 7 13 Q6.6 13 6.3 13.3 6 13.6 6 14 L6 18"/>
    <path stroke="none" fill="#FFFFFF" d="M6 18 L6 14 Q6 13.6 6.3 13.3 6.6 13 7 13 L18 13 18 10 Q18 9.6 18.3 9.3 18.6 9 19 9 19.4 9 19.7 9.3 L25.7 15.3 26 16 25.7 16.7 19.7 22.7 19 23 Q18.6 23 18.3 22.7 18 22.4 18 22 L18 19 7 19 Q6.6 19 6.3 18.7 6 18.4 6 18"/>
  </g>
</svg>
''';
SVGCursorSVGSamples[CSVGArrowN] := '''
<svg width="32" height="32" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#000000" d="M22 14 L22.7 13.7 23 13 22.7 12.3 16.7 6.3 Q16.4 6 16 6 15.6 6 15.3 6.3 L9.3 12.3 Q9 12.6 9 13 9 13.4 9.3 13.7 9.6 14 10 14 L13 14 13 25 Q13 25.4 13.3 25.7 13.6 26 14 26 L18 26 Q18.4 26 18.7 25.7 L19 25 19 14 22 14 M22 16 L21 16 21 25 Q21 26.2 20.15 27.15 19.2 28 18 28 L14 28 Q12.8 28 11.9 27.15 11 26.2 11 25 L11 16 10 16 Q8.8 16 7.9 15.15 7 14.2 7 13 7 11.8 7.9 10.9 L13.9 4.9 Q14.8 4 16 4 17.2 4 18.15 4.9 L24.15 10.9 Q25 11.8 25 13 25 14.2 24.15 15.15 23.2 16 22 16"/>
    <path stroke="none" fill="#FFFFFF" d="M22 14 L19 14 19 25 18.7 25.7 Q18.4 26 18 26 L14 26 Q13.6 26 13.3 25.7 13 25.4 13 25 L13 14 10 14 Q9.6 14 9.3 13.7 9 13.4 9 13 9 12.6 9.3 12.3 L15.3 6.3 Q15.6 6 16 6 16.4 6 16.7 6.3 L22.7 12.3 23 13 22.7 13.7 22 14"/>
  </g>
</svg>
''';
SVGCursorSVGSamples[CSVGArrowNe] := '''
<svg width="32" height="32" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#000000" d="M15.05 5.95 L23.05 6 Q24.25 6 25.15 6.9 26 7.8 26 9 L26.05 16.95 Q26.05 18.15 25.2 19.1 24.25 19.95 23.05 19.95 21.85 19.95 20.95 19.1 L20.4 18.55 13.9 25.1 Q12.95 25.95 11.75 25.95 10.55 25.95 9.65 25.1 L7 22.45 Q6.1 21.5 6.1 20.3 6.1 19.1 7 18.2 L13.5 11.65 12.95 11.1 Q12.05 10.15 12.05 8.95 12.05 7.75 12.95 6.85 13.85 5.95 15.05 5.95 M11.05 23.65 Q11.35 23.95 11.75 23.95 L12.45 23.65 20.4 15.7 22.35 17.65 Q22.65 17.95 23.05 17.95 L23.75 17.65 24.05 16.95 24 9 23.7 8.3 Q23.4 8 23 8 L15.05 7.95 Q14.65 7.95 14.35 8.25 14.05 8.55 14.05 8.95 14.05 9.35 14.35 9.65 L16.35 11.65 8.4 19.6 Q8.1 19.9 8.1 20.3 8.1 20.7 8.4 21 L11.05 23.65"/>
    <path stroke="none" fill="#FFFFFF" d="M11.05 23.65 L8.4 21 Q8.1 20.7 8.1 20.3 8.1 19.9 8.4 19.6 L16.35 11.65 14.35 9.65 Q14.05 9.35 14.05 8.95 14.05 8.55 14.35 8.25 14.65 7.95 15.05 7.95 L23 8 Q23.4 8 23.7 8.3 L24 9 24.05 16.95 23.75 17.65 23.05 17.95 Q22.65 17.95 22.35 17.65 L20.4 15.7 12.45 23.65 11.75 23.95 Q11.35 23.95 11.05 23.65"/>
  </g>
</svg>
''';
SVGCursorSVGSamples[CSVGArrowNw] := '''
<svg width="32" height="32" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#000000" d="M9.1 8 Q8.7 8 8.4 8.3 8.1 8.6 8.1 9 L8.05 16.95 Q8.05 17.35 8.35 17.65 8.65 17.95 9.05 17.95 L9.75 17.65 11.7 15.7 19.65 23.65 20.35 23.95 Q20.75 23.95 21.05 23.65 L23.7 21 24 20.3 23.7 19.6 15.75 11.65 17.75 9.65 18.05 8.95 17.75 8.25 Q17.45 7.95 17.05 7.95 L9.1 8 M17.05 5.95 Q18.25 5.95 19.2 6.85 20.05 7.75 20.05 8.95 20.05 10.15 19.2 11.1 L18.6 11.65 25.15 18.2 Q26 19.1 26 20.3 26 21.5 25.15 22.45 L22.5 25.1 Q21.55 25.95 20.35 25.95 19.15 25.95 18.25 25.1 L11.7 18.55 11.2 19.1 Q10.25 19.95 9.05 19.95 7.85 19.95 6.95 19.1 6.05 18.15 6.05 16.95 L6.1 9 Q6.1 7.8 7 6.9 7.9 6 9.1 6 L17.05 5.95"/>
    <path stroke="none" fill="#FFFFFF" d="M9.1 8 L17.05 7.95 Q17.45 7.95 17.75 8.25 L18.05 8.95 17.75 9.65 15.75 11.65 23.7 19.6 24 20.3 23.7 21 21.05 23.65 Q20.75 23.95 20.35 23.95 L19.65 23.65 11.7 15.7 9.75 17.65 9.05 17.95 Q8.65 17.95 8.35 17.65 8.05 17.35 8.05 16.95 L8.1 9 Q8.1 8.6 8.4 8.3 8.7 8 9.1 8"/>
  </g>
</svg>
''';
SVGCursorSVGSamples[CSVGArrowS] := '''
<svg width="32" height="32" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#000000" d="M14 6 Q13.6 6 13.3 6.3 13 6.6 13 7 L13 18 10 18 Q9.6 18 9.3 18.3 9 18.6 9 19 9 19.4 9.3 19.7 L15.3 25.7 16 26 16.7 25.7 22.7 19.7 23 19 22.7 18.3 Q22.4 18 22 18 L19 18 19 7 18.7 6.3 Q18.4 6 18 6 L14 6 M18 4 Q19.2 4 20.15 4.9 21 5.8 21 7 L21 16 22 16 Q23.2 16 24.15 16.9 25 17.8 25 19 25 20.2 24.15 21.15 L18.15 27.15 Q17.2 28 16 28 14.8 28 13.9 27.15 L7.9 21.15 Q7 20.2 7 19 7 17.8 7.9 16.9 8.8 16 10 16 L11 16 11 7 Q11 5.8 11.9 4.9 12.8 4 14 4 L18 4"/>
    <path stroke="none" fill="#FFFFFF" d="M14 6 L18 6 Q18.4 6 18.7 6.3 L19 7 19 18 22 18 Q22.4 18 22.7 18.3 L23 19 22.7 19.7 16.7 25.7 16 26 15.3 25.7 9.3 19.7 Q9 19.4 9 19 9 18.6 9.3 18.3 9.6 18 10 18 L13 18 13 7 Q13 6.6 13.3 6.3 13.6 6 14 6"/>
  </g>
</svg>
''';
SVGCursorSVGSamples[CSVGArrowSe] := '''
<svg width="32" height="32" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#000000" d="M20.95 12.85 Q21.85 11.95 23.05 11.95 24.25 11.95 25.2 12.85 26.05 13.75 26.05 14.95 L26 22.95 Q26 24.15 25.15 25.05 24.25 25.9 23.05 25.9 L15.05 25.95 Q13.85 25.95 12.95 25.1 12.05 24.15 12.05 22.95 12.05 21.75 12.95 20.85 L13.5 20.25 7 13.75 Q6.1 12.8 6.1 11.6 6.1 10.4 7 9.5 L9.65 6.85 Q10.55 5.95 11.75 5.95 12.95 5.95 13.9 6.85 L20.4 13.35 20.95 12.85 M14.35 22.25 Q14.05 22.55 14.05 22.95 14.05 23.35 14.35 23.65 14.65 23.95 15.05 23.95 L23 23.9 23.7 23.6 24 22.9 24.05 14.95 23.75 14.25 Q23.45 13.95 23.05 13.95 22.65 13.95 22.35 14.25 L20.4 16.2 12.45 8.25 11.75 7.95 Q11.35 7.95 11.05 8.25 L8.4 10.9 Q8.1 11.2 8.1 11.6 8.1 12 8.4 12.3 L16.35 20.25 14.35 22.25"/>
    <path stroke="none" fill="#FFFFFF" d="M14.35 22.25 L16.35 20.25 8.4 12.3 Q8.1 12 8.1 11.6 8.1 11.2 8.4 10.9 L11.05 8.25 Q11.35 7.95 11.75 7.95 L12.45 8.25 20.4 16.2 22.35 14.25 Q22.65 13.95 23.05 13.95 23.45 13.95 23.75 14.25 L24.05 14.95 24 22.9 23.7 23.6 23 23.9 15.05 23.95 Q14.65 23.95 14.35 23.65 14.05 23.35 14.05 22.95 14.05 22.55 14.35 22.25"/>
  </g>
</svg>
''';
SVGCursorSVGSamples[CSVGArrowSw] := '''
<svg width="32" height="32" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#000000" d="M18.25 6.85 Q19.15 5.95 20.35 5.95 21.55 5.95 22.5 6.85 L25.15 9.5 Q26 10.4 26 11.6 26 12.8 25.15 13.75 L18.6 20.25 19.2 20.85 Q20.05 21.75 20.05 22.95 20.05 24.15 19.2 25.1 18.25 25.95 17.05 25.95 L9.1 25.9 Q7.9 25.9 7 25.05 6.1 24.15 6.1 22.95 L6.05 14.95 Q6.05 13.75 6.95 12.85 7.85 11.95 9.05 11.95 10.25 11.95 11.2 12.85 L11.7 13.35 18.25 6.85 M23.7 12.3 L24 11.6 23.7 10.9 21.05 8.25 Q20.75 7.95 20.35 7.95 19.95 7.95 19.65 8.25 L11.7 16.2 9.75 14.25 9.05 13.95 Q8.65 13.95 8.35 14.25 8.05 14.55 8.05 14.95 L8.1 22.9 Q8.1 23.3 8.4 23.6 8.7 23.9 9.1 23.9 L17.05 23.95 17.75 23.65 18.05 22.95 17.75 22.25 15.75 20.25 23.7 12.3"/>
    <path stroke="none" fill="#FFFFFF" d="M23.7 12.3 L15.75 20.25 17.75 22.25 18.05 22.95 17.75 23.65 17.05 23.95 9.1 23.9 Q8.7 23.9 8.4 23.6 8.1 23.3 8.1 22.9 L8.05 14.95 Q8.05 14.55 8.35 14.25 8.65 13.95 9.05 13.95 L9.75 14.25 11.7 16.2 19.65 8.25 Q19.95 7.95 20.35 7.95 20.75 7.95 21.05 8.25 L23.7 10.9 24 11.6 23.7 12.3"/>
  </g>
</svg>
''';
SVGCursorSVGSamples[CSVGArrowW] := '''
<svg width="32" height="32" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
  <defs/>
  <g>
    <path stroke="none" fill="#000000" d="M25 11 Q26.2 11 27.15 11.9 28 12.8 28 14 L28 18 Q28 19.2 27.15 20.15 26.2 21 25 21 L16 21 16 22 Q16 23.2 15.15 24.15 14.2 25 13 25 11.8 25 10.9 24.15 L4.9 18.15 Q4 17.2 4 16 4 14.8 4.9 13.9 L10.9 7.9 Q11.8 7 13 7 14.2 7 15.15 7.9 16 8.8 16 10 L16 11 25 11 M12.3 22.7 L13 23 Q13.4 23 13.7 22.7 L14 22 14 19 25 19 25.7 18.7 26 18 26 14 25.7 13.3 Q25.4 13 25 13 L14 13 14 10 13.7 9.3 Q13.4 9 13 9 12.6 9 12.3 9.3 L6.3 15.3 Q6 15.6 6 16 6 16.4 6.3 16.7 L12.3 22.7"/>
    <path stroke="none" fill="#FFFFFF" d="M12.3 22.7 L6.3 16.7 Q6 16.4 6 16 6 15.6 6.3 15.3 L12.3 9.3 Q12.6 9 13 9 13.4 9 13.7 9.3 L14 10 14 13 25 13 Q25.4 13 25.7 13.3 L26 14 26 18 25.7 18.7 25 19 14 19 14 22 13.7 22.7 Q13.4 23 13 23 L12.3 22.7"/>
  </g>
</svg>
''';

end.
