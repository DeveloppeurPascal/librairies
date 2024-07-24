unit USVGPipesSVGSamples;

// ****************************************
// * SVG from folder :
// * C:\Users\patrickpremartin\Documents\Embarcadero\Studio\Projets\___librairies-et-composants\librairies\samples\Skia.SVGToBitmap\_PipesSVGSamples\uSVGPipesSVGSamples.pas
// ****************************************
//
// This file contains a list of contants and 
// an enumeration to access to SVG source codes 
// from the generated array of strings.
//
// ****************************************
// File generator : SVG Folder to Delphi Unit (1.0)
// Website : https://svgfolder2delphiunit.olfsoftware.fr/
// Generation date : 24/07/2024 10:47:05
//
// Don't do any change on this file.
// They will be erased by next generation !
// ****************************************

interface

const
  CSVGPipeDb = 0;
  CSVGPipeGb = 1;
  CSVGPipeGd = 2;
  CSVGPipeGdb = 3;
  CSVGPipeHb = 4;
  CSVGPipeHd = 5;
  CSVGPipeHdb = 6;
  CSVGPipeHdbg = 7;
  CSVGPipeHg = 8;
  CSVGPipeHgb = 9;
  CSVGPipeHgd = 10;

type
{$SCOPEDENUMS ON}
  TSVGPipesSVGSamplesIndex = (
    PipeDb = CSVGPipeDb,
    PipeGb = CSVGPipeGb,
    PipeGd = CSVGPipeGd,
    PipeGdb = CSVGPipeGdb,
    PipeHb = CSVGPipeHb,
    PipeHd = CSVGPipeHd,
    PipeHdb = CSVGPipeHdb,
    PipeHdbg = CSVGPipeHdbg,
    PipeHg = CSVGPipeHg,
    PipeHgb = CSVGPipeHgb,
    PipeHgd = CSVGPipeHgd);

  TSVGPipesSVGSamples = class
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
    PipeDb = CSVGPipeDb;
    PipeGb = CSVGPipeGb;
    PipeGd = CSVGPipeGd;
    PipeGdb = CSVGPipeGdb;
    PipeHb = CSVGPipeHb;
    PipeHd = CSVGPipeHd;
    PipeHdb = CSVGPipeHdb;
    PipeHdbg = CSVGPipeHdbg;
    PipeHg = CSVGPipeHg;
    PipeHgb = CSVGPipeHgb;
    PipeHgd = CSVGPipeHgd;
    class property Tag: integer read FTag write SetTag;
    class property TagBool: Boolean read FTagBool write SetTagBool;
    class property TagFloat: Single read FTagFloat write SetTagFloat;
    class property TagObject: TObject read FTagObject write SetTagObject;
    class property TagString: string read FTagString write SetTagString;
    class function SVG(const Index: Integer): string; overload;
    class function SVG(const Index: TSVGPipesSVGSamplesIndex) : string; overload;
    class constructor Create;
  end;

var
  SVGPipesSVGSamples : array of String;

implementation

uses
  System.SysUtils;

{ TSVGPipesSVGSamples }

class constructor TSVGPipesSVGSamples.Create;
begin
  inherited;
  FTag := 0;
  FTagBool := false;
  FTagFloat := 0;
  FTagObject := nil;
  FTagString := '';
end;

class procedure TSVGPipesSVGSamples.SetTag(const Value: integer);
begin
  FTag := Value;
end;

class procedure TSVGPipesSVGSamples.SetTagBool(const Value: Boolean);
begin
  FTagBool := Value;
end;

class procedure TSVGPipesSVGSamples.SetTagFloat(const Value: Single);
begin
  FTagFloat := Value;
end;

class procedure TSVGPipesSVGSamples.SetTagObject(const Value: TObject);
begin
  FTagObject := Value;
end;

class procedure TSVGPipesSVGSamples.SetTagString(const Value: string);
begin
  FTagString := Value;
end;

class function TSVGPipesSVGSamples.SVG(const Index: Integer): string;
begin
  if (index < length(SVGPipesSVGSamples)) then
    result := SVGPipesSVGSamples[index]
  else
    raise Exception.Create('SVG not found. Index out of range.');
end;

class function TSVGPipesSVGSamples.SVG(const Index : TSVGPipesSVGSamplesIndex): string;
begin
  result := SVG(ord(index));
end;

initialization

SetLength(SVGPipesSVGSamples, 11);

{$TEXTBLOCK NATIVE XML}
SVGPipesSVGSamples[CSVGPipeDb] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 88.05 87.9"><g id="Calque_1-2"><path d="M88.05,8l-.05,38.6v8c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15h-12c-1.1,0-2.03-.38-2.8-1.15-.8-.8-1.2-1.73-1.2-2.8-6.03.43-10.02,3-11.95,7.7v.05c-.63,1.6-1.07,3.43-1.3,5.5,1.03.03,1.93.43,2.7,1.2s1.15,1.7,1.15,2.8v12c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15H4c-1.1,0-2.03-.38-2.8-1.15-.8-.8-1.2-1.75-1.2-2.85v-12.05c0-1.1.4-2.03,1.2-2.8.77-.8,1.7-1.2,2.8-1.2v-5.6C5.5,24.22,26.23,4.77,66.2,3.9h1.8c.03-1.07.43-1.97,1.2-2.7.77-.8,1.7-1.2,2.8-1.2h12.05c1.1,0,2.05.4,2.85,1.2.77.77,1.15,1.7,1.15,2.8v4M72,50.6v4h12v-8l.05-38.6v-4h-12.05v3.9h-5.75C28.88,8.7,9.47,26.87,8,62.4v9.45h-4v12.05h50.6v-12h-4c-.07-4.33.5-8,1.7-11,2.47-6.4,7.75-9.83,15.85-10.3h3.85" fill="#527914" stroke-width="0"/><path d="M72,50.6V4h12.05v4l-.05,38.6v8h-12v-4M50.6,71.9h4v12H4v-12.05h4l42.6.05" fill="#80be1f" stroke-width="0"/><path
d="M72,7.9v4h-5.75c-37.37.8-56.78,18.97-58.25,54.5v-4C9.47,26.87,28.88,8.7,66.25,7.9h5.75" fill="#b2f04f" stroke-width="0"/><path d="M72,11.9v38.7h-3.85c-8.1.47-13.38,3.9-15.85,10.3-1.2,3-1.77,6.67-1.7,11l-42.6-.05v-5.45C9.47,30.87,28.88,12.7,66.25,11.9h5.75" fill="#a1e03d" stroke-width="0"/></g></svg>
''';
SVGPipesSVGSamples[CSVGPipeGb] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 87.9 88.05"><g id="Calque_1-2"><path d="M16,50.6c4.33-.07,8,.5,11,1.7,6.4,2.47,9.83,7.75,10.3,15.85v3.85h-4v12h8l38.6.05h4v-12.05h-3.9v-5.75c-.8-37.37-18.97-56.78-54.5-58.25h-9.45v-4H4v50.6h12v-4M25.7,4c38,1.5,57.43,22.23,58.3,62.2v1.8c1.07.03,1.98.43,2.75,1.2.77.77,1.15,1.7,1.15,2.8v12.05c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15h-4l-38.6-.05h-8c-1.1,0-2.03-.38-2.8-1.15-.8-.8-1.2-1.75-1.2-2.85v-12c0-1.1.4-2.03,1.2-2.8.77-.8,1.7-1.2,2.8-1.2-.47-6.03-3.05-10.02-7.75-11.95-1.63-.63-3.48-1.07-5.55-1.3-.03,1.03-.42,1.93-1.15,2.7-.8.77-1.75,1.15-2.85,1.15H4c-1.1,0-2.03-.38-2.8-1.15-.8-.8-1.2-1.75-1.2-2.85V4C0,2.9.4,1.97,1.2,1.2,1.97.4,2.9,0,4,0h12.05c1.1,0,2.05.4,2.85,1.2.77.77,1.15,1.7,1.15,2.8h5.65" fill="#527914" stroke-width="0"/><path d="M16,50.6v4H4V4h12.05v8l-.05,38.6M80,72h3.9v12.05h-4l-38.6-.05h-8v-12h46.7" fill="#80be1f" stroke-width="0"/><path
d="M16.05,12v-4h9.45c35.53,1.47,53.7,20.88,54.5,58.25v4c-.8-37.37-18.97-56.78-54.5-58.25h-9.45" fill="#b2f04f" stroke-width="0"/><path d="M16,50.6l.05-38.6h9.45c35.53,1.47,53.7,20.88,54.5,58.25v1.75h-42.7v-3.85c-.47-8.1-3.9-13.38-10.3-15.85-3-1.2-6.67-1.77-11-1.7" fill="#a1e03d" stroke-width="0"/></g></svg>
''';
SVGPipesSVGSamples[CSVGPipeGd] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 117.55 58.6"><g id="Calque_1-2"><path d="M16.05,8.05v-4.05H4v50.6h12v-4h85.5v4h12l.05-50.6h-12.05v4.05H16.05M16.05,0c1.1,0,2.05.4,2.85,1.2.77.77,1.15,1.7,1.15,2.8v.05h77.45v-.05c0-1.1.4-2.03,1.2-2.8.77-.8,1.7-1.2,2.8-1.2h12.05c1.1,0,2.05.4,2.85,1.2.77.77,1.15,1.7,1.15,2.8l-.05,50.6c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15h-12c-1.1,0-2.03-.38-2.8-1.15s-1.2-1.75-1.2-2.85H20c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15H4c-1.1,0-2.03-.38-2.8-1.15s-1.2-1.75-1.2-2.85V4C0,2.9.4,1.97,1.2,1.2c.77-.8,1.7-1.2,2.8-1.2h12.05" fill="#527914" stroke-width="0"/><path d="M16,50.6v4H4V4h12.05v8.05l-.05,38.55M101.5,8.05v-4.05h12.05l-.05,50.6h-12V8.05" fill="#80be1f" stroke-width="0"/><path d="M101.5,8.05v4H16.05v-4h85.45" fill="#b2f04f" stroke-width="0"/><path d="M16,50.6l.05-38.55h85.45v38.55H16" fill="#a1e03d" stroke-width="0"/></g></svg>
''';
SVGPipesSVGSamples[CSVGPipeGdb] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 117.55 88.05"><g id="Calque_1-2"><path
d="M26.6,4.05h70.9v-.05c0-1.1.4-2.03,1.2-2.8.77-.8,1.7-1.2,2.8-1.2h12.05c1.1,0,2.05.4,2.85,1.2.77.77,1.15,1.7,1.15,2.8l-.05,50.6c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15h-12c-1.1,0-2.03-.38-2.8-1.15-.8-.8-1.2-1.75-1.2-2.85h-14.35c.47,3.63.75,7.48.85,11.55v1.85c1.07.03,1.98.43,2.75,1.2s1.15,1.7,1.15,2.8v12.05c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15h-4l-38.6-.05h-8c-1.1,0-2.03-.38-2.8-1.15-.8-.8-1.2-1.75-1.2-2.85v-12c0-1.1.4-2.03,1.2-2.8.77-.8,1.7-1.2,2.8-1.2-.47-6.03-3.05-10.02-7.75-11.95-1.63-.63-3.48-1.07-5.55-1.3-.03,1.03-.42,1.93-1.15,2.7-.8.77-1.75,1.15-2.85,1.15H4c-1.1,0-2.03-.38-2.8-1.15s-1.2-1.75-1.2-2.85V4l.05-.65c.13-.83.52-1.55,1.15-2.15.77-.8,1.7-1.2,2.8-1.2h12.05c1.1,0,2.05.4,2.85,1.2.77.77,1.15,1.7,1.15,2.8h5.45l1.1.05M101.5,8.05H26.6l-1.1-.05h-9.45v-4H4v50.6h12v-4h1.7c3.6.1,6.7.67,9.3,1.7,6.4,2.47,9.83,7.75,10.3,15.85v3.85h-4v12h8l38.6.05h4v-12.05h-3.9v-5.75c-.13-5.63-.65-10.85-1.55-15.65h23.05v4h12l.05-50.6h-12.05v4.05" fill="#527914"
stroke-width="0"/><path d="M101.5,8.05v4H16.05v-4.05h9.45l1.1.05h74.9M78.45,50.6c.9,4.8,1.42,10.02,1.55,15.65v4c-.17-7.23-.97-13.78-2.4-19.65h.85" fill="#b2f04f" stroke-width="0"/><path d="M16.05,8v4.05l-.05,38.55v4H4V4h12.05v4M101.5,12.05V4h12.05l-.05,50.6h-12V12.05M80,72h3.9v12.05h-4l-38.6-.05h-8v-12h46.7" fill="#80be1f" stroke-width="0"/><path d="M80,72h-42.7v-3.85c-.47-8.1-3.9-13.38-10.3-15.85-2.6-1.03-5.7-1.6-9.3-1.7h-1.7l.05-38.55h85.45v38.55h-23.9c1.43,5.87,2.23,12.42,2.4,19.65v1.75" fill="#a1e03d" stroke-width="0"/></g></svg>
''';
SVGPipesSVGSamples[CSVGPipeHb] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 58.6 117.55"><g id="Calque_1-2"><path d="M0,4C0,2.9.4,1.97,1.2,1.2c.77-.8,1.7-1.2,2.8-1.2l50.6.05c1.1,0,2.05.4,2.85,1.2.77.77,1.15,1.7,1.15,2.8v12c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15v77.5c1.1,0,2.05.4,2.85,1.2.77.77,1.15,1.7,1.15,2.8v12c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15H4c-1.1,0-2.03-.38-2.8-1.15-.8-.8-1.2-1.75-1.2-2.85v-12.05c0-1.1.4-2.03,1.2-2.8.77-.8,1.7-1.2,2.8-1.2h.05V20.05h-.05c-1.1,0-2.03-.38-2.8-1.15-.8-.8-1.2-1.75-1.2-2.85V4M50.6,16.05h4V4.05l-50.6-.05v12.05h4.05v85.45h-4.05v12.05h50.6v-12h-4V16.05" fill="#527914" stroke-width="0"/><path d="M50.6,16.05H4V4l50.6.05v12h-4M50.6,101.55h4v12H4v-12.05h4.05l42.55.05" fill="#80be1f" stroke-width="0"/><path d="M50.6,101.55l-42.55-.05V16.05h42.55v85.5" fill="#a1e03d" stroke-width="0"/></g></svg>
''';
SVGPipesSVGSamples[CSVGPipeHd] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 88.05 87.9"><g id="Calque_1-2"><path d="M50.6,17.7v-1.7h4V4H4v12.05h4v9.45c1.47,35.53,20.88,53.7,58.25,54.5h5.75v3.9h12.05v-4l-.05-38.6v-8h-12v4h-3.85c-8.1-.47-13.38-3.9-15.85-10.3-1.03-2.6-1.6-5.7-1.7-9.3M57.45,1.2c.77.77,1.15,1.7,1.15,2.8v12c0,1.1-.38,2.05-1.15,2.85-.77.73-1.67,1.12-2.7,1.15.23,2.07.67,3.92,1.3,5.55,1.93,4.7,5.92,7.28,11.95,7.75,0-1.1.4-2.03,1.2-2.8.77-.8,1.7-1.2,2.8-1.2h12c1.1,0,2.05.4,2.85,1.2.77.77,1.15,1.7,1.15,2.8v8l.05,38.6v4c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15h-12.05c-1.1,0-2.03-.38-2.8-1.15s-1.17-1.68-1.2-2.75h-1.8c-39.97-.87-60.7-20.3-62.2-58.3v-5.65c-1.1,0-2.03-.38-2.8-1.15-.8-.8-1.2-1.75-1.2-2.85V4C0,2.9.4,1.97,1.2,1.2c.77-.8,1.7-1.2,2.8-1.2h50.6c1.1,0,2.05.4,2.85,1.2" fill="#527914" stroke-width="0"/><path d="M8,16.05h-4V4h50.6v12h-4l-42.6.05M72,37.3v-4h12v8l.05,38.6v4h-12.05v-46.6" fill="#80be1f" stroke-width="0"/><path
d="M50.6,16v4c-.07,4.33.5,8,1.7,11,2.47,6.4,7.75,9.83,15.85,10.3h3.85v38.7h-5.75c-37.37-.8-56.78-18.97-58.25-54.5v-9.45l42.6-.05" fill="#a1e03d" stroke-width="0"/><path d="M50.6,17.7c.1,3.6.67,6.7,1.7,9.3,2.47,6.4,7.75,9.83,15.85,10.3h3.85v4h-3.85c-8.1-.47-13.38-3.9-15.85-10.3-1.2-3-1.77-6.67-1.7-11v-2.3" fill="#b2f04f" stroke-width="0"/></g></svg>
''';
SVGPipesSVGSamples[CSVGPipeHdb] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 88.05 117.55"><g id="Calque_1-2"><path
d="M4,0l50.6.05c1.1,0,2.05.4,2.85,1.2.77.77,1.15,1.7,1.15,2.8v12c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15v14.4l11.55-.9h1.85c.03-1.07.43-1.97,1.2-2.7.77-.8,1.7-1.2,2.8-1.2h12.05c1.1,0,2.05.4,2.85,1.2.77.77,1.15,1.7,1.15,2.8v4l-.05,38.6v8c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15h-12c-1.1,0-2.03-.38-2.8-1.15-.8-.8-1.2-1.73-1.2-2.8-6.03.43-10.02,3-11.95,7.7v.05c-.63,1.6-1.07,3.43-1.3,5.5,1.03.03,1.93.43,2.7,1.2.77.77,1.15,1.7,1.15,2.8v12c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15H4c-1.1,0-2.03-.38-2.8-1.15-.8-.8-1.2-1.75-1.2-2.85v-12.05c0-1.1.4-2.03,1.2-2.8.77-.8,1.7-1.2,2.8-1.2v-5.45l.05-1.15V20.05h-.05c-1.1,0-2.03-.38-2.8-1.15-.8-.8-1.2-1.75-1.2-2.85V4C0,2.9.4,1.97,1.2,1.2,1.97.4,2.9,0,4,0M50.6,16.05h4V4.05l-50.6-.05v12.05h4.05v74.95l-.05,1.05v9.45h-4v12.05h50.6v-12h-4v-1.7c.1-3.6.67-6.7,1.7-9.3,2.47-6.4,7.75-9.83,15.85-10.3h3.85v4h12v-8l.05-38.6v-4h-12.05v3.9h-5.75c-5.63.13-10.85.65-15.65,1.55v-23.05" fill="#527914" stroke-width="0"/><path
d="M50.6,16.05H4V4l50.6.05v12h-4M72,37.55v-3.9h12.05v4l-.05,38.6v8h-12v-46.7M50.6,101.55h4v12H4v-12.05h4l42.6.05" fill="#80be1f" stroke-width="0"/><path d="M50.6,39.1c4.8-.9,10.02-1.42,15.65-1.55h5.75v4h-5.75c-5.63.13-10.85.65-15.65,1.55v-4M8,95.7v-3.65l.05-1.05v4l-.05.7" fill="#b2f04f" stroke-width="0"/><path d="M8,95.7l.05-.7V16.05h42.55v27.05c4.8-.9,10.02-1.42,15.65-1.55h5.75v38.7h-3.85c-8.1.47-13.38,3.9-15.85,10.3-1.03,2.6-1.6,5.7-1.7,9.3v1.7l-42.6-.05v-5.8" fill="#a1e03d" stroke-width="0"/></g></svg>
''';
SVGPipesSVGSamples[CSVGPipeHdbg] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 117.55 117.55"><g id="Calque_1-2"><path
d="M88.05,16.05c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15v13.45h13.45v-.05c0-1.1.4-2.03,1.2-2.8.77-.8,1.7-1.2,2.8-1.2h12.05c1.1,0,2.05.4,2.85,1.2s1.15,1.7,1.15,2.8l-.05,50.6c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15h-12c-1.1,0-2.03-.38-2.8-1.15-.8-.8-1.2-1.75-1.2-2.85h-13.45v13.5c1.1,0,2.05.4,2.85,1.2.77.77,1.15,1.7,1.15,2.8v12c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15h-50.6c-1.1,0-2.03-.38-2.8-1.15s-1.2-1.75-1.2-2.85v-12.05c0-1.1.4-2.03,1.2-2.8.77-.8,1.7-1.2,2.8-1.2h.05v-13.45h-13.5c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15H4c-1.1,0-2.03-.38-2.8-1.15-.8-.8-1.2-1.75-1.2-2.85v-50.6c0-1.1.4-2.03,1.2-2.8.77-.8,1.7-1.2,2.8-1.2h12.05c1.1,0,2.05.4,2.85,1.2s1.15,1.7,1.15,2.8v.05h13.45v-13.45h-.05c-1.1,0-2.03-.38-2.8-1.15s-1.2-1.75-1.2-2.85V4c0-1.1.4-2.03,1.2-2.8.77-.8,1.7-1.2,2.8-1.2l50.6.05c1.1,0,2.05.4,2.85,1.2.77.77,1.15,1.7,1.15,2.8v12M80.05,
16.05h4V4.05l-50.6-.05v12.05h4.05v21.45h-21.45v-4.05H4v50.6h12v-4h21.5v21.45h-4.05v12.05h50.6v-12h-4v-21.5h21.45v4h12l.05-50.6h-12.05v4.05h-21.45v-21.45" fill="#527914" stroke-width="0"/><path d="M80.05,16.05h-46.6V4l50.6.05v12h-4M101.5,37.5v-4.05h12.05l-.05,50.6h-12v-46.55M80.05,101.55h4v12h-50.6v-12.05h4.05l42.55.05M16,80.05v4H4v-50.6h12.05v8.05l-.05,38.55" fill="#80be1f" stroke-width="0"/><path d="M80.05,37.5h21.45v4h-21.45v-4M16.05,37.5h21.45v4h-21.45v-4" fill="#b2f04f" stroke-width="0"/><path d="M80.05,16.05v25.45h21.45v38.55h-21.45v21.5l-42.55-.05v-21.45h-21.5l.05-38.55h21.45v-25.45h42.55" fill="#a1e03d" stroke-width="0"/></g></svg>
''';
SVGPipesSVGSamples[CSVGPipeHg] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 87.9 88.05"><g id="Calque_1-2"><path d="M37.3,16.05v3.85c-.47,8.1-3.9,13.38-10.3,15.85-3,1.2-6.67,1.77-11,1.7v-4H4v50.6h12.05v-4h9.45c35.53-1.47,53.7-20.88,54.5-58.25v-5.75h3.9V4h-4l-38.6.05h-8v12h4M30.5,18.9c-.8-.8-1.2-1.75-1.2-2.85V4.05c0-1.1.4-2.03,1.2-2.8.77-.8,1.7-1.2,2.8-1.2h8l38.6-.05h4c1.1,0,2.05.4,2.85,1.2.77.77,1.15,1.7,1.15,2.8v12.05c0,1.1-.38,2.05-1.15,2.85-.77.73-1.68,1.12-2.75,1.15v1.85c-.87,39.93-20.3,60.65-58.3,62.15h-5.65c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15H4c-1.1,0-2.03-.38-2.8-1.15-.8-.8-1.2-1.75-1.2-2.85v-50.6c0-1.1.4-2.03,1.2-2.8.77-.8,1.7-1.2,2.8-1.2h12c1.1,0,2.05.4,2.85,1.2.73.73,1.12,1.62,1.15,2.65,2.07-.23,3.92-.65,5.55-1.25,4.7-1.97,7.28-5.97,7.75-12-1.1,0-2.03-.38-2.8-1.15" fill="#527914" stroke-width="0"/><path d="M16,37.45c4.33.07,8-.5,11-1.7,6.4-2.47,9.83-7.75,10.3-15.85v4c-.47,8.1-3.9,13.38-10.3,15.85-3,1.2-6.67,1.77-11,1.7v-4"
fill="#b2f04f" stroke-width="0"/><path d="M37.3,19.9v-3.85h42.7v5.75c-.8,37.37-18.97,56.78-54.5,58.25h-9.45l-.05-38.6c4.33.07,8-.5,11-1.7,6.4-2.47,9.83-7.75,10.3-15.85v-4" fill="#a1e03d" stroke-width="0"/><path d="M37.3,16.05h-4V4.05h8l38.6-.05h4v12.05h-46.6M16.05,80.05v4H4v-50.6h12v8l.05,38.6" fill="#80be1f" stroke-width="0"/></g></svg>
''';
SVGPipesSVGSamples[CSVGPipeHgb] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 87.9 117.55"><g id="Calque_1-2"><path
d="M87.9,4.05v12c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15v72.65l.1,2.95v1.85c1.07.03,1.98.43,2.75,1.2s1.15,1.7,1.15,2.8v12.05c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15h-50.6c-1.1,0-2.03-.38-2.8-1.15-.8-.8-1.2-1.75-1.2-2.85v-12.05c0-1.1.4-2.03,1.2-2.8.77-.8,1.7-1.2,2.8-1.2-.47-6.03-3.05-10.02-7.75-11.95-1.63-.63-3.48-1.07-5.55-1.3-.03,1.03-.42,1.93-1.15,2.7-.8.77-1.75,1.15-2.85,1.15H4c-1.1,0-2.03-.38-2.8-1.15-.8-.8-1.2-1.75-1.2-2.85v-50.6c0-1.1.4-2.03,1.2-2.8.77-.8,1.7-1.2,2.8-1.2h12.05c1.1,0,2.05.4,2.85,1.2.77.77,1.15,1.7,1.15,2.8h5.6l7.7.65v-14.1h-.05c-1.1,0-2.03-.38-2.8-1.15-.8-.8-1.2-1.75-1.2-2.85V4c0-1.1.4-2.03,1.2-2.8C31.27.4,32.2,0,33.3,0l50.6.05c1.1,0,2.05.4,2.85,1.2.77.77,1.15,1.7,1.15,2.8M79.9,16.05h4V4.05l-50.6-.05v12.05h4.05v22.75c-3.7-.7-7.65-1.13-11.85-1.3h-9.45v-4H4v50.6h12v-4c4.33-.07,8,.5,11,1.7,6.4,2.47,9.83,7.75,10.3,15.85v3.85h-4v12.05h50.6v-12.05h-3.9v-5.75l-.1-2.9V16.05" fill="#527914" stroke-width="0"/><path
d="M79.9,16.05h-46.6V4l50.6.05v12h-4M80,101.5h3.9v12.05h-50.6v-12.05h4l42.6.05v-.05h.1M16,80.1v4H4v-50.6h12.05v8l-.05,38.6" fill="#80be1f" stroke-width="0"/><path d="M79.9,92.85l.1,2.9v4l-.1-2.9v-4M16.05,37.5h9.45c4.2.17,8.15.6,11.85,1.3v4c-3.7-.7-7.65-1.13-11.85-1.3h-9.45v-4" fill="#b2f04f" stroke-width="0"/><path d="M37.35,16.05h42.55v80.8l.1,2.9v1.75h-.1v.05l-42.6-.05v-3.85c-.47-8.1-3.9-13.38-10.3-15.85-3-1.2-6.67-1.77-11-1.7l.05-38.6h9.45c4.2.17,8.15.6,11.85,1.3v-26.75" fill="#a1e03d" stroke-width="0"/></g></svg>
''';
SVGPipesSVGSamples[CSVGPipeHgd] := '''
<?xml version="1.0" encoding="UTF-8"?><svg id="Calque_2" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 117.55 87.9"><g id="Calque_1-2"><path d="M80.05,16v4c-.07,4.33.5,8,1.7,11,1,2.6,2.48,4.72,4.45,6.35,2.8,2.37,6.6,3.68,11.4,3.95h3.85v38.7h-5.75l-2.9-.1H16l.05-38.55h23.55l-.85-4c-.7-3.7-1.13-7.65-1.3-11.85v-9.45l42.6-.05" fill="#a1e03d" stroke-width="0"/><path d="M80.05,17.7c.1,3.6.67,6.7,1.7,9.3,2.47,6.4,7.75,9.83,15.85,10.3h3.85v4h-3.85c-4.8-.27-8.6-1.58-11.4-3.95-1.97-1.63-3.45-3.75-4.45-6.35-1.2-3-1.77-6.67-1.7-11v-2.3M16.05,37.35h22.7l.85,4h-23.55v-4" fill="#b2f04f" stroke-width="0"/><path
d="M16.05,37.35v-4.05H4v50.6h12v-4h76.8l2.9.1h5.75v3.9h12.05l.05-50.6h-12.1v4h-3.85c-8.1-.47-13.38-3.9-15.85-10.3-1.03-2.6-1.6-5.7-1.7-9.3v-1.7h4V4h-50.6v12.05h4v9.45c.17,4.2.6,8.15,1.3,11.85h-22.7M30.65,1.2c.77-.8,1.7-1.2,2.8-1.2h50.6c1.1,0,2.05.4,2.85,1.2s1.15,1.7,1.15,2.8v12c0,1.1-.38,2.05-1.15,2.85-.77.73-1.67,1.12-2.7,1.15.23,2.07.67,3.92,1.3,5.55,1.93,4.7,5.92,7.28,11.95,7.75,0-1.1.4-2.03,1.2-2.8.77-.8,1.7-1.2,2.8-1.2h12.1c1.1,0,2.05.4,2.85,1.2s1.15,1.7,1.15,2.8l-.05,50.6c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15h-12.05c-1.1,0-2.03-.38-2.8-1.15-.77-.77-1.17-1.68-1.2-2.75h-1.85l-2.8-.1H20c0,1.1-.38,2.05-1.15,2.85-.8.77-1.75,1.15-2.85,1.15H4c-1.1,0-2.03-.38-2.8-1.15s-1.2-1.75-1.2-2.85v-50.6c0-1.1.4-2.03,1.2-2.8.77-.8,1.7-1.2,2.8-1.2h12.05c1.1,0,2.05.4,2.85,1.2s1.15,1.7,1.15,2.8v.05h14.05c-.33-2.47-.55-5.03-.65-7.7v-5.6c-1.1,0-2.03-.38-2.8-1.15s-1.2-1.75-1.2-2.85V4c0-1.1.4-2.03,1.2-2.8" fill="#527914" stroke-width="0"/><path
d="M37.45,16.05h-4V4h50.6v12h-4l-42.6.05M101.45,37.3v-4h12.1l-.05,50.6h-12.05v-46.6M16,79.9v4H4v-50.6h12.05v8.05l-.05,38.55" fill="#80be1f" stroke-width="0"/></g></svg>
''';

end.
