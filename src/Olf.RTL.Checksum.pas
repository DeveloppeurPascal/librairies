unit Olf.RTL.Checksum;

interface

// This unit calculates checksums.
//
// If you work between Delphi and Delphi (or Pascal) projects, you can use this
// file. It has been explained at
// https://developpeur-pascal.fr/calculer-et-verifier-un-checksum-pour-dialoguer-avec-l-exterieur.html
//
// If you send things to a web server with PHP, you can generate a checksum or
// test it with the code explained at
// https://trucs-de-developpeur-web.fr/calculer-et-verifier-un-checksum-pour-dialoguer-avec-l-exterieur.html

uses
  system.classes;

type
  TChecksumVerifParamList = class(TStrings)
    function addParam(ch: string): TChecksumVerifParamList;
  end;

  ChecksumVerif = class
  private
    class function get(param: string; key1: string; key2: string; key3: string;
      key4: string; key5: string; isPublic: boolean): string; overload;
  public
    class function get(param: TChecksumVerifParamList; key1: string = '';
      key2: string = ''; key3: string = ''; key4: string = '';
      key5: string = ''): string; overload;
    class function get(param: string; key1: string = ''; key2: string = '';
      key3: string = ''; key4: string = ''; key5: string = ''): string;
      overload;
    class function check(verif: string; param: TChecksumVerifParamList;
      key1: string = ''; key2: string = ''; key3: string = '';
      key4: string = ''; key5: string = ''): boolean; overload;
    class function check(verif: string; param: string; key1: string = '';
      key2: string = ''; key3: string = ''; key4: string = '';
      key5: string = ''): boolean; overload;
  end;

implementation

uses
  u_md5;

class function ChecksumVerif.get(param: string; key1: string; key2: string;
  key3: string; key4: string; key5: string; isPublic: boolean): string;
var
  verif: string;
begin
  verif := MD5(param + key1 + key2 + key3 + key4 + key5);
  if isPublic then
    result := copy(verif, 1 + random(Length(verif) - 10), 10)
  else
    result := verif;
end;

class function ChecksumVerif.get(param: TChecksumVerifParamList;
  key1: string = ''; key2: string = ''; key3: string = ''; key4: string = '';
  key5: string = ''): string;
var
  i: integer;
  ch: string;
begin
  ch := '';
  for i := 0 to param.Count - 1 do
  begin
    ch := ch + param[i];
  end;
  result := get(ch, key1, key2, key3, key4, key5, true);
  param.Free;
end;

class function ChecksumVerif.get(param: string; key1: string = '';
  key2: string = ''; key3: string = ''; key4: string = '';
  key5: string = ''): string;
begin
  result := get(param, key1, key2, key3, key4, key5, true);
end;

class function ChecksumVerif.check(verif: string;
  param: TChecksumVerifParamList; key1: string = ''; key2: string = '';
  key3: string = ''; key4: string = ''; key5: string = ''): boolean;
var
  i: integer;
  ch: string;
begin
  ch := '';
  for i := 0 to param.Count - 1 do
  begin
    ch := ch + param[i];
  end;
  result := check(verif, ch, key1, key2, key3, key4, key5);
end;

class function ChecksumVerif.check(verif: string; param: string;
  key1: string = ''; key2: string = ''; key3: string = ''; key4: string = '';
  key5: string = ''): boolean;
var
  verif_: string;
begin
  verif_ := get(param, key1, key2, key3, key4, key5, false);
  result := 0 < pos(verif, verif_);
end;

{ TChecksumVerifParamList }

function TChecksumVerifParamList.addParam(ch: string): TChecksumVerifParamList;
begin
  add(ch);
  result := self;
end;

initialization

randomize;

end.
