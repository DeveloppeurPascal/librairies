unit uParam;

{
  Gestion automatisée d'un fichier de stockage sous forme de dictionnaire de
  données (clé / valeur) pour les logiciels développés sous Delphi.

  Logiciel open source distribué sous licence MIT.
  Open source software distributed under the MIT license

  Copyright Patrick Prémartin / Olf Software

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

  Find the original source code on
  https://github.com/DeveloppeurPascal/librairies

  Find explanations on
  https://www.developpeur-pascal.fr/p/_5007-plutot-ini-ou-json-pour-stocker-vos-parametres.html

  Liste des mises à jour :
  => 11/05/2020 , Patrick Prémartin :
  *     ajout de la notice de copyright et licence d'utilisation
  *     gestion de données de types Syngle et TDataTime
  *     correction d'une perte de mémoire au niveau du remplacement d'une valeur sur un paramètre existant
}
interface

type
  tParams = class(TObject)
    class procedure save;
    class procedure load;
    class function getValue(key: string; default: string): string; overload;
    class function getValue(key: string; default: boolean): boolean; overload;
    class function getValue(key: string; default: integer): integer; overload;
    class function getValue(key: string; default: single): single; overload;
    class function getValue(key: string; default: TDateTime)
      : TDateTime; overload;
    class procedure setValue(key, value: string); overload;
    class procedure setValue(key: string; value: boolean); overload;
    class procedure setValue(key: string; value: integer); overload;
    class procedure setValue(key: string; value: single); overload;
    class procedure setValue(key: string; value: TDateTime); overload;
  end;

implementation

uses
  System.Generics.collections, System.IOUtils, System.SysUtils, System.JSON,
  System.Classes;

var
  paramChanged: boolean;
  paramList: TJSONObject;

function getParamsFileName: string;
var
  folder: string;
  filename: string;
  app_name: string;
begin
  app_name := TPath.GetFileNameWithoutExtension(paramstr(0));
  folder := TPath.Combine(TPath.GetDocumentsPath, app_name);
  if not tdirectory.Exists(folder) then
    tdirectory.CreateDirectory(folder);
{$IF Defined(DEBUG)}
  filename := app_name + '-debug.par';
{$ELSE if Defined(RELEASE)}
  filename := app_name + '.par';
{$ELSE}
{$MESSAGE FATAIL 'setup problem'}
{$ENDIF}
  result := TPath.Combine(folder, filename);
end;

function getParamValue(key: string): TJSONValue;
begin
  result := nil;
  if assigned(paramList) then
    if (paramList.Count > 0) then
      result := paramList.getValue(key);
end;

procedure setParamValue(key: string; value: TJSONValue);
begin
  if not assigned(paramList) then
    paramList := TJSONObject.Create
  else if (paramList.Count > 0) and (nil <> paramList.getValue(key)) then
    paramList.RemovePair(key).Free;
  paramList.AddPair(key, value);
  paramChanged := true;
end;

class function tParams.getValue(key: string; default: boolean): boolean;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if assigned(jsonvalue) then
    result := jsonvalue.value.ToBoolean
  else
    result := default;
end;

class function tParams.getValue(key: string; default: string): string;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if assigned(jsonvalue) then
    result := jsonvalue.value
  else
    result := default;
end;

class function tParams.getValue(key: string; default: integer): integer;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if assigned(jsonvalue) then
    result := jsonvalue.value.ToInteger
  else
    result := default;
end;

class function tParams.getValue(key: string; default: single): single;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if assigned(jsonvalue) then
    result := jsonvalue.value.ToSingle
  else
    result := default;
end;

class function tParams.getValue(key: string; default: TDateTime): TDateTime;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if assigned(jsonvalue) then
    result := strToDateTime(jsonvalue.value)
  else
    result := default;
end;

class procedure tParams.load;
var
  filename: string;
  buffer: tStringStream;
begin
  filename := getParamsFileName;
  if tfile.Exists(filename) then
  begin
    if assigned(paramList) then
      FreeAndNil(paramList);
    buffer := tStringStream.Create(tfile.ReadAllText(filename, TEncoding.UTF8),
      TEncoding.UTF8);
    try
      paramList := TJSONObject.Create;
      paramList.Parse(buffer.Bytes, 0);
    finally
      buffer.Free;
    end;
  end;
end;

class procedure tParams.save;
var
  filename: string;
begin
  if (paramChanged) then
  begin
    filename := getParamsFileName;
    if assigned(paramList) and (paramList.Count > 0) then
      tfile.WriteAllText(filename, paramList.ToJSON, TEncoding.UTF8)
    else if tfile.Exists(filename) then
      tfile.Delete(filename);
    paramChanged := false;
  end;
end;

class procedure tParams.setValue(key: string; value: single);
var
  jsonvalue: TJSONNumber;
begin
  jsonvalue := TJSONNumber.Create(value);
  try
    setParamValue(key, jsonvalue);
  except
    jsonvalue.Free;
  end;
end;

class procedure tParams.setValue(key: string; value: TDateTime);
var
  jsonvalue: TJSONString;
begin
  jsonvalue := TJSONString.Create(DateTimeToStr(value));
  try
    setParamValue(key, jsonvalue);
  except
    jsonvalue.Free;
  end;
end;

class procedure tParams.setValue(key, value: string);
var
  jsonvalue: TJSONString;
begin
  jsonvalue := TJSONString.Create(value);
  try
    setParamValue(key, jsonvalue);
  except
    jsonvalue.Free;
  end;
end;

class procedure tParams.setValue(key: string; value: boolean);
var
  jsonvalue: TJSONBool;
begin
  jsonvalue := TJSONBool.Create(value);
  try
    setParamValue(key, jsonvalue);
  except
    jsonvalue.Free;
  end;
end;

class procedure tParams.setValue(key: string; value: integer);
var
  jsonvalue: TJSONNumber;
begin
  jsonvalue := TJSONNumber.Create(value);
  try
    setParamValue(key, jsonvalue);
  except
    jsonvalue.Free;
  end;
end;

initialization

paramChanged := false;
paramList := TJSONObject.Create;
tParams.load;

finalization

tParams.save;
if assigned(paramList) then
  FreeAndNil(paramList);

end.
