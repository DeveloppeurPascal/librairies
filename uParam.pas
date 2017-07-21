unit uParam;

interface

type
  tParams = class
    class procedure save;
    class procedure load;
    class function getValue(key: string; default: string): string; overload;
    class function getValue(key: string; default: boolean): boolean; overload;
    class function getValue(key: string; default: integer): integer; overload;
    class procedure setValue(key, value: string); overload;
    class procedure setValue(key: string; value: boolean); overload;
    class procedure setValue(key: string; value: integer); overload;
  end;

implementation

uses
  System.Generics.collections, System.IOUtils, System.SysUtils, System.JSON,
  System.Classes;

var
  paramChanged: boolean;
  paramList: TJSONObject;
  paramFileName: string;

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
{$IFDEF DEBUG}
  filename := app_name + '-debug.par';
{$ELSE}
  filename := app_name + '.par';
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
    paramList.RemovePair(key);
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
      buffer.free;
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

class procedure tParams.setValue(key, value: string);
var
  jsonvalue: TJSONString;
begin
  jsonvalue := TJSONString.Create(value);
  try
    setParamValue(key, jsonvalue);
  except
    jsonvalue.free;
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
    jsonvalue.free;
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
    jsonvalue.free;
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
