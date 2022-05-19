unit Olf.RTL.Params;

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
  => 11/05/2020, Patrick Prémartin :
  *     ajout de la notice de copyright et licence d'utilisation
  *     gestion de données de types Syngle et TDataTime
  *     correction d'une perte de mémoire au niveau du remplacement d'une valeur sur un paramètre existant
  => 22/04/2022, Patrick PRémartin :
  *     ajout de la procédure setFolderName() pour permettre la modification du dossier de stockage du fichier de paramètres
  => 19/05/2022, Patrick Prémartin :
  *     creation de Olf.RTL.Params à partir de uParam
  *     ajout de la fonction ToJSON permettant d'obtenir la représentation du fichier de stockage sous forme de chaîne JSON
  *     ajout de la fonction AsJSONObject pour obtenir la représentation sous forme de TJSONObject des paramètres
  *     ajout de la fonction getFilePath pour obtenir le nom du fichier de stockage avec son chemin d'accès (attention : ne le modifiez pas en direct si vous utilisez aussi la classe TParams dans votre projet)
  *     ajout de la procédure setFilePath() pour définir le chemin et le nom du fichier de stockage des paramètres
  *     ajout de valeurs par défaut pour la valeur par défaut dans les fonctions getValue()
  *     ajout des commentaires XMLDoc
  *     ajout de la classe TParamsFile pour rendre possible la manipulation de plusieurs fichiers de paramètres si on en a besoin
  *     ajout de MoveToFilePath() pour déplacer le fichier de paramètres actuel
  *     ajout de setValue et getValue pour des TJSONValue et ses descendants
}
interface

uses
  System.JSON;

type
  TParamsFile = class(TObject)
  private
    FParamChanged: boolean;
    FParamList: TJSONObject;
    FFolderName: string;
    FFileName: string;
  protected
    function getParamsFileName(ACreateFolder: boolean = False): string;
    function getParamValue(key: string): TJSONValue;
    procedure setParamValue(key: string; value: TJSONValue);
  public
    /// <summary>
    /// Class constructor wich just initialize private fields.
    /// </summary>
    constructor Create; overload;
    /// <summary>
    /// Class constructor wich loads the parameter file specified as parameter.
    /// </summary>
    /// <param name="AFilePath">
    /// Absolute file path to the parameter file (drive+folder+file name+extension)
    /// </param>
    constructor Create(AFilePath: string); overload;
    /// <summary>
    /// Instance destructor
    /// </summary>
    destructor Destroy; override;
    /// <summary>
    /// Save current parameters to actual parameter file
    /// </summary>
    procedure Save;
    /// <summary>
    /// Load parameters from actual parameter file
    /// </summary>
    procedure Load;
    /// <summary>
    /// Cancel current changes and reload previous saved values
    /// </summary>
    procedure Cancel;
    /// <summary>
    /// Get the string value for key parameter with an empty string as default value
    /// </summary>
    function getValue(key: string; default: string = ''): string; overload;
    /// <summary>
    /// Get the boolean value for key parameter with False as default value
    /// </summary>
    function getValue(key: string; default: boolean = False): boolean; overload;
    /// <summary>
    /// Get the integer value for key parameter with zero as default value
    /// </summary>
    function getValue(key: string; default: integer = 0): integer; overload;
    /// <summary>
    /// Get the single value for key parameter with zero as default value
    /// </summary>
    function getValue(key: string; default: single = 0): single; overload;
    /// <summary>
    /// Get the TDateTime value for key parameter with December 30th 1899 at 12:00  as default value
    /// </summary>
    function getValue(key: string; default: TDateTime = 0): TDateTime; overload;
    /// <summary>
    /// Get the JSON value for key parameter with nil as default value
    /// </summary>
    function getValue(key: string; default: TJSONValue = nil)
      : TJSONValue; overload;
    /// <summary>
    /// Set the value for key parameter as string
    /// </summary>
    procedure setValue(key, value: string); overload;
    /// <summary>
    /// Set the value for key parameter as boolean
    /// </summary>
    procedure setValue(key: string; value: boolean); overload;
    /// <summary>
    /// Set the value for key parameter as integer
    /// </summary>
    procedure setValue(key: string; value: integer); overload;
    /// <summary>
    /// Set the value for key parameter as single
    /// </summary>
    procedure setValue(key: string; value: single); overload;
    /// <summary>
    /// Set the value for key parameter as TDateTime
    /// </summary>
    procedure setValue(key: string; value: TDateTime); overload;
    /// <summary>
    /// Set the value for key parameter as TJSONValue
    /// </summary>
    procedure setValue(key: string; value: TJSONValue); overload;
    /// <summary>
    /// Change the folder where is the parameter file.
    /// </summary>
    /// <param name="AFolderName">
    /// Absolute folder path where you want to save the parameter file.
    /// </param>
    /// <param name="AReload">
    /// If set to True (by default), call the Load procedure after changing the folder.
    /// </param>
    /// <remarks>
    /// To change the file name, use setFilePath() instead of setFolderName().
    /// </remarks>
    procedure setFolderName(AFolderName: string; AReload: boolean = true);
    /// <summary>
    /// Change the folder where is the parameter file.
    /// </summary>
    /// <param name="AFilePath">
    /// Absolute file path (drive+folder+file name+extension) to the parameter file you want to use.
    /// </param>
    /// <param name="AReload">
    /// If set to True (by default), call Load procedure after changing the file path.
    /// </param>
    /// <remarks>
    /// If you only want to change the path to the parameter file, use setFolderName procedure instead of this one.
    /// </remarks>
    procedure setFilePath(AFilePath: string; AReload: boolean = true);
    /// <summary>
    /// Move actual parameter file to the new file.
    /// </summary>
    /// <param name="ANewFilePath">
    /// Absolute file path (drive+folder+file name+extension) to the parameter file you want to use.
    /// </param>
    /// <param name="ASave">
    /// If set to True, save actual values to the parameter file.
    /// If set to false, just move the parameter file to it's new folder/filename.
    /// </param>
    procedure MoveToFilePath(ANewFilePath: string; ASave: boolean = true);
    /// <summary>
    /// Return the absolute path to the parameter file (drive+folder+file name+extension)
    /// </summary>
    function getFilePath: string;
    /// <summary>
    /// Return the current parameters as a serialized JSON object.
    /// </summary>
    function ToJSON: string;
    /// <summary>
    /// Return the current parameters as a JSON object
    /// </summary>
    /// <param name="AClone">
    /// If set to True (by default), the result is a clone of actual object. Free it when you have finished to work with it or you'll have memory leaks in your projects.
    /// If set to False, the result is a reference to the internal JSON object. All changes are made to it. Don't destroy it or you'll have Access Violation exception.
    /// </param>
    function AsJSONObject(AClone: boolean = true): TJSONObject;
  end;

  TParams = class(TObject)
  public
    /// <summary>
    /// Save current parameters to actual parameter file
    /// </summary>
    class procedure Save;
    /// <summary>
    /// Load parameters from actual parameter file
    /// </summary>
    class procedure Load;
    /// <summary>
    /// Get the string value for key parameter with an empty string as default value
    /// </summary>
    class function getValue(key: string; default: string = ''): string;
      overload;
    /// <summary>
    /// Get the boolean value for key parameter with False as default value
    /// </summary>
    class function getValue(key: string; default: boolean = False)
      : boolean; overload;
    /// <summary>
    /// Get the integer value for key parameter with zero as default value
    /// </summary>
    class function getValue(key: string; default: integer = 0)
      : integer; overload;
    /// <summary>
    /// Get the single value for key parameter with zero as default value
    /// </summary>
    class function getValue(key: string; default: single = 0): single; overload;
    /// <summary>
    /// Get the TDateTime value for key parameter with December 30th 1899 at 12:00  as default value
    /// </summary>
    class function getValue(key: string; default: TDateTime = 0)
      : TDateTime; overload;
    /// <summary>
    /// Get the JSON value for key parameter with nil as default value
    /// </summary>
    class function getValue(key: string; default: TJSONValue = nil)
      : TJSONValue; overload;
    /// <summary>
    /// Set the value for key parameter as string
    /// </summary>
    class procedure setValue(key, value: string); overload;
    /// <summary>
    /// Set the value for key parameter as boolean
    /// </summary>
    class procedure setValue(key: string; value: boolean); overload;
    /// <summary>
    /// Set the value for key parameter as integer
    /// </summary>
    class procedure setValue(key: string; value: integer); overload;
    /// <summary>
    /// Set the value for key parameter as single
    /// </summary>
    class procedure setValue(key: string; value: single); overload;
    /// <summary>
    /// Set the value for key parameter as TDateTime
    /// </summary>
    class procedure setValue(key: string; value: TDateTime); overload;
    /// <summary>
    /// Set the value for key parameter as TJSONValue
    /// </summary>
    class procedure setValue(key: string; value: TJSONValue); overload;
    /// <summary>
    /// Change the folder where is the parameter file.
    /// </summary>
    /// <param name="AFolderName">
    /// Absolute folder path where you want to save the parameter file.
    /// </param>
    /// <param name="AReload">
    /// If set to True (by default), call the Load procedure after changing the folder.
    /// </param>
    /// <remarks>
    /// To change the file name, use setFilePath() instead of setFolderName().
    /// </remarks>
    class procedure setFolderName(AFolderName: string; AReload: boolean = true);
    /// <summary>
    /// Change the folder where is the parameter file.
    /// </summary>
    /// <param name="AFilePath">
    /// Absolute file path (drive+folder+file name+extension) to the parameter file you want to use.
    /// </param>
    /// <param name="AReload">
    /// If set to True (by default), call Load procedure after changing the file path.
    /// </param>
    /// <remarks>
    /// If you only want to change the path to the parameter file, use setFolderName procedure instead of this one.
    /// </remarks>
    class procedure setFilePath(AFilePath: string; AReload: boolean = true);
    /// <summary>
    /// Move actual parameter file to the new file.
    /// </summary>
    /// <param name="AFilePath">
    /// Absolute file path (drive+folder+file name+extension) to the parameter file you want to use.
    /// </param>
    class procedure MoveToFilePath(AFilePath: string);
    /// <summary>
    /// Return the absolute path to the parameter file (drive+folder+file name+extension)
    /// </summary>
    class function getFilePath: string;
    /// <summary>
    /// Return the current parameters as a serialized JSON object.
    /// </summary>
    class function ToJSON: string;
    /// <summary>
    /// Return the current parameters as a JSON object
    /// </summary>
    class function AsJSONObject: TJSONObject;
  end;

implementation

uses
  System.Generics.collections, System.IOUtils, System.SysUtils, System.Classes;

{ TParamsFile }

function TParamsFile.getParamsFileName(ACreateFolder: boolean = False): string;
var
  folder: string;
  filename: string;
  app_name: string;
begin
  // get folder name
  if FFolderName.IsEmpty then
    folder := TPath.Combine(TPath.GetDocumentsPath, app_name)
  else
    folder := FFolderName;
  // get filename
  if FFileName.IsEmpty then
  begin
    app_name := TPath.GetFileNameWithoutExtension(paramstr(0));
    if ACreateFolder and (not tdirectory.Exists(folder)) then
      tdirectory.CreateDirectory(folder);
{$IF Defined(DEBUG)}
    filename := app_name + '-debug.par';
{$ELSE if Defined(RELEASE)}
    filename := app_name + '.par';
{$ELSE}
{$MESSAGE FATAL 'setup problem'}
{$ENDIF} end
  else
    filename := FFileName;
  // get file path
  result := TPath.Combine(folder, filename);
end;

function TParamsFile.getParamValue(key: string): TJSONValue;
begin
  result := nil;
  if assigned(FParamList) then
    if (FParamList.Count > 0) then
      result := FParamList.getValue(key);
end;

procedure TParamsFile.setParamValue(key: string; value: TJSONValue);
begin
  if not assigned(FParamList) then
    FParamList := TJSONObject.Create
  else if (FParamList.Count > 0) and (nil <> FParamList.getValue(key)) then
    FParamList.RemovePair(key).Free;
  FParamList.AddPair(key, value);
  FParamChanged := true;
end;

function TParamsFile.getValue(key: string; default: boolean): boolean;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if assigned(jsonvalue) then
    result := jsonvalue.value.ToBoolean
  else
    result := default;
end;

function TParamsFile.getValue(key: string; default: string): string;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if assigned(jsonvalue) then
    result := jsonvalue.value
  else
    result := default;
end;

function TParamsFile.getValue(key: string; default: integer): integer;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if assigned(jsonvalue) then
    result := jsonvalue.value.ToInteger
  else
    result := default;
end;

function TParamsFile.getValue(key: string; default: single): single;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if assigned(jsonvalue) then
    result := jsonvalue.value.ToSingle
  else
    result := default;
end;

function TParamsFile.AsJSONObject(AClone: boolean): TJSONObject;
begin
  if not assigned(FParamList) then
    result := nil
  else if AClone then
    result := FParamList.Clone as TJSONObject
  else
    result := FParamList;
end;

constructor TParamsFile.Create;
begin
  FFolderName := '';
  FParamChanged := False;
  FParamList := TJSONObject.Create;
end;

procedure TParamsFile.Cancel;
begin
  Load;
end;

constructor TParamsFile.Create(AFilePath: string);
begin
  Create;
  setFilePath(AFilePath, true);
end;

destructor TParamsFile.Destroy;
begin
  Save;
  if assigned(FParamList) then
    FreeAndNil(FParamList);
  inherited;
end;

function TParamsFile.getFilePath: string;
begin
  result := getParamsFileName;
end;

function TParamsFile.getValue(key: string; default: TDateTime): TDateTime;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if assigned(jsonvalue) then
    result := strToDateTime(jsonvalue.value)
  else
    result := default;
end;

procedure TParamsFile.Load;
var
  filename: string;
  buffer: tStringStream;
begin
  filename := getParamsFileName;
  if tfile.Exists(filename) then
  begin
    if assigned(FParamList) then
      FreeAndNil(FParamList);
    buffer := tStringStream.Create(tfile.ReadAllText(filename, TEncoding.UTF8),
      TEncoding.UTF8);
    try
      FParamList := TJSONObject.Create;
      FParamList.Parse(buffer.Bytes, 0);
    finally
      buffer.Free;
    end;
  end;
end;

procedure TParamsFile.MoveToFilePath(ANewFilePath: string; ASave: boolean);
var
  oldFilePath: string;
begin
  oldFilePath := getFilePath;
  if (oldFilePath <> ANewFilePath) then
  begin
    tfile.Move(oldFilePath, ANewFilePath);
    setFilePath(ANewFilePath, False);
    if ASave then
      Save;
  end;
end;

procedure TParamsFile.Save;
var
  filename: string;
begin
  if (FParamChanged) then
  begin
    filename := getParamsFileName(true);
    if assigned(FParamList) and (FParamList.Count > 0) then
      tfile.WriteAllText(filename, FParamList.ToJSON, TEncoding.UTF8)
    else if tfile.Exists(filename) then
      tfile.Delete(filename);
    FParamChanged := False;
  end;
end;

procedure TParamsFile.setValue(key: string; value: single);
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

procedure TParamsFile.setValue(key: string; value: TDateTime);
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

function TParamsFile.ToJSON: string;
begin
  if assigned(FParamList) then
    result := FParamList.ToJSON
  else
    result := '';
end;

procedure TParamsFile.setValue(key, value: string);
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

procedure TParamsFile.setValue(key: string; value: boolean);
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

procedure TParamsFile.setValue(key: string; value: integer);
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

procedure TParamsFile.setFilePath(AFilePath: string; AReload: boolean);
begin
  if AFilePath.IsEmpty then
  begin
    FFolderName := '';
    FFileName := '';
  end
  else
  begin
    FFolderName := TPath.GetDirectoryName(AFilePath);
    FFileName := TPath.GetFileName(AFilePath);
  end;
  if AReload then
    Load;
end;

procedure TParamsFile.setFolderName(AFolderName: string; AReload: boolean);
begin
  FFolderName := AFolderName;
  if AReload then
    Load;
end;

function TParamsFile.getValue(key: string; default: TJSONValue): TJSONValue;
begin
  result := getParamValue(key);
  if not assigned(result) then
    result := default;
end;

procedure TParamsFile.setValue(key: string; value: TJSONValue);
begin
  setParamValue(key, value);
end;

{ TParams }

var
  DefaultParamsFile: TParamsFile;

class function TParams.AsJSONObject: TJSONObject;
begin
  result := DefaultParamsFile.AsJSONObject(true);
end;

class function TParams.getFilePath: string;
begin
  result := DefaultParamsFile.getFilePath;
end;

class function TParams.getValue(key: string; default: integer): integer;
begin
  result := DefaultParamsFile.getValue(key, default);
end;

class function TParams.getValue(key: string; default: boolean): boolean;
begin
  result := DefaultParamsFile.getValue(key, default);
end;

class function TParams.getValue(key, default: string): string;
begin
  result := DefaultParamsFile.getValue(key, default);
end;

class function TParams.getValue(key: string; default: single): single;
begin
  result := DefaultParamsFile.getValue(key, default);
end;

class function TParams.getValue(key: string; default: TDateTime): TDateTime;
begin
  result := DefaultParamsFile.getValue(key, default);
end;

class procedure TParams.Load;
begin
  DefaultParamsFile.Load;
end;

class procedure TParams.MoveToFilePath(AFilePath: string);
begin
  DefaultParamsFile.MoveToFilePath(AFilePath);
end;

class procedure TParams.Save;
begin
  DefaultParamsFile.Save;
end;

class procedure TParams.setFilePath(AFilePath: string; AReload: boolean);
begin
  DefaultParamsFile.setFilePath(AFilePath, AReload);
end;

class procedure TParams.setFolderName(AFolderName: string; AReload: boolean);
begin
  DefaultParamsFile.setFolderName(AFolderName, AReload);
end;

class procedure TParams.setValue(key: string; value: boolean);
begin
  DefaultParamsFile.setValue(key, value);
end;

class procedure TParams.setValue(key, value: string);
begin
  DefaultParamsFile.setValue(key, value);
end;

class procedure TParams.setValue(key: string; value: TDateTime);
begin
  DefaultParamsFile.setValue(key, value);
end;

class procedure TParams.setValue(key: string; value: single);
begin
  DefaultParamsFile.setValue(key, value);
end;

class procedure TParams.setValue(key: string; value: integer);
begin
  DefaultParamsFile.setValue(key, value);
end;

class function TParams.ToJSON: string;
begin
  result := DefaultParamsFile.ToJSON;
end;

class function TParams.getValue(key: string; default: TJSONValue): TJSONValue;
begin
  result := DefaultParamsFile.getValue(key, default);
end;

class procedure TParams.setValue(key: string; value: TJSONValue);
begin
  DefaultParamsFile.setValue(key, value);
end;

initialization

DefaultParamsFile := TParamsFile.Create;
TParams.Load;

finalization

TParams.Save;
if assigned(DefaultParamsFile) then
  FreeAndNil(DefaultParamsFile);

end.
