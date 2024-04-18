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
  https://developpeur-pascal.fr/plutot-ini-ou-json-pour-stocker-vos-parametres.html

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
  => 31/12/2023, Patrick Prémartin :
  *     ajout d'une variante "cardinal" du getValue() et SetValue()
}
interface

uses
  System.Classes,
  System.JSON;

type
  TParamsFile = class;

  /// <summary>
  /// Method signature for load/save events in TParamsFile
  /// </summary>
  TParamsLoadSaveEvent = procedure(Const AParamsFile: TParamsFile) of object;
  /// <summary>
  /// Procedure signature for load/save events in TParamsFile
  /// </summary>
  TParamsLoadSaveProc = reference to procedure(Const AParamsFile: TParamsFile);

  /// <summary>
  /// Method signature for the crypt event in TParamsFile
  /// </summary>
  TParamsCryptEvent = function(Const AParams: string): TStream of object;
  /// <summary>
  /// Procedure signature for the crypt event in TParamsFile
  /// </summary>
  TParamsCryptProc = reference to function(Const AParams: string): TStream;

  /// <summary>
  /// Method signature for the decrypt event in TParamsFile
  /// </summary>
  TParamsDecryptEvent = function(Const AStream: TStream): string of object;
  /// <summary>
  /// Procedure signature for the decrypt event in TParamsFile
  /// </summary>
  TParamsDecryptProc = reference to function(Const AStream: TStream): string;

  /// <summary>
  /// TParamsFile work as an instance of a settings file.
  /// You can have more than one instance for more than 1 settings file.
  /// </summary>
  TParamsFile = class(TObject)
  private
    FParamChanged: boolean;
    FParamList: TJSONObject;
    FFolderName: string;
    FFileName: string;
    FonAfterSaveEvent: TParamsLoadSaveEvent;
    FonBeforeLoadEvent: TParamsLoadSaveEvent;
    FonBeforeSaveEvent: TParamsLoadSaveEvent;
    FonAfterLoadEvent: TParamsLoadSaveEvent;
    FonAfterSaveProc: TParamsLoadSaveProc;
    FonBeforeLoadProc: TParamsLoadSaveProc;
    FonBeforeSaveProc: TParamsLoadSaveProc;
    FonAfterLoadProc: TParamsLoadSaveProc;
    FonDecryptEvent: TParamsDecryptEvent;
    FonDecryptProc: TParamsDecryptProc;
    FonCryptEvent: TParamsCryptEvent;
    FonCryptProc: TParamsCryptProc;
    FPortableMode: boolean;
    procedure SetonAfterLoadEvent(const Value: TParamsLoadSaveEvent);
    procedure SetonAfterSaveEvent(const Value: TParamsLoadSaveEvent);
    procedure SetonBeforeLoadEvent(const Value: TParamsLoadSaveEvent);
    procedure SetonBeforeSaveEvent(const Value: TParamsLoadSaveEvent);
    procedure SetonAfterLoadProc(const Value: TParamsLoadSaveProc);
    procedure SetonAfterSaveProc(const Value: TParamsLoadSaveProc);
    procedure SetonBeforeLoadProc(const Value: TParamsLoadSaveProc);
    procedure SetonBeforeSaveProc(const Value: TParamsLoadSaveProc);
    procedure SetonCryptEvent(const Value: TParamsCryptEvent);
    procedure SetonCryptProc(const Value: TParamsCryptProc);
    procedure SetonDecryptEvent(const Value: TParamsDecryptEvent);
    procedure SetonDecryptProc(const Value: TParamsDecryptProc);
    procedure SetPortableMode(const Value: boolean);
  protected
    function getParamsFileName(ACreateFolder: boolean = False): string;
    function getParamValue(key: string): TJSONValue;
    procedure setParamValue(key: string; Value: TJSONValue);
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
    /// Get the cardinal value for key parameter with zero as default value
    /// </summary>
    function getValue(key: string; default: cardinal = 0): cardinal; overload;
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
    procedure setValue(key, Value: string); overload;
    /// <summary>
    /// Set the value for key parameter as boolean
    /// </summary>
    procedure setValue(key: string; Value: boolean); overload;
    /// <summary>
    /// Set the value for key parameter as cardinal
    /// </summary>
    procedure setValue(key: string; Value: cardinal); overload;
    /// <summary>
    /// Set the value for key parameter as integer
    /// </summary>
    procedure setValue(key: string; Value: integer); overload;
    /// <summary>
    /// Set the value for key parameter as single
    /// </summary>
    procedure setValue(key: string; Value: single); overload;
    /// <summary>
    /// Set the value for key parameter as TDateTime
    /// </summary>
    procedure setValue(key: string; Value: TDateTime); overload;
    /// <summary>
    /// Set the value for key parameter as TJSONValue
    /// </summary>
    procedure setValue(key: string; Value: TJSONValue); overload;
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
    /// Initialise the folder&filename with a new default tree:
    /// => "Documents / Editor / Software" for DEBUG and iOS
    /// => "AppData (HomePath) / Editor / Software" in RELEASE (except iOS)
    /// </summary>
    procedure InitDefaultFileNameV2(Const AEditor, ASoftware: string;
      AReload: boolean = true);
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
    /// <param name="ACreateFolder">
    /// If set to True, create the folder of file parameter if it doesn't exists.
    /// </param>
    procedure MoveToFilePath(ANewFilePath: string; ASave: boolean = true;
      ACreateFolder: boolean = False);
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
    /// <summary>
    /// Called before loading the settings file.
    /// </summary>
    /// <remarks>
    /// Also called for Cancel operation (which reload the file).
    /// </remarks>
    property onBeforeLoadEvent: TParamsLoadSaveEvent read FonBeforeLoadEvent
      write SetonBeforeLoadEvent;
    property onBeforeLoadProc: TParamsLoadSaveProc read FonBeforeLoadProc
      write SetonBeforeLoadProc;
    /// <summary>
    /// Called after loading the settings file
    /// </summary>
    /// <remarks>
    /// Also called for Cancel operation (which reload the file).
    /// </remarks>
    property onAfterLoadEvent: TParamsLoadSaveEvent read FonAfterLoadEvent
      write SetonAfterLoadEvent;
    property onAfterLoadProc: TParamsLoadSaveProc read FonAfterLoadProc
      write SetonAfterLoadProc;
    /// <summary>
    /// Called before saving the settings file
    /// </summary>
    property onBeforeSaveEvent: TParamsLoadSaveEvent read FonBeforeSaveEvent
      write SetonBeforeSaveEvent;
    property onBeforeSaveProc: TParamsLoadSaveProc read FonBeforeSaveProc
      write SetonBeforeSaveProc;
    /// <summary>
    /// Called after saving the settings file
    /// </summary>
    property onAfterSaveEvent: TParamsLoadSaveEvent read FonAfterSaveEvent
      write SetonAfterSaveEvent;
    property onAfterSaveProc: TParamsLoadSaveProc read FonAfterSaveProc
      write SetonAfterSaveProc;
    /// <summary>
    /// Called before saving the parameters in a file (and after onBeforeSave).
    /// If crypted, the file is saved as a binary format.
    /// If uncrypted, the file is saved as a JSON text file.
    /// </summary>
    property onCryptEvent: TParamsCryptEvent read FonCryptEvent
      write SetonCryptEvent;
    property onCryptProc: TParamsCryptProc read FonCryptProc
      write SetonCryptProc;
    /// <summary>
    /// Called after loading the parameters from a file (and before onAfterLoad).
    /// If crypted, the file is saved as a binary format.
    /// If uncrypted, the file is saved as a JSON text file.
    /// </summary>
    property onDecryptEvent: TParamsDecryptEvent read FonDecryptEvent
      write SetonDecryptEvent;
    property onDecryptProc: TParamsDecryptProc read FonDecryptProc
      write SetonDecryptProc;
    /// <summary>
    /// Portable mode : if true nothing is saved nor loaded.
    /// Default value is false.
    /// </summary>
    property PortableMode: boolean read FPortableMode write SetPortableMode;
    /// <summary>
    /// Retire une clé des paramètres
    /// </summary>
    procedure Remove(key: string);
  end;

  /// <summary>
  /// Use TParams with its class methods if you only want to manage one settings file. If you need more than 1 file to store your settings in the same project, crreate instances of TParamsFile instead of using TParams.
  /// </summary>
  /// <remarks>
  /// TParams is here for compatibility with old projects.
  /// </remarks>
  TParams = class(TObject)
  private
    class procedure SetonAfterLoadEvent(const Value
      : TParamsLoadSaveEvent); static;
    class procedure SetonAfterLoadProc(const Value
      : TParamsLoadSaveProc); static;
    class procedure SetonAfterSaveEvent(const Value
      : TParamsLoadSaveEvent); static;
    class procedure SetonAfterSaveProc(const Value
      : TParamsLoadSaveProc); static;
    class procedure SetonBeforeLoadEvent(const Value
      : TParamsLoadSaveEvent); static;
    class procedure SetonBeforeLoadProc(const Value
      : TParamsLoadSaveProc); static;
    class procedure SetonBeforeSaveEvent(const Value
      : TParamsLoadSaveEvent); static;
    class procedure SetonBeforeSaveProc(const Value
      : TParamsLoadSaveProc); static;
    class function GetonAfterLoadEvent: TParamsLoadSaveEvent; static;
    class function GetonAfterLoadProc: TParamsLoadSaveProc; static;
    class function GetonAfterSaveEvent: TParamsLoadSaveEvent; static;
    class function GetonAfterSaveProc: TParamsLoadSaveProc; static;
    class function GetonBeforeLoadEvent: TParamsLoadSaveEvent; static;
    class function GetonBeforeLoadProc: TParamsLoadSaveProc; static;
    class function GetonBeforeSaveEvent: TParamsLoadSaveEvent; static;
    class function GetonBeforeSaveProc: TParamsLoadSaveProc; static;
    class function GetonCryptEvent: TParamsCryptEvent; static;
    class function GetonCryptProc: TParamsCryptProc; static;
    class function GetonDecryptEvent: TParamsDecryptEvent; static;
    class function GetonDecryptProc: TParamsDecryptProc; static;
    class procedure SetonCryptEvent(const Value: TParamsCryptEvent); static;
    class procedure SetonCryptProc(const Value: TParamsCryptProc); static;
    class procedure SetonDecryptEvent(const Value: TParamsDecryptEvent); static;
    class procedure SetonDecryptProc(const Value: TParamsDecryptProc); static;
    class function GetPortableMode: boolean; static;
    class procedure SetPortableMode(const Value: boolean); static;
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
    /// Get the cardinal value for key parameter with zero as default value
    /// </summary>
    class function getValue(key: string; default: cardinal = 0)
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
    class procedure setValue(key, Value: string); overload;
    /// <summary>
    /// Set the value for key parameter as boolean
    /// </summary>
    class procedure setValue(key: string; Value: boolean); overload;
    /// <summary>
    /// Set the value for key parameter as integer
    /// </summary>
    class procedure setValue(key: string; Value: integer); overload;
    /// <summary>
    /// Set the value for key parameter as cardinal
    /// </summary>
    class procedure setValue(key: string; Value: cardinal); overload;
    /// <summary>
    /// Set the value for key parameter as single
    /// </summary>
    class procedure setValue(key: string; Value: single); overload;
    /// <summary>
    /// Set the value for key parameter as TDateTime
    /// </summary>
    class procedure setValue(key: string; Value: TDateTime); overload;
    /// <summary>
    /// Set the value for key parameter as TJSONValue
    /// </summary>
    class procedure setValue(key: string; Value: TJSONValue); overload;
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
    /// Initialise the folder&filename with a new default tree:
    /// => "Documents / Editor / Software" for DEBUG and iOS
    /// => "AppData (HomePath) / Editor / Software" in RELEASE (except iOS)
    /// </summary>
    class procedure InitDefaultFileNameV2(Const AEditor, ASoftware: string;
      AReload: boolean = true);
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
    /// <param name="ACreateFolder">
    /// If set to True, create the folder of file parameter if it doesn't exists.
    /// </param>
    class procedure MoveToFilePath(ANewFilePath: string; ASave: boolean = true;
      ACreateFolder: boolean = False);
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
    /// <summary>
    /// Called before loading the settings file
    /// </summary>
    /// <remarks>
    /// Also called for Cancel operation (which reload the file).
    /// </remarks>
    class property onBeforeLoadEvent: TParamsLoadSaveEvent
      read GetonBeforeLoadEvent write SetonBeforeLoadEvent;
    class property onBeforeLoadProc: TParamsLoadSaveProc
      read GetonBeforeLoadProc write SetonBeforeLoadProc;
    /// <summary>
    /// Called after loading the settings file
    /// </summary>
    /// <remarks>
    /// Also called for Cancel operation (which reload the file).
    /// </remarks>
    class property onAfterLoadEvent: TParamsLoadSaveEvent
      read GetonAfterLoadEvent write SetonAfterLoadEvent;
    class property onAfterLoadProc: TParamsLoadSaveProc read GetonAfterLoadProc
      write SetonAfterLoadProc;
    /// <summary>
    /// Called before saving the settings file
    /// </summary>
    /// <remarks>
    /// The finalization of this unit calls the TParams.Save. If you have a BeforeSaveEvent or BeforeEventProc, beware of potential access violation by using something perhaps already destroyed.
    /// </remarks>
    class property onBeforeSaveEvent: TParamsLoadSaveEvent
      read GetonBeforeSaveEvent write SetonBeforeSaveEvent;
    class property onBeforeSaveProc: TParamsLoadSaveProc
      read GetonBeforeSaveProc write SetonBeforeSaveProc;
    /// <summary>
    /// Called after saving the settings file
    /// </summary>
    /// <remarks>
    /// The finalization of this unit calls the TParams.Save. If you have a BeforeSaveEvent or BeforeEventProc, beware of potential access violation by using something perhaps already destroyed.
    /// </remarks>
    class property onAfterSaveEvent: TParamsLoadSaveEvent
      read GetonAfterSaveEvent write SetonAfterSaveEvent;
    class property onAfterSaveProc: TParamsLoadSaveProc read GetonAfterSaveProc
      write SetonAfterSaveProc;
    /// <summary>
    /// Called before saving the parameters in a file (and after onBeforeSave).
    /// If crypted, the file is saved as a binary format.
    /// If uncrypted, the file is saved as a JSON text file.
    /// </summary>
    class property onCryptEvent: TParamsCryptEvent read GetonCryptEvent
      write SetonCryptEvent;
    class property onCryptProc: TParamsCryptProc read GetonCryptProc
      write SetonCryptProc;
    /// <summary>
    /// Called after loading the parameters from a file (and before onAfterLoad).
    /// If crypted, the file is saved as a binary format.
    /// If uncrypted, the file is saved as a JSON text file.
    /// </summary>
    class property onDecryptEvent: TParamsDecryptEvent read GetonDecryptEvent
      write SetonDecryptEvent;
    class property onDecryptProc: TParamsDecryptProc read GetonDecryptProc
      write SetonDecryptProc;
    /// <summary>
    /// Portable mode : if true nothing is done on the storage.
    /// Default value is false.
    /// </summary>
    class property PortableMode: boolean read GetPortableMode
      write SetPortableMode;
    /// <summary>
    /// Retire une clé des paramètres
    /// </summary>
    class procedure Remove(key: string);
  end;

implementation

uses
  System.Generics.collections,
  System.IOUtils,
  System.SysUtils;

{ TParamsFile }

function TParamsFile.getParamsFileName(ACreateFolder: boolean): string;
var
  Folder: string;
  FileName: string;
  AppName: string;
  Extension: string;
begin
  AppName := TPath.GetFileNameWithoutExtension(paramstr(0));

  if Assigned(onCryptEvent) or Assigned(onCryptProc) or Assigned(onDecryptEvent)
    or Assigned(onDecryptProc) then
    Extension := '.parc'
  else
    Extension := '.par';

  // get filename
  if FFileName.IsEmpty then
  begin
{$IF Defined(DEBUG)}
    FileName := AppName + '-debug' + Extension;
{$ELSE IF Defined(RELEASE)}
    FileName := app_name + Extension;
{$ELSE}
{$MESSAGE FATAL 'not implemented'}
{$ENDIF} end
  else
    FileName := FFileName;

  // get folder name
  if FFolderName.IsEmpty then
    Folder := TPath.Combine(TPath.GetDocumentsPath, AppName)
  else
    Folder := FFolderName;
  if ACreateFolder and (not tdirectory.Exists(Folder)) and (not FPortableMode)
  then
    tdirectory.CreateDirectory(Folder);

  // get file path
  result := TPath.Combine(Folder, FileName);
end;

function TParamsFile.getParamValue(key: string): TJSONValue;
begin
  result := nil;
  if Assigned(FParamList) then
    if (FParamList.Count > 0) then
      result := FParamList.getValue(key);
end;

procedure TParamsFile.setParamValue(key: string; Value: TJSONValue);
begin
  if not Assigned(FParamList) then
    FParamList := TJSONObject.Create
  else if (FParamList.Count > 0) and (nil <> FParamList.getValue(key)) then
    FParamList.RemovePair(key).Free;
  FParamList.AddPair(key, Value);
  FParamChanged := true;
end;

procedure TParamsFile.SetPortableMode(const Value: boolean);
begin
  FPortableMode := Value;
end;

procedure TParamsFile.setValue(key: string; Value: cardinal);
var
  jsonvalue: TJSONNumber;
begin
  jsonvalue := TJSONNumber.Create(Value);
  try
    setParamValue(key, jsonvalue);
  except
    jsonvalue.Free;
  end;
end;

function TParamsFile.getValue(key: string; default: boolean): boolean;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if Assigned(jsonvalue) then
    result := jsonvalue.Value.ToBoolean
  else
    result := default;
end;

function TParamsFile.getValue(key: string; default: string): string;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if Assigned(jsonvalue) then
    result := jsonvalue.Value
  else
    result := default;
end;

function TParamsFile.getValue(key: string; default: integer): integer;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if Assigned(jsonvalue) then
    result := jsonvalue.Value.ToInteger
  else
    result := default;
end;

function TParamsFile.getValue(key: string; default: single): single;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if Assigned(jsonvalue) then
    result := jsonvalue.Value.ToSingle
  else
    result := default;
end;

function TParamsFile.AsJSONObject(AClone: boolean): TJSONObject;
begin
  if not Assigned(FParamList) then
    result := nil
  else if AClone then
    result := FParamList.Clone as TJSONObject
  else
    result := FParamList;
end;

constructor TParamsFile.Create;
begin
  FFolderName := '';
  FFileName := '';
  FParamChanged := False;
  FParamList := TJSONObject.Create;

  FonAfterSaveEvent := nil;
  FonBeforeLoadEvent := nil;
  FonBeforeSaveEvent := nil;
  FonAfterLoadEvent := nil;
  FonAfterSaveProc := nil;
  FonBeforeLoadProc := nil;
  FonBeforeSaveProc := nil;
  FonAfterLoadProc := nil;

  FonCryptEvent := nil;
  FonCryptProc := nil;
  FonDecryptEvent := nil;
  FonDecryptProc := nil;

  FPortableMode := False;
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
  if Assigned(FParamList) then
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
  if Assigned(jsonvalue) then
    result := strToDateTime(jsonvalue.Value)
  else
    result := default;
end;

procedure TParamsFile.Load;
var
  FileName: string;
  fs: TFileStream;
  JSON: string;
begin
  // Call the Before Load event if it exists
  if Assigned(onBeforeLoadProc) then
    onBeforeLoadProc(self);
  if Assigned(onBeforeLoadEvent) then
    onBeforeLoadEvent(self);

  // Load the file and its settings
  if not FPortableMode then
  begin
    FileName := getParamsFileName;
    if tfile.Exists(FileName) then
    begin
      if Assigned(FParamList) then
        FreeAndNil(FParamList);
      if Assigned(onDecryptEvent) or Assigned(onDecryptProc) then
      begin
        fs := TFileStream.Create(FileName, fmOpenRead);
        try
          if Assigned(onDecryptEvent) then
            JSON := onDecryptEvent(fs)
          else if Assigned(onDecryptProc) then
            JSON := onDecryptEvent(fs)
          else
            JSON := '';
        finally
          fs.Free;
        end;
      end
      else
        JSON := tfile.ReadAllText(FileName, TEncoding.UTF8);
      FParamList := TJSONObject.ParseJSONValue(JSON) as TJSONObject;
    end;
  end;

  // Call the After Load event if it exists
  if Assigned(onAfterLoadProc) then
    onAfterLoadProc(self);
  if Assigned(onAfterLoadEvent) then
    onAfterLoadEvent(self);
end;

procedure TParamsFile.MoveToFilePath(ANewFilePath: string; ASave: boolean;
  ACreateFolder: boolean);
var
  oldFilePath: string;
  NewPath: string;
begin
  oldFilePath := getFilePath;
  if (oldFilePath <> ANewFilePath) then
  begin
    NewPath := TPath.GetDirectoryName(ANewFilePath);
    if not tdirectory.Exists(NewPath) then
      if ACreateFolder and (not FPortableMode) then
        tdirectory.CreateDirectory(NewPath)
      else
        raise Exception.Create('Folder "' + NewPath + '" doesn''t exist.');
    tfile.Move(oldFilePath, ANewFilePath);
    setFilePath(ANewFilePath, False);
    if ASave then
      Save;
  end;
end;

procedure TParamsFile.Remove(key: string);
begin
  if (FParamList.Count > 0) and (nil <> FParamList.getValue(key)) then
  begin
    FParamList.RemovePair(key).Free;
    FParamChanged := true;
  end;
end;

procedure TParamsFile.Save;
var
  FileName: string;
  cs: TStream;
  fs: TFileStream;
begin
  // Call the Before Save event if it exists
  if Assigned(onBeforeSaveProc) then
    onBeforeSaveProc(self);
  if Assigned(onBeforeSaveEvent) then
    onBeforeSaveEvent(self);

  // Save the settings if anything has changed in this file since previous Save or Load operation
  if FParamChanged and (not FPortableMode) then
  begin
    FileName := getParamsFileName(true);
    if Assigned(FParamList) and (FParamList.Count > 0) then
    begin
      cs := nil;
      if Assigned(onCryptEvent) then
        cs := onCryptEvent(FParamList.ToJSON)
      else if Assigned(onCryptProc) then
        cs := onCryptProc(FParamList.ToJSON)
      else
        tfile.WriteAllText(FileName, FParamList.ToJSON, TEncoding.UTF8);
      if Assigned(cs) then
        try
          fs := TFileStream.Create(FileName, fmOpenWrite + fmCreate);
          try
            cs.position := 0;
            fs.CopyFrom(cs);
          finally
            fs.Free;
          end;
        finally
          cs.Free;
        end;
    end
    else if tfile.Exists(FileName) then
      tfile.Delete(FileName);
    FParamChanged := False;
  end;

  // Call the After Save event if it exists
  if Assigned(onAfterSaveProc) then
    onAfterSaveProc(self);
  if Assigned(onAfterSaveEvent) then
    onAfterSaveEvent(self);
end;

procedure TParamsFile.setValue(key: string; Value: single);
var
  jsonvalue: TJSONNumber;
begin
  jsonvalue := TJSONNumber.Create(Value);
  try
    setParamValue(key, jsonvalue);
  except
    jsonvalue.Free;
  end;
end;

procedure TParamsFile.setValue(key: string; Value: TDateTime);
var
  jsonvalue: TJSONString;
begin
  jsonvalue := TJSONString.Create(DateTimeToStr(Value));
  try
    setParamValue(key, jsonvalue);
  except
    jsonvalue.Free;
  end;
end;

function TParamsFile.ToJSON: string;
begin
  if Assigned(FParamList) then
    result := FParamList.ToJSON
  else
    result := '';
end;

procedure TParamsFile.setValue(key, Value: string);
var
  jsonvalue: TJSONString;
begin
  jsonvalue := TJSONString.Create(Value);
  try
    setParamValue(key, jsonvalue);
  except
    jsonvalue.Free;
  end;
end;

procedure TParamsFile.setValue(key: string; Value: boolean);
var
  jsonvalue: TJSONBool;
begin
  jsonvalue := TJSONBool.Create(Value);
  try
    setParamValue(key, jsonvalue);
  except
    jsonvalue.Free;
  end;
end;

procedure TParamsFile.setValue(key: string; Value: integer);
var
  jsonvalue: TJSONNumber;
begin
  jsonvalue := TJSONNumber.Create(Value);
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
    if not tdirectory.Exists(FFolderName) then
      raise Exception.Create('Folder "' + FFolderName + '" doesn''t exist.');
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

procedure TParamsFile.SetonAfterLoadEvent(const Value: TParamsLoadSaveEvent);
begin
  FonAfterLoadEvent := Value;
end;

procedure TParamsFile.SetonAfterLoadProc(const Value: TParamsLoadSaveProc);
begin
  FonAfterLoadProc := Value;
end;

procedure TParamsFile.SetonAfterSaveEvent(const Value: TParamsLoadSaveEvent);
begin
  FonAfterSaveEvent := Value;
end;

procedure TParamsFile.SetonAfterSaveProc(const Value: TParamsLoadSaveProc);
begin
  FonAfterSaveProc := Value;
end;

procedure TParamsFile.SetonBeforeLoadEvent(const Value: TParamsLoadSaveEvent);
begin
  FonBeforeLoadEvent := Value;
end;

procedure TParamsFile.SetonBeforeLoadProc(const Value: TParamsLoadSaveProc);
begin
  FonBeforeLoadProc := Value;
end;

procedure TParamsFile.SetonBeforeSaveEvent(const Value: TParamsLoadSaveEvent);
begin
  FonBeforeSaveEvent := Value;
end;

procedure TParamsFile.SetonBeforeSaveProc(const Value: TParamsLoadSaveProc);
begin
  FonBeforeSaveProc := Value;
end;

procedure TParamsFile.SetonCryptEvent(const Value: TParamsCryptEvent);
begin
  FonCryptEvent := Value;
end;

procedure TParamsFile.SetonCryptProc(const Value: TParamsCryptProc);
begin
  FonCryptProc := Value;
end;

procedure TParamsFile.SetonDecryptEvent(const Value: TParamsDecryptEvent);
begin
  FonDecryptEvent := Value;
end;

procedure TParamsFile.SetonDecryptProc(const Value: TParamsDecryptProc);
begin
  FonDecryptProc := Value;
end;

function TParamsFile.getValue(key: string; default: TJSONValue): TJSONValue;
begin
  result := getParamValue(key);
  if not Assigned(result) then
    result := default;
end;

procedure TParamsFile.InitDefaultFileNameV2(const AEditor, ASoftware: string;
  AReload: boolean);
var
  Folder: string;
begin
  if AEditor.IsEmpty and ASoftware.IsEmpty then
    raise Exception.Create('Needs at least an Editor or Software name.');

{$IF Defined(DEBUG) or Defined(IOS)}
  Folder := TPath.GetDocumentsPath;
{$ELSE IF Defined(RELEASE)}
  Folder := TPath.GetHomePath;
{$ELSE}
{$MESSAGE FATAL 'not implemented'}
{$ENDIF}
  //
  if not AEditor.IsEmpty then
{$IFDEF DEBUG}
    Folder := TPath.Combine(Folder, AEditor + '-DEBUG');
{$ELSE}
    Folder := TPath.Combine(Folder, AEditor);
{$ENDIF}
  //
  if not ASoftware.IsEmpty then
{$IFDEF DEBUG}
    Folder := TPath.Combine(Folder, ASoftware + '-DEBUG');
{$ELSE}
    Folder := TPath.Combine(Folder, ASoftware);
{$ENDIF}
  //
  setFolderName(Folder, AReload);
end;

function TParamsFile.getValue(key: string; default: cardinal): cardinal;
var
  jsonvalue: TJSONValue;
begin
  jsonvalue := getParamValue(key);
  if Assigned(jsonvalue) then
    result := jsonvalue.Value.ToInt64
  else
    result := default;
end;

procedure TParamsFile.setValue(key: string; Value: TJSONValue);
begin
  setParamValue(key, Value);
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

class function TParams.GetonAfterLoadEvent: TParamsLoadSaveEvent;
begin
  result := DefaultParamsFile.onAfterLoadEvent;
end;

class function TParams.GetonAfterLoadProc: TParamsLoadSaveProc;
begin
  result := DefaultParamsFile.onAfterLoadProc;
end;

class function TParams.GetonAfterSaveEvent: TParamsLoadSaveEvent;
begin
  result := DefaultParamsFile.onAfterSaveEvent;
end;

class function TParams.GetonAfterSaveProc: TParamsLoadSaveProc;
begin
  result := DefaultParamsFile.onAfterSaveProc;
end;

class function TParams.GetonBeforeLoadEvent: TParamsLoadSaveEvent;
begin
  result := DefaultParamsFile.onBeforeLoadEvent;
end;

class function TParams.GetonBeforeLoadProc: TParamsLoadSaveProc;
begin
  result := DefaultParamsFile.onBeforeLoadProc;
end;

class function TParams.GetonBeforeSaveEvent: TParamsLoadSaveEvent;
begin
  result := DefaultParamsFile.onBeforeSaveEvent;
end;

class function TParams.GetonBeforeSaveProc: TParamsLoadSaveProc;
begin
  result := DefaultParamsFile.onBeforeSaveProc;
end;

class function TParams.GetonCryptEvent: TParamsCryptEvent;
begin
  result := DefaultParamsFile.onCryptEvent;
end;

class function TParams.GetonCryptProc: TParamsCryptProc;
begin
  result := DefaultParamsFile.onCryptProc
end;

class function TParams.GetonDecryptEvent: TParamsDecryptEvent;
begin
  result := DefaultParamsFile.onDecryptEvent;
end;

class function TParams.GetonDecryptProc: TParamsDecryptProc;
begin
  result := DefaultParamsFile.FonDecryptProc
end;

class function TParams.GetPortableMode: boolean;
begin
  result := DefaultParamsFile.PortableMode;
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

class procedure TParams.MoveToFilePath(ANewFilePath: string; ASave: boolean;
  ACreateFolder: boolean);
begin
  DefaultParamsFile.MoveToFilePath(ANewFilePath, ASave, ACreateFolder);
end;

class procedure TParams.Remove(key: string);
begin
  DefaultParamsFile.Remove(key);
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

class procedure TParams.SetonAfterLoadEvent(const Value: TParamsLoadSaveEvent);
begin
  DefaultParamsFile.onAfterLoadEvent := Value;
end;

class procedure TParams.SetonAfterLoadProc(const Value: TParamsLoadSaveProc);
begin
  DefaultParamsFile.onAfterLoadProc := Value;
end;

class procedure TParams.SetonAfterSaveEvent(const Value: TParamsLoadSaveEvent);
begin
  DefaultParamsFile.onAfterSaveEvent := Value;
end;

class procedure TParams.SetonAfterSaveProc(const Value: TParamsLoadSaveProc);
begin
  DefaultParamsFile.onAfterSaveProc := Value;
end;

class procedure TParams.SetonBeforeLoadEvent(const Value: TParamsLoadSaveEvent);
begin
  DefaultParamsFile.onBeforeLoadEvent := Value;
end;

class procedure TParams.SetonBeforeLoadProc(const Value: TParamsLoadSaveProc);
begin
  DefaultParamsFile.onBeforeLoadProc := Value;
end;

class procedure TParams.SetonBeforeSaveEvent(const Value: TParamsLoadSaveEvent);
begin
  DefaultParamsFile.onBeforeSaveEvent := Value;
end;

class procedure TParams.SetonBeforeSaveProc(const Value: TParamsLoadSaveProc);
begin
  DefaultParamsFile.onBeforeSaveProc := Value;
end;

class procedure TParams.SetonCryptEvent(const Value: TParamsCryptEvent);
begin
  DefaultParamsFile.onCryptEvent := Value;
end;

class procedure TParams.SetonCryptProc(const Value: TParamsCryptProc);
begin
  DefaultParamsFile.onCryptProc := Value;
end;

class procedure TParams.SetonDecryptEvent(const Value: TParamsDecryptEvent);
begin
  DefaultParamsFile.onDecryptEvent := Value;
end;

class procedure TParams.SetonDecryptProc(const Value: TParamsDecryptProc);
begin
  DefaultParamsFile.onDecryptProc := Value;
end;

class procedure TParams.SetPortableMode(const Value: boolean);
begin
  DefaultParamsFile.PortableMode := Value;
end;

class procedure TParams.setValue(key: string; Value: boolean);
begin
  DefaultParamsFile.setValue(key, Value);
end;

class procedure TParams.setValue(key, Value: string);
begin
  DefaultParamsFile.setValue(key, Value);
end;

class procedure TParams.setValue(key: string; Value: TDateTime);
begin
  DefaultParamsFile.setValue(key, Value);
end;

class procedure TParams.setValue(key: string; Value: single);
begin
  DefaultParamsFile.setValue(key, Value);
end;

class procedure TParams.setValue(key: string; Value: integer);
begin
  DefaultParamsFile.setValue(key, Value);
end;

class function TParams.ToJSON: string;
begin
  result := DefaultParamsFile.ToJSON;
end;

class function TParams.getValue(key: string; default: TJSONValue): TJSONValue;
begin
  result := DefaultParamsFile.getValue(key, default);
end;

class procedure TParams.InitDefaultFileNameV2(const AEditor, ASoftware: string;
  AReload: boolean);
begin
  DefaultParamsFile.InitDefaultFileNameV2(AEditor, ASoftware, AReload);
end;

class function TParams.getValue(key: string; default: cardinal): integer;
begin
  result := DefaultParamsFile.getValue(key, default);
end;

class procedure TParams.setValue(key: string; Value: TJSONValue);
begin
  DefaultParamsFile.setValue(key, Value);
end;

class procedure TParams.setValue(key: string; Value: cardinal);
begin
  DefaultParamsFile.setValue(key, Value);
end;

initialization

DefaultParamsFile := TParamsFile.Create;
TParams.Load;

finalization

TParams.Save;
if Assigned(DefaultParamsFile) then
  FreeAndNil(DefaultParamsFile);

end.
