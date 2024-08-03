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
/// File last update : 28/05/2024 12:19:15
/// Signature : 9caad9e44a57a56661335d6de8663b9444ad456d
/// ***************************************************************************
/// </summary>

unit AgentObjects_TLB;

// ************************************************************************ //
// AVERTISSEMENT                                                                 
// -------                                                                    
// Les types déclarés dans ce fichier ont été générés à partir de données lues 
// depuis la bibliothèque de types. Si cette dernière (via une autre bibliothèque de types 
// s'y référant) est explicitement ou indirectement ré-importée, ou la commande "Rafraîchir"  
// de l'éditeur de bibliothèque de types est activée lors de la modification de la bibliothèque 
// de types, le contenu de ce fichier sera régénéré et toutes les modifications      
// manuellement apportées seront perdues.                                     
// ************************************************************************ //

// PASTLWTR : $Revision:   1.88.1.0.1.0  $
// Fichier généré le 24/07/2001 16:45:24 depuis la bibliothèque de types ci-dessous.

// ************************************************************************ //
// Bibl.Types     : C:\WINDOWS\msagent\agentctl.dll (1)
// IID\LCID       : {F5BE8BC2-7DE6-11D0-91FE-00C04FD701A5}\0
// Fichier d'aide : 
// DepndLst       : 
//   (1) v2.0 stdole, (C:\WINDOWS\System32\stdole2.tlb)
//   (2) v4.0 StdVCL, (C:\WINDOWS\system32\stdvcl40.dll)
// Erreurs :
//   Remarque : paramètre 'On' dans IAgentCtlCharacterEx.AutoPopupMenu changé en 'On_'
//   Remarque : paramètre 'On' dans IAgentCtlCharacterEx.AutoPopupMenu changé en 'On_'
//   Remarque : paramètre 'On' dans IAgentCtlCharacterEx.HelpModeOn changé en 'On_'
//   Remarque : paramètre 'On' dans IAgentCtlCharacterEx.HelpModeOn changé en 'On_'
//   Remarque : paramètre 'File' dans IAgentCtlCharacterEx.HelpFile changé en 'File_'
//   Remarque : paramètre 'File' dans IAgentCtlCharacterEx.HelpFile changé en 'File_'
//   Remarque : paramètre 'On' dans IAgentCtlCharacter.IdleOn changé en 'On_'
//   Remarque : paramètre 'On' dans IAgentCtlCharacter.IdleOn changé en 'On_'
//   Remarque : paramètre 'Type' dans IAgentCtlCharacter.Get changé en 'Type_'
//   Remarque : paramètre 'On' dans IAgentCtlCharacter.SoundEffectsOn changé en 'On_'
//   Remarque : paramètre 'On' dans IAgentCtlCharacter.SoundEffectsOn changé en 'On_'
// ************************************************************************ //
{$TYPEDADDRESS OFF} // L'unité doit être compilée sans vérification de type des pointeurs. 
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, OleCtrls, StdVCL;

// *********************************************************************//
// GUIDS déclarés dans la bibliothèque de types. Préfixes utilisés :    
//   Bibliothèques de types : LIBID_xxxx                                      
//   CoClasses              : CLASS_xxxx                                      
//   DISPInterfaces         : DIID_xxxx                                       
//   Non-DISP interfaces    : IID_xxxx                                        
// *********************************************************************//
const
  // Version majeure et mineure de la bibliothèque de types
  AgentObjectsMajorVersion = 2;
  AgentObjectsMinorVersion = 0;

  LIBID_AgentObjects: TGUID = '{F5BE8BC2-7DE6-11D0-91FE-00C04FD701A5}';

  IID_IAgentCtlCharacters: TGUID = '{F5BE8BE8-7DE6-11D0-91FE-00C04FD701A5}';
  IID_IAgentCtlCharacter: TGUID = '{F5BE8BD9-7DE6-11D0-91FE-00C04FD701A5}';
  IID_IAgentCtlCharacterEx: TGUID = '{DE8EF600-2F82-11D1-ACAC-00C04FD97575}';
  IID_IAgentCtlBalloon: TGUID = '{F5BE8BD3-7DE6-11D0-91FE-00C04FD701A5}';
  IID_IAgentCtlBalloonEx: TGUID = '{822DB1C0-8879-11D1-9EC6-00C04FD7081F}';
  IID_IAgentCtlCommands: TGUID = '{F5BE8BE1-7DE6-11D0-91FE-00C04FD701A5}';
  IID_IAgentCtlCommandsEx: TGUID = '{6BA90C01-3910-11D1-ACB3-00C04FD97575}';
  IID_IAgentCtlCommand: TGUID = '{F5BE8BE3-7DE6-11D0-91FE-00C04FD701A5}';
  IID_IAgentCtlCommandEx: TGUID = '{B0913410-3B44-11D1-ACBA-00C04FD97575}';
  IID_IAgentCtlRequest: TGUID = '{1DAB85C3-803A-11D0-AC63-00C04FD97575}';
  IID_IAgentCtlAnimationNames: TGUID = '{8B77181C-D3EF-11D1-8500-00C04FA34A14}';
  IID_IAgentCtlAudioObject: TGUID = '{F5BE8BDB-7DE6-11D0-91FE-00C04FD701A5}';
  IID_IAgentCtlAudioObjectEx: TGUID = '{F5BE8BF0-7DE6-11D0-91FE-00C04FD701A5}';
  IID_IAgentCtlSpeechInput: TGUID = '{F5BE8BDD-7DE6-11D0-91FE-00C04FD701A5}';
  IID_IAgentCtlPropertySheet: TGUID = '{F5BE8BDF-7DE6-11D0-91FE-00C04FD701A5}';
  IID_IAgentCtlUserInput: TGUID = '{C4ABF875-8100-11D0-AC63-00C04FD97575}';
  IID_IAgentCtlCommandsWindow: TGUID = '{6D0ECB27-9968-11D0-AC6E-00C04FD97575}';
  IID_IAgentCtl: TGUID = '{F5BE8BD1-7DE6-11D0-91FE-00C04FD701A5}';
  IID_IAgentCtlEx: TGUID = '{8563FF20-8ECC-11D1-B9B4-00C04FD97575}';
  DIID__AgentEvents: TGUID = '{F5BE8BD4-7DE6-11D0-91FE-00C04FD701A5}';
  CLASS_Agent: TGUID = '{D45FD31B-5C6E-11D1-9EC1-00C04FD7081F}';
type

// *********************************************************************//
// Déclaration Forward des types définis dans la bibliothèque de types    
// *********************************************************************//
  IAgentCtlCharacters = interface;
  IAgentCtlCharactersDisp = dispinterface;
  IAgentCtlCharacter = interface;
  IAgentCtlCharacterDisp = dispinterface;
  IAgentCtlCharacterEx = interface;
  IAgentCtlCharacterExDisp = dispinterface;
  IAgentCtlBalloon = interface;
  IAgentCtlBalloonDisp = dispinterface;
  IAgentCtlBalloonEx = interface;
  IAgentCtlBalloonExDisp = dispinterface;
  IAgentCtlCommands = interface;
  IAgentCtlCommandsDisp = dispinterface;
  IAgentCtlCommandsEx = interface;
  IAgentCtlCommandsExDisp = dispinterface;
  IAgentCtlCommand = interface;
  IAgentCtlCommandDisp = dispinterface;
  IAgentCtlCommandEx = interface;
  IAgentCtlCommandExDisp = dispinterface;
  IAgentCtlRequest = interface;
  IAgentCtlRequestDisp = dispinterface;
  IAgentCtlAnimationNames = interface;
  IAgentCtlAnimationNamesDisp = dispinterface;
  IAgentCtlAudioObject = interface;
  IAgentCtlAudioObjectDisp = dispinterface;
  IAgentCtlAudioObjectEx = interface;
  IAgentCtlAudioObjectExDisp = dispinterface;
  IAgentCtlSpeechInput = interface;
  IAgentCtlSpeechInputDisp = dispinterface;
  IAgentCtlPropertySheet = interface;
  IAgentCtlPropertySheetDisp = dispinterface;
  IAgentCtlUserInput = interface;
  IAgentCtlUserInputDisp = dispinterface;
  IAgentCtlCommandsWindow = interface;
  IAgentCtlCommandsWindowDisp = dispinterface;
  IAgentCtl = interface;
  IAgentCtlDisp = dispinterface;
  IAgentCtlEx = interface;
  IAgentCtlExDisp = dispinterface;
  _AgentEvents = dispinterface;

// *********************************************************************//
// Déclaration de CoClasses définies dans la bibliothèque de types 
// (REMARQUE: On affecte chaque CoClass à son Interface par défaut)              
// *********************************************************************//
  Agent = IAgentCtlEx;


// *********************************************************************//
// Interface   : IAgentCtlCharacters
// Indicateurs : (4416) Dual OleAutomation Dispatchable
// GUID        : {F5BE8BE8-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentCtlCharacters = interface(IDispatch)
    ['{F5BE8BE8-7DE6-11D0-91FE-00C04FD701A5}']
    function  Get_Item(const CharacterID: WideString): IAgentCtlCharacterEx; safecall;
    function  Character(const CharacterID: WideString): IAgentCtlCharacterEx; safecall;
    function  Get_Enum: IUnknown; safecall;
    procedure Unload(const CharacterID: WideString); safecall;
    function  Load(const CharacterID: WideString; LoadKey: OleVariant): IAgentCtlRequest; safecall;
    property Item[const CharacterID: WideString]: IAgentCtlCharacterEx read Get_Item; default;
    property Enum: IUnknown read Get_Enum;
  end;

// *********************************************************************//
// DispIntf:  IAgentCtlCharactersDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F5BE8BE8-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentCtlCharactersDisp = dispinterface
    ['{F5BE8BE8-7DE6-11D0-91FE-00C04FD701A5}']
    property Item[const CharacterID: WideString]: IAgentCtlCharacterEx readonly dispid 0; default;
    function  Character(const CharacterID: WideString): IAgentCtlCharacterEx; dispid 3;
    property Enum: IUnknown readonly dispid -4;
    procedure Unload(const CharacterID: WideString); dispid 2;
    function  Load(const CharacterID: WideString; LoadKey: OleVariant): IAgentCtlRequest; dispid 1;
  end;

// *********************************************************************//
// Interface   : IAgentCtlCharacter
// Indicateurs : (4416) Dual OleAutomation Dispatchable
// GUID        : {F5BE8BD9-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentCtlCharacter = interface(IDispatch)
    ['{F5BE8BD9-7DE6-11D0-91FE-00C04FD701A5}']
    function  Get_Balloon: IAgentCtlBalloonEx; safecall;
    function  Get_Commands: IAgentCtlCommandsEx; safecall;
    function  Get_Name: WideString; safecall;
    function  Get_Description: WideString; safecall;
    function  Get_Visible: WordBool; safecall;
    procedure Set_Left(Left: Smallint); safecall;
    function  Get_Left: Smallint; safecall;
    procedure Set_Top(Top: Smallint); safecall;
    function  Get_Top: Smallint; safecall;
    procedure Set_Height(Height: Smallint); safecall;
    function  Get_Height: Smallint; safecall;
    procedure Set_Width(Width: Smallint); safecall;
    function  Get_Width: Smallint; safecall;
    function  Get_Speed: Integer; safecall;
    function  Get_Pitch: Integer; safecall;
    procedure Set_IdleOn(On_: WordBool); safecall;
    function  Get_IdleOn: WordBool; safecall;
    function  Activate(State: OleVariant): WordBool; safecall;
    function  Play(const Animation: WideString): IAgentCtlRequest; safecall;
    function  Get(const Type_: WideString; const Name: WideString; Queue: OleVariant): IAgentCtlRequest; safecall;
    procedure Stop(Request: OleVariant); safecall;
    function  Wait(const WaitForRequest: IDispatch): IAgentCtlRequest; safecall;
    function  Interrupt(const InterruptRequest: IDispatch): IAgentCtlRequest; safecall;
    function  Speak(Text: OleVariant; Url: OleVariant): IAgentCtlRequest; safecall;
    function  GestureAt(x: Smallint; y: Smallint): IAgentCtlRequest; safecall;
    function  MoveTo(x: Smallint; y: Smallint; Speed: OleVariant): IAgentCtlRequest; safecall;
    function  Hide(Fast: OleVariant): IAgentCtlRequest; safecall;
    function  Show(Fast: OleVariant): IAgentCtlRequest; safecall;
    procedure StopAll(Types: OleVariant); safecall;
    function  Get_MoveCause: Smallint; safecall;
    function  Get_VisibilityCause: Smallint; safecall;
    function  Get_HasOtherClients: WordBool; safecall;
    procedure Set_SoundEffectsOn(On_: WordBool); safecall;
    function  Get_SoundEffectsOn: WordBool; safecall;
    procedure Set_Name(const Name: WideString); safecall;
    procedure Set_Description(const Description: WideString); safecall;
    function  Get_ExtraData: WideString; safecall;
    property Balloon: IAgentCtlBalloonEx read Get_Balloon;
    property Commands: IAgentCtlCommandsEx read Get_Commands;
    property Name: WideString read Get_Name write Set_Name;
    property Description: WideString read Get_Description write Set_Description;
    property Visible: WordBool read Get_Visible;
    property Left: Smallint read Get_Left write Set_Left;
    property Top: Smallint read Get_Top write Set_Top;
    property Height: Smallint read Get_Height write Set_Height;
    property Width: Smallint read Get_Width write Set_Width;
    property Speed: Integer read Get_Speed;
    property Pitch: Integer read Get_Pitch;
    property IdleOn: WordBool read Get_IdleOn write Set_IdleOn;
    property MoveCause: Smallint read Get_MoveCause;
    property VisibilityCause: Smallint read Get_VisibilityCause;
    property HasOtherClients: WordBool read Get_HasOtherClients;
    property SoundEffectsOn: WordBool read Get_SoundEffectsOn write Set_SoundEffectsOn;
    property ExtraData: WideString read Get_ExtraData;
  end;

// *********************************************************************//
// DispIntf:  IAgentCtlCharacterDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F5BE8BD9-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentCtlCharacterDisp = dispinterface
    ['{F5BE8BD9-7DE6-11D0-91FE-00C04FD701A5}']
    property Balloon: IAgentCtlBalloonEx readonly dispid 23;
    property Commands: IAgentCtlCommandsEx readonly dispid 25;
    property Name: WideString dispid 24;
    property Description: WideString dispid 28;
    property Visible: WordBool readonly dispid 2;
    property Left: Smallint dispid 3;
    property Top: Smallint dispid 4;
    property Height: Smallint dispid 5;
    property Width: Smallint dispid 6;
    property Speed: Integer readonly dispid 10;
    property Pitch: Integer readonly dispid 11;
    property IdleOn: WordBool dispid 29;
    function  Activate(State: OleVariant): WordBool; dispid 26;
    function  Play(const Animation: WideString): IAgentCtlRequest; dispid 13;
    function  Get(const Type_: WideString; const Name: WideString; Queue: OleVariant): IAgentCtlRequest; dispid 27;
    procedure Stop(Request: OleVariant); dispid 14;
    function  Wait(const WaitForRequest: IDispatch): IAgentCtlRequest; dispid 22;
    function  Interrupt(const InterruptRequest: IDispatch): IAgentCtlRequest; dispid 21;
    function  Speak(Text: OleVariant; Url: OleVariant): IAgentCtlRequest; dispid 15;
    function  GestureAt(x: Smallint; y: Smallint): IAgentCtlRequest; dispid 17;
    function  MoveTo(x: Smallint; y: Smallint; Speed: OleVariant): IAgentCtlRequest; dispid 18;
    function  Hide(Fast: OleVariant): IAgentCtlRequest; dispid 19;
    function  Show(Fast: OleVariant): IAgentCtlRequest; dispid 20;
    procedure StopAll(Types: OleVariant); dispid 31;
    property MoveCause: Smallint readonly dispid 32;
    property VisibilityCause: Smallint readonly dispid 33;
    property HasOtherClients: WordBool readonly dispid 34;
    property SoundEffectsOn: WordBool dispid 35;
    property ExtraData: WideString readonly dispid 36;
  end;

// *********************************************************************//
// Interface   : IAgentCtlCharacterEx
// Indicateurs : (4416) Dual OleAutomation Dispatchable
// GUID        : {DE8EF600-2F82-11D1-ACAC-00C04FD97575}
// *********************************************************************//
  IAgentCtlCharacterEx = interface(IAgentCtlCharacter)
    ['{DE8EF600-2F82-11D1-ACAC-00C04FD97575}']
    function  ShowPopupMenu(x: Smallint; y: Smallint): WordBool; safecall;
    procedure Set_AutoPopupMenu(On_: WordBool); safecall;
    function  Get_AutoPopupMenu: WordBool; safecall;
    procedure Set_HelpModeOn(On_: WordBool); safecall;
    function  Get_HelpModeOn: WordBool; safecall;
    procedure Set_HelpContextID(ID: Integer); safecall;
    function  Get_HelpContextID: Integer; safecall;
    function  Get_Active: Smallint; safecall;
    function  Listen(Listen: WordBool): WordBool; safecall;
    procedure Set_LanguageID(LanguageID: Integer); safecall;
    function  Get_LanguageID: Integer; safecall;
    function  Get_SRModeID: WideString; safecall;
    procedure Set_SRModeID(const EngineModeId: WideString); safecall;
    function  Get_TTSModeID: WideString; safecall;
    procedure Set_TTSModeID(const EngineModeId: WideString); safecall;
    function  Get_HelpFile: WideString; safecall;
    procedure Set_HelpFile(const File_: WideString); safecall;
    function  Get_GUID: WideString; safecall;
    function  Get_OriginalHeight: Smallint; safecall;
    function  Get_OriginalWidth: Smallint; safecall;
    function  Think(const Text: WideString): IAgentCtlRequest; safecall;
    function  Get_Version: WideString; safecall;
    function  Get_AnimationNames: IAgentCtlAnimationNames; safecall;
    function  Get_SRStatus: Integer; safecall;
    property AutoPopupMenu: WordBool read Get_AutoPopupMenu write Set_AutoPopupMenu;
    property HelpModeOn: WordBool read Get_HelpModeOn write Set_HelpModeOn;
    property HelpContextID: Integer read Get_HelpContextID write Set_HelpContextID;
    property Active: Smallint read Get_Active;
    property LanguageID: Integer read Get_LanguageID write Set_LanguageID;
    property SRModeID: WideString read Get_SRModeID write Set_SRModeID;
    property TTSModeID: WideString read Get_TTSModeID write Set_TTSModeID;
    property HelpFile: WideString read Get_HelpFile write Set_HelpFile;
    property GUID: WideString read Get_GUID;
    property OriginalHeight: Smallint read Get_OriginalHeight;
    property OriginalWidth: Smallint read Get_OriginalWidth;
    property Version: WideString read Get_Version;
    property AnimationNames: IAgentCtlAnimationNames read Get_AnimationNames;
    property SRStatus: Integer read Get_SRStatus;
  end;

// *********************************************************************//
// DispIntf:  IAgentCtlCharacterExDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DE8EF600-2F82-11D1-ACAC-00C04FD97575}
// *********************************************************************//
  IAgentCtlCharacterExDisp = dispinterface
    ['{DE8EF600-2F82-11D1-ACAC-00C04FD97575}']
    function  ShowPopupMenu(x: Smallint; y: Smallint): WordBool; dispid 37;
    property AutoPopupMenu: WordBool dispid 38;
    property HelpModeOn: WordBool dispid 39;
    property HelpContextID: Integer dispid 40;
    property Active: Smallint readonly dispid 41;
    function  Listen(Listen: WordBool): WordBool; dispid 42;
    property LanguageID: Integer dispid 43;
    property SRModeID: WideString dispid 46;
    property TTSModeID: WideString dispid 47;
    property HelpFile: WideString dispid 48;
    property GUID: WideString readonly dispid 49;
    property OriginalHeight: Smallint readonly dispid 50;
    property OriginalWidth: Smallint readonly dispid 51;
    function  Think(const Text: WideString): IAgentCtlRequest; dispid 52;
    property Version: WideString readonly dispid 53;
    property AnimationNames: IAgentCtlAnimationNames readonly dispid 54;
    property SRStatus: Integer readonly dispid 55;
    property Balloon: IAgentCtlBalloonEx readonly dispid 23;
    property Commands: IAgentCtlCommandsEx readonly dispid 25;
    property Name: WideString dispid 24;
    property Description: WideString dispid 28;
    property Visible: WordBool readonly dispid 2;
    property Left: Smallint dispid 3;
    property Top: Smallint dispid 4;
    property Height: Smallint dispid 5;
    property Width: Smallint dispid 6;
    property Speed: Integer readonly dispid 10;
    property Pitch: Integer readonly dispid 11;
    property IdleOn: WordBool dispid 29;
    function  Activate(State: OleVariant): WordBool; dispid 26;
    function  Play(const Animation: WideString): IAgentCtlRequest; dispid 13;
    function  Get(const Type_: WideString; const Name: WideString; Queue: OleVariant): IAgentCtlRequest; dispid 27;
    procedure Stop(Request: OleVariant); dispid 14;
    function  Wait(const WaitForRequest: IDispatch): IAgentCtlRequest; dispid 22;
    function  Interrupt(const InterruptRequest: IDispatch): IAgentCtlRequest; dispid 21;
    function  Speak(Text: OleVariant; Url: OleVariant): IAgentCtlRequest; dispid 15;
    function  GestureAt(x: Smallint; y: Smallint): IAgentCtlRequest; dispid 17;
    function  MoveTo(x: Smallint; y: Smallint; Speed: OleVariant): IAgentCtlRequest; dispid 18;
    function  Hide(Fast: OleVariant): IAgentCtlRequest; dispid 19;
    function  Show(Fast: OleVariant): IAgentCtlRequest; dispid 20;
    procedure StopAll(Types: OleVariant); dispid 31;
    property MoveCause: Smallint readonly dispid 32;
    property VisibilityCause: Smallint readonly dispid 33;
    property HasOtherClients: WordBool readonly dispid 34;
    property SoundEffectsOn: WordBool dispid 35;
    property ExtraData: WideString readonly dispid 36;
  end;

// *********************************************************************//
// Interface   : IAgentCtlBalloon
// Indicateurs : (4416) Dual OleAutomation Dispatchable
// GUID        : {F5BE8BD3-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentCtlBalloon = interface(IDispatch)
    ['{F5BE8BD3-7DE6-11D0-91FE-00C04FD701A5}']
    function  Get_Enabled: WordBool; safecall;
    function  Get_NumberOfLines: Integer; safecall;
    function  Get_CharsPerLine: Integer; safecall;
    function  Get_FontName: WideString; safecall;
    function  Get_FontSize: Integer; safecall;
    function  Get_FontBold: WordBool; safecall;
    function  Get_FontItalic: WordBool; safecall;
    function  Get_FontStrikethru: WordBool; safecall;
    function  Get_FontUnderline: WordBool; safecall;
    function  Get_ForeColor: Integer; safecall;
    function  Get_BackColor: Integer; safecall;
    function  Get_BorderColor: Integer; safecall;
    procedure Set_Visible(Visible: WordBool); safecall;
    function  Get_Visible: WordBool; safecall;
    procedure Set_FontName(const FontName: WideString); safecall;
    procedure Set_FontSize(FontSize: Integer); safecall;
    procedure Set_FontCharSet(FontCharSet: Smallint); safecall;
    function  Get_FontCharSet: Smallint; safecall;
    property Enabled: WordBool read Get_Enabled;
    property NumberOfLines: Integer read Get_NumberOfLines;
    property CharsPerLine: Integer read Get_CharsPerLine;
    property FontName: WideString read Get_FontName write Set_FontName;
    property FontSize: Integer read Get_FontSize write Set_FontSize;
    property FontBold: WordBool read Get_FontBold;
    property FontItalic: WordBool read Get_FontItalic;
    property FontStrikethru: WordBool read Get_FontStrikethru;
    property FontUnderline: WordBool read Get_FontUnderline;
    property ForeColor: Integer read Get_ForeColor;
    property BackColor: Integer read Get_BackColor;
    property BorderColor: Integer read Get_BorderColor;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property FontCharSet: Smallint read Get_FontCharSet write Set_FontCharSet;
  end;

// *********************************************************************//
// DispIntf:  IAgentCtlBalloonDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F5BE8BD3-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentCtlBalloonDisp = dispinterface
    ['{F5BE8BD3-7DE6-11D0-91FE-00C04FD701A5}']
    property Enabled: WordBool readonly dispid 1;
    property NumberOfLines: Integer readonly dispid 2;
    property CharsPerLine: Integer readonly dispid 3;
    property FontName: WideString dispid 4;
    property FontSize: Integer dispid 5;
    property FontBold: WordBool readonly dispid 10;
    property FontItalic: WordBool readonly dispid 11;
    property FontStrikethru: WordBool readonly dispid 12;
    property FontUnderline: WordBool readonly dispid 13;
    property ForeColor: Integer readonly dispid 7;
    property BackColor: Integer readonly dispid 8;
    property BorderColor: Integer readonly dispid 9;
    property Visible: WordBool dispid 14;
    property FontCharSet: Smallint dispid 15;
  end;

// *********************************************************************//
// Interface   : IAgentCtlBalloonEx
// Indicateurs : (4416) Dual OleAutomation Dispatchable
// GUID        : {822DB1C0-8879-11D1-9EC6-00C04FD7081F}
// *********************************************************************//
  IAgentCtlBalloonEx = interface(IAgentCtlBalloon)
    ['{822DB1C0-8879-11D1-9EC6-00C04FD7081F}']
    procedure Set_Style(Style: Integer); safecall;
    function  Get_Style: Integer; safecall;
    property Style: Integer read Get_Style write Set_Style;
  end;

// *********************************************************************//
// DispIntf:  IAgentCtlBalloonExDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {822DB1C0-8879-11D1-9EC6-00C04FD7081F}
// *********************************************************************//
  IAgentCtlBalloonExDisp = dispinterface
    ['{822DB1C0-8879-11D1-9EC6-00C04FD7081F}']
    property Style: Integer dispid 16;
    property Enabled: WordBool readonly dispid 1;
    property NumberOfLines: Integer readonly dispid 2;
    property CharsPerLine: Integer readonly dispid 3;
    property FontName: WideString dispid 4;
    property FontSize: Integer dispid 5;
    property FontBold: WordBool readonly dispid 10;
    property FontItalic: WordBool readonly dispid 11;
    property FontStrikethru: WordBool readonly dispid 12;
    property FontUnderline: WordBool readonly dispid 13;
    property ForeColor: Integer readonly dispid 7;
    property BackColor: Integer readonly dispid 8;
    property BorderColor: Integer readonly dispid 9;
    property Visible: WordBool dispid 14;
    property FontCharSet: Smallint dispid 15;
  end;

// *********************************************************************//
// Interface   : IAgentCtlCommands
// Indicateurs : (4416) Dual OleAutomation Dispatchable
// GUID        : {F5BE8BE1-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentCtlCommands = interface(IDispatch)
    ['{F5BE8BE1-7DE6-11D0-91FE-00C04FD701A5}']
    function  Get_Item(const Name: WideString): IAgentCtlCommandEx; safecall;
    function  Command(const Name: WideString): IAgentCtlCommandEx; safecall;
    function  Get_Count: Integer; safecall;
    function  Get_Caption: WideString; safecall;
    procedure Set_Caption(const Caption: WideString); safecall;
    function  Get_Voice: WideString; safecall;
    procedure Set_Voice(const Voice: WideString); safecall;
    function  Get_Visible: WordBool; safecall;
    procedure Set_Visible(Visible: WordBool); safecall;
    function  Get_Enum: IUnknown; safecall;
    function  Add(const Name: WideString; Caption: OleVariant; Voice: OleVariant; 
                  Enabled: OleVariant; Visible: OleVariant): IAgentCtlCommand; safecall;
    function  Insert(const Name: WideString; const RefName: WideString; Before: OleVariant; 
                     Caption: OleVariant; Voice: OleVariant; Enabled: OleVariant; 
                     Visible: OleVariant): IAgentCtlCommand; safecall;
    procedure Remove(const Name: WideString); safecall;
    procedure RemoveAll; safecall;
    property Item[const Name: WideString]: IAgentCtlCommandEx read Get_Item; default;
    property Count: Integer read Get_Count;
    property Caption: WideString read Get_Caption write Set_Caption;
    property Voice: WideString read Get_Voice write Set_Voice;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property Enum: IUnknown read Get_Enum;
  end;

// *********************************************************************//
// DispIntf:  IAgentCtlCommandsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F5BE8BE1-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentCtlCommandsDisp = dispinterface
    ['{F5BE8BE1-7DE6-11D0-91FE-00C04FD701A5}']
    property Item[const Name: WideString]: IAgentCtlCommandEx readonly dispid 0; default;
    function  Command(const Name: WideString): IAgentCtlCommandEx; dispid 15;
    property Count: Integer readonly dispid 2;
    property Caption: WideString dispid 3;
    property Voice: WideString dispid 4;
    property Visible: WordBool dispid 5;
    property Enum: IUnknown readonly dispid -4;
    function  Add(const Name: WideString; Caption: OleVariant; Voice: OleVariant; 
                  Enabled: OleVariant; Visible: OleVariant): IAgentCtlCommand; dispid 10;
    function  Insert(const Name: WideString; const RefName: WideString; Before: OleVariant; 
                     Caption: OleVariant; Voice: OleVariant; Enabled: OleVariant; 
                     Visible: OleVariant): IAgentCtlCommand; dispid 11;
    procedure Remove(const Name: WideString); dispid 13;
    procedure RemoveAll; dispid 14;
  end;

// *********************************************************************//
// Interface   : IAgentCtlCommandsEx
// Indicateurs : (4416) Dual OleAutomation Dispatchable
// GUID        : {6BA90C01-3910-11D1-ACB3-00C04FD97575}
// *********************************************************************//
  IAgentCtlCommandsEx = interface(IAgentCtlCommands)
    ['{6BA90C01-3910-11D1-ACB3-00C04FD97575}']
    procedure Set_DefaultCommand(const Name: WideString); safecall;
    function  Get_DefaultCommand: WideString; safecall;
    procedure Set_HelpContextID(ID: Integer); safecall;
    function  Get_HelpContextID: Integer; safecall;
    procedure Set_FontName(const FontName: WideString); safecall;
    function  Get_FontName: WideString; safecall;
    function  Get_FontSize: Integer; safecall;
    procedure Set_FontSize(FontSize: Integer); safecall;
    procedure Set_VoiceCaption(const VoiceCaption: WideString); safecall;
    function  Get_VoiceCaption: WideString; safecall;
    procedure Set_GlobalVoiceCommandsEnabled(Enable: WordBool); safecall;
    function  Get_GlobalVoiceCommandsEnabled: WordBool; safecall;
    property DefaultCommand: WideString read Get_DefaultCommand write Set_DefaultCommand;
    property HelpContextID: Integer read Get_HelpContextID write Set_HelpContextID;
    property FontName: WideString read Get_FontName write Set_FontName;
    property FontSize: Integer read Get_FontSize write Set_FontSize;
    property VoiceCaption: WideString read Get_VoiceCaption write Set_VoiceCaption;
    property GlobalVoiceCommandsEnabled: WordBool read Get_GlobalVoiceCommandsEnabled write Set_GlobalVoiceCommandsEnabled;
  end;

// *********************************************************************//
// DispIntf:  IAgentCtlCommandsExDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6BA90C01-3910-11D1-ACB3-00C04FD97575}
// *********************************************************************//
  IAgentCtlCommandsExDisp = dispinterface
    ['{6BA90C01-3910-11D1-ACB3-00C04FD97575}']
    property DefaultCommand: WideString dispid 16;
    property HelpContextID: Integer dispid 17;
    property FontName: WideString dispid 21;
    property FontSize: Integer dispid 23;
    property VoiceCaption: WideString dispid 22;
    property GlobalVoiceCommandsEnabled: WordBool dispid 24;
    property Item[const Name: WideString]: IAgentCtlCommandEx readonly dispid 0; default;
    function  Command(const Name: WideString): IAgentCtlCommandEx; dispid 15;
    property Count: Integer readonly dispid 2;
    property Caption: WideString dispid 3;
    property Voice: WideString dispid 4;
    property Visible: WordBool dispid 5;
    property Enum: IUnknown readonly dispid -4;
    function  Add(const Name: WideString; Caption: OleVariant; Voice: OleVariant; 
                  Enabled: OleVariant; Visible: OleVariant): IAgentCtlCommand; dispid 10;
    function  Insert(const Name: WideString; const RefName: WideString; Before: OleVariant; 
                     Caption: OleVariant; Voice: OleVariant; Enabled: OleVariant; 
                     Visible: OleVariant): IAgentCtlCommand; dispid 11;
    procedure Remove(const Name: WideString); dispid 13;
    procedure RemoveAll; dispid 14;
  end;

// *********************************************************************//
// Interface   : IAgentCtlCommand
// Indicateurs : (4416) Dual OleAutomation Dispatchable
// GUID        : {F5BE8BE3-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentCtlCommand = interface(IDispatch)
    ['{F5BE8BE3-7DE6-11D0-91FE-00C04FD701A5}']
    function  Get_Voice: WideString; safecall;
    procedure Set_Voice(const Voice: WideString); safecall;
    function  Get_Caption: WideString; safecall;
    procedure Set_Caption(const Caption: WideString); safecall;
    function  Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Enabled: WordBool); safecall;
    function  Get_Visible: WordBool; safecall;
    procedure Set_Visible(Visible: WordBool); safecall;
    function  Get_Confidence: Integer; safecall;
    procedure Set_Confidence(Confidence: Integer); safecall;
    function  Get_ConfidenceText: WideString; safecall;
    procedure Set_ConfidenceText(const Text: WideString); safecall;
    property Voice: WideString read Get_Voice write Set_Voice;
    property Caption: WideString read Get_Caption write Set_Caption;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property Confidence: Integer read Get_Confidence write Set_Confidence;
    property ConfidenceText: WideString read Get_ConfidenceText write Set_ConfidenceText;
  end;

// *********************************************************************//
// DispIntf:  IAgentCtlCommandDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F5BE8BE3-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentCtlCommandDisp = dispinterface
    ['{F5BE8BE3-7DE6-11D0-91FE-00C04FD701A5}']
    property Voice: WideString dispid 1;
    property Caption: WideString dispid 2;
    property Enabled: WordBool dispid 5;
    property Visible: WordBool dispid 6;
    property Confidence: Integer dispid 3;
    property ConfidenceText: WideString dispid 4;
  end;

// *********************************************************************//
// Interface   : IAgentCtlCommandEx
// Indicateurs : (4416) Dual OleAutomation Dispatchable
// GUID        : {B0913410-3B44-11D1-ACBA-00C04FD97575}
// *********************************************************************//
  IAgentCtlCommandEx = interface(IAgentCtlCommand)
    ['{B0913410-3B44-11D1-ACBA-00C04FD97575}']
    procedure Set_HelpContextID(ID: Integer); safecall;
    function  Get_HelpContextID: Integer; safecall;
    procedure Set_VoiceCaption(const VoiceCaption: WideString); safecall;
    function  Get_VoiceCaption: WideString; safecall;
    property HelpContextID: Integer read Get_HelpContextID write Set_HelpContextID;
    property VoiceCaption: WideString read Get_VoiceCaption write Set_VoiceCaption;
  end;

// *********************************************************************//
// DispIntf:  IAgentCtlCommandExDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B0913410-3B44-11D1-ACBA-00C04FD97575}
// *********************************************************************//
  IAgentCtlCommandExDisp = dispinterface
    ['{B0913410-3B44-11D1-ACBA-00C04FD97575}']
    property HelpContextID: Integer dispid 7;
    property VoiceCaption: WideString dispid 8;
    property Voice: WideString dispid 1;
    property Caption: WideString dispid 2;
    property Enabled: WordBool dispid 5;
    property Visible: WordBool dispid 6;
    property Confidence: Integer dispid 3;
    property ConfidenceText: WideString dispid 4;
  end;

// *********************************************************************//
// Interface   : IAgentCtlRequest
// Indicateurs : (4416) Dual OleAutomation Dispatchable
// GUID        : {1DAB85C3-803A-11D0-AC63-00C04FD97575}
// *********************************************************************//
  IAgentCtlRequest = interface(IDispatch)
    ['{1DAB85C3-803A-11D0-AC63-00C04FD97575}']
    function  Get_ID: Integer; safecall;
    function  Get_Status: Integer; safecall;
    function  Get_Description: WideString; safecall;
    function  Get_Number: Integer; safecall;
    property ID: Integer read Get_ID;
    property Status: Integer read Get_Status;
    property Description: WideString read Get_Description;
    property Number: Integer read Get_Number;
  end;

// *********************************************************************//
// DispIntf:  IAgentCtlRequestDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1DAB85C3-803A-11D0-AC63-00C04FD97575}
// *********************************************************************//
  IAgentCtlRequestDisp = dispinterface
    ['{1DAB85C3-803A-11D0-AC63-00C04FD97575}']
    property ID: Integer readonly dispid 0;
    property Status: Integer readonly dispid 1;
    property Description: WideString readonly dispid 2;
    property Number: Integer readonly dispid 3;
  end;

// *********************************************************************//
// Interface   : IAgentCtlAnimationNames
// Indicateurs : (4416) Dual OleAutomation Dispatchable
// GUID        : {8B77181C-D3EF-11D1-8500-00C04FA34A14}
// *********************************************************************//
  IAgentCtlAnimationNames = interface(IDispatch)
    ['{8B77181C-D3EF-11D1-8500-00C04FA34A14}']
    function  Get_Enum: IUnknown; safecall;
    property Enum: IUnknown read Get_Enum;
  end;

// *********************************************************************//
// DispIntf:  IAgentCtlAnimationNamesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8B77181C-D3EF-11D1-8500-00C04FA34A14}
// *********************************************************************//
  IAgentCtlAnimationNamesDisp = dispinterface
    ['{8B77181C-D3EF-11D1-8500-00C04FA34A14}']
    property Enum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface   : IAgentCtlAudioObject
// Indicateurs : (4416) Dual OleAutomation Dispatchable
// GUID        : {F5BE8BDB-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentCtlAudioObject = interface(IDispatch)
    ['{F5BE8BDB-7DE6-11D0-91FE-00C04FD701A5}']
    function  Get_Enabled: WordBool; safecall;
    function  Get_SoundEffects: WordBool; safecall;
    property Enabled: WordBool read Get_Enabled;
    property SoundEffects: WordBool read Get_SoundEffects;
  end;

// *********************************************************************//
// DispIntf:  IAgentCtlAudioObjectDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F5BE8BDB-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentCtlAudioObjectDisp = dispinterface
    ['{F5BE8BDB-7DE6-11D0-91FE-00C04FD701A5}']
    property Enabled: WordBool readonly dispid 1;
    property SoundEffects: WordBool readonly dispid 2;
  end;

// *********************************************************************//
// Interface   : IAgentCtlAudioObjectEx
// Indicateurs : (4416) Dual OleAutomation Dispatchable
// GUID        : {F5BE8BF0-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentCtlAudioObjectEx = interface(IAgentCtlAudioObject)
    ['{F5BE8BF0-7DE6-11D0-91FE-00C04FD701A5}']
    function  Get_Status: Smallint; safecall;
    property Status: Smallint read Get_Status;
  end;

// *********************************************************************//
// DispIntf:  IAgentCtlAudioObjectExDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F5BE8BF0-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentCtlAudioObjectExDisp = dispinterface
    ['{F5BE8BF0-7DE6-11D0-91FE-00C04FD701A5}']
    property Status: Smallint readonly dispid 3;
    property Enabled: WordBool readonly dispid 1;
    property SoundEffects: WordBool readonly dispid 2;
  end;

// *********************************************************************//
// Interface   : IAgentCtlSpeechInput
// Indicateurs : (4416) Dual OleAutomation Dispatchable
// GUID        : {F5BE8BDD-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentCtlSpeechInput = interface(IDispatch)
    ['{F5BE8BDD-7DE6-11D0-91FE-00C04FD701A5}']
    function  Get_Enabled: WordBool; safecall;
    function  Get_Language: WideString; safecall;
    function  Get_HotKey: WideString; safecall;
    function  Get_Installed: WordBool; safecall;
    function  Get_Engine: WideString; safecall;
    procedure Set_Engine(const Engine: WideString); safecall;
    function  Get_ListeningTip: WordBool; safecall;
    property Enabled: WordBool read Get_Enabled;
    property Language: WideString read Get_Language;
    property HotKey: WideString read Get_HotKey;
    property Installed: WordBool read Get_Installed;
    property Engine: WideString read Get_Engine write Set_Engine;
    property ListeningTip: WordBool read Get_ListeningTip;
  end;

// *********************************************************************//
// DispIntf:  IAgentCtlSpeechInputDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F5BE8BDD-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentCtlSpeechInputDisp = dispinterface
    ['{F5BE8BDD-7DE6-11D0-91FE-00C04FD701A5}']
    property Enabled: WordBool readonly dispid 1;
    property Language: WideString readonly dispid 2;
    property HotKey: WideString readonly dispid 3;
    property Installed: WordBool readonly dispid 4;
    property Engine: WideString dispid 5;
    property ListeningTip: WordBool readonly dispid 6;
  end;

// *********************************************************************//
// Interface   : IAgentCtlPropertySheet
// Indicateurs : (4416) Dual OleAutomation Dispatchable
// GUID        : {F5BE8BDF-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentCtlPropertySheet = interface(IDispatch)
    ['{F5BE8BDF-7DE6-11D0-91FE-00C04FD701A5}']
    function  Get_Left: Smallint; safecall;
    function  Get_Top: Smallint; safecall;
    function  Get_Height: Smallint; safecall;
    function  Get_Width: Smallint; safecall;
    procedure Set_Visible(Visible: WordBool); safecall;
    function  Get_Visible: WordBool; safecall;
    procedure Set_Page(const Page: WideString); safecall;
    function  Get_Page: WideString; safecall;
    property Left: Smallint read Get_Left;
    property Top: Smallint read Get_Top;
    property Height: Smallint read Get_Height;
    property Width: Smallint read Get_Width;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property Page: WideString read Get_Page write Set_Page;
  end;

// *********************************************************************//
// DispIntf:  IAgentCtlPropertySheetDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F5BE8BDF-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentCtlPropertySheetDisp = dispinterface
    ['{F5BE8BDF-7DE6-11D0-91FE-00C04FD701A5}']
    property Left: Smallint readonly dispid 1;
    property Top: Smallint readonly dispid 2;
    property Height: Smallint readonly dispid 3;
    property Width: Smallint readonly dispid 4;
    property Visible: WordBool dispid 6;
    property Page: WideString dispid 5;
  end;

// *********************************************************************//
// Interface   : IAgentCtlUserInput
// Indicateurs : (4416) Dual OleAutomation Dispatchable
// GUID        : {C4ABF875-8100-11D0-AC63-00C04FD97575}
// *********************************************************************//
  IAgentCtlUserInput = interface(IDispatch)
    ['{C4ABF875-8100-11D0-AC63-00C04FD97575}']
    function  Get_Count: Smallint; safecall;
    function  Get_Name: WideString; safecall;
    function  Get_CharacterID: WideString; safecall;
    function  Get_Confidence: Integer; safecall;
    function  Get_Voice: WideString; safecall;
    function  Get_Alt1Name: WideString; safecall;
    function  Get_Alt1Confidence: Integer; safecall;
    function  Get_Alt1Voice: WideString; safecall;
    function  Get_Alt2Name: WideString; safecall;
    function  Get_Alt2Confidence: Integer; safecall;
    function  Get_Alt2Voice: WideString; safecall;
    property Count: Smallint read Get_Count;
    property Name: WideString read Get_Name;
    property CharacterID: WideString read Get_CharacterID;
    property Confidence: Integer read Get_Confidence;
    property Voice: WideString read Get_Voice;
    property Alt1Name: WideString read Get_Alt1Name;
    property Alt1Confidence: Integer read Get_Alt1Confidence;
    property Alt1Voice: WideString read Get_Alt1Voice;
    property Alt2Name: WideString read Get_Alt2Name;
    property Alt2Confidence: Integer read Get_Alt2Confidence;
    property Alt2Voice: WideString read Get_Alt2Voice;
  end;

// *********************************************************************//
// DispIntf:  IAgentCtlUserInputDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C4ABF875-8100-11D0-AC63-00C04FD97575}
// *********************************************************************//
  IAgentCtlUserInputDisp = dispinterface
    ['{C4ABF875-8100-11D0-AC63-00C04FD97575}']
    property Count: Smallint readonly dispid 1610743808;
    property Name: WideString readonly dispid 1610743809;
    property CharacterID: WideString readonly dispid 1610743810;
    property Confidence: Integer readonly dispid 1610743811;
    property Voice: WideString readonly dispid 1610743812;
    property Alt1Name: WideString readonly dispid 1610743813;
    property Alt1Confidence: Integer readonly dispid 1610743814;
    property Alt1Voice: WideString readonly dispid 1610743815;
    property Alt2Name: WideString readonly dispid 1610743816;
    property Alt2Confidence: Integer readonly dispid 1610743817;
    property Alt2Voice: WideString readonly dispid 1610743818;
  end;

// *********************************************************************//
// Interface   : IAgentCtlCommandsWindow
// Indicateurs : (4416) Dual OleAutomation Dispatchable
// GUID        : {6D0ECB27-9968-11D0-AC6E-00C04FD97575}
// *********************************************************************//
  IAgentCtlCommandsWindow = interface(IDispatch)
    ['{6D0ECB27-9968-11D0-AC6E-00C04FD97575}']
    function  Get_Visible: WordBool; safecall;
    procedure Set_Visible(Visible: WordBool); safecall;
    function  Get_Left: Smallint; safecall;
    function  Get_Top: Smallint; safecall;
    function  Get_Height: Smallint; safecall;
    function  Get_Width: Smallint; safecall;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property Left: Smallint read Get_Left;
    property Top: Smallint read Get_Top;
    property Height: Smallint read Get_Height;
    property Width: Smallint read Get_Width;
  end;

// *********************************************************************//
// DispIntf:  IAgentCtlCommandsWindowDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6D0ECB27-9968-11D0-AC6E-00C04FD97575}
// *********************************************************************//
  IAgentCtlCommandsWindowDisp = dispinterface
    ['{6D0ECB27-9968-11D0-AC6E-00C04FD97575}']
    property Visible: WordBool dispid 5;
    property Left: Smallint readonly dispid 6;
    property Top: Smallint readonly dispid 7;
    property Height: Smallint readonly dispid 8;
    property Width: Smallint readonly dispid 9;
  end;

// *********************************************************************//
// Interface   : IAgentCtl
// Indicateurs : (4416) Dual OleAutomation Dispatchable
// GUID        : {F5BE8BD1-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentCtl = interface(IDispatch)
    ['{F5BE8BD1-7DE6-11D0-91FE-00C04FD701A5}']
    function  Get_Characters: IAgentCtlCharacters; safecall;
    function  Get_AudioOutput: IAgentCtlAudioObjectEx; safecall;
    function  Get_SpeechInput: IAgentCtlSpeechInput; safecall;
    function  Get_PropertySheet: IAgentCtlPropertySheet; safecall;
    function  Get_CommandsWindow: IAgentCtlCommandsWindow; safecall;
    function  Get_Connected: WordBool; safecall;
    procedure Set_Connected(Connected: WordBool); safecall;
    function  Get_Suspended: WordBool; safecall;
    property Characters: IAgentCtlCharacters read Get_Characters;
    property AudioOutput: IAgentCtlAudioObjectEx read Get_AudioOutput;
    property SpeechInput: IAgentCtlSpeechInput read Get_SpeechInput;
    property PropertySheet: IAgentCtlPropertySheet read Get_PropertySheet;
    property CommandsWindow: IAgentCtlCommandsWindow read Get_CommandsWindow;
    property Connected: WordBool read Get_Connected write Set_Connected;
    property Suspended: WordBool read Get_Suspended;
  end;

// *********************************************************************//
// DispIntf:  IAgentCtlDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F5BE8BD1-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  IAgentCtlDisp = dispinterface
    ['{F5BE8BD1-7DE6-11D0-91FE-00C04FD701A5}']
    property Characters: IAgentCtlCharacters readonly dispid 3;
    property AudioOutput: IAgentCtlAudioObjectEx readonly dispid 4;
    property SpeechInput: IAgentCtlSpeechInput readonly dispid 5;
    property PropertySheet: IAgentCtlPropertySheet readonly dispid 8;
    property CommandsWindow: IAgentCtlCommandsWindow readonly dispid 12;
    property Connected: WordBool dispid 9;
    property Suspended: WordBool readonly dispid 14;
  end;

// *********************************************************************//
// Interface   : IAgentCtlEx
// Indicateurs : (4416) Dual OleAutomation Dispatchable
// GUID        : {8563FF20-8ECC-11D1-B9B4-00C04FD97575}
// *********************************************************************//
  IAgentCtlEx = interface(IAgentCtl)
    ['{8563FF20-8ECC-11D1-B9B4-00C04FD97575}']
    procedure ShowDefaultCharacterProperties(x: OleVariant; y: OleVariant); safecall;
    function  Get_RaiseRequestErrors: WordBool; safecall;
    procedure Set_RaiseRequestErrors(RaiseErrors: WordBool); safecall;
    property RaiseRequestErrors: WordBool read Get_RaiseRequestErrors write Set_RaiseRequestErrors;
  end;

// *********************************************************************//
// DispIntf:  IAgentCtlExDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8563FF20-8ECC-11D1-B9B4-00C04FD97575}
// *********************************************************************//
  IAgentCtlExDisp = dispinterface
    ['{8563FF20-8ECC-11D1-B9B4-00C04FD97575}']
    procedure ShowDefaultCharacterProperties(x: OleVariant; y: OleVariant); dispid 20;
    property RaiseRequestErrors: WordBool dispid 21;
    property Characters: IAgentCtlCharacters readonly dispid 3;
    property AudioOutput: IAgentCtlAudioObjectEx readonly dispid 4;
    property SpeechInput: IAgentCtlSpeechInput readonly dispid 5;
    property PropertySheet: IAgentCtlPropertySheet readonly dispid 8;
    property CommandsWindow: IAgentCtlCommandsWindow readonly dispid 12;
    property Connected: WordBool dispid 9;
    property Suspended: WordBool readonly dispid 14;
  end;

// *********************************************************************//
// DispIntf:  _AgentEvents
// Flags:     (4096) Dispatchable
// GUID:      {F5BE8BD4-7DE6-11D0-91FE-00C04FD701A5}
// *********************************************************************//
  _AgentEvents = dispinterface
    ['{F5BE8BD4-7DE6-11D0-91FE-00C04FD701A5}']
    procedure ActivateInput(const CharacterID: WideString); dispid 1;
    procedure DeactivateInput(const CharacterID: WideString); dispid 3;
    procedure Click(const CharacterID: WideString; Button: Smallint; Shift: Smallint; x: Smallint; 
                    y: Smallint); dispid 2;
    procedure DblClick(const CharacterID: WideString; Button: Smallint; Shift: Smallint; 
                       x: Smallint; y: Smallint); dispid 4;
    procedure DragStart(const CharacterID: WideString; Button: Smallint; Shift: Smallint; 
                        x: Smallint; y: Smallint); dispid 5;
    procedure DragComplete(const CharacterID: WideString; Button: Smallint; Shift: Smallint; 
                           x: Smallint; y: Smallint); dispid 6;
    procedure Show(const CharacterID: WideString; Cause: Smallint); dispid 15;
    procedure Hide(const CharacterID: WideString; Cause: Smallint); dispid 7;
    procedure RequestStart(const Request: IDispatch); dispid 9;
    procedure RequestComplete(const Request: IDispatch); dispid 11;
    procedure Restart; dispid 21;
    procedure Shutdown; dispid 12;
    procedure Bookmark(BookmarkID: Integer); dispid 16;
    procedure Command(const UserInput: IDispatch); dispid 17;
    procedure IdleStart(const CharacterID: WideString); dispid 19;
    procedure IdleComplete(const CharacterID: WideString); dispid 20;
    procedure Move(const CharacterID: WideString; x: Smallint; y: Smallint; Cause: Smallint); dispid 22;
    procedure Size(const CharacterID: WideString; Width: Smallint; Height: Smallint); dispid 23;
    procedure BalloonShow(const CharacterID: WideString); dispid 24;
    procedure BalloonHide(const CharacterID: WideString); dispid 25;
    procedure HelpComplete(const CharacterID: WideString; const Name: WideString; Cause: Smallint); dispid 26;
    procedure ListenStart(const CharacterID: WideString); dispid 27;
    procedure ListenComplete(const CharacterID: WideString; Cause: Smallint); dispid 28;
    procedure DefaultCharacterChange(const GUID: WideString); dispid 30;
    procedure AgentPropertyChange; dispid 31;
    procedure ActiveClientChange(const CharacterID: WideString; Active: WordBool); dispid 32;
  end;


// *********************************************************************//
// Déclaration de classe proxy de contrôle OLE
// Nom du contrôle      : TAgent
// Chaîne d'aide        : Microsoft Agent Control 2.0
// Interface par défaut : IAgentCtlEx
// DISP Int. Déf. ?     : No
// Interface événements : _AgentEvents
// TypeFlags            : (2) CanCreate
// *********************************************************************//
  TAgentActivateInput = procedure(Sender: TObject; const CharacterID: WideString) of object;
  TAgentDeactivateInput = procedure(Sender: TObject; const CharacterID: WideString) of object;
  TAgentClick = procedure(Sender: TObject; const CharacterID: WideString; Button: Smallint; 
                                           Shift: Smallint; x: Smallint; y: Smallint) of object;
  TAgentDblClick = procedure(Sender: TObject; const CharacterID: WideString; Button: Smallint; 
                                              Shift: Smallint; x: Smallint; y: Smallint) of object;
  TAgentDragStart = procedure(Sender: TObject; const CharacterID: WideString; Button: Smallint; 
                                               Shift: Smallint; x: Smallint; y: Smallint) of object;
  TAgentDragComplete = procedure(Sender: TObject; const CharacterID: WideString; Button: Smallint; 
                                                  Shift: Smallint; x: Smallint; y: Smallint) of object;
  TAgentShow = procedure(Sender: TObject; const CharacterID: WideString; Cause: Smallint) of object;
  TAgentHide = procedure(Sender: TObject; const CharacterID: WideString; Cause: Smallint) of object;
  TAgentRequestStart = procedure(Sender: TObject; const Request: IDispatch) of object;
  TAgentRequestComplete = procedure(Sender: TObject; const Request: IDispatch) of object;
  TAgentBookmark = procedure(Sender: TObject; BookmarkID: Integer) of object;
  TAgentCommand = procedure(Sender: TObject; const UserInput: IDispatch) of object;
  TAgentIdleStart = procedure(Sender: TObject; const CharacterID: WideString) of object;
  TAgentIdleComplete = procedure(Sender: TObject; const CharacterID: WideString) of object;
  TAgentMove = procedure(Sender: TObject; const CharacterID: WideString; x: Smallint; y: Smallint; 
                                          Cause: Smallint) of object;
  TAgentSize = procedure(Sender: TObject; const CharacterID: WideString; Width: Smallint; 
                                          Height: Smallint) of object;
  TAgentBalloonShow = procedure(Sender: TObject; const CharacterID: WideString) of object;
  TAgentBalloonHide = procedure(Sender: TObject; const CharacterID: WideString) of object;
  TAgentHelpComplete = procedure(Sender: TObject; const CharacterID: WideString; 
                                                  const Name: WideString; Cause: Smallint) of object;
  TAgentListenStart = procedure(Sender: TObject; const CharacterID: WideString) of object;
  TAgentListenComplete = procedure(Sender: TObject; const CharacterID: WideString; Cause: Smallint) of object;
  TAgentDefaultCharacterChange = procedure(Sender: TObject; const GUID: WideString) of object;
  TAgentActiveClientChange = procedure(Sender: TObject; const CharacterID: WideString; 
                                                        Active: WordBool) of object;

  TAgent = class(TOleControl)
  private
    FOnActivateInput: TAgentActivateInput;
    FOnDeactivateInput: TAgentDeactivateInput;
    FOnClick: TAgentClick;
    FOnDblClick: TAgentDblClick;
    FOnDragStart: TAgentDragStart;
    FOnDragComplete: TAgentDragComplete;
    FOnShow: TAgentShow;
    FOnHide: TAgentHide;
    FOnRequestStart: TAgentRequestStart;
    FOnRequestComplete: TAgentRequestComplete;
    FOnRestart: TNotifyEvent;
    FOnShutdown: TNotifyEvent;
    FOnBookmark: TAgentBookmark;
    FOnCommand: TAgentCommand;
    FOnIdleStart: TAgentIdleStart;
    FOnIdleComplete: TAgentIdleComplete;
    FOnMove: TAgentMove;
    FOnSize: TAgentSize;
    FOnBalloonShow: TAgentBalloonShow;
    FOnBalloonHide: TAgentBalloonHide;
    FOnHelpComplete: TAgentHelpComplete;
    FOnListenStart: TAgentListenStart;
    FOnListenComplete: TAgentListenComplete;
    FOnDefaultCharacterChange: TAgentDefaultCharacterChange;
    FOnAgentPropertyChange: TNotifyEvent;
    FOnActiveClientChange: TAgentActiveClientChange;
    FIntf: IAgentCtlEx;
    function  GetControlInterface: IAgentCtlEx;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function  Get_Characters: IAgentCtlCharacters;
    function  Get_AudioOutput: IAgentCtlAudioObjectEx;
    function  Get_SpeechInput: IAgentCtlSpeechInput;
    function  Get_PropertySheet: IAgentCtlPropertySheet;
    function  Get_CommandsWindow: IAgentCtlCommandsWindow;
  public
    procedure ShowDefaultCharacterProperties; overload;
    procedure ShowDefaultCharacterProperties(x: OleVariant); overload;
    procedure ShowDefaultCharacterProperties(x: OleVariant; y: OleVariant); overload;
    property  ControlInterface: IAgentCtlEx read GetControlInterface;
    property  DefaultInterface: IAgentCtlEx read GetControlInterface;
    property Characters: IAgentCtlCharacters read Get_Characters;
    property AudioOutput: IAgentCtlAudioObjectEx read Get_AudioOutput;
    property SpeechInput: IAgentCtlSpeechInput read Get_SpeechInput;
    property PropertySheet: IAgentCtlPropertySheet read Get_PropertySheet;
    property CommandsWindow: IAgentCtlCommandsWindow read Get_CommandsWindow;
    property Suspended: WordBool index 14 read GetWordBoolProp;
  published
    property Connected: WordBool index 9 read GetWordBoolProp write SetWordBoolProp stored False;
    property RaiseRequestErrors: WordBool index 21 read GetWordBoolProp write SetWordBoolProp stored False;
    property OnActivateInput: TAgentActivateInput read FOnActivateInput write FOnActivateInput;
    property OnDeactivateInput: TAgentDeactivateInput read FOnDeactivateInput write FOnDeactivateInput;
    property OnClick: TAgentClick read FOnClick write FOnClick;
    property OnDblClick: TAgentDblClick read FOnDblClick write FOnDblClick;
    property OnDragStart: TAgentDragStart read FOnDragStart write FOnDragStart;
    property OnDragComplete: TAgentDragComplete read FOnDragComplete write FOnDragComplete;
    property OnShow: TAgentShow read FOnShow write FOnShow;
    property OnHide: TAgentHide read FOnHide write FOnHide;
    property OnRequestStart: TAgentRequestStart read FOnRequestStart write FOnRequestStart;
    property OnRequestComplete: TAgentRequestComplete read FOnRequestComplete write FOnRequestComplete;
    property OnRestart: TNotifyEvent read FOnRestart write FOnRestart;
    property OnShutdown: TNotifyEvent read FOnShutdown write FOnShutdown;
    property OnBookmark: TAgentBookmark read FOnBookmark write FOnBookmark;
    property OnCommand: TAgentCommand read FOnCommand write FOnCommand;
    property OnIdleStart: TAgentIdleStart read FOnIdleStart write FOnIdleStart;
    property OnIdleComplete: TAgentIdleComplete read FOnIdleComplete write FOnIdleComplete;
    property OnMove: TAgentMove read FOnMove write FOnMove;
    property OnSize: TAgentSize read FOnSize write FOnSize;
    property OnBalloonShow: TAgentBalloonShow read FOnBalloonShow write FOnBalloonShow;
    property OnBalloonHide: TAgentBalloonHide read FOnBalloonHide write FOnBalloonHide;
    property OnHelpComplete: TAgentHelpComplete read FOnHelpComplete write FOnHelpComplete;
    property OnListenStart: TAgentListenStart read FOnListenStart write FOnListenStart;
    property OnListenComplete: TAgentListenComplete read FOnListenComplete write FOnListenComplete;
    property OnDefaultCharacterChange: TAgentDefaultCharacterChange read FOnDefaultCharacterChange write FOnDefaultCharacterChange;
    property OnAgentPropertyChange: TNotifyEvent read FOnAgentPropertyChange write FOnAgentPropertyChange;
    property OnActiveClientChange: TAgentActiveClientChange read FOnActiveClientChange write FOnActiveClientChange;
  end;

procedure Register;

implementation

uses ComObj;

procedure TAgent.InitControlData;
const
  CEventDispIDs: array [0..25] of DWORD = (
    $00000001, $00000003, $00000002, $00000004, $00000005, $00000006,
    $0000000F, $00000007, $00000009, $0000000B, $00000015, $0000000C,
    $00000010, $00000011, $00000013, $00000014, $00000016, $00000017,
    $00000018, $00000019, $0000001A, $0000001B, $0000001C, $0000001E,
    $0000001F, $00000020);
  CControlData: TControlData2 = (
    ClassID: '{D45FD31B-5C6E-11D1-9EC1-00C04FD7081F}';
    EventIID: '{F5BE8BD4-7DE6-11D0-91FE-00C04FD701A5}';
    EventCount: 26;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$80004002*);
    Flags: $00000000;
    Version: 401);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnActivateInput) - Cardinal(Self);
end;

procedure TAgent.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IAgentCtlEx;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TAgent.GetControlInterface: IAgentCtlEx;
begin
  CreateControl;
  Result := FIntf;
end;

function  TAgent.Get_Characters: IAgentCtlCharacters;
begin
  Result := DefaultInterface.Get_Characters;
end;

function  TAgent.Get_AudioOutput: IAgentCtlAudioObjectEx;
begin
  Result := DefaultInterface.Get_AudioOutput;
end;

function  TAgent.Get_SpeechInput: IAgentCtlSpeechInput;
begin
  Result := DefaultInterface.Get_SpeechInput;
end;

function  TAgent.Get_PropertySheet: IAgentCtlPropertySheet;
begin
  Result := DefaultInterface.Get_PropertySheet;
end;

function  TAgent.Get_CommandsWindow: IAgentCtlCommandsWindow;
begin
  Result := DefaultInterface.Get_CommandsWindow;
end;

procedure TAgent.ShowDefaultCharacterProperties;
begin
  DefaultInterface.ShowDefaultCharacterProperties(EmptyParam, EmptyParam);
end;

procedure TAgent.ShowDefaultCharacterProperties(x: OleVariant);
begin
  DefaultInterface.ShowDefaultCharacterProperties(x, EmptyParam);
end;

procedure TAgent.ShowDefaultCharacterProperties(x: OleVariant; y: OleVariant);
begin
  DefaultInterface.ShowDefaultCharacterProperties(x, y);
end;

procedure Register;
begin
  RegisterComponents('ActiveX',[TAgent]);
end;

end.
