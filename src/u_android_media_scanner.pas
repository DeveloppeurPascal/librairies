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
/// Signature : 875e8c3b3c6a59044494f85191ebbbd807b1797e
/// ***************************************************************************
/// </summary>

unit u_android_media_scanner;

// Cette unité Delphi contient des procedures à utiliser sous Android lorsqu'on
// désire donner accès en USB (MTP) à des fichiers créés depuis une application
// (c) 2016 Patrick Prémartin / Olf Software
//
// Liste des mises à jour :
// 16/06/2016, Patrick Prémartin : mise en production de la version initiale

interface

/// <summary>Add a file to the media files cache.</summary>
/// <para>Call it after creating the file you want to share with USB connected device.</para>
/// <param name="filename">absolute path + name of the file to add</param>
procedure android_media_scanner_add_file(filename: string);

/// <summary>Add a folder to the media files cache</summary>
/// <para>Call it after creating the folder you want to share with USB connected device.</para>
/// <param name="foldername">absolute path + name of the folder to add</param>
procedure android_media_scanner_add_folder(foldername: string);

implementation

{$IFDEF ANDROID}
uses AndroidApi.JNI.GraphicsContentViewText, AndroidApi.JNI.App,
  AndroidApi.JNI.Net, AndroidApi.Helpers;
{$ENDIF}

procedure android_media_scanner_add_file(filename: string);
{$IFDEF ANDROID}
var
  Intent: JIntent;
{$ENDIF}
begin
{$IFDEF ANDROID}
  Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_MEDIA_SCANNER_SCAN_FILE);
  Intent.setData(StrToJURI('file://' + filename));
  TAndroidHelper.Activity.sendBroadcast(Intent);
{$ENDIF}
end;

procedure android_media_scanner_add_folder(foldername: string);
{$IFDEF ANDROID}
var
  Intent: JIntent;
{$ENDIF}
begin
{$IFDEF ANDROID}
  Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_MEDIA_MOUNTED);
  Intent.setData(StrToJURI('file://' + foldername));
  TAndroidHelper.Activity.sendBroadcast(Intent);
{$ENDIF}
end;

end.
