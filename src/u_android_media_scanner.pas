(* C2PP
  ***************************************************************************

  My libraries for Delphi
  Copyright (c) 1990-2026 Patrick PREMARTIN

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

  ***************************************************************************

  This repository contains functions, procedures and classes to use in
  Delphi projects (console, VCL, FireMonkey and others). It's my "everything reuseable things" toolbox.

  The units to be used in your projects can be found in the "src" folder.
  Some features are explained on my blog or have been coded live on Twitch.

  Examples of use in the form of VCL or FireMonkey projects are available in
  the "samples" subfolder.

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://librairies.developpeur-pascal.fr

  Project site :
  https://github.com/DeveloppeurPascal/librairies

  ***************************************************************************
  File last update : 2026-03-30T16:35:19.678+02:00
  Signature : 3831db2725845a0f900771c92ce98e692cdd4866
  ***************************************************************************
*)

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
