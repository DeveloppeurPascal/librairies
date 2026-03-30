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
  Signature : 6b6d16ea5fabed543275bb7e666c14f42b15e662
  ***************************************************************************
*)

unit u_vibrate;

interface

(*
  Permet le déclenchement du vibreur du smartphone.
  basé sur https://blogs.embarcadero.com/how-to-vibrate-ios-and-android-phones-using-firemonkey-and-xe8/

  Pour Android :
  - activer l'autorisation "vibrate" ou "vibrer" dans les autorisations de l'application

  Pour iOS :
  - ajouter le framework "AudioToolbox" dans le SDK Manager avec comme chemin "$(SDKROOT)/System/Library/Frameworks"

  Pour les autres plateformes :
  - ne rien faire puisqu'elles ne sont pas gérées.

  Liste des mises à jour :
  23/05/2016, Patrick Prémartin : version initiale
*)

/// <summary>Use vibrator's smartphone or tablet (Android / iOS)</summary>
/// <param name="milliseconds">Number of milliseconds the device vibrate for Android. On iOs, there is no delay.</param>
procedure vibrate(milliseconds: cardinal = 500);

implementation

{$IFDEF ANDROID}

uses
  Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.Helpers,
  Androidapi.JNIBridge;
{$ENDIF}
{$IFDEF IOS}

uses
  IOSapi.MediaPlayer,
  IOSapi.CoreGraphics,
  FMX.Platform,
  FMX.Platform.IOS,
  IOSapi.UIKit,
  Macapi.ObjCRuntime,
  Macapi.ObjectiveC,
  IOSapi.Cocoatypes,
  Macapi.CoreFoundation,
  IOSapi.Foundation,
  IOSapi.CoreImage,
  IOSapi.QuartzCore,
  IOSapi.CoreData;

Const
  libAudioToolbox =
    '/System/Library/Frameworks/AudioToolbox.framework/AudioToolbox';
  kSystemSoundID_vibrate = $FFF;

Procedure AudioServicesPlaySystemSound(inSystemSoundID: integer); Cdecl;
  External libAudioToolbox Name _PU + 'AudioServicesPlaySystemSound';
{$ENDIF}

procedure vibrate(milliseconds: cardinal);
{$IFDEF ANDROID}
Var
  Vibrator: JVibrator;
{$ENDIF}
begin
{$IFDEF ANDROID}
  Vibrator := TJVibrator.Wrap
    ((TAndroidHelper.Context.getSystemService
    (TJContext.JavaClass.VIBRATOR_SERVICE) as ILocalObject).GetObjectID);
  Vibrator.vibrate(milliseconds);
{$ENDIF}
{$IFDEF IOS}
  AudioServicesPlaySystemSound(kSystemSoundID_vibrate);
{$ENDIF}
end;

end.
