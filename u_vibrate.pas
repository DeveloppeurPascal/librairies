unit u_vibrate;

interface

(*
  Permet le déclenchement du vibreur du smartphone.
  basé sur http://community.embarcadero.com/blogs/entry/how-to-vibrate-ios-and-android-phones-using-firemonkey-and-xe8

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
    ((SharedActivityContext.getSystemService
    (TJContext.JavaClass.VIBRATOR_SERVICE) as ILocalObject).GetObjectID);
  Vibrator.vibrate(milliseconds);
{$ENDIF}
{$IFDEF IOS}
  AudioServicesPlaySystemSound(kSystemSoundID_vibrate);
{$ENDIF}
end;

end.
