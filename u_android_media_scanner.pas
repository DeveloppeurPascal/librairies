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
