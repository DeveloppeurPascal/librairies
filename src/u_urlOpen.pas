unit u_urlOpen;

interface

/// <summary>Launch URL website on the default browser</summary>
/// <param name="URL">Absolute address of the website to open in the web browser</param>
procedure url_Open_In_Browser(URL: string);

implementation

uses
 System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
{$IF Defined(IOS)}
macapi.helpers, iOSapi.Foundation, FMX.Helpers.iOS;
{$ELSEIF Defined(ANDROID)}
 Androidapi.JNI.GraphicsContentViewText,
 Androidapi.Helpers;
{$ELSEIF Defined(MACOS)}
  Posix.Stdlib;
{$ELSEIF Defined(MSWINDOWS)}
 Winapi.ShellAPI, Winapi.Windows;
{$ENDIF}

procedure url_Open_In_Browser(URL: string);
{$IF Defined(ANDROID)}
var
 Intent: JIntent;
{$ENDIF}
begin
{$IF Defined(ANDROID)}
 Intent := TJIntent.Create;
 Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
 Intent.setData(StrToJURI(URL));
 // SharedActivity.startActivity(Intent);
 tandroidhelper.Activity.startActivity(Intent);
{$ELSEIF Defined(MSWINDOWS)}
 ShellExecute(0, 'OPEN', PWideChar(URL), nil, nil, SW_SHOWNORMAL);
{$ELSEIF Defined(IOS)}
 SharedApplication.OpenURL(StrToNSUrl(Url));
{$ELSEIF Defined(MACOS)}
  _system(PAnsiChar('open ' + AnsiString(URL)));
{$ENDIF}
end;

end.
