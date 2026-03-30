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
  File last update : 2026-03-30T16:35:19.666+02:00
  Signature : eadafcbc47edef84cbf0b5d0bdc5ff5b6b81a3c5
  ***************************************************************************
*)

unit uGetDeviceName;

interface

function getDeviceName: string;

implementation

{$IF Defined(MSWINDOWS)}

uses system.sysutils;
{$ELSEIF Defined(IOS)}

uses
  iOSapi.CocoaTypes, iOSapi.Foundation, Macapi.ObjectiveC, Macapi.helpers;

type
  UIDeviceClass = interface(NSObjectClass)
    ['{A2DCE998-BF3A-4AB0-9B8D-4182B341C9DF}']
    function currentDevice: Pointer; cdecl;
  end;

  UIDevice = interface(NSObject)
    ['{70BB371D-314A-4BA9-912E-2EF72EB0F558}']
    function localizedModel: NSString; cdecl;
    function model: NSString; cdecl;
    function name: NSString; cdecl;
    function systemName: NSString; cdecl;
    function systemVersion: NSString; cdecl;
    function uniqueIdentifier: NSString; cdecl;
  end;

  TUIDevice = class(TOCGenericImport<UIDeviceClass, UIDevice>)
  end;
{$ELSEIF Defined(MACOS)}

uses Macapi.Foundation, Macapi.helpers;
{$ELSEIF Defined(ANDROID)}

uses Androidapi.JNI.Os, Androidapi.JNI.Provider, Androidapi.helpers;
{$ENDIF}

function getDeviceName: string;
{$IF Defined(IOS)}
var
  hote: UIDevice;
{$ELSEIF Defined(MACOS)}
var
  hote: NSHost;
{$ENDIF}
begin
{$IF Defined(MSWINDOWS)}
  Result := GetEnvironmentVariable('COMPUTERNAME');
  if ('' = Result) then
    Result := GetEnvironmentVariable('HOSTNAME');
{$ELSEIF Defined(IOS)}
  hote := TUIDevice.Create;
  Result := NSStrToStr(hote.name);
{$ELSEIF Defined(MACOS)}
  hote := tnshost.Create;
  Result := NSStrToStr(hote.localizedName);
{$ELSEIF Defined(ANDROID)}
  Result := JStringToString(TJSettings_Secure.JavaClass.getString
    (TAndroidHelper.ContentResolver, StringToJString('bluetooth_name')));
  if ('' = Result) then
    Result := JStringToString(TJSettings_Global.JavaClass.getString
      (TAndroidHelper.ContentResolver, StringToJString('device_name')));
  if ('' = Result) then
    Result := JStringToString(tjbuild.JavaClass.model);
  if ('' = Result) then
    Result := JStringToString(tjbuild.JavaClass.DISPLAY);
{$ELSE}
  Result := GetEnvironmentVariable('HOSTNAME');
{$ENDIF}
end;

end.
