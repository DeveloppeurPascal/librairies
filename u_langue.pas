unit u_langue;

// (c) Patrick Prémartin / Olf Software 08/2016

interface

function GetOSLangID: String;

implementation

uses FMX.Platform
{$IFDEF MACOS}
,iOSAPI.Foundation, MacAPI.ObjectiveC
{$ENDIF}
;

// récupération du code langue de l'appareil
// https://forums.embarcadero.com/thread.jspa?threadID=108333
// ou http://www.synaptica.info/en/2015/12/21/delphi-10seattle-get-current-device-language/
// ou  http://codeverge.com/embarcadero.delphi.firemonkey/detect-current-language-on-andr/2001235#sthash.zjLIi2KY.dpuf

function GetOSLangID: String;
{$IFDEF MACOS}
var
  Languages: NSArray;
begin
  Languages := TNSLocale.OCClass.preferredLanguages;
  Result := TNSString.Wrap(Languages.objectAtIndex(0)).UTF8String;
{$ENDIF}
{$IFDEF ANDROID}
  var
    LocServ: IFMXLocaleService;
  begin
    if TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService,
      IInterface(LocServ)) then
      Result := LocServ.GetCurrentLangID;
{$ENDIF}
{$IFDEF MSWINDOWS}
    var
      buffer: MarshaledString;
      UserLCID: LCID;
      BufLen: Integer;
    begin
      // defaults
      UserLCID := GetUserDefaultLCID;
      BufLen := GetLocaleInfo(UserLCID, LOCALE_SISO639LANGNAME, nil, 0);
      buffer := StrAlloc(BufLen);
      if GetLocaleInfo(UserLCID, LOCALE_SISO639LANGNAME, buffer, BufLen) <> 0
      then
        Result := buffer
      else
        Result := 'en';
      StrDispose(buffer);
{$ENDIF}
    end;

end.
