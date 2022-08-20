unit u_version;

interface

function GetApplicationVersion: string;
function GetFileVersion(aFileName : String; var Found : Boolean) : String;

implementation

uses
    SysUtils, windows;

function GetApplicationVersion: string;
var
   trouve : boolean;
begin
  result := GetFileVersion (ParamStr (0), trouve);
end;

function GetFileVersion(aFileName : String; var Found : Boolean) : String;
var
  woMajorVersion, woMinorVersion, woSubVersion, woBuildNumber : Word;
  doInfoSize, doVersionSize : LongWord;
{$IFDEF VER100}
  Wnd : Integer;
{$ELSE}
  Wnd : Cardinal;
{$ENDIF}
  ptVersion : Pointer;
  ptFixedFileInfo : PVSFixedFileInfo;
  s : String;
begin
  s := '';
  aFileName := ExtractFileName(aFileName);
  doInfoSize := GetFileVersionInfoSize(PChar(aFileName), Wnd);
  if (doInfoSize <> 0) then begin
    GetMem(ptVersion, doInfoSize);
    try
      if GetFileVersionInfo(PChar(aFileName), Wnd, doInfoSize, ptVersion) then
        if VerQueryValue(ptVersion, '\', Pointer(ptFixedFileInfo), doVersionSize) then begin
          woMajorVersion := (ptFixedFileInfo^.dwFileVersionMS div $FFFF);
          woMinorVersion := (ptFixedFileInfo^.dwFileVersionMS and $0000FFFF);
          woSubVersion := (ptFixedFileInfo^.dwFileVersionLS div $FFFF);
          woBuildNumber := (ptFixedFileInfo^.dwFileVersionLS and $0000FFFF);
          s := Format('(v %d.%d.%d.%d)', [woMajorVersion, woMinorVersion, woSubVersion, woBuildNumber]);
          Found := True;
        end;
      {endif}
    finally
      FreeMem(ptVersion);
    end;
  end else begin
    s := '';
    Found := True;
  end;
  if s = '' then
    s := '(v inconnue)';
  {endif}
  result := s;
end;

end.
 