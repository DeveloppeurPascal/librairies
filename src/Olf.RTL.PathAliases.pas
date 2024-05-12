unit Olf.RTL.PathAliases;

interface

uses
  System.Generics.Collections;

type
  TKeyValueList = TDictionary<string, string>;
  TGetPathForAliasFunc = reference to function(Const AAlias: string): string;
  TGetPathForAliasEvent = function(Const AAlias: string): string of object;

  /// <summary>
  /// Replace aliases in a path by their value and remove "../" parts.
  /// </summary>
  /// <remarks>
  /// Alias are used by Delphi/C++Builder/RADStudio IDE in project options and
  /// editor options. You can find $(BDS), $(PROJECTDIR) and others in your
  /// paths.
  ///
  /// The replacement is done from the Key/Value list, the registry keys
  /// (Delphi/C++Builder/RAD Studio is installed), the environment variables or
  /// asked to the call by AGetAliasProc.
  /// </remarks>
function ReplaceAliasesInPath(Const ASourcePath: string;
  Const AAliasList: TKeyValueList; Const AAllowEmptyAlias: boolean = false;
  Const ABDSVersion: string = '23.0' { RAD Studio 12 Athens };
  Const AGetPathForAliasFunc: TGetPathForAliasFunc = nil): string; overload;

function ReplaceAliasesInPath(Const ASourcePath: string;
  Const AAliasList: TKeyValueList; Const AAllowEmptyAlias: boolean;
  Const ABDSVersion: string; Const AGetPathForAliasEvent: TGetPathForAliasEvent)
  : string; overload;

implementation

uses
{$IFDEF MSWINDOWS}
  Win.Registry,
{$ENDIF}
  System.SysUtils,
  System.IOUtils;

function ReplaceAliasesInPath(Const ASourcePath: string;
  const AAliasList: TKeyValueList; Const AAllowEmptyAlias: boolean;
  Const ABDSVersion: string;
  Const AGetPathForAliasFunc: TGetPathForAliasFunc): string;
var
  AliasPosStart, AliasPosEnd: integer;
  Alias, AliasPath: string;
{$IFDEF MSWINDOWS}
  Reg: TRegistry;
{$ENDIF}
  // IdxDotDot,
  IdxPathSeparator: integer;
begin
  result := ASourcePath;
  AliasPosStart := result.IndexOf('$(');
  while (AliasPosStart >= 0) do
  begin
    AliasPosEnd := result.IndexOf(')', AliasPosStart);
    Alias := result.Substring(AliasPosStart + 2,
      AliasPosEnd - AliasPosStart - 2);
    AliasPath := '';
    if not AAliasList.TryGetValue(Alias, AliasPath) then
    begin
{$IFDEF MSWINDOWS}
      Reg := TRegistry.Create; // HKEY_CURRENT_USER by default
      try
        if (Alias = 'BDS') and Reg.OpenKey('Software\Embarcadero\BDS\' +
          ABDSVersion, false) and Reg.ValueExists('RootDir') then
        begin
          AliasPath := Reg.ReadString('RootDir');
          Reg.CloseKey;
        end
        else if Reg.OpenKey('Software\Embarcadero\BDS\' + ABDSVersion +
          '\Environment Variables', false) and Reg.ValueExists(Alias) then
        begin
          AliasPath := Reg.ReadString(Alias);
          Reg.CloseKey;
        end
        else
{$ENDIF}
          AliasPath := GetEnvironmentVariable(Alias);
        if AliasPath.IsEmpty and assigned(AGetPathForAliasFunc) then
          AliasPath := AGetPathForAliasFunc(Alias);
{$IFDEF MSWINDOWS}
      finally
        Reg.Free;
      end;
{$ENDIF}
    end;
    if (not AAllowEmptyAlias) and AliasPath.IsEmpty then
      raise Exception.Create('Alias $(' + Alias + ') is empty or unknown.');
    result := result.Replace('$(' + Alias + ')', AliasPath);
    AliasPosStart := result.IndexOf('$(');
  end;
  if (not result.IsEmpty) and tpath.IsRelativePath(result) then
    result := tpath.combine(ReplaceAliasesInPath('$(PROJECTDIR)', AAliasList,
      AAllowEmptyAlias, ABDSVersion), result);
  //
  // TODO : ok for "../" but not for "../../"
  // IdxDotDot := result.IndexOf(tpath.DirectorySeparatorChar + '..');
  // while (IdxDotDot >= 0) do
  // begin
  // IdxPathSeparator := result.LastIndexOf(tpath.DirectorySeparatorChar,
  // IdxDotDot - 1);
  // if (IdxPathSeparator >= 0) then
  // begin
  // result := result.Substring(0, IdxPathSeparator) +
  // result.Substring(IdxDotDot + 3);
  // IdxDotDot := result.IndexOf(tpath.DirectorySeparatorChar + '..');
  // end
  // else
  // IdxDotDot := -1;
  // end;
  //
  IdxPathSeparator := result.IndexOf(tpath.DirectorySeparatorChar +
    tpath.DirectorySeparatorChar, 2);
  while (IdxPathSeparator >= 2) do
  begin
    result := result.Substring(0, IdxPathSeparator) +
      result.Substring(IdxPathSeparator + 1);
    IdxPathSeparator := result.IndexOf(tpath.DirectorySeparatorChar +
      tpath.DirectorySeparatorChar, 2);
  end;
end;

function ReplaceAliasesInPath(Const ASourcePath: string;
  Const AAliasList: TKeyValueList; Const AAllowEmptyAlias: boolean;
  Const ABDSVersion: string; Const AGetPathForAliasEvent
  : TGetPathForAliasEvent): string;
begin
  ReplaceAliasesInPath(ASourcePath, AAliasList, AAllowEmptyAlias, ABDSVersion,
    function(Const AAlias: string): string
    begin
      if assigned(AGetPathForAliasEvent) then
        result := AGetPathForAliasEvent(AAlias)
      else
        result := '';
    end);
end;

end.
