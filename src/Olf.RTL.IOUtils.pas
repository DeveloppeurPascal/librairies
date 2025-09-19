unit Olf.RTL.IOUtils;

interface

type
  TOlfPath = class
    /// <summary>
    ///   Return relative path of ForPath depending on CurrentPath
    /// </summary>
    class function GetRelativePath(const ForPath, CurrentPath: string): string;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils;

class function TOlfPath.GetRelativePath(const ForPath, CurrentPath: string):
string;
var
  i, j: integer;
begin
  i := 0;
  j := 0;
  while (i < length(ForPath)) and (i < length(CurrentPath)) and
  (ForPath.Chars[i] = CurrentPath.Chars[i]) do
  begin
    if ForPath.Chars[i] = tpath.DirectorySeparatorChar then
      j := i;
    inc(i);
  end;
  if (i < length(ForPath)) or ((i < length(CurrentPath)) and
    (CurrentPath.Chars[i] <> tpath.DirectorySeparatorChar)) then
  begin
    result := ForPath.Substring(j + 1);
    i := j + 1;
  end
  else
    result := '';
  while (i < length(CurrentPath)) do
  begin
    if CurrentPath.Chars[i] = tpath.DirectorySeparatorChar then
      if result.IsEmpty then
        result := '.' + tpath.DirectorySeparatorChar
      else if result = '.' + tpath.DirectorySeparatorChar then
        result := '.' + result
      else
        result := '..' + tpath.DirectorySeparatorChar + result;
    inc(i);
  end;
end;

end.

