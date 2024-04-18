unit Olf.RTL.GenRandomID;

interface

type
  TOlfRandomIDGenerator = class
  public
    /// <summary>
    /// Generate a string composed by a random list of chars (0..9 -> numbers, 0..35 -> numbers + lowercased letters ('a'-'z'), 0 ..61 -> numbers and letters ('a'-'z' and 'A'-'Z')
    /// </summary>
    class function getID(Const Base: Byte; Const ASize: cardinal = 0): string;
    /// <summary>
    /// Generate a string with random numbers between '0' and '1'
    /// </summary>
    class function getIDBase2(Const Size: cardinal = 0): string;
    /// <summary>
    /// Generate a string with random numbers between '0' and '9'
    /// </summary>
    class function getIDBase10(Const Size: cardinal = 0): string;
    /// <summary>
    /// Generate a string with random numbers between '0' and '9' or letters between 'a' and 'z'
    /// </summary>
    class function getIDBase36(Const Size: cardinal = 0): string;
    /// <summary>
    /// Generate a string with random numbers between '0' and '9' or letters between 'a' and 'z' or 'A' and 'Z'
    /// </summary>
    class function getIDBase62(Const Size: cardinal = 0): string;
    /// <summary>
    /// Set the default size used by other methods of this class when no size is given when generating an ID.
    /// </summary>
    class procedure SetDefaultSize(Const Size: cardinal);
  end;

implementation

uses
  System.SysUtils;

var
  DefaultSize: cardinal;

  { TOlfRandomIDGenerator }

class function TOlfRandomIDGenerator.getID(const Base: Byte;
  const ASize: cardinal): string;
var
  i, n: Byte;
  LSize: cardinal;
begin
  result := '';

  if ASize > 0 then
    LSize := ASize
  else
    LSize := DefaultSize;

  for i := 1 to LSize do
  begin
    n := random(Base);
    case n of
      0 .. 9:
        result := result + chr(ord('0') + n);
      10 .. 35:
        result := result + chr(ord('a') + n - 10);
      36 .. 61:
        result := result + chr(ord('A') + n - 36);
    end;
  end;
end;

class function TOlfRandomIDGenerator.getIDBase10(const Size: cardinal): string;
begin
  result := getID(10, Size);
end;

class function TOlfRandomIDGenerator.getIDBase2(const Size: cardinal): string;
begin
  result := getID(2, Size);
end;

class function TOlfRandomIDGenerator.getIDBase36(const Size: cardinal): string;
begin
  result := getID(36, Size);
end;

class function TOlfRandomIDGenerator.getIDBase62(const Size: cardinal): string;
begin
  result := getID(62, Size);
end;

class procedure TOlfRandomIDGenerator.SetDefaultSize(const Size: cardinal);
begin
  if (Size < 1) then
    raise exception.create('Size must be greater then 0.');

  DefaultSize := Size;
end;

initialization

randomize;
DefaultSize := 10;

end.
