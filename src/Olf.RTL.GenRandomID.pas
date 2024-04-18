unit Olf.RTL.GenRandomID;

interface

type
  TOlfRandomIDGenerator = class
  public
    /// <summary>
    /// Generate a string composed by a random list of chars (0..9 -> numbers, 0..35 -> numbers + lowercased letters ('a'-'z'), 0 ..61 -> numbers and letters ('a'-'z' and 'A'-'Z')
    /// </summary>
    class function getID(Const Base: Byte; Const Size: cardinal): string;
    /// <summary>
    /// Generate a string with random numbers between '0' and '1'
    /// </summary>
    class function getIDBase2(Const Size: cardinal = 10): string;
    /// <summary>
    /// Generate a string with random numbers between '0' and '9'
    /// </summary>
    class function getIDBase10(Const Size: cardinal = 10): string;
    /// <summary>
    /// Generate a string with random numbers between '0' and '9' or letters between 'a' and 'z'
    /// </summary>
    class function getIDBase36(Const Size: cardinal = 10): string;
    /// <summary>
    /// Generate a string with random numbers between '0' and '9' or letters between 'a' and 'z' or 'A' and 'Z'
    /// </summary>
    class function getIDBase62(Const Size: cardinal = 10): string;
  end;

implementation

{ TOlfRandomIDGenerator }

class function TOlfRandomIDGenerator.getID(const Base: Byte;
  const Size: cardinal): string;
var
  i, n: Byte;
begin
  result := '';
  for i := 1 to Size do
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

initialization

randomize;

end.
