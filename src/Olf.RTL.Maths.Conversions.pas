/// <summary>
/// ***************************************************************************
///
/// Librairies pour Delphi
///
/// Copyright 1990-2024 Patrick Pr�martin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// This repository contains functions, procedures and classes to use in
/// Delphi projects (console, VCL, FireMonkey and others). It's my "everything reuseable things" toolbox.
///
/// The units to be used in your projects can be found in the "src" folder.
/// Some features are explained on my blog or have been coded live on Twitch.
///
/// Examples of use in the form of VCL or FireMonkey projects are available in
/// the "samples" subfolder.
///
/// ***************************************************************************
///
/// Author(s) :
///      Patrick PREMARTIN
///
/// Site :
///      https://developpeur-pascal.fr/librairies-publiques.html
///
/// Project site :
///      https://github.com/DeveloppeurPascal/librairies
///
/// ***************************************************************************
/// File last update : 28/05/2024 12:19:15
/// Signature : 5c127cac3e8a5155fdc04d356421a963a0e94ffd
/// ***************************************************************************
/// </summary>

unit Olf.RTL.Maths.Conversions;

interface

type
  TOlfNumberConversion = class
  private
  protected
  public
    class function FromDecimalToBaseXX(Const ANumber: uint64;
      Const ABase: byte): string;
    class function ToDecimalFromBaseXX(Const ANumber: string;
      Const ABase: byte): uint64;
    class function DecimalToBinary(Const ANumber: uint64): string;
    class function BinaryToDecimal(Const ANumber: string): uint64;
    class function DecimalToOctal(Const ANumber: uint64): string;
    class function OctalToDecimal(Const ANumber: string): uint64;
    class function DecimalToHexadecimal(Const ANumber: uint64;
      Const WithLeftZero: boolean = false): string;
    class function HexadecimalToDecimal(Const ANumber: string): uint64;
    class function DecimalToBase36(Const ANumber: uint64): string;
    class function Base36ToDecimal(Const ANumber: string): uint64;
    class function DecimalToBase62(Const ANumber: uint64): string;
    class function Base62ToDecimal(Const ANumber: string): uint64;
    class function DecimalToDecimal(Const ANumber: uint64): string; overload;
    class function DecimalToDecimal(Const ANumber: string): uint64; overload;
  end;

implementation

uses
  System.SysUtils;

class function TOlfNumberConversion.FromDecimalToBaseXX(Const ANumber: uint64;
  Const ABase: byte): string;
var
  nb, reste: uint64;
begin
  if (ABase < 2) or (ABase > 62) then
    raise EConvertError.create('Base ' + ABase.ToString + ' noit implemented.');

  result := '';
  nb := ANumber;
  if nb = 0 then
    result := '0'
  else
    while (nb > 0) do
    begin
      reste := nb mod ABase;
      case reste of
        0 .. 9: // base 2 � base 10
          result := reste.ToString + result;
        1 + 9 .. 26 + 9: // base 11 � base 36
          result := chr(reste - 1 - 9 + ord('A')) + result;
        26 + 9 + 1 .. 26 + 9 + 26: // base 37 � base 62
          result := chr(reste - 26 - 9 - 1 + ord('a')) + result;
      end;
      nb := nb div ABase;
    end;
end;

class function TOlfNumberConversion.Base36ToDecimal(Const ANumber
  : string): uint64;
begin
  result := ToDecimalFromBaseXX(ANumber, 36);
end;

class function TOlfNumberConversion.Base62ToDecimal(Const ANumber
  : string): uint64;
begin
  result := ToDecimalFromBaseXX(ANumber, 62);
end;

class function TOlfNumberConversion.BinaryToDecimal(Const ANumber
  : string): uint64;
begin
  result := ToDecimalFromBaseXX(ANumber, 2);
end;

class function TOlfNumberConversion.DecimalToBase36(Const ANumber
  : uint64): string;
begin
  result := FromDecimalToBaseXX(ANumber, 36);
end;

class function TOlfNumberConversion.DecimalToBase62(Const ANumber
  : uint64): string;
begin
  result := FromDecimalToBaseXX(ANumber, 62);
end;

class function TOlfNumberConversion.DecimalToBinary(Const ANumber
  : uint64): string;
begin
  result := FromDecimalToBaseXX(ANumber, 2);
end;

class function TOlfNumberConversion.DecimalToDecimal(const ANumber
  : string): uint64;
begin
  result := ToDecimalFromBaseXX(ANumber, 10);
end;

class function TOlfNumberConversion.DecimalToDecimal(const ANumber
  : uint64): string;
begin
  result := FromDecimalToBaseXX(ANumber, 10);
end;

class function TOlfNumberConversion.DecimalToHexadecimal(Const ANumber: uint64;
  Const WithLeftZero: boolean): string;
begin
  result := FromDecimalToBaseXX(ANumber, 16);
  if WithLeftZero and (result.Length mod 2 = 1) then
    result := '0' + result;
end;

class function TOlfNumberConversion.DecimalToOctal(Const ANumber
  : uint64): string;
begin
  result := FromDecimalToBaseXX(ANumber, 8);
end;

class function TOlfNumberConversion.HexadecimalToDecimal(Const ANumber
  : string): uint64;
begin
  result := ToDecimalFromBaseXX(ANumber, 16);
end;

class function TOlfNumberConversion.OctalToDecimal(Const ANumber
  : string): uint64;
begin
  result := ToDecimalFromBaseXX(ANumber, 8);
end;

class function TOlfNumberConversion.ToDecimalFromBaseXX(const ANumber: string;
  const ABase: byte): uint64;
var
  c: char;
  c_value: byte;
  i: integer;
begin
  if (ABase < 2) or (ABase > 62) then
    raise EConvertError.create('Base ' + ABase.ToString + ' noit implemented.');

  result := 0;
  for i := 0 to ANumber.Length - 1 do
  begin
    c := ANumber.Chars[i];
    case c of
      '0' .. '9':
        c_value := ord(c) - ord('0');
      'A' .. 'Z':
        c_value := 10 + ord(c) - ord('A');
      'a' .. 'z':
        c_value := 26 + 10 + ord(c) - ord('a');
    else
      raise EConvertError.create('Character "' + c +
        '" not authorized in a number.');
    end;
    if c_value >= ABase then
      raise EConvertError.create('Character "' + c +
        '" should not appear in base ' + ABase.ToString + '.');
    result := result * ABase + c_value;
  end;
end;

end.
