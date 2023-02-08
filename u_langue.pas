unit u_langue;

// (c) Patrick Prémartin / Olf Software 08/2016
//
// This file is distributed under AGPL license.
//
// Get the latest version at
// https://github.com/DeveloppeurPascal/librairies

interface

uses Olf.RTL.Language;

function GetOSLangID: String;

implementation

{$MESSAGE WARN 'u_langue unit is deprecated. Use Olf.RTL.Language unit instead.'}

function GetOSLangID: String;
begin
  result := GetCurrentLanguageISOCode;
end;

end.
