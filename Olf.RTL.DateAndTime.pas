unit Olf.RTL.DateAndTime;

interface

/// <summary>
/// Ressort la date du jour en AAAAMMJJ
/// </summary>
function DateToString8: string; overload;

/// <summary>
/// Ressort la date passée en AAAAMMJJ
/// </summary>
function DateToString8(Const ADate: TDateTime): string; overload;

/// <summary>
/// Get the TDate value of a YYYYMMDD string
/// </summary>
function Date8ToDate(Const Date8: string): tdate;

/// <summary>
/// Transforme une date AAAAMMJJ dans son format d'affichage JJ/MM/AAAA
/// </summary>
function Date8ToString(Const Date8AAfficher: string): string;

/// <summary>
/// Transforme une date AAAAMMJJ dans son format d'affichage AAAA-MM-JJ
/// </summary>
function Date8ToStringISO(Const Date8AAfficher: string): string;

/// <summary>
/// Transforme une date AAAAMMJJ dans son format d'affichage RFC822
/// </summary>
function Date8ToStringRFC822(Const Date8AAfficher: string): string;

/// <summary>
/// Ressort l'heure en cours en HHMMSS
/// </summary>
function TimeToString6: string; overload;

/// <summary>
/// Ressort l'heure passée en HHMMSS
/// </summary>
function TimeToString6(Const ATime: TDateTime): string; overload;

/// <summary>
/// Transforme une heure HHMMSS dans son format d'affichage HH:MM:SS
/// </summary>
function Time6ToString(Const Time6AAfficher: string): string;

/// <summary>
/// Transforme la date et heure du moment en AAAAMMJJHHMMSS
/// Ce format est utilisé dans le stockage d'infos de création et de modification dans la base de données et permettre des tris chronologiques sur l'ordre alphabétique.
/// </summary>
function DateTimeToString14: string; overload;

/// <summary>
/// Transforme la date et heure passée en AAAAMMJJHHMMSS
/// Ce format est utilisé dans le stockage d'infos de création et de modification dans la base de données et permettre des tris chronologiques sur l'ordre alphabétique.
/// </summary>
function DateTimeToString14(Const ADateTime: TDateTime): string; overload;

/// <summary>
/// Converti une valeur en secondes vers son équivalent en HMS
/// </summary>
function SecToHMS(Const Valeur_En_secondes: Integer): String; overload;
procedure SecToHMS(Const Valeur_En_secondes: Integer;
  var HH, MM, SS: Integer); overload;

/// <summary>
/// Converti une valeur HMS (xxH xxM xxS) en son équivalent en secondes
/// </summary>
function HMSToSec(Const Valeur_En_HMS: String): Integer; overload;
function HMSToSec(Const HH, MM, SS: Integer): Integer; overload;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  System.Character;

function DateToString8: string;
begin
  Result := DateToString8(Now);
end;

function DateToString8(Const ADate: TDateTime): string;
begin
  Result := FormatDateTime('yyyymmdd', ADate);
end;

function Date8ToDate(Const Date8: string): tdate;
begin
  Result := EncodeDate(Date8.Substring(0, 4).tointeger,
    Date8.Substring(4, 2).tointeger, Date8.Substring(6, 2).tointeger);
end;

function Date8ToString(Const Date8AAfficher: string): string;
var
  MM, jj: string;
begin
  // TODO : gérer les formats de date non européens de l'ouest
  MM := Date8AAfficher.Substring(4, 2);
  jj := Date8AAfficher.Substring(6, 2);
  if MM = '00' then
    Result := Date8AAfficher.Substring(0, 4)
  else if jj = '00' then
    Result := MM + FormatSettings.DateSeparator + Date8AAfficher.Substring(0, 4)
  else
    Result := jj + FormatSettings.DateSeparator + MM +
      FormatSettings.DateSeparator + Date8AAfficher.Substring(0, 4);
end;

function Date8ToStringISO(Const Date8AAfficher: string): string;
var
  MM, jj: string;
begin
  // TODO : gérer les formats de date non européens de l'ouest
  MM := Date8AAfficher.Substring(4, 2);
  jj := Date8AAfficher.Substring(6, 2);
  if MM = '00' then
    Result := Date8AAfficher.Substring(0, 4) + '-00-00'
  else if jj = '00' then
    Result := Date8AAfficher.Substring(0, 4) + '-' + MM + '-00'
  else
    Result := Date8AAfficher.Substring(0, 4) + '-' + MM + '-' + jj;
end;

function Date8ToStringRFC822(Const Date8AAfficher: string): string;
var
  x: Integer;
begin
  if Date8AAfficher.IsEmpty then
    raise Exception.Create
      ('Date non renseignée. Impossible à convertir dans Date8ToStringRFC822.');
  x := Date8AAfficher.Substring(6, 2).tointeger;
  if x < 1 then
    x := 1;
  Result := x.ToString + ' ';
  case Date8AAfficher.Substring(4, 2).tointeger of
    0, 1:
      Result := Result + 'Jan';
    2:
      Result := Result + 'Feb';
    3:
      Result := Result + 'Mar';
    4:
      Result := Result + 'Apr';
    5:
      Result := Result + 'May';
    6:
      Result := Result + 'Jun';
    7:
      Result := Result + 'Jul';
    8:
      Result := Result + 'Aug';
    9:
      Result := Result + 'Sep';
    10:
      Result := Result + 'Oct';
    11:
      Result := Result + 'Nov';
    12:
      Result := Result + 'Dec';
  end;
  Result := Result + ' ' + Date8AAfficher.Substring(0, 4) + ' 00:00:00 GMT';
end;

function TimeToString6: string;
begin
  Result := TimeToString6(Now);
end;

function TimeToString6(Const ATime: TDateTime): string;
begin
  Result := FormatDateTime('hhnnss', ATime);
end;

function Time6ToString(Const Time6AAfficher: string): string;
begin
  Result := Time6AAfficher.Substring(0, 2) + FormatSettings.TimeSeparator +
    Time6AAfficher.Substring(2, 2) + FormatSettings.TimeSeparator +
    Time6AAfficher.Substring(4, 2);
end;

function DateTimeToString14: string;
begin
  Result := DateTimeToString14(Now);
end;

function DateTimeToString14(Const ADateTime: TDateTime): string;
begin
  Result := DateToString8(ADateTime) + TimeToString6(ADateTime);
end;

function SecToHMS(Const Valeur_En_secondes: Integer): String;
var
  h, m, s: Integer;
begin
  SecToHMS(Valeur_En_secondes, h, m, s);
  Result := '';
  if (h > 0) then
    Result := Result + h.ToString + 'H ';
  if (m > 0) then
    Result := Result + m.ToString + 'M ';
  if (s > 0) or (Valeur_En_secondes = 0) then
    Result := Result + s.ToString + 'S ';
end;

procedure SecToHMS(Const Valeur_En_secondes: Integer; var HH, MM, SS: Integer);
begin
  SS := Valeur_En_secondes;
  HH := SS div SecsPerHour;
  SS := SS - HH * SecsPerHour;
  MM := SS div SecsPerMin;
  SS := SS - MM * SecsPerMin;
end;

function HMSToSec(Const Valeur_En_HMS: String): Integer;
var
  ch: string;
  i: Integer;
begin
  Result := 0;
  ch := Valeur_En_HMS.Trim.Replace(' ', '').ToUpper;
  i := ch.IndexOf('H');
  if (i > 0) then
  begin
    Result := Result + ch.Substring(0, i).tointeger * SecsPerHour;
    ch := ch.Substring(i + 1);
  end;
  i := ch.IndexOf('M');
  if (i > 0) then
  begin
    Result := Result + ch.Substring(0, i).tointeger * SecsPerMin;
    ch := ch.Substring(i + 1);
  end;
  i := ch.IndexOf('S');
  if (i > 0) then
    Result := Result + ch.Substring(0, i).tointeger;
end;

function HMSToSec(Const HH, MM, SS: Integer): Integer;
begin
  Result := HH * SecsPerHour + MM * SecsPerMin + SS;
end;

end.
