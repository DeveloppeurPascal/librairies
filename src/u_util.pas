unit u_util;

interface

function sans_decimal_separator (ch: string): string;

function sans_les_blancs (ch : string): string;

function convert_date (date: string): string;

function convert_heure (heure: string): string;

implementation

uses
    SysUtils;

function sans_decimal_separator (ch: string): string;
var
   n : integer;
begin
  if (DecimalSeparator <> '.') then begin
    n := pos (DecimalSeparator, ch);
    while (n > 0) do begin
      delete (ch, n, Length (DecimalSeparator));
      insert ('.', ch, n);
      n := pos (DecimalSeparator, ch);
    end;
  end;
  Result := ch;
end;

function sans_les_blancs (ch : string): string;
var
   ch2 : string;
begin
  ch2 := '';
  while (ch > '') do begin
    if (ch[1] in ['0'..'9', ',', '.']) then
      ch2 := ch2+ch[1];
    {endif}
    delete (ch, 1, 1);
  end;
  Result := ch2;
end;

function convert_date (date: string): string;
begin
  Result := copy (date, 7, 2)+'/'+copy (date, 5, 2)+'/'+copy (date, 1, 4);
end;

function convert_heure (heure: string): string;
begin
  Result := copy (heure, 1, 2)+':'+copy (heure, 3, 2)+':'+copy (heure, 5, 2);
end;

end.
