unit f_operation_en_cours;

interface

Procedure oec_Ouverture (nb_operation: cardinal);
Procedure oec_Operation_Suivante;
Procedure oec_Fermeture;

implementation

{$R *.DFM}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls;

type
  Tfrm = class(TForm)
    ProgressBar1: TProgressBar;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  frm: Tfrm;

Procedure oec_Ouverture (nb_operation: cardinal);
begin
  if (frm = nil) then frm := Tfrm.Create (Nil);
  try
    frm.ProgressBar1.Min := 0;
    frm.ProgressBar1.Max := nb_operation;
    frm.ProgressBar1.Position := 0;
    frm.Show;
  except
    try
      frm.Free;
    finally
      frm := nil;
    end;
  end;
end;

Procedure oec_Operation_Suivante;
begin
  if (frm = nil) then oec_Ouverture (0);
  try
    frm.ProgressBar1.StepIt;
  except
    try
      frm.Free;
    finally
      frm := nil;
    end;
  end;
end;

Procedure oec_Fermeture;
begin
  if (frm = nil) then oec_Ouverture (0);
  try
    frm.Hide;
  except
    try
      frm.Free;
    finally
      frm := nil;
    end;
  end;
end;

initialization
  frm := nil;

finalization
  if (frm <> nil)
  then
    begin
      frm.Free;
      frm := nil;
    end;
  {endif}
end.
