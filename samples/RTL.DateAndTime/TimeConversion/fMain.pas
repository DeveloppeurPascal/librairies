(* C2PP
  ***************************************************************************

  My libraries for Delphi
  Copyright (c) 1990-2026 Patrick PREMARTIN

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

  ***************************************************************************

  This repository contains functions, procedures and classes to use in
  Delphi projects (console, VCL, FireMonkey and others). It's my "everything reuseable things" toolbox.

  The units to be used in your projects can be found in the "src" folder.
  Some features are explained on my blog or have been coded live on Twitch.

  Examples of use in the form of VCL or FireMonkey projects are available in
  the "samples" subfolder.

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://librairies.developpeur-pascal.fr

  Project site :
  https://github.com/DeveloppeurPascal/librairies

  ***************************************************************************
  File last update : 2026-03-30T18:45:06.000+02:00
  Signature : e3ecea41a7b555ec94ee3f466732d1f06dafa253
  ***************************************************************************
*)

unit fMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Edit;

type
  TfrmMain = class(TForm)
    edtHH: TEdit;
    edtMM: TEdit;
    edtSS: TEdit;
    edtSeconds: TEdit;
    btnFromHHMMSS: TButton;
    btnFromSeconds: TButton;
    mmoResult: TMemo;
    procedure btnFromHHMMSSClick(Sender: TObject);
    procedure btnFromSecondsClick(Sender: TObject);
  private
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  Olf.RTL.DateAndTime;

procedure TfrmMain.btnFromHHMMSSClick(Sender: TObject);
var
  sHH, sMM, sSS: string;
  HH, MM, SS, Seconds: Int64;
begin
  sHH := edtHH.Text;
  sMM := edtMM.Text;
  sSS := edtSS.Text;

  if not sHH.IsEmpty then
  begin
    if sMM.IsEmpty then
      sMM := '0';
    if sSS.IsEmpty then
      sSS := '0';
    Seconds := HMSToSec(sHH + 'H ' + sMM + 'M ' + sSS + 'S');
  end
  else if not sMM.IsEmpty then
  begin
    if sSS.IsEmpty then
      sSS := '0';
    Seconds := HMSToSec(sMM + 'M ' + sSS + 'S');
    sHH := '0';
  end
  else if not sSS.IsEmpty then
  begin
    Seconds := HMSToSec(sSS + 'S');
    sHH := '0';
    sMM := '0';
  end
  else
    raise exception.Create('Please fill HH:MM:SS fields.');

  if Seconds <> HMSToSec(sHH.ToInt64, sMM.ToInt64, sSS.ToInt64) then
    raise exception.Create('Can''t convert HH:MM:SS to seconds.');

  mmoResult.Lines.Add('HH : ' + sHH);
  mmoResult.Lines.Add('MM : ' + sMM);
  mmoResult.Lines.Add('SS : ' + sSS);
  mmoResult.Lines.Add('HMSToSec : ' + Seconds.ToString);
  mmoResult.Lines.Add('SecToHMS : ' + SecToHMS(Seconds));
  mmoResult.Lines.Add('SecToTime : ' + SecToTime(Seconds));
  SecToHMS(Seconds, HH, MM, SS);
  mmoResult.Lines.Add('HH : ' + HH.ToString);
  mmoResult.Lines.Add('MM : ' + MM.ToString);
  mmoResult.Lines.Add('SS : ' + SS.ToString);
  mmoResult.Lines.Add('----------');
  mmoResult.Lines.Add('');
end;

procedure TfrmMain.btnFromSecondsClick(Sender: TObject);
var
  HH, MM, SS, Seconds: Int64;
begin
  Seconds := edtSeconds.Text.ToInt64;
  SecToHMS(Seconds, HH, MM, SS);

  if Seconds <> HMSToSec(HH, MM, SS) then
    raise exception.Create('Can''t convert seconds to HH:MM:SS.');

  mmoResult.Lines.Add('Seconds : ' + Seconds.ToString);
  mmoResult.Lines.Add('HH : ' + HH.ToString);
  mmoResult.Lines.Add('MM : ' + MM.ToString);
  mmoResult.Lines.Add('SS : ' + SS.ToString);
  mmoResult.Lines.Add('SecToHMS : ' + SecToHMS(Seconds));
  mmoResult.Lines.Add('SecToTime : ' + SecToTime(Seconds));
  mmoResult.Lines.Add('----------');
  mmoResult.Lines.Add('');
end;

end.
