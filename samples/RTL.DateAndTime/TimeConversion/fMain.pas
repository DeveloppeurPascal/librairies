/// <summary>
/// ***************************************************************************
///
/// Librairies pour Delphi
///
/// Copyright 1990-2024 Patrick Prémartin under AGPL 3.0 license.
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
/// Patrick PREMARTIN
///
/// Site :
/// https://librairies.developpeur-pascal.fr
///
/// Project site :
/// https://github.com/DeveloppeurPascal/librairies
///
/// ***************************************************************************
/// File last update : 2024-12-24T17:08:46.000+01:00
/// Signature : e3b3da9c62f17ab2e062d29aee1f59184228f80b
/// ***************************************************************************
/// </summary>

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
  HH, MM, SS, Seconds: integer;
  s: string;
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
  HH, MM, SS, Seconds: integer;
  s: string;
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
