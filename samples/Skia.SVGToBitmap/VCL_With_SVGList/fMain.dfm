object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 15
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 624
    Height = 441
    Align = alClient
    ExplicitLeft = 392
    ExplicitTop = 256
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 320
    Top = 232
  end
end
