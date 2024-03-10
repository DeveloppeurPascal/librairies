object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 624
    Height = 400
    Align = alClient
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 41
    Align = alTop
    Caption = 'Panel1'
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 1
    object Edit1: TEdit
      Left = 6
      Top = 6
      Width = 537
      Height = 29
      Align = alClient
      TabOrder = 0
      Text = 'Edit1'
      ExplicitHeight = 23
    end
    object Button1: TButton
      Left = 543
      Top = 6
      Width = 75
      Height = 29
      Align = alRight
      Caption = 'Button1'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
end
