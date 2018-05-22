object Form1: TForm1
  Left = 267
  Top = 183
  Width = 1088
  Height = 563
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox1: TPaintBox
    Left = 0
    Top = 0
    Width = 895
    Height = 536
    Align = alClient
    OnPaint = PaintBox1Paint
  end
  object Memo1: TMemo
    Left = 895
    Top = 0
    Width = 185
    Height = 536
    Align = alRight
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
end
