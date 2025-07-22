object Form35: TForm35
  Left = 0
  Top = 0
  Caption = 'Form35'
  ClientHeight = 553
  ClientWidth = 655
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 24
    Top = 40
    Width = 129
    Height = 25
    Caption = 'Data to Stream'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 24
    Top = 144
    Width = 321
    Height = 273
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Stream: TButton
    Left = 192
    Top = 40
    Width = 89
    Height = 25
    Caption = 'Stream'
    TabOrder = 2
    OnClick = StreamClick
  end
end
