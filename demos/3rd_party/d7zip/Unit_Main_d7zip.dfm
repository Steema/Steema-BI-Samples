object Form35: TForm35
  Left = 0
  Top = 0
  Caption = 'TeeBI use 7z zip compression with Data items'
  ClientHeight = 470
  ClientWidth = 414
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 8
    Width = 314
    Height = 14
    Caption = 'Make sure the 7z.dll (32 or 64bit) can be located !!'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button1: TButton
    Left = 24
    Top = 40
    Width = 153
    Height = 25
    Caption = 'Data to Stream'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 24
    Top = 96
    Width = 353
    Height = 353
    TabOrder = 1
  end
  object TestStream: TButton
    Left = 216
    Top = 40
    Width = 161
    Height = 25
    Caption = 'Stream'
    TabOrder = 2
    OnClick = TestStreamClick
  end
end
