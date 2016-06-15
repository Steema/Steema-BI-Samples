object SingleRecordMenu: TSingleRecordMenu
  Left = 0
  Top = 0
  Caption = 'TeeBI - Single Record Example'
  ClientHeight = 186
  ClientWidth = 365
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 56
    Top = 56
    Width = 257
    Height = 25
    Caption = 'Manual usage example'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 56
    Top = 104
    Width = 257
    Height = 25
    Caption = 'Automatic usage as "provider"'
    TabOrder = 1
    OnClick = Button2Click
  end
end
