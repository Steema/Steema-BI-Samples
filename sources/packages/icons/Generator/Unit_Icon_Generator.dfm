object FormIconGenerator: TFormIconGenerator
  Left = 0
  Top = 0
  Caption = 'TeeBI Icon Generator'
  ClientHeight = 678
  ClientWidth = 940
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ImageOriginal: TImage
    Left = 258
    Top = 75
    Width = 647
    Height = 574
  end
  object Label1: TLabel
    Left = 258
    Top = 56
    Width = 40
    Height = 13
    Caption = 'Original:'
  end
  object Image16x16: TImage
    Left = 66
    Top = 497
    Width = 16
    Height = 16
  end
  object Label2: TLabel
    Left = 66
    Top = 478
    Width = 34
    Height = 13
    Caption = '16x16:'
  end
  object Label3: TLabel
    Left = 66
    Top = 526
    Width = 34
    Height = 13
    Caption = '32x32:'
  end
  object Image32x32: TImage
    Left = 66
    Top = 545
    Width = 32
    Height = 32
  end
  object Image24x24: TImage
    Left = 130
    Top = 497
    Width = 24
    Height = 24
  end
  object Label4: TLabel
    Left = 130
    Top = 478
    Width = 34
    Height = 13
    Caption = '24x24:'
  end
  object LBIcons: TListBox
    Left = 24
    Top = 56
    Width = 201
    Height = 393
    ItemHeight = 13
    TabOrder = 0
    OnClick = LBIconsClick
  end
  object EPath: TEdit
    Left = 24
    Top = 8
    Width = 201
    Height = 21
    TabOrder = 1
    Text = '.\..\..\..'
  end
  object BLoad: TButton
    Left = 240
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 2
    OnClick = BLoadClick
  end
  object Button1: TButton
    Left = 400
    Top = 8
    Width = 233
    Height = 25
    Caption = 'SAVE ALL BMP ICONS'
    TabOrder = 3
    OnClick = Button1Click
  end
end
