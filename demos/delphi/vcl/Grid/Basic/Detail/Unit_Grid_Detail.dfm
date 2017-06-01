object FormGridDetail: TFormGridDetail
  Left = 0
  Top = 0
  Caption = 'TeeBI - Automatic Detail Grid'
  ClientHeight = 641
  ClientWidth = 1028
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BIGrid1: TBIGrid
    Left = 0
    Top = 62
    Width = 1028
    Height = 579
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    ExplicitLeft = 360
    ExplicitTop = 280
    ExplicitWidth = 320
    ExplicitHeight = 120
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1028
    Height = 62
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 24
      Top = 25
      Width = 84
      Height = 13
      Caption = 'Show detail data:'
    end
    object RGDetail: TRadioGroup
      Left = 128
      Top = 7
      Width = 321
      Height = 43
      Columns = 3
      ItemIndex = 1
      Items.Strings = (
        'None'
        'Address Coord'
        'Grades')
      TabOrder = 0
      OnClick = RGDetailClick
    end
  end
end
