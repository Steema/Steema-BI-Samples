object SortEditor: TSortEditor
  Left = 0
  Top = 0
  Caption = 'Sort Editor'
  ClientHeight = 359
  ClientWidth = 363
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 164
    Width = 363
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 35
    ExplicitWidth = 173
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 363
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object CBAscending: TCheckBox
      Left = 16
      Top = 8
      Width = 124
      Height = 17
      Caption = '&Ascending'
      Enabled = False
      TabOrder = 0
      OnClick = CBAscendingClick
    end
    object BDelete: TButton
      Left = 160
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Delete'
      Enabled = False
      TabOrder = 1
      OnClick = BDeleteClick
    end
    object CBIgnoreCase: TCheckBox
      Left = 16
      Top = 29
      Width = 124
      Height = 17
      Caption = '&Ignore text case'
      Enabled = False
      TabOrder = 2
      OnClick = CBIgnoreCaseClick
    end
  end
  object LBAvailable: TListBox
    Left = 0
    Top = 208
    Width = 363
    Height = 151
    Align = alBottom
    ItemHeight = 13
    TabOrder = 1
    OnClick = LBAvailableClick
  end
  object Panel2: TPanel
    Left = 0
    Top = 167
    Width = 363
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object BAdd: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Add'
      Enabled = False
      TabOrder = 0
      OnClick = BAddClick
    end
  end
end
