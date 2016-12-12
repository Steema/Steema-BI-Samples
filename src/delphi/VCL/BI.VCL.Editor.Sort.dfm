object SortEditor: TSortEditor
  Left = 0
  Top = 0
  Caption = 'Sort Editor'
  ClientHeight = 385
  ClientWidth = 363
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
  object Splitter1: TSplitter
    Left = 0
    Top = 184
    Width = 363
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 35
    ExplicitWidth = 173
  end
  object Panel1: TPanel
    Left = 0
    Top = 131
    Width = 363
    Height = 53
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
      Left = 241
      Top = 6
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
    object BAdd: TButton
      Left = 160
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Add'
      Enabled = False
      TabOrder = 3
      OnClick = BAddClick
    end
  end
  object LBAvailable: TListBox
    Left = 0
    Top = 0
    Width = 363
    Height = 131
    Align = alTop
    ItemHeight = 13
    TabOrder = 1
    OnClick = LBAvailableClick
    ExplicitLeft = 8
    ExplicitTop = -63
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 344
    Width = 363
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    ExplicitLeft = -8
    ExplicitTop = 364
    object PanelOk: TPanel
      Left = 174
      Top = 0
      Width = 189
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object BOk: TButton
        Left = 14
        Top = 8
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        Enabled = False
        ModalResult = 1
        TabOrder = 0
      end
      object BCancel: TButton
        Left = 102
        Top = 8
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
end
