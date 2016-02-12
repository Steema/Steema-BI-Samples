object FormSearchDemo: TFormSearchDemo
  Left = 0
  Top = 0
  Caption = 'Incremental Search Demo'
  ClientHeight = 704
  ClientWidth = 1081
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1081
    Height = 105
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 14
      Width = 37
      Height = 13
      Caption = '&Search:'
      FocusControl = ESearch
    end
    object Label2: TLabel
      Left = 51
      Top = 78
      Width = 34
      Height = 13
      Caption = 'Found:'
    end
    object LFound: TLabel
      Left = 91
      Top = 78
      Width = 6
      Height = 13
      Caption = '0'
    end
    object SBUp: TSpeedButton
      Left = 187
      Top = 73
      Width = 23
      Height = 22
      Caption = '^'
      Enabled = False
      OnClick = SBUpClick
    end
    object SBDown: TSpeedButton
      Left = 216
      Top = 73
      Width = 23
      Height = 22
      Caption = 'v'
      Enabled = False
      OnClick = SBDownClick
    end
    object ESearch: TEdit
      Left = 51
      Top = 11
      Width = 121
      Height = 21
      TabOrder = 0
      OnChange = ESearchChange
    end
    object CBCaseSensitive: TCheckBox
      Left = 51
      Top = 40
      Width = 97
      Height = 17
      Caption = '&Case Sensitive'
      TabOrder = 1
      OnClick = CBCaseSensitiveClick
    end
    object CBPartial: TComboBox
      Left = 154
      Top = 38
      Width = 81
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 2
      Text = 'Anywhere'
      OnChange = CBPartialChange
      Items.Strings = (
        'Anywhere'
        'At start'
        'At end'
        'Exact')
    end
    object CBAll: TCheckBox
      Left = 360
      Top = 13
      Width = 65
      Height = 17
      Caption = '&All Items'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CBAllClick
    end
    object CBItems: TComboBox
      Left = 200
      Top = 11
      Width = 145
      Height = 21
      Style = csDropDownList
      Enabled = False
      TabOrder = 4
      OnChange = CBItemsChange
    end
    object CBBackground: TCheckBox
      Left = 252
      Top = 40
      Width = 73
      Height = 17
      Caption = 'Background'
      TabOrder = 5
    end
    object RGMode: TRadioGroup
      Left = 431
      Top = 11
      Width = 214
      Height = 32
      Columns = 3
      ItemIndex = 2
      Items.Strings = (
        'Both'
        'Filter'
        'Highlight')
      TabOrder = 6
      OnClick = RGModeClick
    end
    object Button1: TButton
      Left = 864
      Top = 18
      Width = 105
      Height = 25
      Caption = 'Select Data...'
      TabOrder = 7
      OnClick = Button1Click
    end
  end
  object BIGrid1: TBIGrid
    Left = 0
    Top = 105
    Width = 1081
    Height = 558
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    TabOrder = 1
    DataSource = DataSource1
  end
  object Panel2: TPanel
    Left = 0
    Top = 663
    Width = 1081
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object DBNavigator1: TDBNavigator
      Left = 185
      Top = 0
      Width = 896
      Height = 41
      DataSource = DataSource1
      VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast]
      Align = alClient
      TabOrder = 0
    end
    object CurrentRecord: TPanel
      Left = 0
      Top = 0
      Width = 185
      Height = 41
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  object BIDataset1: TBIDataset
    Left = 536
    Top = 360
  end
  object DataSource1: TDataSource
    OnDataChange = DataSource1DataChange
    Left = 616
    Top = 352
  end
end
