object FormDataRank: TFormDataRank
  Left = 0
  Top = 0
  Caption = 'TeeBI - Data Rank Example'
  ClientHeight = 585
  ClientWidth = 571
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object BIGrid1: TBIGrid
    Left = 0
    Top = 41
    Width = 571
    Height = 544
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    ExplicitWidth = 453
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 571
    Height = 41
    Align = alTop
    TabOrder = 1
    ExplicitWidth = 453
    object Label1: TLabel
      Left = 13
      Top = 14
      Width = 43
      Height = 13
      Caption = 'Rank &by:'
      FocusControl = CBGroup
    end
    object CBHideGroups: TCheckBox
      Left = 316
      Top = 13
      Width = 121
      Height = 17
      Caption = '&Hide duplicate groups'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 0
      OnClick = CBHideGroupsClick
    end
    object CBGroup: TComboBox
      Left = 62
      Top = 11
      Width = 112
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = '(none)'
      OnChange = CBGroupChange
      Items.Strings = (
        '(none)'
        'Year'
        'Person')
    end
    object BAsGrid: TButton
      Left = 443
      Top = 10
      Width = 94
      Height = 25
      Caption = 'Show as &Grid...'
      TabOrder = 2
      OnClick = BAsGridClick
    end
    object CBAscending: TCheckBox
      Left = 188
      Top = 13
      Width = 97
      Height = 17
      Caption = '&Ascending'
      TabOrder = 3
      OnClick = CBAscendingClick
    end
  end
end
