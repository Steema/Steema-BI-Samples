object BIGridEditor: TBIGridEditor
  Left = 0
  Top = 0
  Caption = 'BIGrid Editor'
  ClientHeight = 360
  ClientWidth = 405
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 405
    Height = 360
    ActivePage = TabBIGrid
    Align = alClient
    TabOrder = 0
    OnChange = PageControl1Change
    object TabBIGrid: TTabSheet
      Caption = 'Options'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label1: TLabel
        Left = 16
        Top = 200
        Width = 60
        Height = 13
        Caption = 'Show &Items:'
        FocusControl = CBShowItems
      end
      object GroupBox1: TGroupBox
        Left = 16
        Top = 16
        Width = 145
        Height = 89
        Caption = '&Alternate Rows:'
        TabOrder = 0
        object CBAltRows: TCheckBox
          Left = 16
          Top = 24
          Width = 123
          Height = 17
          Caption = '&Enabled'
          TabOrder = 0
          OnClick = CBAltRowsClick
        end
        object BAltColor: TButton
          Left = 16
          Top = 48
          Width = 89
          Height = 25
          Caption = '&Color...'
          TabOrder = 1
          OnClick = BAltColorClick
        end
      end
      object CBRowNumbers: TCheckBox
        Left = 16
        Top = 120
        Width = 161
        Height = 17
        Caption = '&Row Numbers'
        TabOrder = 1
        OnClick = CBRowNumbersClick
      end
      object CBFilter: TCheckBox
        Left = 16
        Top = 143
        Width = 137
        Height = 17
        Caption = 'Filter'
        TabOrder = 2
        OnClick = CBFilterClick
      end
      object CBSearch: TCheckBox
        Left = 16
        Top = 166
        Width = 137
        Height = 17
        Caption = '&Search'
        TabOrder = 3
        OnClick = CBSearchClick
      end
      object CBShowItems: TComboBox
        Left = 16
        Top = 219
        Width = 145
        Height = 21
        Style = csDropDownList
        TabOrder = 4
        OnChange = CBShowItemsChange
        Items.Strings = (
          'Automatic'
          'Yes'
          'No')
      end
      object CBColorize: TCheckBox
        Left = 18
        Top = 262
        Width = 137
        Height = 17
        Caption = 'Colorize'
        TabOrder = 5
        OnClick = CBColorizeClick
      end
    end
    object TabPlugin: TTabSheet
      Caption = 'Plugin'
      ImageIndex = 1
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
end
