object BIGridEditor: TBIGridEditor
  Left = 0
  Top = 0
  Caption = 'Grid Editor'
  ClientHeight = 394
  ClientWidth = 283
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 283
    Height = 394
    ActivePage = TabMenu
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Columns'
      object Tree: TTreeView
        Left = 16
        Top = 16
        Width = 209
        Height = 197
        HideSelection = False
        HotTrack = True
        Indent = 19
        TabOrder = 0
        OnChange = TreeChange
      end
      object GBColumn: TGroupBox
        Left = 16
        Top = 232
        Width = 209
        Height = 121
        Caption = 'Column:'
        TabOrder = 1
        Visible = False
        object Label1: TLabel
          Left = 58
          Top = 86
          Width = 27
          Height = 13
          Alignment = taRightJustify
          Caption = '&Align:'
          FocusControl = CBColAlign
        end
        object BColColor: TButton
          Left = 122
          Top = 17
          Width = 75
          Height = 25
          Caption = '&Color...'
          TabOrder = 0
          OnClick = BColColorClick
        end
        object CBColVisible: TCheckBox
          Left = 10
          Top = 21
          Width = 106
          Height = 17
          Caption = '&Visible'
          TabOrder = 1
          OnClick = CBColVisibleClick
        end
        object Button1: TButton
          Left = 122
          Top = 48
          Width = 75
          Height = 25
          Caption = '&Font...'
          TabOrder = 2
          OnClick = Button1Click
        end
        object CBColAlign: TComboBox
          Left = 98
          Top = 83
          Width = 97
          Height = 21
          Style = csDropDownList
          TabOrder = 3
          OnChange = CBColAlignChange
          Items.Strings = (
            'Left'
            'Right'
            'Center')
        end
        object CBColExpanded: TCheckBox
          Left = 10
          Top = 44
          Width = 105
          Height = 17
          Caption = '&Expanded'
          TabOrder = 4
          OnClick = CBColExpandedClick
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Options'
      ImageIndex = 1
      object CBOptions: TCheckListBox
        Left = 6
        Top = 7
        Width = 185
        Height = 193
        OnClickCheck = CBOptionsClickCheck
        ItemHeight = 13
        Items.Strings = (
          'Always show editor'
          'Always show selection'
          'Cancel On Exit'
          'Column Lines'
          'Column Resize'
          'Confirm Delete'
          'Editing'
          'Indicator'
          'Row Lines'
          'Row Select'
          'Tabs'
          'Title Click'
          'Title HotTrack'
          'Titles')
        TabOrder = 0
      end
      object GroupBox2: TGroupBox
        Left = 6
        Top = 210
        Width = 185
        Height = 127
        Caption = '&Defaults:'
        TabOrder = 1
        object Label3: TLabel
          Left = 9
          Top = 18
          Width = 70
          Height = 13
          Caption = 'Column Width:'
          FocusControl = EColWidth
        end
        object Label4: TLabel
          Left = 9
          Top = 64
          Width = 59
          Height = 13
          Caption = 'Row Height:'
          FocusControl = ERowHeight
        end
        object Label5: TLabel
          Left = 97
          Top = 18
          Width = 54
          Height = 13
          Caption = 'Line Width:'
          FocusControl = ELineWidth
        end
        object EColWidth: TEdit
          Left = 9
          Top = 37
          Width = 44
          Height = 21
          TabOrder = 0
          Text = '0'
          OnChange = EColWidthChange
        end
        object ERowHeight: TEdit
          Left = 9
          Top = 83
          Width = 44
          Height = 21
          TabOrder = 1
          Text = '0'
          OnChange = ERowHeightChange
        end
        object UDColWidth: TUpDown
          Left = 53
          Top = 37
          Width = 16
          Height = 21
          Associate = EColWidth
          Max = 10000
          TabOrder = 2
        end
        object UDRowHeight: TUpDown
          Left = 53
          Top = 83
          Width = 16
          Height = 21
          Associate = ERowHeight
          Max = 10000
          TabOrder = 3
        end
        object ELineWidth: TEdit
          Left = 97
          Top = 37
          Width = 44
          Height = 21
          TabOrder = 4
          Text = '0'
          OnChange = ELineWidthChange
        end
        object UDLineWidth: TUpDown
          Left = 141
          Top = 37
          Width = 16
          Height = 21
          Associate = ELineWidth
          TabOrder = 5
        end
      end
      object CBReadOnly: TCheckBox
        Left = 15
        Top = 343
        Width = 176
        Height = 17
        Caption = 'Read only'
        TabOrder = 2
        OnClick = CBReadOnlyClick
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Style'
      ImageIndex = 2
      object CBStyle: TComboBox
        Left = 8
        Top = 11
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemIndex = 1
        TabOrder = 0
        Text = 'Themed'
        OnChange = CBStyleChange
        Items.Strings = (
          'Classic'
          'Themed'
          'Gradient')
      end
      object CBBorder: TCheckBox
        Left = 8
        Top = 53
        Width = 97
        Height = 17
        Caption = '&Border'
        TabOrder = 1
        OnClick = CBBorderClick
      end
      object BBackColor: TButton
        Left = 8
        Top = 88
        Width = 113
        Height = 25
        Caption = 'Back Color...'
        TabOrder = 2
        OnClick = BBackColorClick
      end
      object BTitleFont: TButton
        Left = 8
        Top = 128
        Width = 113
        Height = 25
        Caption = 'Title &Font...'
        TabOrder = 3
        OnClick = BTitleFontClick
      end
      object BCellsFont: TButton
        Left = 8
        Top = 168
        Width = 113
        Height = 25
        Caption = 'Cells &Font...'
        TabOrder = 4
        OnClick = BCellsFontClick
      end
    end
    object TabMenu: TTabSheet
      Caption = 'Menu'
      ImageIndex = 3
      object CBMenu: TCheckBox
        Left = 16
        Top = 16
        Width = 153
        Height = 17
        Caption = '&Visible'
        TabOrder = 0
        OnClick = CBMenuClick
      end
      object GroupBox1: TGroupBox
        Left = 16
        Top = 56
        Width = 145
        Height = 89
        Caption = '&Alternate Rows:'
        TabOrder = 1
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
        Top = 160
        Width = 161
        Height = 17
        Caption = '&Row Numbers'
        TabOrder = 2
        OnClick = CBRowNumbersClick
      end
      object CBSort: TCheckBox
        Left = 16
        Top = 183
        Width = 137
        Height = 17
        Caption = 'Column Sorting'
        TabOrder = 3
        OnClick = CBSortClick
      end
      object CBFilter: TCheckBox
        Left = 16
        Top = 206
        Width = 137
        Height = 17
        Caption = 'Filter'
        TabOrder = 4
        OnClick = CBFilterClick
      end
      object CBSearch: TCheckBox
        Left = 16
        Top = 229
        Width = 137
        Height = 17
        Caption = '&Search'
        TabOrder = 5
        OnClick = CBSearchClick
      end
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [fdEffects, fdApplyButton]
    OnApply = FontDialog1Apply
    Left = 96
    Top = 56
  end
  object FontDialog2: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [fdEffects, fdApplyButton]
    OnApply = FontDialog2Apply
    Left = 96
    Top = 120
  end
  object FontDialog3: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [fdEffects, fdApplyButton]
    OnApply = FontDialog3Apply
    Left = 88
    Top = 248
  end
end
