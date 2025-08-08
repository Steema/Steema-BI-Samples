object BIQueryEditor: TBIQueryEditor
  Left = 0
  Top = 0
  Caption = 'Query Editor'
  ClientHeight = 660
  ClientWidth = 904
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ShowHint = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 13
  object SplitterSelector: TSplitter
    Left = 257
    Top = 0
    Height = 619
    ExplicitLeft = 259
    ExplicitTop = -6
    ExplicitHeight = 627
  end
  object PanelSelector: TPanel
    Left = 0
    Top = 0
    Width = 257
    Height = 619
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object PageData: TPageControl
      Left = 0
      Top = 0
      Width = 257
      Height = 619
      ActivePage = TabData
      Align = alClient
      TabOrder = 0
      OnChange = PageDataChange
      object TabData: TTabSheet
        Caption = 'Data'
      end
      object TabFilter: TTabSheet
        Caption = 'Filter'
        ImageIndex = 1
      end
      object TabSort: TTabSheet
        Caption = 'Sort'
        ImageIndex = 2
      end
    end
  end
  object OuterPanel: TPanel
    Left = 260
    Top = 0
    Width = 644
    Height = 619
    Align = alClient
    TabOrder = 1
    object SplitterPreview: TSplitter
      Left = 1
      Top = 313
      Width = 642
      Height = 3
      Cursor = crVSplit
      Align = alTop
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 312
    end
    object PanelEdit: TPanel
      Left = 1
      Top = 1
      Width = 642
      Height = 312
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object SplitterRows: TSplitter
        Left = 185
        Top = 75
        Height = 237
        ExplicitLeft = 320
        ExplicitTop = 108
        ExplicitHeight = 100
      end
      object PanelRows: TPanel
        Left = 0
        Top = 75
        Width = 185
        Height = 237
        Align = alLeft
        Color = clWhite
        ParentBackground = False
        TabOrder = 0
        ExplicitTop = 72
        object Panel1: TPanel
          Left = 1
          Top = 1
          Width = 183
          Height = 41
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object Label2: TLabel
            Left = 4
            Top = 5
            Width = 34
            Height = 14
            Caption = 'Rows'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object BDeleteRow: TButton
            Left = 51
            Top = 7
            Width = 25
            Height = 25
            Caption = '-'
            Enabled = False
            TabOrder = 0
            OnClick = BDeleteRowClick
          end
          object Panel6: TPanel
            Left = 123
            Top = 0
            Width = 60
            Height = 41
            Align = alRight
            BevelOuter = bvNone
            TabOrder = 1
            object SBRowUp: TSpeedButton
              Left = 3
              Top = 9
              Width = 23
              Height = 22
              Caption = '^'
              Enabled = False
              OnClick = SBRowUpClick
            end
            object SBRowDown: TSpeedButton
              Left = 31
              Top = 9
              Width = 23
              Height = 22
              Caption = 'v'
              Enabled = False
              OnClick = SBRowDownClick
            end
          end
        end
        object ListRows: TCheckListBox
          Left = 1
          Top = 42
          Width = 183
          Height = 106
          Align = alClient
          ItemHeight = 17
          TabOrder = 1
          OnClick = ListRowsClick
          OnClickCheck = ListRowsClickCheck
          OnDragDrop = ListRowsDragDrop
          OnDragOver = ListRowsDragOver
        end
        object Panel4: TPanel
          Left = 1
          Top = 148
          Width = 183
          Height = 88
          Align = alBottom
          TabOrder = 2
          object LMax: TLabel
            Left = 10
            Top = 57
            Width = 24
            Height = 13
            Caption = '&Max:'
          end
          object LStart: TLabel
            Left = 99
            Top = 56
            Width = 28
            Height = 13
            Caption = '&Start:'
          end
          object CBDistinct: TCheckBox
            Left = 10
            Top = 8
            Width = 77
            Height = 17
            Caption = 'Distinct'
            Enabled = False
            TabOrder = 0
            OnClick = CBDistinctClick
          end
          object CBRemoveRows: TCheckBox
            Left = 10
            Top = 30
            Width = 137
            Height = 17
            Caption = 'Remove Missing'
            Enabled = False
            TabOrder = 1
            OnClick = CBRemoveRowsClick
          end
          object EMax: TEdit
            Left = 38
            Top = 54
            Width = 51
            Height = 21
            TabOrder = 2
            OnChange = EMaxChange
          end
          object EStart: TEdit
            Left = 131
            Top = 53
            Width = 44
            Height = 21
            TabOrder = 3
            OnChange = EStartChange
          end
        end
      end
      object PanelColumns: TPanel
        Left = 0
        Top = 0
        Width = 642
        Height = 75
        Align = alTop
        Color = clWhite
        ParentBackground = False
        TabOrder = 1
        object Panel2: TPanel
          Left = 1
          Top = 1
          Width = 183
          Height = 73
          Align = alLeft
          Alignment = taRightJustify
          BevelOuter = bvNone
          TabOrder = 0
          object SBSwap: TSpeedButton
            Left = 51
            Top = 22
            Width = 23
            Height = 22
            Hint = 'Swap rows and columns'
            Glyph.Data = {
              36050000424D3605000000000000360400002800000010000000100000000100
              08000000000000010000C40E0000C40E00000001000000000000000000000000
              80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
              A6000020400000206000002080000020A0000020C0000020E000004000000040
              20000040400000406000004080000040A0000040C0000040E000006000000060
              20000060400000606000006080000060A0000060C0000060E000008000000080
              20000080400000806000008080000080A0000080C0000080E00000A0000000A0
              200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
              200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
              200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
              20004000400040006000400080004000A0004000C0004000E000402000004020
              20004020400040206000402080004020A0004020C0004020E000404000004040
              20004040400040406000404080004040A0004040C0004040E000406000004060
              20004060400040606000406080004060A0004060C0004060E000408000004080
              20004080400040806000408080004080A0004080C0004080E00040A0000040A0
              200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
              200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
              200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
              20008000400080006000800080008000A0008000C0008000E000802000008020
              20008020400080206000802080008020A0008020C0008020E000804000008040
              20008040400080406000804080008040A0008040C0008040E000806000008060
              20008060400080606000806080008060A0008060C0008060E000808000008080
              20008080400080806000808080008080A0008080C0008080E00080A0000080A0
              200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
              200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
              200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
              2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
              2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
              2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
              2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
              2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
              2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
              2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFF00000000000000FFFFFF00FFFFFFFFFF00FFFFFFFF
              FF00FFFF0000FFFFFFFFFF00FFFFFFFFFF00FF000000000000FFFF00FFFFFFFF
              FF00FFFF0000FFFF00FFFF00000000000000FFFFFF00FFFF00FFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FF
              FFFF00000000000000FFFF00FFFF0000FFFF00FFFFFFFFFF00FFFF0000000000
              00FF00FFFFFFFFFF00FFFFFFFFFF0000FFFF00FFFFFFFFFF00FFFFFFFFFF00FF
              FFFF00000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
            OnClick = SBSwapClick
          end
          object SBSelector: TSpeedButton
            Left = 5
            Top = 2
            Width = 23
            Height = 22
            Caption = '<<'
            Flat = True
            OnClick = SBSelectorClick
          end
          object Panel8: TPanel
            Left = 116
            Top = 0
            Width = 67
            Height = 73
            Align = alRight
            BevelOuter = bvNone
            TabOrder = 0
            object SBColUp: TSpeedButton
              Left = 8
              Top = 6
              Width = 23
              Height = 22
              Caption = '^'
              Enabled = False
              OnClick = SBColUpClick
            end
            object SBColDown: TSpeedButton
              Left = 37
              Top = 6
              Width = 23
              Height = 22
              Caption = 'v'
              Enabled = False
              OnClick = SBColDownClick
            end
            object Label8: TLabel
              Left = 9
              Top = 34
              Width = 52
              Height = 14
              Caption = 'Columns'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentFont = False
            end
          end
        end
        object ListColumns: TCheckListBox
          Left = 184
          Top = 1
          Width = 339
          Height = 73
          Align = alClient
          ItemHeight = 17
          TabOrder = 1
          OnClick = ListColumnsClick
          OnClickCheck = ListRowsClickCheck
        end
        object Panel7: TPanel
          Left = 523
          Top = 1
          Width = 118
          Height = 73
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 2
          object BDeleteColumn: TButton
            Left = 7
            Top = 9
            Width = 25
            Height = 25
            Caption = '-'
            Enabled = False
            TabOrder = 0
            OnClick = BDeleteColumnClick
          end
          object CBRemoveCols: TCheckBox
            Left = 9
            Top = 43
            Width = 119
            Height = 17
            Caption = 'Remove Missing'
            Enabled = False
            TabOrder = 1
            OnClick = CBRemoveColsClick
          end
        end
      end
      object PanelMeasures: TPanel
        Left = 188
        Top = 75
        Width = 454
        Height = 237
        Align = alClient
        TabOrder = 2
        ExplicitLeft = 185
        ExplicitWidth = 457
        object Splitter1: TSplitter
          Left = 176
          Top = 42
          Height = 194
          Align = alRight
          ExplicitLeft = 228
          ExplicitTop = 68
          ExplicitHeight = 100
        end
        object Panel3: TPanel
          Left = 1
          Top = 1
          Width = 452
          Height = 41
          Align = alTop
          BevelOuter = bvNone
          Color = clWhite
          ParentBackground = False
          TabOrder = 0
          ExplicitWidth = 455
          object Label1: TLabel
            Left = 5
            Top = 5
            Width = 57
            Height = 14
            Caption = 'Measures'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object SBMeasureUp: TSpeedButton
            Left = 125
            Top = 9
            Width = 23
            Height = 22
            Caption = '^'
            Enabled = False
            OnClick = SBMeasureUpClick
          end
          object SBMeasureDown: TSpeedButton
            Left = 153
            Top = 9
            Width = 23
            Height = 22
            Caption = 'v'
            Enabled = False
            OnClick = SBMeasureDownClick
          end
          object BDeleteMeasure: TButton
            Left = 75
            Top = 7
            Width = 25
            Height = 25
            Caption = '-'
            Enabled = False
            TabOrder = 0
            OnClick = BDeleteMeasureClick
          end
        end
        object ListMeasures: TCheckListBox
          Left = 1
          Top = 42
          Width = 175
          Height = 194
          Align = alClient
          ItemHeight = 17
          TabOrder = 1
          OnClick = ListMeasuresClick
          OnClickCheck = ListRowsClickCheck
          OnDragOver = ListMeasuresDragOver
          ExplicitWidth = 181
        end
        object Panel5: TPanel
          Left = 179
          Top = 42
          Width = 274
          Height = 194
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 2
          ExplicitLeft = 182
          object PageOptions: TPageControl
            Left = 0
            Top = 0
            Width = 274
            Height = 194
            ActivePage = TabMeasureOptions
            Align = alClient
            TabOrder = 0
            Visible = False
            object TabItem: TTabSheet
              Caption = 'Options'
              object Label3: TLabel
                Left = 9
                Top = 4
                Width = 52
                Height = 13
                Caption = 'Date Time:'
                FocusControl = CBDatePart
              end
              object CBDatePart: TComboBox
                Left = 9
                Top = 23
                Width = 145
                Height = 21
                Style = csDropDownList
                TabOrder = 0
                OnChange = CBDatePartChange
              end
              object GBHistogram: TGroupBox
                Left = 9
                Top = 51
                Width = 145
                Height = 77
                Caption = 'Histogram:'
                TabOrder = 1
                object Label6: TLabel
                  Left = 11
                  Top = 46
                  Width = 23
                  Height = 13
                  Caption = 'Bins:'
                end
                object CBHistoActive: TCheckBox
                  Left = 9
                  Top = 20
                  Width = 120
                  Height = 17
                  Caption = 'Active'
                  TabOrder = 0
                  OnClick = CBHistoActiveClick
                end
                object EBins: TEdit
                  Left = 44
                  Top = 43
                  Width = 49
                  Height = 21
                  TabOrder = 1
                  Text = '0'
                  OnChange = EBinsChange
                end
                object UDBins: TUpDown
                  Left = 93
                  Top = 43
                  Width = 16
                  Height = 21
                  Associate = EBins
                  Max = 32767
                  TabOrder = 2
                end
              end
            end
            object TabMeasureOptions: TTabSheet
              Caption = 'Options'
              ImageIndex = 1
              object PageMeasures: TPageControl
                Left = 0
                Top = 0
                Width = 266
                Height = 166
                ActivePage = TabMeasure
                Align = alClient
                TabOrder = 0
                object TabMeasure: TTabSheet
                  Caption = 'Aggregate'
                  object CBAggregate: TComboBox
                    Left = 8
                    Top = 14
                    Width = 145
                    Height = 21
                    Style = csDropDownList
                    ItemIndex = 0
                    TabOrder = 0
                    Text = 'Count'
                    OnChange = CBAggregateChange
                    Items.Strings = (
                      'Count'
                      'Sum'
                      'Average'
                      'Minimum'
                      'Maximum'
                      'First'
                      'Last')
                  end
                  object CBMissingAsZero: TCheckBox
                    Left = 8
                    Top = 46
                    Width = 136
                    Height = 17
                    Caption = 'Missing as &Zero'
                    TabOrder = 1
                    OnClick = CBMissingAsZeroClick
                  end
                end
                object TabCalc: TTabSheet
                  Caption = 'Calculation'
                  ImageIndex = 3
                  object RGRunning: TRadioGroup
                    Left = 135
                    Top = 3
                    Width = 113
                    Height = 78
                    Caption = '&Running:'
                    ItemIndex = 0
                    Items.Strings = (
                      'None'
                      'Cumulative'
                      'Difference')
                    TabOrder = 0
                    OnClick = RGRunningClick
                  end
                  object CBRunningRows: TCheckBox
                    Left = 140
                    Top = 86
                    Width = 108
                    Height = 17
                    Caption = 'By R&ows'
                    TabOrder = 1
                    OnClick = CBRunningRowsClick
                  end
                  object RGPercentage: TRadioGroup
                    Left = 4
                    Top = 3
                    Width = 113
                    Height = 100
                    Caption = '&Percentage:'
                    ItemIndex = 0
                    Items.Strings = (
                      'No'
                      'Column'
                      'Row'
                      'Total')
                    TabOrder = 2
                    OnClick = RGPercentageClick
                  end
                end
              end
            end
            object TabItemData: TTabSheet
              Caption = 'Data'
              ImageIndex = 2
              object SpeedButton1: TSpeedButton
                Left = 231
                Top = 22
                Width = 23
                Height = 22
                Caption = '...'
                OnClick = SpeedButton1Click
              end
              object LExpressionError: TLabel
                Left = 9
                Top = 49
                Width = 3
                Height = 13
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clMaroon
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentFont = False
              end
              object Label5: TLabel
                Left = 9
                Top = 4
                Width = 56
                Height = 13
                Caption = '&Expression:'
              end
              object Label4: TLabel
                Left = 9
                Top = 82
                Width = 24
                Height = 13
                Caption = 'Kind:'
              end
              object LabelItemKind: TLabel
                Left = 9
                Top = 100
                Width = 3
                Height = 13
              end
              object EItemExpression: TEdit
                Left = 9
                Top = 23
                Width = 216
                Height = 21
                TabOrder = 0
                OnChange = EItemExpressionChange
              end
            end
          end
        end
      end
    end
    object PagePreview: TPageControl
      Left = 1
      Top = 316
      Width = 642
      Height = 302
      ActivePage = TabGrid
      Align = alClient
      TabOrder = 0
      OnChange = PagePreviewChange
      object TabGrid: TTabSheet
        Caption = 'Grid'
        object BIGrid1: TBIGrid
          Left = 0
          Top = 0
          Width = 634
          Height = 274
          Align = alClient
          UseDockManager = False
          ParentBackground = False
          ParentColor = False
          TabOrder = 0
        end
      end
      object TabChart: TTabSheet
        Caption = 'Chart'
        ImageIndex = 1
      end
      object TabSQL: TTabSheet
        Caption = 'SQL'
        ImageIndex = 2
        object MemoSQL: TMemo
          Left = 0
          Top = 0
          Width = 634
          Height = 274
          Align = alClient
          TabOrder = 0
        end
      end
    end
  end
  object PanelOptions: TPanel
    Left = 0
    Top = 619
    Width = 904
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Label7: TLabel
      Left = 16
      Top = 14
      Width = 24
      Height = 13
      Caption = 'Title:'
    end
    object PanelButtons: TPanel
      Left = 719
      Top = 0
      Width = 185
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object BOK: TButton
        Left = 9
        Top = 6
        Width = 75
        Height = 25
        Caption = 'OK'
        Enabled = False
        ModalResult = 1
        TabOrder = 0
        OnClick = BOKClick
      end
      object Button1: TButton
        Left = 99
        Top = 6
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        Default = True
        ModalResult = 2
        TabOrder = 1
      end
    end
    object ETitle: TEdit
      Left = 53
      Top = 11
      Width = 308
      Height = 21
      TabOrder = 1
      OnChange = ETitleChange
    end
    object CBPreview: TCheckBox
      Left = 384
      Top = 14
      Width = 119
      Height = 17
      Caption = 'Preview'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CBPreviewClick
    end
  end
  object BIQuery1: TBIQuery
    Left = 416
    Top = 424
  end
end
