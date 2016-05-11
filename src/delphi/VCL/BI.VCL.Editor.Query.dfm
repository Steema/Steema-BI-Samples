object BIQueryEditor: TBIQueryEditor
  Left = 0
  Top = 0
  Caption = 'Query Editor'
  ClientHeight = 668
  ClientWidth = 904
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter2: TSplitter
    Left = 257
    Top = 0
    Height = 627
    ExplicitLeft = 448
    ExplicitTop = 224
    ExplicitHeight = 100
  end
  object PanelSelector: TPanel
    Left = 0
    Top = 0
    Width = 257
    Height = 627
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
  end
  object PanelEdit: TPanel
    Left = 260
    Top = 0
    Width = 644
    Height = 627
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 0
      Top = 334
      Width = 644
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 0
      ExplicitWidth = 312
    end
    object BIGrid1: TBIGrid
      Left = 0
      Top = 337
      Width = 644
      Height = 290
      Align = alBottom
      UseDockManager = False
      ParentBackground = False
      ParentColor = False
      TabOrder = 0
    end
    object PanelRows: TPanel
      Left = 0
      Top = 75
      Width = 185
      Height = 218
      Align = alLeft
      Color = clWhite
      ParentBackground = False
      TabOrder = 1
      object Panel1: TPanel
        Left = 1
        Top = 1
        Width = 183
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label2: TLabel
          Left = 8
          Top = 14
          Width = 26
          Height = 13
          Caption = 'Rows'
        end
        object SBRowUp: TSpeedButton
          Left = 119
          Top = 9
          Width = 23
          Height = 22
          Caption = '^'
          Enabled = False
          OnClick = SBRowUpClick
        end
        object SBRowDown: TSpeedButton
          Left = 147
          Top = 9
          Width = 23
          Height = 22
          Caption = 'v'
          Enabled = False
          OnClick = SBRowDownClick
        end
        object BDeleteRow: TButton
          Left = 51
          Top = 7
          Width = 59
          Height = 25
          Caption = 'Delete'
          Enabled = False
          TabOrder = 0
          OnClick = BDeleteRowClick
        end
      end
      object ListRows: TCheckListBox
        Left = 1
        Top = 42
        Width = 183
        Height = 87
        OnClickCheck = ListRowsClickCheck
        Align = alClient
        ItemHeight = 13
        TabOrder = 1
        OnClick = ListRowsClick
        OnDragDrop = ListRowsDragDrop
        OnDragOver = ListRowsDragOver
      end
      object Panel4: TPanel
        Left = 1
        Top = 129
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
          Left = 50
          Top = 54
          Width = 83
          Height = 21
          TabOrder = 2
          OnChange = EMaxChange
        end
      end
    end
    object PanelColumns: TPanel
      Left = 0
      Top = 0
      Width = 644
      Height = 75
      Align = alTop
      Color = clWhite
      ParentBackground = False
      TabOrder = 2
      object Panel2: TPanel
        Left = 1
        Top = 1
        Width = 183
        Height = 73
        Align = alLeft
        Alignment = taRightJustify
        BevelOuter = bvNone
        Caption = 'Columns:   '
        TabOrder = 0
      end
      object ListColumns: TCheckListBox
        Left = 184
        Top = 1
        Width = 326
        Height = 73
        OnClickCheck = ListRowsClickCheck
        Align = alClient
        ItemHeight = 13
        TabOrder = 1
        OnClick = ListColumnsClick
      end
      object Panel7: TPanel
        Left = 510
        Top = 1
        Width = 133
        Height = 73
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 2
        object SBColUp: TSpeedButton
          Left = 78
          Top = 10
          Width = 23
          Height = 22
          Caption = '^'
          Enabled = False
          OnClick = SBColUpClick
        end
        object SBColDown: TSpeedButton
          Left = 106
          Top = 10
          Width = 23
          Height = 22
          Caption = 'v'
          Enabled = False
          OnClick = SBColDownClick
        end
        object BDeleteColumn: TButton
          Left = 7
          Top = 9
          Width = 63
          Height = 25
          Caption = 'Delete'
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
      Left = 185
      Top = 75
      Width = 459
      Height = 218
      Align = alClient
      TabOrder = 3
      object Panel3: TPanel
        Left = 1
        Top = 1
        Width = 457
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 0
        object Label1: TLabel
          Left = 10
          Top = 14
          Width = 46
          Height = 13
          Caption = 'Measures'
        end
        object SBMeasureUp: TSpeedButton
          Left = 163
          Top = 9
          Width = 23
          Height = 22
          Caption = '^'
          Enabled = False
          OnClick = SBMeasureUpClick
        end
        object SBMeasureDown: TSpeedButton
          Left = 191
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
          Width = 75
          Height = 25
          Caption = 'Delete'
          Enabled = False
          TabOrder = 0
          OnClick = BDeleteMeasureClick
        end
      end
      object ListMeasures: TCheckListBox
        Left = 1
        Top = 42
        Width = 183
        Height = 175
        OnClickCheck = ListRowsClickCheck
        Align = alClient
        ItemHeight = 13
        TabOrder = 1
        OnClick = ListMeasuresClick
        OnDragOver = ListMeasuresDragOver
      end
      object Panel5: TPanel
        Left = 184
        Top = 42
        Width = 274
        Height = 175
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 2
        object PageOptions: TPageControl
          Left = 0
          Top = 0
          Width = 274
          Height = 175
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
              Height = 81
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
              Height = 147
              ActivePage = TabMeasureInfo
              Align = alClient
              TabOrder = 0
              object TabMeasure: TTabSheet
                Caption = 'Aggregate'
                ExplicitLeft = 0
                ExplicitTop = 0
                ExplicitWidth = 0
                ExplicitHeight = 0
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
                    'Maximum')
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
                ExplicitLeft = 0
                ExplicitTop = 0
                ExplicitWidth = 0
                ExplicitHeight = 0
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
                  Left = 5
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
              object TabMeasureInfo: TTabSheet
                Caption = 'Data'
                ImageIndex = 2
                object Label4: TLabel
                  Left = 9
                  Top = 82
                  Width = 24
                  Height = 13
                  Caption = 'Kind:'
                end
                object LabelMeasureKind: TLabel
                  Left = 9
                  Top = 100
                  Width = 3
                  Height = 13
                end
                object Label5: TLabel
                  Left = 9
                  Top = 4
                  Width = 56
                  Height = 13
                  Caption = '&Expression:'
                end
                object LMeasureError: TLabel
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
                object EMeasureExpression: TEdit
                  Left = 9
                  Top = 22
                  Width = 240
                  Height = 21
                  TabOrder = 0
                  OnChange = EMeasureExpressionChange
                end
              end
            end
          end
        end
      end
    end
    object PanelFilter: TPanel
      Left = 0
      Top = 293
      Width = 644
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 4
      object LFilterError: TLabel
        Left = 354
        Top = 16
        Width = 3
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object EFilter: TEdit
        Left = 71
        Top = 9
        Width = 271
        Height = 21
        TabOrder = 0
        OnChange = EFilterChange
      end
      object CBFilter: TCheckBox
        Left = 10
        Top = 11
        Width = 55
        Height = 17
        Caption = 'Filter:'
        TabOrder = 1
        OnClick = CBFilterClick
      end
    end
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 627
    Width = 904
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Panel9: TPanel
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
        OnClick = Button1Click
      end
    end
  end
end
