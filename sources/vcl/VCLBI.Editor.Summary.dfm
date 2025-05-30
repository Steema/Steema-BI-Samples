object SummaryEditor: TSummaryEditor
  Left = 0
  Top = 0
  Caption = 'SummaryEditor'
  ClientHeight = 409
  ClientWidth = 309
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 309
    Height = 368
    ActivePage = TabMeasures
    Align = alClient
    TabOrder = 0
    object TabMeasures: TTabSheet
      Caption = 'Measures'
      object PanelGroups: TPanel
        Left = 0
        Top = 0
        Width = 301
        Height = 340
        Align = alClient
        TabOrder = 0
        object LMeasures: TCheckListBox
          Left = 1
          Top = 42
          Width = 299
          Height = 96
          Align = alClient
          DragMode = dmAutomatic
          ItemHeight = 13
          TabOrder = 2
          OnClick = LMeasuresClick
          OnClickCheck = LMeasuresClickCheck
          OnDragDrop = LMeasuresDragDrop
          OnDragOver = LMeasuresDragOver
        end
        object Panel1: TPanel
          Left = 1
          Top = 1
          Width = 299
          Height = 41
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object BUp: TSpeedButton
            Left = 218
            Top = 8
            Width = 23
            Height = 22
            Caption = '^'
            Enabled = False
            OnClick = BUpClick
          end
          object BDown: TSpeedButton
            Left = 247
            Top = 8
            Width = 23
            Height = 22
            Caption = 'v'
            Enabled = False
            OnClick = BDownClick
          end
          object Button1: TButton
            Left = 12
            Top = 8
            Width = 75
            Height = 25
            Caption = '&Add...'
            TabOrder = 0
            OnClick = Button1Click
          end
          object BRemoveMeasure: TButton
            Left = 104
            Top = 8
            Width = 75
            Height = 25
            Caption = '&Remove'
            Enabled = False
            TabOrder = 1
            OnClick = BRemoveMeasureClick
          end
        end
        object PageMeasures: TPageControl
          Left = 1
          Top = 138
          Width = 299
          Height = 201
          ActivePage = TabMeasure
          Align = alBottom
          TabOrder = 0
          OnResize = PageMeasuresResize
          object TabMeasure: TTabSheet
            Caption = 'Aggregate'
            object CBAggregate: TComboBox
              Left = 8
              Top = 9
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
            object Label11: TLabel
              Left = 9
              Top = 123
              Width = 31
              Height = 13
              Caption = '&Name:'
            end
            object EMeasureExpression: TEdit
              Left = 9
              Top = 22
              Width = 240
              Height = 21
              TabOrder = 0
              OnChange = EMeasureExpressionChange
            end
            object EMeasureName: TEdit
              Left = 9
              Top = 141
              Width = 240
              Height = 21
              TabOrder = 1
              OnChange = EMeasureNameChange
            end
          end
        end
      end
    end
    object TabDimensions: TTabSheet
      Caption = 'Groups'
      ImageIndex = 2
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 301
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object UpDim: TSpeedButton
          Left = 218
          Top = 8
          Width = 23
          Height = 22
          Caption = '^'
          Enabled = False
          OnClick = UpDimClick
        end
        object DownDim: TSpeedButton
          Left = 249
          Top = 8
          Width = 23
          Height = 22
          Caption = 'v'
          Enabled = False
          OnClick = DownDimClick
        end
        object Button3: TButton
          Left = 12
          Top = 8
          Width = 75
          Height = 25
          Caption = '&Add...'
          TabOrder = 0
          OnClick = Button3Click
        end
        object BRemoveDim: TButton
          Left = 104
          Top = 8
          Width = 75
          Height = 25
          Caption = '&Remove'
          Enabled = False
          TabOrder = 1
          OnClick = BRemoveDimClick
        end
      end
      object LDimensions: TCheckListBox
        Left = 0
        Top = 41
        Width = 301
        Height = 99
        Align = alClient
        DragMode = dmAutomatic
        ItemHeight = 13
        TabOrder = 1
        OnClick = LDimensionsClick
        OnClickCheck = LDimensionsClickCheck
        OnDragDrop = LDimensionsDragDrop
        OnDragOver = LMeasuresDragOver
      end
      object PageGroups: TPageControl
        Left = 0
        Top = 140
        Width = 301
        Height = 200
        ActivePage = TabGroup
        Align = alBottom
        TabOrder = 2
        OnResize = PageGroupsResize
        object TabGroup: TTabSheet
          Caption = 'Layout'
          object RGLayout: TRadioGroup
            Left = 8
            Top = 11
            Width = 105
            Height = 73
            ItemIndex = 0
            Items.Strings = (
              'Automatic'
              'By rows'
              'By columns')
            TabOrder = 0
            OnClick = RGLayoutClick
          end
        end
        object TabHistogram: TTabSheet
          Caption = 'Histogram'
          ImageIndex = 1
          object Label1: TLabel
            Left = 8
            Top = 76
            Width = 23
            Height = 13
            Caption = '&Bins:'
          end
          object Label2: TLabel
            Left = 8
            Top = 29
            Width = 44
            Height = 13
            Caption = 'M&inimum:'
            FocusControl = EHistoMin
          end
          object Label8: TLabel
            Left = 144
            Top = 29
            Width = 48
            Height = 13
            Caption = 'M&aximum:'
            FocusControl = EHistoMax
          end
          object Label9: TLabel
            Left = 8
            Top = 131
            Width = 38
            Height = 13
            Caption = '&Format:'
            FocusControl = EHistFormat
          end
          object CBHistogram: TCheckBox
            Left = 8
            Top = 6
            Width = 113
            Height = 17
            Caption = '&Enabled'
            TabOrder = 0
            OnClick = CBHistogramClick
          end
          object EBins: TEdit
            Left = 8
            Top = 95
            Width = 57
            Height = 21
            TabOrder = 1
            Text = '0'
            OnChange = EBinsChange
          end
          object UDBins: TUpDown
            Left = 65
            Top = 95
            Width = 16
            Height = 21
            Associate = EBins
            Max = 10000
            TabOrder = 2
          end
          object CBMinAuto: TCheckBox
            Left = 89
            Top = 47
            Width = 49
            Height = 17
            Caption = 'Auto'
            TabOrder = 3
            OnClick = CBMinAutoClick
          end
          object CBMaxAuto: TCheckBox
            Left = 225
            Top = 47
            Width = 49
            Height = 17
            Caption = 'Auto'
            TabOrder = 4
            OnClick = CBMaxAutoClick
          end
          object EHistoMin: TEdit
            Left = 8
            Top = 45
            Width = 75
            Height = 21
            TabOrder = 5
            OnChange = EHistoMinChange
          end
          object EHistoMax: TEdit
            Left = 144
            Top = 45
            Width = 75
            Height = 21
            TabOrder = 6
            OnChange = EHistoMaxChange
          end
          object CBAutoBins: TCheckBox
            Left = 89
            Top = 97
            Width = 49
            Height = 17
            Caption = 'Auto'
            Checked = True
            State = cbChecked
            TabOrder = 7
            OnClick = CBAutoBinsClick
          end
          object EHistFormat: TEdit
            Left = 52
            Top = 128
            Width = 86
            Height = 21
            TabOrder = 8
            OnChange = EHistFormatChange
          end
        end
        object TabGroupData: TTabSheet
          Caption = 'Data'
          ImageIndex = 2
          object Label3: TLabel
            Left = 8
            Top = 12
            Width = 56
            Height = 13
            Caption = '&Expression:'
          end
          object LGroupError: TLabel
            Left = 8
            Top = 57
            Width = 3
            Height = 13
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clMaroon
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object Label10: TLabel
            Left = 8
            Top = 82
            Width = 73
            Height = 13
            Caption = '&Date time part:'
            FocusControl = CBDatePart
          end
          object Label12: TLabel
            Left = 8
            Top = 127
            Width = 31
            Height = 13
            Caption = '&Name:'
          end
          object EGroupExpression: TEdit
            Left = 8
            Top = 30
            Width = 240
            Height = 21
            TabOrder = 0
            OnChange = EGroupExpressionChange
          end
          object CBDatePart: TComboBox
            Left = 8
            Top = 101
            Width = 145
            Height = 21
            Style = csDropDownList
            TabOrder = 1
            OnChange = CBDatePartChange
          end
          object EGroupName: TEdit
            Left = 8
            Top = 143
            Width = 240
            Height = 21
            TabOrder = 2
            OnChange = EGroupNameChange
          end
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Options'
      ImageIndex = 1
      object GroupBox1: TGroupBox
        Left = 19
        Top = 17
        Width = 178
        Height = 72
        Caption = 'Remove &Missing:'
        TabOrder = 0
        object CBRemoveMissing: TCheckBox
          Left = 8
          Top = 18
          Width = 167
          Height = 17
          Caption = '&Rows'
          TabOrder = 0
          OnClick = CBRemoveMissingClick
        end
        object CBRemoveCols: TCheckBox
          Left = 8
          Top = 41
          Width = 161
          Height = 17
          Caption = '&Columns'
          TabOrder = 1
          OnClick = CBRemoveColsClick
        end
      end
    end
    object TabFilter: TTabSheet
      Caption = 'Filter'
      ImageIndex = 3
      object LFilter: TLabel
        Left = 12
        Top = 64
        Width = 3
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object LHaving: TLabel
        Left = 12
        Top = 152
        Width = 3
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label6: TLabel
        Left = 12
        Top = 5
        Width = 28
        Height = 13
        Caption = '&Filter:'
        FocusControl = EFilter
      end
      object Label7: TLabel
        Left = 12
        Top = 93
        Width = 37
        Height = 13
        Caption = '&Having:'
        FocusControl = EHaving
      end
      object EFilter: TEdit
        Left = 12
        Top = 24
        Width = 253
        Height = 21
        TabOrder = 0
        OnChange = EFilterChange
      end
      object EHaving: TEdit
        Left = 12
        Top = 112
        Width = 253
        Height = 21
        TabOrder = 1
        OnChange = EHavingChange
      end
    end
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 368
    Width = 309
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    object Panel9: TPanel
      Left = 124
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
      end
      object Button2: TButton
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
  end
end
