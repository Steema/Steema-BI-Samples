object ChartVisualizerEditor: TChartVisualizerEditor
  Left = 415
  Top = 276
  Caption = 'TeeChart Visualizer Editor'
  ClientHeight = 461
  ClientWidth = 427
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
    Width = 427
    Height = 461
    ActivePage = TabOptions
    Align = alClient
    TabOrder = 0
    OnChange = PageControl1Change
    object TabOptions: TTabSheet
      Caption = 'Options'
      object Label7: TLabel
        Left = 9
        Top = 48
        Width = 39
        Height = 13
        Caption = '&Render:'
        FocusControl = CBRender
      end
      object Label10: TLabel
        Left = 9
        Top = 183
        Width = 28
        Height = 13
        Caption = '&Axes:'
        FocusControl = CBMultiAxis
      end
      object BEditChart: TButton
        Left = 9
        Top = 12
        Width = 75
        Height = 25
        Caption = '&Template...'
        TabOrder = 0
        OnClick = BEditChartClick
      end
      object CBRender: TComboBox
        Left = 9
        Top = 67
        Width = 145
        Height = 21
        Style = csDropDownList
        DropDownCount = 20
        ItemIndex = 0
        TabOrder = 1
        Text = 'Default'
        OnChange = CBRenderChange
        Items.Strings = (
          'Default'
          'Fast 2D'
          'Fast 3D')
      end
      object CBLegend: TCheckBox
        Left = 9
        Top = 104
        Width = 145
        Height = 17
        Caption = '&Legend'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = CBLegendClick
      end
      object CBMarks: TCheckBox
        Left = 9
        Top = 127
        Width = 145
        Height = 17
        Caption = '&Marks'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = CBMarksClick
      end
      object CBChartSettings: TCheckBox
        Left = 9
        Top = 150
        Width = 145
        Height = 17
        Caption = 'Settings Button'
        Checked = True
        State = cbChecked
        TabOrder = 4
        OnClick = CBChartSettingsClick
      end
      object CBMultiAxis: TComboBox
        Left = 9
        Top = 202
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 5
        Text = 'Automatic'
        OnChange = CBMultiAxisChange
        Items.Strings = (
          'Automatic'
          'Single Axis'
          'Two Axes'
          'Multiple Zones')
      end
    end
    object TabSeries: TTabSheet
      Caption = 'Series'
      ImageIndex = 1
      object Label4: TLabel
        Left = 9
        Top = 35
        Width = 30
        Height = 13
        Caption = '&Stack:'
        FocusControl = CBAutoStack
      end
      object Label1: TLabel
        Left = 9
        Top = 94
        Width = 44
        Height = 13
        Caption = '&2D Style:'
        FocusControl = CB2D
      end
      object Label6: TLabel
        Left = 9
        Top = 123
        Width = 44
        Height = 13
        Caption = '&3D Style:'
        FocusControl = CB3D
      end
      object Label8: TLabel
        Left = 9
        Top = 159
        Width = 39
        Height = 13
        Caption = '&Render:'
        FocusControl = CBRenderSeries
      end
      object Label11: TLabel
        Left = 9
        Top = 64
        Width = 28
        Height = 13
        Caption = 'Style:'
        FocusControl = CBStyle
      end
      object CBAddNulls: TCheckBox
        Left = 9
        Top = 8
        Width = 97
        Height = 17
        Caption = '&Add Nulls'
        TabOrder = 0
        OnClick = CBAddNullsClick
      end
      object CBAutoStack: TComboBox
        Left = 73
        Top = 31
        Width = 89
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 1
        Text = 'Automatic'
        OnChange = CBAutoStackChange
        Items.Strings = (
          'Automatic'
          'Yes'
          'Yes 100%'
          'No')
      end
      object CB2D: TComboBox
        Left = 73
        Top = 91
        Width = 145
        Height = 21
        Style = csDropDownList
        DropDownCount = 20
        TabOrder = 2
        OnChange = CB2DChange
      end
      object CB3D: TComboBox
        Left = 73
        Top = 120
        Width = 145
        Height = 21
        Style = csDropDownList
        DropDownCount = 20
        TabOrder = 3
        OnChange = CB3DChange
      end
      object CBRenderSeries: TComboBox
        Left = 73
        Top = 156
        Width = 145
        Height = 21
        Style = csDropDownList
        DropDownCount = 20
        ItemIndex = 0
        TabOrder = 4
        Text = 'Default'
        OnChange = CBRenderSeriesChange
        Items.Strings = (
          'Default'
          'Fast 2D'
          'Fast 3D')
      end
      object CBStyle: TComboBox
        Left = 73
        Top = 61
        Width = 145
        Height = 21
        Style = csDropDownList
        DropDownCount = 20
        ItemIndex = 0
        TabOrder = 5
        Text = 'Automatic'
        OnChange = CBStyleChange
        Items.Strings = (
          'Automatic'
          '2D'
          '3D'
          'Geographic')
      end
    end
    object TabSubChart: TTabSheet
      Caption = 'SubChart'
      ImageIndex = 2
      object Label15: TLabel
        Left = 9
        Top = 9
        Width = 44
        Height = 13
        Caption = '&Columns:'
        FocusControl = ESubColumns
      end
      object ESubColumns: TEdit
        Left = 9
        Top = 28
        Width = 44
        Height = 21
        TabOrder = 0
        Text = '0'
        OnChange = ESubColumnsChange
      end
      object UDSubColumns: TUpDown
        Left = 53
        Top = 28
        Width = 16
        Height = 21
        Associate = ESubColumns
        TabOrder = 1
      end
      object CBSubColumns: TCheckBox
        Left = 78
        Top = 30
        Width = 131
        Height = 17
        Caption = '&Automatic'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = CBSubColumnsClick
      end
      object CBSameAxisRange: TCheckBox
        Left = 9
        Top = 72
        Width = 128
        Height = 17
        Caption = 'Same Axis &Range'
        TabOrder = 3
        OnClick = CBSameAxisRangeClick
      end
    end
    object TabChart: TTabSheet
      Caption = 'Chart'
      ImageIndex = 3
    end
  end
end
