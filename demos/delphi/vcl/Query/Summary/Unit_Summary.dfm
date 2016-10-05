object FormSummary: TFormSummary
  Left = 0
  Top = 0
  Caption = 'Summary Rows and Columns Test'
  ClientHeight = 615
  ClientWidth = 1176
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 297
    Top = 41
    Height = 539
    ExplicitLeft = 544
    ExplicitTop = 280
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1176
    Height = 41
    Align = alTop
    TabOrder = 0
    object CBHideDuplicates: TCheckBox
      Left = 445
      Top = 10
      Width = 137
      Height = 17
      Caption = 'Hide Duplicates'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBHideDuplicatesClick
    end
    object Button1: TButton
      Left = 800
      Top = 6
      Width = 89
      Height = 25
      Caption = 'Data Viewer...'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 10
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Test All'
      TabOrder = 2
      OnClick = Button2Click
    end
    object BDiagram: TButton
      Left = 654
      Top = 6
      Width = 131
      Height = 25
      Caption = 'SQLite Diagramm...'
      TabOrder = 3
      OnClick = BDiagramClick
    end
    object CBChart: TCheckBox
      Left = 568
      Top = 10
      Width = 57
      Height = 17
      Caption = 'Chart'
      TabOrder = 4
      OnClick = CBChartClick
    end
    object CBStore: TComboBox
      Left = 128
      Top = 8
      Width = 163
      Height = 21
      Style = csDropDownList
      TabOrder = 5
      OnChange = CBStoreChange
    end
    object Button4: TButton
      Left = 297
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Manage...'
      TabOrder = 6
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 897
      Top = 6
      Width = 96
      Height = 25
      Caption = 'Data Manager...'
      TabOrder = 7
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 999
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Grid...'
      TabOrder = 8
      OnClick = Button6Click
    end
    object BEditChart: TButton
      Left = 1086
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Edit Chart'
      Enabled = False
      TabOrder = 9
      OnClick = BEditChartClick
    end
  end
  object PanelList: TPanel
    Left = 0
    Top = 41
    Width = 297
    Height = 539
    Align = alLeft
    TabOrder = 1
    object Splitter4: TSplitter
      Left = 1
      Top = 209
      Width = 295
      Height = 3
      Cursor = crVSplit
      Align = alTop
      ExplicitTop = 1
      ExplicitWidth = 278
    end
    object LBTest: TListBox
      Left = 1
      Top = 1
      Width = 295
      Height = 208
      Align = alTop
      ItemHeight = 13
      Items.Strings = (
        ' 0 One Measure'
        ' 1 Two Measures'
        ' 2 Three Measures'
        ' 3 One GroupBy (auto)'
        ' 4 One GroupBy (columns)'
        ' 5 Two GroupBy (auto)'
        ' 6 Two GroupBy (all rows)'
        ' 7 Three GroupBy (auto)'
        ' 8 Three GroupBy (all rows)'
        ' 9 Three GroupBy (2 at columns)'
        '10 Two Measures, One GroupBy (auto)'
        '11 By Year'
        '12 By Year and ShipVia'
        '13 By Year, Quarter, ShipVia'
        '14 By Year, Quarter, Month, ShipVia'
        '15 By Year, Quarter, Month, ShipVia, Category'
        '16 By Year and Discontinued (boolean)'
        '17 By Year and UnitPrice (histogram)'
        '18 By Year, Quarter, ShipVia and Shipper.CompanyName'
        '19 Expression Measure'
        '20 Expression Measure by Year'
        '21 Measure by Expression'
        '22 Measure by Length(Customers CompanyName)'
        '23 Quantity Min and Max, by Year and ShipVia'
        '24 By CompanyName (text histogram)'
        '25 By Discontinued (boolean histogram)'
        '26 One GroupBy (columns) SORTED'
        '27 First Order Quantity by Product'
        '28 Last Order Quantity by Product'
        '29 By CategoryName, CategoryID (redundant)')
      PopupMenu = PopupMenu1
      TabOrder = 0
      OnClick = LBTestClick
    end
    object Panel3: TPanel
      Left = 1
      Top = 448
      Width = 295
      Height = 90
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object Label1: TLabel
        Left = 9
        Top = 0
        Width = 28
        Height = 13
        Caption = '&Filter:'
      end
      object LFilterError: TLabel
        Left = 9
        Top = 46
        Width = 272
        Height = 39
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object EFilter: TEdit
        Left = 9
        Top = 19
        Width = 275
        Height = 21
        TabOrder = 0
        Text = 'Suppliers.CompanyName<>'#39'Leka Trading'#39
        OnChange = EFilterChange
      end
      object CBFilter: TCheckBox
        Left = 72
        Top = -1
        Width = 97
        Height = 17
        Caption = '&Enabled'
        TabOrder = 1
        OnClick = CBFilterClick
      end
    end
    object PageControl2: TPageControl
      Left = 1
      Top = 212
      Width = 295
      Height = 236
      ActivePage = TabSummary
      Align = alClient
      TabOrder = 2
      object TabSummary: TTabSheet
        Caption = 'Summary'
      end
      object TabVizEditor: TTabSheet
        Caption = 'Visualizer'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
    end
  end
  object PageControl1: TPageControl
    Left = 300
    Top = 41
    Width = 876
    Height = 539
    ActivePage = TabSQL
    Align = alClient
    TabOrder = 2
    OnChange = PageControl1Change
    object TabGrid: TTabSheet
      Caption = 'Grid'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object SplitterChart: TSplitter
        Left = 0
        Top = 508
        Width = 868
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        Visible = False
        ExplicitLeft = 865
        ExplicitTop = 0
        ExplicitWidth = 511
      end
      object BIGrid1: TBIGrid
        Left = 0
        Top = 0
        Width = 868
        Height = 508
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        ParentColor = False
        TabOrder = 0
        Alternate.Enabled = True
      end
    end
    object TabTree: TTabSheet
      Caption = 'Tree'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Splitter3: TSplitter
        Left = 0
        Top = 275
        Width = 868
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 0
        ExplicitWidth = 0
      end
      object TreeView1: TTreeView
        Left = 0
        Top = 0
        Width = 868
        Height = 275
        Align = alClient
        Indent = 19
        TabOrder = 0
      end
      object TreeView2: TTreeView
        Left = 0
        Top = 278
        Width = 868
        Height = 233
        Align = alBottom
        Indent = 19
        TabOrder = 1
      end
    end
    object TabVisualizer: TTabSheet
      Caption = 'Visualizer'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 868
        Height = 41
        Align = alTop
        TabOrder = 0
        object Button8: TButton
          Left = 10
          Top = 8
          Width = 103
          Height = 25
          Caption = 'Control Tree...'
          TabOrder = 0
          OnClick = Button8Click
        end
      end
      object Viz: TBIComposer
        Left = 0
        Top = 41
        Width = 868
        Height = 470
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        TabOrder = 1
        Groups = <
          item
          end
          item
          end>
        Values = <
          item
          end
          item
          end>
        Origin = '|Animals|Animals'
      end
    end
    object TabSQL: TTabSheet
      Caption = 'SQL'
      ImageIndex = 3
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 868
        Height = 57
        Align = alTop
        TabOrder = 0
        object LSQLError: TLabel
          Left = 141
          Top = 8
          Width = 3
          Height = 13
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object BExecuteSQL: TButton
          Left = 13
          Top = 5
          Width = 108
          Height = 25
          Caption = 'Execute SQL'
          TabOrder = 0
          OnClick = BExecuteSQLClick
        end
        object CBAutoExecute: TCheckBox
          Left = 13
          Top = 36
          Width = 191
          Height = 17
          Caption = 'Auto Execute when SQL changes'
          TabOrder = 1
        end
        object Button3: TButton
          Left = 246
          Top = 5
          Width = 75
          Height = 25
          Caption = 'View Hops...'
          TabOrder = 2
          OnClick = Button3Click
        end
      end
      object MemoSQL: TMemo
        Left = 0
        Top = 57
        Width = 868
        Height = 89
        Align = alTop
        TabOrder = 1
        OnChange = MemoSQLChange
      end
      object BIGridSQL: TBIGrid
        Left = 0
        Top = 146
        Width = 868
        Height = 365
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        ParentColor = False
        TabOrder = 2
      end
    end
    object TabTotals: TTabSheet
      Caption = 'Totals'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Splitter2: TSplitter
        Left = 0
        Top = 258
        Width = 868
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 0
        ExplicitWidth = 261
      end
      object BIGridTotals: TBIGrid
        Left = 0
        Top = 0
        Width = 868
        Height = 258
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        ParentColor = False
        TabOrder = 0
        OnDataChange = BIGridTotalsDataChange
      end
      object BIChart1: TBIChart
        Left = 0
        Top = 261
        Width = 868
        Align = alBottom
        UseDockManager = False
        Padding.Top = 24
        ParentBackground = False
        TabOrder = 1
        object BITChart1: TBITChart
          Left = 0
          Top = 24
          Width = 868
          Height = 226
          BackWall.Brush.Gradient.Direction = gdBottomTop
          BackWall.Brush.Gradient.EndColor = clWhite
          BackWall.Brush.Gradient.StartColor = 15395562
          BackWall.Brush.Gradient.Visible = True
          BackWall.Transparent = False
          Foot.Font.Color = clBlue
          Foot.Font.Name = 'Verdana'
          Gradient.Direction = gdBottomTop
          Gradient.EndColor = clWhite
          Gradient.MidColor = 15395562
          Gradient.StartColor = 15395562
          LeftWall.Color = 14745599
          Legend.Font.Name = 'Verdana'
          Legend.Shadow.Transparency = 0
          RightWall.Color = 14745599
          Title.Font.Name = 'Verdana'
          BottomAxis.Axis.Color = 4210752
          BottomAxis.Grid.Color = 11119017
          BottomAxis.LabelsFormat.Font.Name = 'Verdana'
          BottomAxis.TicksInner.Color = 11119017
          BottomAxis.Title.Font.Name = 'Verdana'
          DepthAxis.Axis.Color = 4210752
          DepthAxis.Grid.Color = 11119017
          DepthAxis.LabelsFormat.Font.Name = 'Verdana'
          DepthAxis.TicksInner.Color = 11119017
          DepthAxis.Title.Font.Name = 'Verdana'
          DepthTopAxis.Axis.Color = 4210752
          DepthTopAxis.Grid.Color = 11119017
          DepthTopAxis.LabelsFormat.Font.Name = 'Verdana'
          DepthTopAxis.TicksInner.Color = 11119017
          DepthTopAxis.Title.Font.Name = 'Verdana'
          LeftAxis.Axis.Color = 4210752
          LeftAxis.Grid.Color = 11119017
          LeftAxis.LabelsFormat.Font.Name = 'Verdana'
          LeftAxis.TicksInner.Color = 11119017
          LeftAxis.Title.Font.Name = 'Verdana'
          RightAxis.Axis.Color = 4210752
          RightAxis.Grid.Color = 11119017
          RightAxis.LabelsFormat.Font.Name = 'Verdana'
          RightAxis.TicksInner.Color = 11119017
          RightAxis.Title.Font.Name = 'Verdana'
          TopAxis.Axis.Color = 4210752
          TopAxis.Grid.Color = 11119017
          TopAxis.LabelsFormat.Font.Name = 'Verdana'
          TopAxis.TicksInner.Color = 11119017
          TopAxis.Title.Font.Name = 'Verdana'
          TabOrder = 0
          DefaultCanvas = 'TGDIPlusCanvas'
          ColorPaletteIndex = 9
          object TMarksTipTool
            Format.CustomPosition = True
            Format.Left = 0
            Format.TextAlignment = taCenter
            Format.Top = 0
            Format.Visible = False
            Style = smsLabelValue
          end
        end
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 580
    Width = 1176
    Height = 35
    Align = alBottom
    TabOrder = 3
    object Label3: TLabel
      Left = 152
      Top = 11
      Width = 77
      Height = 13
      Caption = 'Quarter format:'
    end
    object Label2: TLabel
      Left = 466
      Top = 12
      Width = 42
      Height = 13
      Caption = 'Colorize:'
    end
    object CBStyle: TComboBox
      Left = 10
      Top = 6
      Width = 127
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Windows'
      OnChange = CBStyleChange
      Items.Strings = (
        'Windows'
        'Windows 10'
        'Windows 10 Blue'
        'Windows 10 Dark')
    end
    object EQuarter: TEdit
      Left = 235
      Top = 6
      Width = 94
      Height = 21
      TabOrder = 1
      OnChange = EQuarterChange
    end
    object Button7: TButton
      Left = 953
      Top = 6
      Width = 114
      Height = 25
      Caption = 'View Source Data...'
      TabOrder = 2
      OnClick = Button7Click
    end
    object CBColorize: TComboBox
      Left = 514
      Top = 8
      Width = 97
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 3
      Text = 'No'
      OnChange = CBColorizeChange
      Items.Strings = (
        'No'
        'Yes'
        'Inverted')
    end
    object CBAutoTextColor: TCheckBox
      Left = 617
      Top = 10
      Width = 97
      Height = 17
      Caption = 'Auto Text Color'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = CBAutoTextColorClick
    end
    object CBFillColor: TComboBox
      Left = 720
      Top = 8
      Width = 57
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 5
      Text = 'Full'
      OnChange = CBFillColorChange
      Items.Strings = (
        'Full'
        'Left')
    end
    object Button10: TButton
      Left = 800
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Benchmark'
      TabOrder = 6
      OnClick = Button10Click
    end
    object CBAlternate: TCheckBox
      Left = 352
      Top = 10
      Width = 97
      Height = 17
      Caption = 'Alternate Rows'
      TabOrder = 7
      OnClick = CBAlternateClick
    end
    object CBMultiCPU: TCheckBox
      Left = 881
      Top = 11
      Width = 69
      Height = 17
      Caption = 'Multi-CPU'
      TabOrder = 8
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 536
    Top = 312
    object Savesummaries1: TMenuItem
      Caption = 'Save summaries...'
      OnClick = Savesummaries1Click
    end
  end
  object SaveDialog1: TSaveDialog
    FileName = 'Summaries.sum'
    Filter = '*.sum'
    Left = 528
    Top = 376
  end
end
