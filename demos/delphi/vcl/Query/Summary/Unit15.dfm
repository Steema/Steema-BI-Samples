object Form15: TForm15
  Left = 0
  Top = 0
  Caption = 'Summary Rows and Columns Test'
  ClientHeight = 615
  ClientWidth = 1081
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
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
    Width = 1081
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
    object CBChart: TCheckBox
      Left = 568
      Top = 10
      Width = 57
      Height = 17
      Caption = 'Chart'
      TabOrder = 3
      OnClick = CBChartClick
    end
    object CBStore: TComboBox
      Left = 128
      Top = 8
      Width = 163
      Height = 21
      Style = csDropDownList
      TabOrder = 4
      OnChange = CBStoreChange
    end
    object Button4: TButton
      Left = 297
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Manage...'
      TabOrder = 5
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 897
      Top = 6
      Width = 96
      Height = 25
      Caption = 'Data Manager...'
      TabOrder = 6
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 999
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Grid...'
      TabOrder = 7
      OnClick = Button6Click
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
      Top = 329
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
      Height = 328
      Align = alTop
      ItemHeight = 13
      Items.Strings = (
        'One Measure'
        'Two Measures'
        'Three Measures'
        'One GroupBy (auto)'
        'One GroupBy (columns)'
        'Two GroupBy (auto)'
        'Two GroupBy (all rows)'
        'Three GroupBy (auto)'
        'Three GroupBy (all rows)'
        'Three GroupBy (2 at columns)'
        'Two Measures, One GroupBy (auto)'
        'By Year'
        'By Year and ShipVia'
        'By Year, Quarter, ShipVia'
        'By Year, Quarter, Month, ShipVia'
        'By Year, Quarter, Month, ShipVia, Category'
        'By Year and Discontinued (boolean)'
        'By Year and UnitPrice (histogram)'
        'By Year, Quarter, ShipVia and Shipper.CompanyName'
        'Expression Measure'
        'Expression Measure by Year'
        'Measure by Expression'
        'Measure by Length(Customers CompanyName)')
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
  end
  object PageControl1: TPageControl
    Left = 300
    Top = 41
    Width = 781
    Height = 539
    ActivePage = TabGrid
    Align = alClient
    TabOrder = 2
    OnChange = PageControl1Change
    object TabGrid: TTabSheet
      Caption = 'Grid'
      object Splitter2: TSplitter
        Left = 0
        Top = 508
        Width = 773
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        Visible = False
        ExplicitTop = 0
        ExplicitWidth = 505
      end
      object SplitterChart: TSplitter
        Left = 770
        Top = 0
        Height = 508
        Align = alRight
        Visible = False
        ExplicitLeft = 384
        ExplicitTop = 208
        ExplicitHeight = 100
      end
      object G: TBIGrid
        Left = 0
        Top = 0
        Width = 770
        Height = 508
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        ParentColor = False
        TabOrder = 0
      end
    end
    object TabTree: TTabSheet
      Caption = 'Tree'
      ImageIndex = 1
      object Splitter3: TSplitter
        Left = 0
        Top = 275
        Width = 773
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 0
        ExplicitWidth = 0
      end
      object TreeView1: TTreeView
        Left = 0
        Top = 0
        Width = 773
        Height = 275
        Align = alClient
        Indent = 19
        TabOrder = 0
      end
      object TreeView2: TTreeView
        Left = 0
        Top = 278
        Width = 773
        Height = 233
        Align = alBottom
        Indent = 19
        TabOrder = 1
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 580
    Width = 1081
    Height = 35
    Align = alBottom
    TabOrder = 3
    object Label3: TLabel
      Left = 200
      Top = 11
      Width = 77
      Height = 13
      Caption = 'Quarter format:'
    end
    object CBStyle: TComboBox
      Left = 10
      Top = 6
      Width = 145
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
      Left = 283
      Top = 6
      Width = 94
      Height = 21
      TabOrder = 1
      OnChange = EQuarterChange
    end
    object Button7: TButton
      Left = 728
      Top = 6
      Width = 161
      Height = 25
      Caption = 'View Source Data...'
      TabOrder = 2
      OnClick = Button7Click
    end
  end
end
