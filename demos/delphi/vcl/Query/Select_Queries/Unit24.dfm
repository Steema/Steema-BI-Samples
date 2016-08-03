object Form24: TForm24
  Left = 0
  Top = 0
  Caption = 'TeeBI Select Queries'
  ClientHeight = 638
  ClientWidth = 1046
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 361
    Top = 41
    Height = 519
    ExplicitLeft = 392
    ExplicitTop = 168
    ExplicitHeight = 100
  end
  object ListBox1: TListBox
    Left = 0
    Top = 41
    Width = 361
    Height = 519
    Align = alLeft
    ItemHeight = 13
    Items.Strings = (
      ' 0 * from Customers'
      ' 1 CompanyName,City from Customers'
      ' 2 CompanyName,City from Customers sort by City'
      ' 3 City from Customers'
      ' 4 distinct City from Customers'
      ' 5 distinct Country from Customers'
      ' 6 Customer, Orders ShipVia'
      ' 7 distinct Country from Customers (<>"Mexico") sort by Country'
      
        ' 8 distinct Country from Customers (City<>"Cork") sort by Countr' +
        'y'
      
        ' 9 distinct Country from Customers (City<>"Salzburg" and City<>"' +
        'Graz")'
      '10 Country from Customers (City= "Madrid")'
      '11 Suppliers CompanyName, Region (test missing values)'
      '12 distinct Suppliers Region (has null values)'
      '13 * from Customers sort by ContactTitle'
      '14 top 10 Customers'
      '15 top 10 Customers sort by City'
      '16 top 10 distinct City'
      '17 top 5 Products UnitPrice sort by descending'
      '18 Product, UnitPrice * (UnitsInStock + UnitsOnOrder)'
      '19 Year(OrderDate)'
      
        '20 City from select distinct City, Country where Country = "Fran' +
        'ce"'
      
        '21 ProductName, UnitPrice where UnitPrice > select Average(UnitP' +
        'rice)'
      '22 title, year, length from movies'
      '23 CategoryName, Products.*, Categories.CategoryID'
      '24 "Sum of Quantity"."Quarter of OrderDate" from Summary'
      '25 distinct ProductID, Discount from "Order Details"'
      '26 "Count of Quantity" from Summary (Layout Items)'
      '27 "Count of Quantity"."WeekOfYear of OrderDate" from Summary'
      '28 top 100 offset 15000 year, length from movies'
      '29 Top 1 Country from Customers (City= "Madrid"), offset 2'
      '30 distinct UnitPrice * (UnitsInStock + UnitsOnOrder)')
    TabOrder = 0
    OnClick = ListBox1Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1046
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 368
      Top = 10
      Width = 97
      Height = 25
      Caption = '&Data Manager...'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 8
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Run All'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Benchmark: TButton
      Left = 175
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Benchmark'
      Enabled = False
      TabOrder = 2
      OnClick = BenchmarkClick
    end
    object BViewData: TButton
      Left = 658
      Top = 10
      Width = 113
      Height = 25
      Caption = 'View result data...'
      Enabled = False
      TabOrder = 3
      OnClick = BViewDataClick
    end
    object Button3: TButton
      Left = 777
      Top = 10
      Width = 81
      Height = 25
      Caption = '&Edit Query...'
      TabOrder = 4
      OnClick = Button3Click
    end
    object CBVerifySQL: TCheckBox
      Left = 97
      Top = 14
      Width = 72
      Height = 17
      Caption = 'Verify SQL'
      TabOrder = 5
    end
    object Button4: TButton
      Left = 256
      Top = 10
      Width = 99
      Height = 25
      Caption = 'Benchmark All'
      TabOrder = 6
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 864
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Save Results'
      TabOrder = 7
    end
    object Button6: TButton
      Left = 952
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Verify Saved'
      Enabled = False
      TabOrder = 8
    end
    object Button7: TButton
      Left = 488
      Top = 9
      Width = 73
      Height = 25
      Caption = 'Thread Test'
      TabOrder = 9
      OnClick = Button7Click
    end
    object CBMultiCPU: TCheckBox
      Left = 567
      Top = 2
      Width = 68
      Height = 17
      Caption = 'Multi CPU'
      Checked = True
      State = cbChecked
      TabOrder = 10
    end
    object CBLoopThread: TCheckBox
      Left = 567
      Top = 18
      Width = 85
      Height = 17
      Caption = 'Loop in thread'
      TabOrder = 11
    end
  end
  object PageControl1: TPageControl
    Left = 364
    Top = 41
    Width = 682
    Height = 519
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 2
    OnChange = PageControl1Change
    object TabSheet1: TTabSheet
      Caption = 'Output'
      object BIGrid1: TBIGrid
        Left = 0
        Top = 0
        Width = 674
        Height = 491
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        ParentColor = False
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Visualizer'
      ImageIndex = 1
      object BIVisualizer1: TBIComposer
        Left = 0
        Top = 0
        Width = 674
        Height = 491
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        TabOrder = 0
        Groups = <>
        Values = <>
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Grid Options'
      ImageIndex = 2
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 560
    Width = 1046
    Height = 78
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 97
      Height = 58
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object BExecSQL: TButton
        Left = 8
        Top = 6
        Width = 75
        Height = 25
        Caption = 'Run SQL'
        Enabled = False
        TabOrder = 0
        OnClick = BExecSQLClick
      end
    end
    object Memo1: TMemo
      Left = 97
      Top = 0
      Width = 949
      Height = 58
      Align = alClient
      TabOrder = 1
      OnChange = Memo1Change
    end
    object Panel4: TPanel
      Left = 0
      Top = 58
      Width = 1046
      Height = 20
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      object LError: TLabel
        Left = 0
        Top = 0
        Width = 1046
        Height = 20
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ExplicitWidth = 3
        ExplicitHeight = 13
      end
    end
  end
end
