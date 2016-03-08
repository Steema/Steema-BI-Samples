object DataViewer: TDataViewer
  Left = 0
  Top = 0
  Caption = 'Data Viewer'
  ClientHeight = 691
  ClientWidth = 1091
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 213
    Top = 41
    Height = 650
    ExplicitLeft = 552
    ExplicitTop = 312
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1091
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 14
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object LName: TLabel
      Left = 64
      Top = 14
      Width = 32
      Height = 13
      Caption = 'LName'
    end
    object CBViewData: TCheckBox
      Left = 232
      Top = 13
      Width = 50
      Height = 17
      Caption = '&View:'
      Enabled = False
      TabOrder = 0
      OnClick = CBViewDataClick
    end
    object BDiagram: TButton
      Left = 720
      Top = 10
      Width = 75
      Height = 25
      Caption = '&Diagram...'
      TabOrder = 1
      OnClick = BDiagramClick
    end
    object Button2: TButton
      Left = 840
      Top = 10
      Width = 97
      Height = 25
      Caption = 'View &Cache...'
      TabOrder = 2
      OnClick = Button2Click
    end
    object CBView: TComboBox
      Left = 288
      Top = 11
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 3
      Text = 'Data'
      OnChange = CBViewChange
      Items.Strings = (
        'Data'
        'Data Map')
    end
  end
  object PanelItems: TPanel
    Left = 216
    Top = 41
    Width = 875
    Height = 650
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object SplitterData: TSplitter
      Left = 0
      Top = 261
      Width = 875
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 0
      ExplicitWidth = 400
    end
    object ItemsGrid: TBIGrid
      Left = 0
      Top = 0
      Width = 875
      Height = 261
      Align = alClient
      UseDockManager = False
      ParentBackground = False
      ParentColor = False
      TabOrder = 0
      DataSource = DataSource2
    end
    object PanelData: TPanel
      Left = 0
      Top = 264
      Width = 875
      Height = 386
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object PanelDataGrid: TPanel
        Left = 0
        Top = 0
        Width = 875
        Height = 386
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object DataGrid: TBIGrid
          Left = 0
          Top = 0
          Width = 875
          Height = 352
          Align = alClient
          UseDockManager = False
          ParentBackground = False
          ParentColor = False
          TabOrder = 0
          ShowItems = True
        end
        object PanelNav: TPanel
          Left = 0
          Top = 352
          Width = 875
          Height = 34
          Align = alBottom
          BevelOuter = bvNone
          Caption = 'PanelNav'
          TabOrder = 1
          object DBNavigator1: TDBNavigator
            Left = 113
            Top = 0
            Width = 762
            Height = 34
            VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast]
            Align = alClient
            TabOrder = 0
          end
          object Panel2: TPanel
            Left = 0
            Top = 0
            Width = 113
            Height = 34
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 1
            object LRow: TLabel
              Left = 8
              Top = 11
              Width = 16
              Height = 13
              Caption = '0/0'
            end
          end
        end
      end
    end
  end
  object PanelDatas: TPanel
    Left = 0
    Top = 41
    Width = 213
    Height = 650
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 5
    ExplicitTop = 47
    object DataTotals: TStringGrid
      Left = 0
      Top = 600
      Width = 213
      Height = 50
      Align = alBottom
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      Options = [goVertLine, goHorzLine, goRangeSelect]
      ScrollBars = ssHorizontal
      TabOrder = 0
      Visible = False
      ColWidths = (
        64
        64
        64
        64
        64)
      RowHeights = (
        24)
    end
  end
  object DataSource2: TDataSource
    DataSet = Items
    Left = 440
    Top = 96
  end
  object PopupMenu1: TPopupMenu
    Left = 56
    Top = 192
    object View1: TMenuItem
      Caption = '&View...'
      OnClick = View1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
  end
  object Items: TBIDataset
    RowNumbers = False
    Left = 360
    Top = 96
  end
end
