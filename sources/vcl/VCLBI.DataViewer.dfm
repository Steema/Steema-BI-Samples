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
  object PanelTop: TPanel
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
        'Data Map'
        'Statistics')
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
    object PanelData: TPanel
      Left = 0
      Top = 264
      Width = 875
      Height = 386
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object PanelDataGrid: TPanel
        Left = 0
        Top = 0
        Width = 875
        Height = 386
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object PanelNav: TPanel
          Left = 0
          Top = 356
          Width = 875
          Height = 30
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 0
          object Panel2: TPanel
            Left = 0
            Top = 0
            Width = 113
            Height = 30
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 0
            object LRow: TLabel
              Left = 8
              Top = 11
              Width = 16
              Height = 13
              Caption = '0/0'
            end
          end
          object Panel4: TPanel
            Left = 113
            Top = 0
            Width = 240
            Height = 30
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 1
            object DBNavigator1: TDBNavigator
              Left = 0
              Top = 0
              Width = 240
              Height = 30
              VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast]
              Align = alClient
              TabOrder = 0
            end
          end
          object CBRecord: TCheckBox
            Left = 380
            Top = 8
            Width = 97
            Height = 17
            Caption = '&Record View'
            TabOrder = 2
            OnClick = CBRecordClick
          end
        end
        object Panel5: TPanel
          Left = 0
          Top = 0
          Width = 875
          Height = 356
          Align = alClient
          Caption = 'Panel5'
          TabOrder = 1
          object SplitterRecord: TSplitter
            Left = 551
            Top = 1
            Height = 354
            Align = alRight
            Visible = False
            ExplicitLeft = 440
            ExplicitTop = 128
            ExplicitHeight = 100
          end
          object DataGrid: TBIGrid
            Left = 1
            Top = 1
            Width = 550
            Height = 354
            Align = alClient
            UseDockManager = False
            ParentBackground = False
            ParentColor = False
            TabOrder = 0
            OnDataChange = DataGridDataChange
          end
          object RecordView: TBIGrid
            Left = 554
            Top = 1
            Width = 320
            Height = 354
            Align = alRight
            UseDockManager = False
            ParentBackground = False
            ParentColor = False
            TabOrder = 1
            Visible = False
          end
        end
      end
    end
    object PanelItemsGrid: TPanel
      Left = 0
      Top = 0
      Width = 875
      Height = 261
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object ItemsGrid: TBIGrid
        Left = 0
        Top = 30
        Width = 875
        Height = 231
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        ParentColor = False
        TabOrder = 0
        DataSource = DataSource2
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 875
        Height = 30
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object DBNavigator2: TDBNavigator
          Left = 0
          Top = 0
          Width = 249
          Height = 30
          DataSource = DataSource2
          VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast]
          Align = alLeft
          TabOrder = 0
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
    AfterInsert = ItemsAfterInsert
    RowNumbers = False
    Left = 360
    Top = 96
  end
end
