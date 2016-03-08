object DataDiagram: TDataDiagram
  Left = 0
  Top = 0
  Caption = 'Meta Datas'
  ClientHeight = 390
  ClientWidth = 711
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 469
    Top = 41
    Height = 349
    Align = alRight
    ExplicitLeft = 360
    ExplicitTop = 168
    ExplicitHeight = 100
  end
  object Tree1: TTree
    Left = 0
    Top = 41
    Width = 469
    Height = 349
    CrossBox.SignPen.Width = 0
    Page.Height = 1058
    Page.Width = 771
    Page.UsePrinter = False
    OnBeforeDraw = Tree1BeforeDraw
    OnSelectShape = Tree1SelectShape
    Align = alClient
    TabOrder = 0
    DefaultCanvas = 'TGDIPlusCanvas'
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 711
    Height = 41
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 344
      Top = 11
      Width = 47
      Height = 13
      Caption = '&Text size:'
      FocusControl = TBFont
    end
    object Button1: TButton
      Left = 88
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 0
      OnClick = Button1Click
    end
    object BLoad: TButton
      Left = 7
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Load...'
      TabOrder = 1
      OnClick = BLoadClick
    end
    object TBFont: TTrackBar
      Left = 397
      Top = 11
      Width = 150
      Height = 24
      LineSize = 5
      Max = 100
      Min = 2
      PageSize = 10
      Frequency = 10
      Position = 2
      TabOrder = 2
      ThumbLength = 14
      OnChange = TBFontChange
    end
    object CBGDIPlus: TCheckBox
      Left = 592
      Top = 10
      Width = 57
      Height = 17
      Caption = 'GDI+'
      TabOrder = 3
      OnClick = CBGDIPlusClick
    end
  end
  object Panel2: TPanel
    Left = 472
    Top = 41
    Width = 239
    Height = 349
    Align = alRight
    TabOrder = 2
    object Grid: TBIGrid
      Left = 1
      Top = 1
      Width = 237
      Height = 347
      Align = alClient
      UseDockManager = False
      ParentBackground = False
      ParentColor = False
      TabOrder = 0
      DataSource = DataSource1
    end
  end
  object DataSource1: TDataSource
    DataSet = BIDataset1
    Left = 592
    Top = 176
  end
  object BIDataset1: TBIDataset
    RowNumbers = False
    Left = 592
    Top = 240
  end
end
