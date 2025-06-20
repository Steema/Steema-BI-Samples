object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'TeeBI Start Here Tutorial'
  ClientHeight = 586
  ClientWidth = 1119
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 409
    Top = 59
    Height = 527
    ExplicitLeft = 568
    ExplicitTop = 264
    ExplicitHeight = 100
  end
  object BIChart1: TBIChart
    Left = 412
    Top = 59
    Width = 707
    Height = 527
    Align = alClient
    UseDockManager = False
    Padding.Top = 24
    ParentBackground = False
    TabOrder = 0
    Options.Dimensions = Orthogonal
    Options.Legend = Hide
    Options.Marks = Hide
    Options.Stacked = Stacked100
    object BITChart1: TBITChart
      Left = 0
      Top = 24
      Width = 707
      Height = 503
      Legend.Shadow.Visible = False
      Legend.Symbol.Shadow.Visible = False
      Legend.Visible = False
      View3D = True
      TabOrder = 0
      DefaultCanvas = 'TGDIPlusCanvas'
      ColorPaletteIndex = 9
    end
  end
  object BIGrid1: TBIGrid
    Left = 0
    Top = 59
    Width = 409
    Height = 527
    Align = alLeft
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1119
    Height = 59
    Align = alTop
    TabOrder = 2
    object Label1: TLabel
      Left = 16
      Top = 22
      Width = 238
      Height = 15
      Caption = 'Very simple demo showcasing Tee BI features'
    end
    object Button1: TButton
      Left = 296
      Top = 18
      Width = 75
      Height = 25
      Caption = '&Grid...'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 393
      Top = 18
      Width = 75
      Height = 25
      Caption = '&Chart...'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 488
      Top = 18
      Width = 75
      Height = 25
      Caption = '&Query...'
      TabOrder = 2
      OnClick = Button3Click
    end
  end
  object BIQuery1: TBIQuery
    Left = 552
    Top = 296
  end
end
