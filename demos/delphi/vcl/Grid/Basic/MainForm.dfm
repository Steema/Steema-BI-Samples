object GridDemoForm: TGridDemoForm
  Left = 0
  Top = 0
  Caption = 'TeeBI Grid Example'
  ClientHeight = 623
  ClientWidth = 972
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 289
    Top = 0
    Height = 623
    ExplicitLeft = 496
    ExplicitTop = 280
    ExplicitHeight = 100
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 289
    Height = 623
    ActivePage = TabOptions
    Align = alLeft
    TabOrder = 0
    object TabOptions: TTabSheet
      Caption = 'Grid Options'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TabData: TTabSheet
      Caption = 'Data'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
  object Panel1: TPanel
    Left = 292
    Top = 0
    Width = 680
    Height = 623
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object BIGrid1: TBIGrid
      Left = 0
      Top = 0
      Width = 680
      Height = 586
      Align = alClient
      UseDockManager = False
      ParentBackground = False
      ParentColor = False
      TabOrder = 0
    end
    object Panel2: TPanel
      Left = 0
      Top = 586
      Width = 680
      Height = 37
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object DBNavigator1: TDBNavigator
        Left = 16
        Top = 6
        Width = 224
        Height = 25
        VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast]
        TabOrder = 0
      end
    end
  end
end
