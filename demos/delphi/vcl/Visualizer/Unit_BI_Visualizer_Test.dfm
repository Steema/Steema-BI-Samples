object FormViz: TFormViz
  Left = 0
  Top = 0
  Caption = 'TDataVisualizer Test'
  ClientHeight = 560
  ClientWidth = 899
  Color = clWhite
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
    Left = 305
    Top = 41
    Height = 519
    ExplicitLeft = 368
    ExplicitTop = 200
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 899
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    object SpeedButton1: TSpeedButton
      Left = 4
      Top = 6
      Width = 23
      Height = 22
      AllowAllUp = True
      GroupIndex = 1
      Flat = True
      Glyph.Data = {
        F6060000424DF606000000000000360000002800000018000000180000000100
        180000000000C0060000C40E0000C40E00000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        C8C8C8BDBDBDBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
        BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBDBDBDC8C8C8FFFFFFFF
        FFFFFFFFFFC6C6C66C6C6C777777777777777777777777777777777777777777
        7777777777777777777777777777777777777777777777777777777777777777
        776C6C6CC7C7C7FFFFFFFFFFFFC6C6C66C6C6C77777777777777777777777777
        7777777777777777777777777777777777777777777777777777777777777777
        7777777777777777776C6C6CC6C6C6FFFFFFFFFFFFFFFFFFC8C8C8BDBDBDBFBF
        BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
        BFBFBFBFBFBFBFBFBFBFBFBFBFBFBDBDBDC8C8C8FFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC8C8C8BDBDBDBFBFBFBFBFBFBFBFBFBF
        BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
        BFBFBFBFBFBFBDBDBDC8C8C8FFFFFFFFFFFFFFFFFFC6C6C66C6C6C7777777777
        7777777777777777777777777777777777777777777777777777777777777777
        77777777777777777777777777777777776C6C6CC7C7C7FFFFFFFFFFFFC7C7C7
        6C6C6C7777777777777777777777777777777777777777777777777777777777
        777777777777777777777777777777777777777777777777776C6C6CC6C6C6FF
        FFFFFFFFFFFFFFFFC8C8C8BDBDBDBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
        BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBDBD
        BDC8C8C8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        C8C8C8BDBDBDBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
        BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBDBDBDC8C8C8FFFFFFFF
        FFFFFFFFFFC6C6C66C6C6C777777777777777777777777777777777777777777
        7777777777777777777777777777777777777777777777777777777777777777
        776C6C6CC7C7C7FFFFFFFFFFFFC7C7C76C6C6C77777777777777777777777777
        7777777777777777777777777777777777777777777777777777777777777777
        7777777777777777776C6C6CC6C6C6FFFFFFFFFFFFFFFFFFC8C8C8BDBDBDBFBF
        BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
        BFBFBFBFBFBFBFBFBFBFBFBFBFBFBDBDBDC8C8C8FFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      OnClick = SpeedButton1Click
    end
    object Button1: TButton
      Left = 343
      Top = 10
      Width = 129
      Height = 25
      Caption = 'Control Tree...'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 512
      Top = 10
      Width = 113
      Height = 25
      Caption = 'Data Manager...'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 664
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Visualizer...'
      TabOrder = 2
      OnClick = Button3Click
    end
    object BData: TButton
      Left = 792
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Data...'
      TabOrder = 3
      OnClick = BDataClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 305
    Height = 519
    Align = alLeft
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 305
      Height = 519
      ActivePage = TabSettings
      Align = alClient
      TabOrder = 0
      object TabSettings: TTabSheet
        Caption = 'Settings'
      end
      object TabData: TTabSheet
        Caption = 'Data'
        ImageIndex = 2
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 297
          Height = 41
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object CBSample: TCheckBox
            Left = 8
            Top = 8
            Width = 105
            Height = 17
            Caption = 'Sample Summary'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = CBSampleClick
          end
          object BRefresh: TButton
            Left = 200
            Top = 4
            Width = 75
            Height = 25
            Caption = 'Refresh'
            TabOrder = 1
            Visible = False
            OnClick = BRefreshClick
          end
          object CBQuery: TCheckBox
            Left = 117
            Top = 8
            Width = 60
            Height = 17
            Caption = 'Query'
            TabOrder = 2
            OnClick = CBQueryClick
          end
        end
        object PanelSummary: TPanel
          Left = 0
          Top = 41
          Width = 297
          Height = 450
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
        end
      end
    end
  end
  object BIVisualizer1: TBIComposer
    Left = 308
    Top = 41
    Width = 591
    Height = 519
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    TabOrder = 2
    Groups = <>
    Values = <>
  end
  object BIQuery1: TBIQuery
    Left = 352
    Top = 232
  end
end