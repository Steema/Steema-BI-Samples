object BIChartEditor: TBIChartEditor
  Left = 0
  Top = 0
  ActiveControl = BOK
  Caption = 'BIChart Editor'
  ClientHeight = 450
  ClientWidth = 503
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 503
    Height = 413
    ActivePage = TabOptions
    Align = alClient
    TabOrder = 0
    OnChange = PageControl1Change
    object TabOptions: TTabSheet
      Caption = 'Options'
      ExplicitLeft = 8
      ExplicitTop = 28
      object GroupBox1: TGroupBox
        Left = 16
        Top = 16
        Width = 185
        Height = 153
        Caption = '&Style:'
        TabOrder = 0
        object RBAuto: TRadioButton
          Left = 16
          Top = 24
          Width = 137
          Height = 17
          Caption = '&Automatic'
          TabOrder = 0
          OnClick = RBAutoClick
        end
        object RBXY: TRadioButton
          Left = 16
          Top = 47
          Width = 137
          Height = 17
          Caption = '&2D XY'
          TabOrder = 1
          OnClick = RBXYClick
        end
        object RB3D: TRadioButton
          Left = 16
          Top = 70
          Width = 137
          Height = 17
          Caption = '&3D XYZ'
          TabOrder = 2
          OnClick = RB3DClick
        end
        object RBFinancial: TRadioButton
          Left = 16
          Top = 93
          Width = 137
          Height = 17
          Caption = '&Financial'
          TabOrder = 3
          OnClick = RBFinancialClick
        end
        object RBGeo: TRadioButton
          Left = 16
          Top = 116
          Width = 137
          Height = 17
          Caption = '&Geographic Maps'
          TabOrder = 4
          OnClick = RBGeoClick
        end
      end
    end
    object TabChart: TTabSheet
      Caption = 'Chart'
      ImageIndex = 1
    end
    object TabData: TTabSheet
      Caption = 'Data'
      ImageIndex = 2
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 495
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Button1: TButton
          Left = 16
          Top = 8
          Width = 75
          Height = 25
          Caption = '&Change...'
          TabOrder = 0
          OnClick = Button1Click
        end
      end
      object BIGrid1: TBIGrid
        Left = 0
        Top = 41
        Width = 495
        Height = 344
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        ParentColor = False
        TabOrder = 1
      end
    end
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 413
    Width = 503
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Panel9: TPanel
      Left = 400
      Top = 0
      Width = 103
      Height = 37
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object BOK: TButton
        Left = 9
        Top = 6
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Close'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
    end
  end
end
