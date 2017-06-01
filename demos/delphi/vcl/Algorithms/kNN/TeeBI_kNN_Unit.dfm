object FormkNN: TFormkNN
  Left = 0
  Top = 0
  Caption = 'TeeBI kNN algorithm example'
  ClientHeight = 682
  ClientWidth = 1071
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
    Left = 305
    Top = 41
    Height = 641
    ExplicitLeft = 544
    ExplicitTop = 312
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1071
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 12
      Top = 12
      Width = 32
      Height = 13
      Caption = '&Model:'
      FocusControl = CBModel
    end
    object LTime: TLabel
      Left = 888
      Top = 13
      Width = 26
      Height = 13
      Caption = 'Time:'
    end
    object CBModel: TComboBox
      Left = 64
      Top = 9
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object Button1: TButton
      Left = 408
      Top = 7
      Width = 75
      Height = 25
      Caption = 'CrossTable'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 230
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Calculate'
      TabOrder = 2
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 496
      Top = 8
      Width = 129
      Height = 25
      Caption = 'Linear Regression'
      TabOrder = 3
      OnClick = Button3Click
    end
    object CBNativeR: TCheckBox
      Left = 688
      Top = 11
      Width = 137
      Height = 17
      Caption = 'Use "opaR" native'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = CBNativeRClick
    end
    object Button4: TButton
      Left = 311
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Plot'
      TabOrder = 5
      OnClick = Button4Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 305
    Height = 641
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 305
      Height = 641
      ActivePage = TabSheet1
      Align = alClient
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'Iris'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Splitter3: TSplitter
          Left = 0
          Top = 493
          Width = 297
          Height = 3
          Cursor = crVSplit
          Align = alBottom
          ExplicitTop = 0
          ExplicitWidth = 496
        end
        object BIGrid1: TBIGrid
          Left = 0
          Top = 0
          Width = 297
          Height = 493
          Align = alClient
          UseDockManager = False
          ParentBackground = False
          ParentColor = False
          TabOrder = 0
        end
        object BIGrid2: TBIGrid
          Left = 0
          Top = 496
          Width = 297
          Height = 117
          Align = alBottom
          UseDockManager = False
          ParentBackground = False
          ParentColor = False
          TabOrder = 1
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Normalized'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object BIGrid3: TBIGrid
          Left = 0
          Top = 0
          Width = 297
          Height = 613
          Align = alClient
          UseDockManager = False
          ParentBackground = False
          ParentColor = False
          TabOrder = 0
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'Predicted'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object BIGrid4: TBIGrid
          Left = 0
          Top = 0
          Width = 297
          Height = 372
          Align = alClient
          UseDockManager = False
          ParentBackground = False
          ParentColor = False
          TabOrder = 0
        end
        object BIGrid5: TBIGrid
          Left = 0
          Top = 413
          Width = 297
          Height = 200
          Align = alBottom
          UseDockManager = False
          ParentBackground = False
          ParentColor = False
          TabOrder = 1
        end
        object Panel4: TPanel
          Left = 0
          Top = 372
          Width = 297
          Height = 41
          Align = alBottom
          TabOrder = 2
          object Label2: TLabel
            Left = 9
            Top = 14
            Width = 40
            Height = 13
            Caption = 'Correct:'
          end
          object LCorrect: TLabel
            Left = 72
            Top = 14
            Width = 6
            Height = 13
            Caption = '0'
          end
        end
      end
    end
  end
  object PageControl2: TPageControl
    Left = 308
    Top = 41
    Width = 763
    Height = 641
    ActivePage = TabSheet4
    Align = alClient
    TabOrder = 2
    object TabSheet4: TTabSheet
      Caption = 'Chart'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 755
        Height = 613
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object BIChart1: TBIChart
          Left = 0
          Top = 0
          Width = 755
          Height = 613
          Align = alClient
          UseDockManager = False
          Padding.Top = 24
          ParentBackground = False
          TabOrder = 0
          object BITChart1: TBITChart
            Left = 0
            Top = 24
            Width = 755
            Height = 589
            Legend.Shadow.Visible = False
            Legend.Symbol.Shadow.Visible = False
            Title.Text.Strings = (
              'TBITChart')
            TabOrder = 0
            DefaultCanvas = 'TGDIPlusCanvas'
            ColorPaletteIndex = 9
          end
        end
      end
    end
    object TabConsole: TTabSheet
      Caption = 'Console'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
end
