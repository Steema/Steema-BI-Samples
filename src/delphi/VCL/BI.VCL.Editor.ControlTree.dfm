object BIControlTree: TBIControlTree
  Left = 0
  Top = 0
  Caption = 'Visualizer Tree'
  ClientHeight = 522
  ClientWidth = 402
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 243
    Top = 0
    Height = 522
    Align = alRight
    ExplicitLeft = 280
    ExplicitTop = 232
    ExplicitHeight = 100
  end
  object TreeView1: TTreeView
    Left = 0
    Top = 0
    Width = 243
    Height = 522
    Align = alClient
    Indent = 19
    TabOrder = 0
    OnChange = TreeView1Change
  end
  object Panel1: TPanel
    Left = 246
    Top = 0
    Width = 156
    Height = 522
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 232
      Width = 32
      Height = 13
      Caption = 'Width:'
    end
    object Label2: TLabel
      Left = 16
      Top = 278
      Width = 35
      Height = 13
      Caption = 'Height:'
    end
    object RGAlign: TRadioGroup
      Left = 16
      Top = 16
      Width = 97
      Height = 169
      Caption = 'Align:'
      Items.Strings = (
        'None'
        'Top'
        'Bottom'
        'Left'
        'Right'
        'Client'
        'Custom')
      TabOrder = 0
      OnClick = RGAlignClick
    end
    object CBVisible: TCheckBox
      Left = 16
      Top = 191
      Width = 97
      Height = 17
      Caption = '&Visible'
      TabOrder = 1
      OnClick = CBVisibleClick
    end
    object EWidth: TEdit
      Left = 16
      Top = 251
      Width = 65
      Height = 21
      TabOrder = 2
      Text = '0'
      OnChange = EWidthChange
    end
    object UDWidth: TUpDown
      Left = 81
      Top = 251
      Width = 17
      Height = 21
      Associate = EWidth
      Max = 2000
      TabOrder = 3
      Thousands = False
    end
    object EHeight: TEdit
      Left = 16
      Top = 297
      Width = 65
      Height = 21
      TabOrder = 4
      Text = '0'
      OnChange = EHeightChange
    end
    object UDHeight: TUpDown
      Left = 81
      Top = 297
      Width = 17
      Height = 21
      Associate = EHeight
      Max = 2000
      TabOrder = 5
      Thousands = False
    end
    object ColorListBox1: TColorListBox
      Left = 16
      Top = 344
      Width = 121
      Height = 97
      TabOrder = 6
      OnClick = ColorListBox1Click
    end
    object BEditChart: TButton
      Left = 16
      Top = 464
      Width = 97
      Height = 25
      Caption = '&Edit Chart...'
      TabOrder = 7
      OnClick = BEditChartClick
    end
  end
end
