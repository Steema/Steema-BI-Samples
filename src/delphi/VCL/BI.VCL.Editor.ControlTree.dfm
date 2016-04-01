object BIControlTree: TBIControlTree
  Left = 0
  Top = 0
  Caption = 'Visualizer Tree'
  ClientHeight = 495
  ClientWidth = 618
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
    Left = 380
    Top = 0
    Height = 495
    Align = alRight
    ExplicitLeft = 280
    ExplicitTop = 232
    ExplicitHeight = 100
  end
  object TreeView1: TTreeView
    Left = 0
    Top = 0
    Width = 380
    Height = 495
    Align = alClient
    Indent = 19
    TabOrder = 0
    OnChange = TreeView1Change
    ExplicitWidth = 349
  end
  object Panel1: TPanel
    Left = 383
    Top = 0
    Width = 235
    Height = 495
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 352
    object Label1: TLabel
      Left = 142
      Top = 104
      Width = 32
      Height = 13
      Caption = 'Width:'
    end
    object Label2: TLabel
      Left = 142
      Top = 146
      Width = 35
      Height = 13
      Caption = 'Height:'
    end
    object Label3: TLabel
      Left = 142
      Top = 16
      Width = 23
      Height = 13
      Caption = 'Left:'
    end
    object Label4: TLabel
      Left = 142
      Top = 58
      Width = 22
      Height = 13
      Caption = 'Top:'
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
      Left = 142
      Top = 119
      Width = 65
      Height = 21
      TabOrder = 2
      Text = '0'
      OnChange = EWidthChange
    end
    object UDWidth: TUpDown
      Left = 207
      Top = 119
      Width = 16
      Height = 21
      Associate = EWidth
      Max = 2000
      TabOrder = 3
      Thousands = False
    end
    object EHeight: TEdit
      Left = 142
      Top = 161
      Width = 65
      Height = 21
      TabOrder = 4
      Text = '0'
      OnChange = EHeightChange
    end
    object UDHeight: TUpDown
      Left = 207
      Top = 161
      Width = 16
      Height = 21
      Associate = EHeight
      Max = 2000
      TabOrder = 5
      Thousands = False
    end
    object ColorListBox1: TColorListBox
      Left = 16
      Top = 280
      Width = 111
      Height = 97
      TabOrder = 6
      OnClick = ColorListBox1Click
    end
    object BEditChart: TButton
      Left = 16
      Top = 400
      Width = 97
      Height = 25
      Caption = '&Edit Chart...'
      TabOrder = 7
      OnClick = BEditChartClick
    end
    object Edit1: TEdit
      Left = 142
      Top = 31
      Width = 65
      Height = 21
      TabOrder = 8
      Text = '0'
    end
    object UDLeft: TUpDown
      Left = 207
      Top = 31
      Width = 16
      Height = 21
      Associate = Edit1
      Max = 2000
      TabOrder = 9
      Thousands = False
    end
    object Edit2: TEdit
      Left = 142
      Top = 73
      Width = 65
      Height = 21
      TabOrder = 10
      Text = '0'
    end
    object UDTop: TUpDown
      Left = 207
      Top = 73
      Width = 16
      Height = 21
      Associate = Edit2
      Max = 2000
      TabOrder = 11
      Thousands = False
    end
    object CBParentColor: TCheckBox
      Left = 16
      Top = 224
      Width = 113
      Height = 17
      Caption = 'Parent Color'
      TabOrder = 12
      OnClick = CBParentColorClick
    end
    object CBParentBack: TCheckBox
      Left = 16
      Top = 247
      Width = 129
      Height = 17
      Caption = 'Parent Background'
      TabOrder = 13
      OnClick = CBParentBackClick
    end
    object Button1: TButton
      Left = 16
      Top = 456
      Width = 75
      Height = 25
      Caption = '&Font...'
      TabOrder = 14
      OnClick = Button1Click
    end
    object CBParentFont: TCheckBox
      Left = 16
      Top = 431
      Width = 113
      Height = 17
      Caption = 'Parent Font'
      TabOrder = 15
      OnClick = CBParentFontClick
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 288
    Top = 256
  end
end
