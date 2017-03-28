object LayoutDesigner: TLayoutDesigner
  Left = 0
  Top = 0
  Caption = 'Template Layout Designer'
  ClientHeight = 603
  ClientWidth = 955
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
  object Splitter1: TSplitter
    Left = 222
    Top = 0
    Height = 603
    ExplicitLeft = 80
  end
  object PageControl1: TPageControl
    Left = 225
    Top = 0
    Width = 730
    Height = 603
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'Preview'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object BIVisual1: TBIVisual
        Left = 0
        Top = 0
        Width = 722
        Height = 575
        Align = alClient
        TabOrder = 0
        Dashboards = <>
        Data = <>
        Layouts = <>
        Panels = <>
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Template'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object MemoTemplate: TMemo
        Left = 0
        Top = 0
        Width = 722
        Height = 575
        Align = alClient
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object GalleryTemplate: TMemo
    Left = 320
    Top = 104
    Width = 489
    Height = 281
    Lines.Strings = (
      '{'
      '  "dashboards": ['
      '   {'
      '     "name": "gallery",'
      '     "layout": "3x3",'
      '     "panels": ['
      
        '       { "layout": "2 Rows", "type": "text", "text": "Hello", "b' +
        'ack": "Aqua" },'
      
        '       { "layout": "2 Rows", "type": "text", "text": "World", "b' +
        'ack": "Aqua" }'
      '     ]'
      '   }'
      '  ]'
      '}')
    TabOrder = 0
    Visible = False
    WordWrap = False
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 222
    Height = 603
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter2: TSplitter
      Left = 0
      Top = 258
      Width = 222
      Height = 3
      Cursor = crVSplit
      Align = alTop
      ExplicitTop = 217
      ExplicitWidth = 386
    end
    object Gallery: TBIVisual
      Left = 0
      Top = 261
      Width = 222
      Height = 211
      Align = alClient
      TabOrder = 0
      Dashboards = <>
      Data = <>
      Layouts = <>
      Panels = <>
    end
    object LBLayouts: TListBox
      Left = 0
      Top = 41
      Width = 222
      Height = 217
      Align = alTop
      DragMode = dmAutomatic
      ItemHeight = 13
      TabOrder = 1
      OnClick = LBLayoutsClick
    end
    object Panel1: TPanel
      Left = 0
      Top = 472
      Width = 222
      Height = 131
      Align = alBottom
      TabOrder = 2
      object LAlign: TLabel
        Left = 8
        Top = 48
        Width = 27
        Height = 13
        Caption = 'Align:'
        Enabled = False
      end
      object Label1: TLabel
        Left = 8
        Top = 78
        Width = 42
        Height = 13
        Caption = '&Padding:'
      end
      object BClear: TButton
        Left = 8
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Clear'
        Enabled = False
        TabOrder = 0
        OnClick = BClearClick
      end
      object BDelete: TButton
        Left = 104
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Delete'
        Enabled = False
        TabOrder = 1
        OnClick = BDeleteClick
      end
      object CBAlign: TComboBox
        Left = 58
        Top = 45
        Width = 105
        Height = 21
        Style = csDropDownList
        Enabled = False
        TabOrder = 2
        Items.Strings = (
          'None'
          'Left'
          'Right'
          'Top'
          'Bottom'
          'Client')
      end
      object EPadding: TEdit
        Left = 58
        Top = 75
        Width = 49
        Height = 21
        TabOrder = 3
        Text = '10'
        OnChange = EPaddingChange
      end
      object UDPadding: TUpDown
        Left = 107
        Top = 75
        Width = 16
        Height = 21
        Associate = EPadding
        Max = 2000
        Position = 10
        TabOrder = 4
      end
    end
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 222
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
      object Button1: TButton
        Left = 9
        Top = 9
        Width = 75
        Height = 25
        Caption = 'Options'
        TabOrder = 0
        OnClick = Button1Click
      end
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 104
    Top = 8
    object Captions1: TMenuItem
      Caption = '&Captions'
      Checked = True
      OnClick = Captions1Click
    end
    object Colors1: TMenuItem
      Caption = 'C&olors'
      Checked = True
      OnClick = Colors1Click
    end
    object Frames1: TMenuItem
      Caption = '&Frames'
      OnClick = Frames1Click
    end
    object Splitters1: TMenuItem
      Caption = '&Splitters'
      Checked = True
      OnClick = Splitters1Click
    end
  end
end
