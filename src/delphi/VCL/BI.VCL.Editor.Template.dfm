object TemplateEditor: TTemplateEditor
  Left = 0
  Top = 0
  Caption = 'Visual Editor'
  ClientHeight = 624
  ClientWidth = 836
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter2: TSplitter
    Left = 273
    Top = 0
    Height = 624
    ExplicitLeft = 424
    ExplicitTop = 280
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 273
    Height = 624
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 0
      Top = 408
      Width = 273
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 0
      ExplicitWidth = 431
    end
    object BITree1: TBITree
      Left = 0
      Top = 41
      Width = 273
      Height = 367
      Align = alClient
      UseDockManager = False
      ParentBackground = False
      ParentColor = False
      PopupMenu = PopupItem
      TabOrder = 0
      OnChange = BITree1Change
      ExplicitLeft = 1
      ExplicitTop = 39
      ExplicitHeight = 387
    end
    object PageProperties: TPageControl
      Left = 0
      Top = 411
      Width = 273
      Height = 213
      ActivePage = TabItem
      Align = alBottom
      TabOrder = 1
      Visible = False
      object TabData: TTabSheet
        Caption = 'Data'
        ExplicitHeight = 165
        object Label2: TLabel
          Left = 7
          Top = 69
          Width = 27
          Height = 13
          Caption = '&Data:'
        end
        object LData: TLabel
          Left = 41
          Top = 69
          Width = 3
          Height = 13
        end
        object BChange: TButton
          Left = 7
          Top = 90
          Width = 75
          Height = 25
          Caption = '&Change...'
          Enabled = False
          TabOrder = 0
          OnClick = BChangeClick
        end
        object BEditProvider: TButton
          Left = 164
          Top = 30
          Width = 75
          Height = 25
          Caption = 'Edit...'
          Enabled = False
          TabOrder = 1
          OnClick = BEditProviderClick
        end
      end
      object TabPanel: TTabSheet
        Caption = 'Panel'
        ImageIndex = 1
        ExplicitHeight = 165
        object Label3: TLabel
          Left = 7
          Top = 60
          Width = 24
          Height = 13
          Caption = '&Kind:'
          FocusControl = CBPanelKind
        end
        object Label7: TLabel
          Left = 7
          Top = 8
          Width = 27
          Height = 13
          Caption = '&Data:'
          FocusControl = CBPanelData
        end
        object CBPanelKind: TComboBox
          Left = 7
          Top = 79
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = CBPanelKindChange
          Items.Strings = (
            'Automatic'
            'Buttons'
            'Chart'
            'Check Boxes'
            'Check Listbox'
            'Combo Box'
            'Grid'
            'Image'
            'List'
            'Navigator'
            'Radio Buttons'
            'Slider'
            'Text'
            'Tree')
        end
        object CBPanelData: TComboBox
          Left = 7
          Top = 27
          Width = 145
          Height = 21
          Style = csDropDownList
          TabOrder = 1
          OnChange = CBPanelDataChange
          Items.Strings = (
            'Undefined'
            'Grid'
            'List'
            'Text'
            'Check Listbox'
            'Combo Box'
            'Buttons'
            'Image'
            'Slider'
            'Check Boxes'
            'Navigator'
            'Tree'
            'Radio Buttons'
            'Chart')
        end
      end
      object TabDashboard: TTabSheet
        Caption = 'Dashboard'
        ImageIndex = 2
        ExplicitHeight = 165
        object LLayout: TLabel
          Left = 98
          Top = 85
          Width = 3
          Height = 13
        end
        object CBSplitters: TCheckBox
          Left = 9
          Top = 9
          Width = 97
          Height = 17
          Caption = '&Splitters'
          TabOrder = 0
          OnClick = CBSplittersClick
        end
        object CBTitles: TCheckBox
          Left = 9
          Top = 32
          Width = 97
          Height = 17
          Caption = '&Titles'
          TabOrder = 1
          OnClick = CBTitlesClick
        end
        object BSelectLayout: TButton
          Left = 9
          Top = 80
          Width = 75
          Height = 25
          Caption = '&Layout...'
          TabOrder = 2
          OnClick = BSelectLayoutClick
        end
      end
      object TabItem: TTabSheet
        Caption = 'Item'
        ImageIndex = 3
        ExplicitHeight = 165
        object PageControl2: TPageControl
          Left = 0
          Top = 0
          Width = 265
          Height = 185
          ActivePage = TabItemOptions
          Align = alClient
          TabOrder = 0
          object TabItemOptions: TTabSheet
            Caption = 'Options'
            object Label4: TLabel
              Left = 7
              Top = 9
              Width = 30
              Height = 13
              Caption = '&Panel:'
              FocusControl = CBPanels
            end
            object Label5: TLabel
              Left = 7
              Top = 56
              Width = 41
              Height = 13
              Caption = 'P&osition:'
              FocusControl = CBPosition
            end
            object Label6: TLabel
              Left = 7
              Top = 104
              Width = 24
              Height = 13
              Caption = '&Kind:'
              FocusControl = CBItemKind
            end
            object LRealKind: TLabel
              Left = 165
              Top = 126
              Width = 3
              Height = 13
            end
            object CBPanels: TComboBox
              Left = 7
              Top = 28
              Width = 145
              Height = 21
              Style = csDropDownList
              TabOrder = 0
              OnChange = CBPanelsChange
            end
            object CBPosition: TComboBox
              Left = 7
              Top = 75
              Width = 145
              Height = 21
              Style = csDropDownList
              TabOrder = 1
              OnChange = CBPositionChange
            end
            object CBItemKind: TComboBox
              Left = 7
              Top = 123
              Width = 145
              Height = 21
              Style = csDropDownList
              TabOrder = 2
              OnChange = CBItemKindChange
              Items.Strings = (
                'Automatic'
                'Buttons'
                'Chart'
                'Check Boxes'
                'Check Listbox'
                'Combo Box'
                'Grid'
                'Image'
                'List'
                'Navigator'
                'Radio Buttons'
                'Slider'
                'Text'
                'Tree')
            end
            object CBAutoPosition: TCheckBox
              Left = 165
              Top = 77
              Width = 84
              Height = 17
              Caption = 'Automatic'
              TabOrder = 3
              OnClick = CBAutoPositionClick
            end
          end
          object TabItemSize: TTabSheet
            Caption = 'Size'
            ImageIndex = 1
            object Label1: TLabel
              Left = 16
              Top = 16
              Width = 32
              Height = 13
              Caption = '&Width:'
              FocusControl = EWidth
            end
            object Label8: TLabel
              Left = 16
              Top = 64
              Width = 35
              Height = 13
              Caption = '&Height:'
              FocusControl = EHeight
            end
            object EWidth: TEdit
              Left = 16
              Top = 32
              Width = 65
              Height = 21
              TabOrder = 0
              Text = '0'
            end
            object CBWidthUnits: TComboBox
              Left = 92
              Top = 32
              Width = 73
              Height = 21
              Style = csDropDownList
              ItemIndex = 0
              TabOrder = 1
              Text = 'Pixels'
              Items.Strings = (
                'Pixels'
                'Percent')
            end
            object EHeight: TEdit
              Left = 16
              Top = 80
              Width = 65
              Height = 21
              TabOrder = 2
              Text = '0'
            end
            object CBHeightUnits: TComboBox
              Left = 92
              Top = 80
              Width = 73
              Height = 21
              Style = csDropDownList
              ItemIndex = 0
              TabOrder = 3
              Text = 'Pixels'
              Items.Strings = (
                'Pixels'
                'Percent')
            end
          end
        end
      end
    end
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 273
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      object BDelete: TButton
        Left = 155
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Delete'
        Enabled = False
        TabOrder = 0
        OnClick = BDeleteClick
      end
      object BAdd: TButton
        Left = 13
        Top = 8
        Width = 116
        Height = 25
        Caption = 'New Dashboard...'
        TabOrder = 1
        OnClick = BAddClick
      end
    end
  end
  object PageControl1: TPageControl
    Left = 276
    Top = 0
    Width = 560
    Height = 624
    ActivePage = TabPreview
    Align = alClient
    TabOrder = 1
    OnChange = PageControl1Change
    object TabPreview: TTabSheet
      Caption = 'Preview'
      object BIGrid1: TBIGrid
        Left = 0
        Top = 0
        Width = 552
        Height = 596
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        ParentColor = False
        TabOrder = 0
        Visible = False
      end
      object BIVisual1: TBIVisual
        Left = 0
        Top = 0
        Width = 552
        Height = 596
        Align = alClient
        TabOrder = 1
        Dashboards = <>
        Data = <>
        Layouts = <>
        Panels = <>
      end
    end
    object TabTemplate: TTabSheet
      Caption = 'Template'
      ImageIndex = 1
      object MemoJSON: TMemo
        Left = 0
        Top = 0
        Width = 552
        Height = 596
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object PopupData: TPopupMenu
    Left = 72
    Top = 104
    object Data1: TMenuItem
      Caption = '&From Store...'
      OnClick = Data1Click
    end
    object Query1: TMenuItem
      Caption = '&Query...'
      OnClick = Query1Click
    end
    object Import1: TMenuItem
      Caption = '&Import'
      object Files1: TMenuItem
        Caption = '&Files...'
        OnClick = Files1Click
      end
      object DatabaseServer1: TMenuItem
        Caption = '&Database Server...'
        OnClick = DatabaseServer1Click
      end
      object BIWebServer1: TMenuItem
        Caption = '&BIWeb Server...'
        OnClick = BIWebServer1Click
      end
    end
  end
  object PopupItem: TPopupMenu
    OnPopup = PopupItemPopup
    Left = 72
    Top = 176
    object AddItem1: TMenuItem
      Caption = '&Add Panel...'
      OnClick = AddItem1Click
    end
    object RemoveItem1: TMenuItem
      Caption = '&Remove'
      OnClick = RemoveItem1Click
    end
    object Rename1: TMenuItem
      Caption = 'Re&name'
      OnClick = Rename1Click
    end
  end
end
