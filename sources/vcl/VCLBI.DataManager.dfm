object DataManager: TDataManager
  Left = 320
  Top = 203
  ActiveControl = ESearch
  Caption = 'Data Manager'
  ClientHeight = 492
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 224
    Top = 41
    Height = 410
    ExplicitHeight = 386
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 451
    Width = 633
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object PanelOk: TPanel
      Left = 444
      Top = 0
      Width = 189
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object BOk: TButton
        Left = 14
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Select'
        Default = True
        Enabled = False
        ModalResult = 1
        TabOrder = 0
      end
      object BCancel: TButton
        Left = 102
        Top = 8
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
    object BAdd: TButton
      Left = 9
      Top = 6
      Width = 75
      Height = 25
      Caption = '&Add...'
      Enabled = False
      TabOrder = 1
      OnClick = BAddClick
    end
    object BDelete: TButton
      Left = 102
      Top = 6
      Width = 75
      Height = 25
      Caption = '&Remove...'
      Enabled = False
      TabOrder = 2
      OnClick = BDeleteClick
    end
    object BRename: TButton
      Left = 198
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Re&name...'
      Enabled = False
      TabOrder = 3
      OnClick = BRenameClick
    end
  end
  object PanelSearch: TPanel
    Left = 0
    Top = 0
    Width = 633
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = PanelSearchResize
    object PanelStores: TPanel
      Left = 359
      Top = 0
      Width = 274
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      OnResize = PanelStoresResize
      object Label1: TLabel
        Left = 11
        Top = 14
        Width = 30
        Height = 13
        Caption = 'S&tore:'
        FocusControl = CBStores
      end
      object CBStores: TComboBox
        Left = 44
        Top = 11
        Width = 145
        Height = 21
        Style = csDropDownList
        DropDownCount = 25
        TabOrder = 0
        OnChange = CBStoresChange
      end
      object PanelManage: TPanel
        Left = 196
        Top = 0
        Width = 78
        Height = 41
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitLeft = 195
        object BManageStores: TButton
          Left = 5
          Top = 9
          Width = 69
          Height = 25
          Caption = '&Manage...'
          TabOrder = 0
          OnClick = BManageStoresClick
        end
      end
    end
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 359
      Height = 41
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object LSearch: TLabel
        Left = 9
        Top = 14
        Width = 37
        Height = 13
        Caption = '&Search:'
        FocusControl = ESearch
      end
      object ESearch: TEdit
        Left = 52
        Top = 11
        Width = 172
        Height = 21
        TabOrder = 0
        OnChange = ESearchChange
      end
    end
  end
  object Tree: TTreeView
    Left = 0
    Top = 41
    Width = 224
    Height = 410
    Align = alLeft
    HideSelection = False
    HotTrack = True
    Indent = 19
    PopupMenu = DataMenu
    ReadOnly = True
    TabOrder = 2
    OnChange = TreeChange
    OnDblClick = TreeDblClick
    OnExpanding = TreeExpanding
  end
  object PageControl1: TPageControl
    Left = 227
    Top = 41
    Width = 406
    Height = 410
    ActivePage = TabData
    Align = alClient
    TabOrder = 3
    OnChange = PageControl1Change
    object TabSettings: TTabSheet
      Caption = 'Settings'
    end
    object TabData: TTabSheet
      Caption = 'Data'
      ImageIndex = 1
      object MemoImportLog: TMemo
        Left = 0
        Top = 273
        Width = 398
        Height = 109
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
        Visible = False
        WordWrap = False
      end
      object PanelDataTop: TPanel
        Left = 0
        Top = 0
        Width = 398
        Height = 273
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label2: TLabel
          Left = 11
          Top = 44
          Width = 57
          Height = 13
          Caption = 'Last import:'
        end
        object LLastImport: TLabel
          Left = 11
          Top = 63
          Width = 5
          Height = 13
          Caption = '?'
        end
        object BViewData: TButton
          Left = 129
          Top = 10
          Width = 102
          Height = 25
          Caption = '&View Data...'
          Enabled = False
          TabOrder = 0
          OnClick = BViewDataClick
        end
        object MemoDataInfo: TMemo
          Left = 11
          Top = 82
          Width = 254
          Height = 89
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 1
        end
        object BImportNow: TButton
          Left = 11
          Top = 10
          Width = 75
          Height = 25
          Caption = 'Import &Now'
          TabOrder = 2
          OnClick = BImportNowClick
        end
        object CBParallel: TCheckBox
          Left = 11
          Top = 219
          Width = 190
          Height = 17
          Caption = 'Use multiple CPU'
          TabOrder = 3
          OnClick = CBParallelClick
        end
        object CBStoponerrors: TCheckBox
          Left = 11
          Top = 242
          Width = 182
          Height = 17
          Caption = 'Stop on errors'
          TabOrder = 4
        end
        object ImportProgress: TProgressBar
          Left = 11
          Top = 189
          Width = 254
          Height = 17
          TabOrder = 5
          Visible = False
        end
      end
    end
    object TabLinks: TTabSheet
      Caption = 'Links'
      ImageIndex = 2
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 96
    Top = 256
    object Files1: TMenuItem
      Caption = '&Files or folders'
      OnClick = Files1Click
    end
    object DatabaseServer1: TMenuItem
      Caption = '&Database Server'
      OnClick = DatabaseServer1Click
    end
    object BIWeb1: TMenuItem
      Caption = '&BI Web'
      OnClick = BIWeb1Click
    end
    object Custommanual1: TMenuItem
      Caption = '&Custom (manual)'
      OnClick = Custommanual1Click
    end
  end
  object DataMenu: TPopupMenu
    Left = 88
    Top = 120
    object ViewData1: TMenuItem
      Caption = '&View Data...'
      OnClick = ViewData1Click
    end
  end
end
