object StoreEditor: TStoreEditor
  Left = 0
  Top = 0
  Caption = 'Store Manager'
  ClientHeight = 429
  ClientWidth = 504
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 504
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 11
      Top = 9
      Width = 75
      Height = 25
      Caption = '&Add...'
      TabOrder = 0
      OnClick = Button1Click
    end
    object BRemove: TButton
      Left = 104
      Top = 9
      Width = 75
      Height = 25
      Caption = '&Remove'
      Enabled = False
      TabOrder = 1
      OnClick = BRemoveClick
    end
  end
  object LBStores: TListBox
    Left = 0
    Top = 41
    Width = 185
    Height = 347
    Style = lbOwnerDrawFixed
    Align = alLeft
    ItemHeight = 13
    TabOrder = 1
    OnClick = LBStoresClick
    OnDrawItem = LBStoresDrawItem
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 388
    Width = 504
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Panel4: TPanel
      Left = 400
      Top = 0
      Width = 104
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object BClose: TButton
        Left = 13
        Top = 8
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Close'
        Default = True
        ModalResult = 1
        TabOrder = 0
        OnClick = BCloseClick
      end
    end
    object BRename: TButton
      Left = 16
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Re&name...'
      Enabled = False
      TabOrder = 1
      OnClick = BRenameClick
    end
    object CBDefault: TCheckBox
      Left = 114
      Top = 10
      Width = 97
      Height = 17
      Caption = '&Default'
      Enabled = False
      TabOrder = 2
      OnClick = CBDefaultClick
    end
  end
  object PageControl1: TPageControl
    Left = 185
    Top = 41
    Width = 319
    Height = 347
    ActivePage = TabWeb
    Align = alClient
    TabOrder = 3
    object TabFolder: TTabSheet
      Caption = 'Folder'
      object Label1: TLabel
        Left = 16
        Top = 16
        Width = 34
        Height = 13
        Caption = '&Folder:'
        FocusControl = EFolder
      end
      object SpeedButton1: TSpeedButton
        Left = 263
        Top = 35
        Width = 23
        Height = 22
        Caption = '...'
        OnClick = SpeedButton1Click
      end
      object LBadFolder: TLabel
        Left = 19
        Top = 72
        Width = 101
        Height = 13
        Caption = 'Folder does not exist'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 206
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Visible = False
      end
      object EFolder: TEdit
        Left = 16
        Top = 35
        Width = 241
        Height = 21
        TabOrder = 0
        OnChange = EFolderChange
      end
    end
    object TabWeb: TTabSheet
      Caption = 'Web'
      ImageIndex = 1
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 64
    Top = 72
    object Folder1: TMenuItem
      Caption = '&Folder'
      OnClick = Folder1Click
    end
    object WebServer1: TMenuItem
      Caption = '&Web Server'
      OnClick = WebServer1Click
    end
  end
end
