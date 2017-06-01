object DataComponent: TDataComponent
  Left = 0
  Top = 0
  ActiveControl = Tree
  Caption = 'DataComponent'
  ClientHeight = 375
  ClientWidth = 310
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Tree: TTreeView
    Left = 0
    Top = 0
    Width = 310
    Height = 334
    Align = alClient
    HideSelection = False
    HotTrack = True
    Indent = 19
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnChange = TreeChange
    OnExpanding = TreeExpanding
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 334
    Width = 310
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    object PanelOk: TPanel
      Left = 121
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
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 152
    Top = 192
    object ViewData1: TMenuItem
      Caption = '&View Data...'
      OnClick = ViewData1Click
    end
  end
end
