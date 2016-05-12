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
    Height = 375
    Align = alClient
    HideSelection = False
    HotTrack = True
    Indent = 19
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnChange = TreeChange
    OnExpanding = TreeExpanding
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
