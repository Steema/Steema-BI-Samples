object DataSelector: TDataSelector
  Left = 0
  Top = 0
  Caption = 'Data Select'
  ClientHeight = 498
  ClientWidth = 426
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 426
    Height = 457
    ActivePage = TabComponent
    Align = alClient
    TabOrder = 0
    OnChange = PageControl1Change
    object TabStore: TTabSheet
      Caption = 'Store'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TabComponent: TTabSheet
      Caption = 'Component'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 418
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Button1: TButton
          Left = 8
          Top = 8
          Width = 75
          Height = 25
          Caption = '&New...'
          TabOrder = 0
          OnClick = Button1Click
        end
      end
    end
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 457
    Width = 426
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Panel2: TPanel
      Left = 241
      Top = 0
      Width = 185
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object BOK: TButton
        Left = 0
        Top = 8
        Width = 75
        Height = 25
        Caption = 'OK'
        Enabled = False
        ModalResult = 1
        TabOrder = 0
      end
      object BCancel: TButton
        Left = 96
        Top = 8
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        Default = True
        ModalResult = 2
        TabOrder = 1
      end
    end
    object BClear: TButton
      Left = 19
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Clear'
      Enabled = False
      TabOrder = 1
      OnClick = BClearClick
    end
  end
  object PopupMenu2: TPopupMenu
    Left = 142
    Top = 312
    object Import1: TMenuItem
      Caption = '&Import'
      object Database1: TMenuItem
        Caption = '&Database...'
        OnClick = Database1Click
      end
      object Files1: TMenuItem
        Caption = '&Files...'
        OnClick = Files1Click
      end
      object Web1: TMenuItem
        Caption = '&BI Web...'
        OnClick = Web1Click
      end
    end
    object Query1: TMenuItem
      Caption = '&Query...'
      OnClick = Query1Click
    end
  end
end
