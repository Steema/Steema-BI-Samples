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
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 426
    Height = 457
    ActivePage = TabSource
    Align = alClient
    TabOrder = 0
    OnChange = PageControl1Change
    ExplicitTop = 2
    object TabSource: TTabSheet
      Caption = 'Source'
      ImageIndex = 2
    end
    object TabStore: TTabSheet
      Caption = 'Store'
    end
    object TabComponent: TTabSheet
      Caption = 'Component'
      ImageIndex = 1
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 418
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object BNew: TButton
          Left = 8
          Top = 8
          Width = 75
          Height = 25
          Caption = '&New...'
          TabOrder = 0
          OnClick = BNewClick
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
    object PanelAlignButtons: TPanel
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
  object ImportMenu: TPopupMenu
    Left = 118
    Top = 40
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
    object CustomData1: TMenuItem
      Caption = '&Custom Data...'
      OnClick = CustomData1Click
    end
  end
end
