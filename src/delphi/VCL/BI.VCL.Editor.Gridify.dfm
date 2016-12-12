object GridifyEditor: TGridifyEditor
  Left = 0
  Top = 0
  Caption = 'Gridify Editor'
  ClientHeight = 372
  ClientWidth = 385
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelButtons: TPanel
    Left = 0
    Top = 331
    Width = 385
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    Visible = False
    object PanelOk: TPanel
      Left = 196
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
        Caption = 'OK'
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 385
    Height = 331
    ActivePage = TabValue
    Align = alClient
    TabOrder = 1
    object TabValue: TTabSheet
      Caption = 'Value'
      ExplicitHeight = 262
    end
    object TabRows: TTabSheet
      Caption = 'Rows'
      ImageIndex = 1
      ExplicitHeight = 262
    end
    object TabColumns: TTabSheet
      Caption = 'Columns'
      ImageIndex = 2
    end
  end
end
