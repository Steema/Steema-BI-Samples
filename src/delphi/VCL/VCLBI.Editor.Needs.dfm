object ProviderNeedsEditor: TProviderNeedsEditor
  Left = 0
  Top = 0
  Caption = 'Provider Needs Editor'
  ClientHeight = 415
  ClientWidth = 450
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageNeeds: TPageControl
    Left = 0
    Top = 0
    Width = 450
    Height = 374
    Align = alClient
    TabOrder = 0
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 374
    Width = 450
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object BApply: TButton
      Left = 16
      Top = 6
      Width = 121
      Height = 25
      Caption = '&Apply Changes'
      Enabled = False
      TabOrder = 0
      OnClick = BApplyClick
    end
    object PanelOk: TPanel
      Left = 261
      Top = 0
      Width = 189
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      Visible = False
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
end
