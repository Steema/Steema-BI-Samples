object PanelGallery: TPanelGallery
  Left = 0
  Top = 0
  Caption = 'Template Panels Gallery'
  ClientHeight = 514
  ClientWidth = 717
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 145
    Top = 0
    Height = 473
    ExplicitLeft = 368
    ExplicitTop = 224
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 473
    Width = 717
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Panel2: TPanel
      Left = 532
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
  end
  object LBPanels: TListBox
    Left = 0
    Top = 0
    Width = 145
    Height = 473
    Align = alLeft
    ItemHeight = 13
    TabOrder = 1
    OnClick = LBPanelsClick
  end
  object BIVisual1: TBIVisual
    Left = 148
    Top = 0
    Width = 569
    Height = 473
    Align = alClient
    TabOrder = 2
    Dashboards = <>
    Data = <>
    Layouts = <>
    Panels = <>
    ExplicitLeft = 145
    ExplicitWidth = 572
  end
end
