object LayoutGallery: TLayoutGallery
  Left = 0
  Top = 0
  Caption = 'Template Layout Gallery'
  ClientHeight = 591
  ClientWidth = 660
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 185
    Top = 0
    Height = 550
    ExplicitLeft = 336
    ExplicitTop = 264
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 550
    Width = 660
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Panel2: TPanel
      Left = 475
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
    object Button1: TButton
      Left = 17
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Design...'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object BIVisual1: TBIVisual
    Left = 188
    Top = 0
    Width = 472
    Height = 550
    Align = alClient
    TabOrder = 1
    Dashboards = <>
    Data = <>
    Layouts = <>
    Panels = <>
    ExplicitLeft = 185
    ExplicitWidth = 475
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 550
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = -6
    object LBLayouts: TListBox
      Left = 0
      Top = 41
      Width = 185
      Height = 509
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = LBLayoutsClick
    end
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 185
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = -6
      ExplicitTop = -6
      object CBCurrent: TCheckBox
        Left = 17
        Top = 13
        Width = 97
        Height = 17
        Caption = '&Current'
        TabOrder = 0
        OnClick = CBCurrentClick
      end
    end
  end
end
