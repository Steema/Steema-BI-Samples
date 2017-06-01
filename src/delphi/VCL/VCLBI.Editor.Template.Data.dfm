object DataGallery: TDataGallery
  Left = 0
  Top = 0
  Caption = 'Template Data Gallery'
  ClientHeight = 547
  ClientWidth = 650
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
    Left = 233
    Top = 0
    Height = 506
    ExplicitLeft = 336
    ExplicitTop = 240
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 506
    Width = 650
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Panel2: TPanel
      Left = 465
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
  object LBData: TListBox
    Left = 0
    Top = 0
    Width = 233
    Height = 506
    Align = alLeft
    ItemHeight = 13
    TabOrder = 1
    OnClick = LBDataClick
  end
  object BIGrid1: TBIGrid
    Left = 236
    Top = 0
    Width = 414
    Height = 506
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 2
    ExplicitLeft = 233
    ExplicitWidth = 417
  end
end
