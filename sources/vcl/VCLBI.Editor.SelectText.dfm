object SelectTextItems: TSelectTextItems
  Left = 0
  Top = 0
  Caption = 'Select Text Items'
  ClientHeight = 455
  ClientWidth = 488
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageText: TPageControl
    Left = 0
    Top = 0
    Width = 488
    Height = 455
    ActivePage = TabMultiText
    Align = alClient
    TabOrder = 0
    OnChange = PageTextChange
    object TabMultiText: TTabSheet
      Caption = 'Multiple'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TabSingleText: TTabSheet
      Caption = 'Single'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LBSingleText: TListBox
        Left = 0
        Top = 0
        Width = 480
        Height = 427
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnClick = LBSingleTextClick
      end
    end
  end
end
