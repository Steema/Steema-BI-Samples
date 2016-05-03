object FormData: TFormData
  Left = 0
  Top = 0
  Caption = 'Form Data'
  ClientHeight = 589
  ClientWidth = 321
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
    Left = 0
    Top = 265
    Width = 321
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 324
  end
  object ListData: TListBox
    Left = 0
    Top = 0
    Width = 321
    Height = 265
    Align = alTop
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListDataClick
  end
  object ListItems: TListBox
    Left = 0
    Top = 268
    Width = 321
    Height = 321
    Align = alClient
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 1
    OnClick = ListItemsClick
  end
end
