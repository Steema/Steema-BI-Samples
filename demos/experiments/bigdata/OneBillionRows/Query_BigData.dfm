object FormQuery: TFormQuery
  Left = 0
  Top = 0
  Caption = 'TeeBI - One Billion Row data'
  ClientHeight = 568
  ClientWidth = 869
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 221
    Top = 41
    Width = 6
    Height = 527
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 869
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitLeft = 408
    ExplicitTop = 392
    ExplicitWidth = 185
  end
  object ListBox1: TListBox
    Left = 0
    Top = 41
    Width = 221
    Height = 527
    Align = alLeft
    ItemHeight = 15
    Items.Strings = (
      'Customers per Country')
    TabOrder = 1
  end
  object BIComposer1: TBIComposer
    Left = 227
    Top = 41
    Width = 642
    Height = 527
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    TabOrder = 2
    Groups = <>
    Values = <>
    ExplicitLeft = 244
    ExplicitTop = 180
    ExplicitWidth = 400
    ExplicitHeight = 250
  end
end
