object HopsViewer: THopsViewer
  Left = 0
  Top = 0
  Caption = 'Hops Viewer'
  ClientHeight = 320
  ClientWidth = 528
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
  object LMain: TLabel
    Left = 64
    Top = 15
    Width = 27
    Height = 13
    Caption = 'LMain'
  end
  object LCount: TLabel
    Left = 499
    Top = 181
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label1: TLabel
    Left = 16
    Top = 15
    Width = 26
    Height = 13
    Caption = 'Main:'
  end
  object Label2: TLabel
    Left = 16
    Top = 181
    Width = 36
    Height = 13
    Caption = 'Parent:'
  end
  object LParent: TLabel
    Left = 58
    Top = 181
    Width = 3
    Height = 13
  end
  object LBHops: TListBox
    Left = 16
    Top = 48
    Width = 201
    Height = 121
    ItemHeight = 13
    TabOrder = 0
    OnClick = LBHopsClick
  end
  object LBItems: TListBox
    Left = 16
    Top = 200
    Width = 489
    Height = 97
    ItemHeight = 13
    TabOrder = 1
  end
end
