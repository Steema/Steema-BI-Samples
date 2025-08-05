object FormQuery: TFormQuery
  Left = 0
  Top = 0
  Caption = 'TeeBI - One Billion big data'
  ClientHeight = 568
  ClientWidth = 869
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
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
    ExplicitWidth = 863
    object BOptions: TButton
      Left = 232
      Top = 10
      Width = 105
      Height = 25
      Caption = '&View Options...'
      Enabled = False
      TabOrder = 0
      OnClick = BOptionsClick
    end
    object BQuery: TButton
      Left = 12
      Top = 10
      Width = 75
      Height = 25
      Caption = '&Query...'
      Enabled = False
      TabOrder = 1
      OnClick = BQueryClick
    end
  end
  object LBExample: TListBox
    Left = 0
    Top = 41
    Width = 221
    Height = 527
    Align = alLeft
    ItemHeight = 15
    Items.Strings = (
      'Customers per Country'
      'Yearly Sales')
    TabOrder = 1
    OnClick = LBExampleClick
    ExplicitHeight = 510
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
    ExplicitWidth = 636
    ExplicitHeight = 510
  end
  object BIQuery1: TBIQuery
    Left = 68
    Top = 112
  end
end
