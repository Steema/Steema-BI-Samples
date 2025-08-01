object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'TeeBI Composer example'
  ClientHeight = 905
  ClientWidth = 1391
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 345
    Top = 41
    Width = 6
    Height = 864
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 1391
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitTop = 3
    object Button1: TButton
      Left = 5
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Options...'
      TabOrder = 0
      OnClick = Button1Click
    end
    object ButtonQuery: TButton
      Left = 136
      Top = 11
      Width = 75
      Height = 25
      Caption = 'Query...'
      Enabled = False
      TabOrder = 1
      OnClick = ButtonQueryClick
    end
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 41
    Width = 345
    Height = 864
    Align = alLeft
    Caption = 'PanelLeft'
    TabOrder = 1
    ExplicitLeft = 928
    ExplicitTop = 104
    ExplicitHeight = 745
    object PanelExample: TPanel
      Left = 1
      Top = 1
      Width = 343
      Height = 33
      Align = alTop
      TabOrder = 0
      object Label1: TLabel
        Left = 10
        Top = 7
        Width = 50
        Height = 15
        Caption = 'Example:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object LBTest: TListBox
      Left = 1
      Top = 34
      Width = 343
      Height = 829
      Align = alClient
      ItemHeight = 15
      TabOrder = 1
      OnClick = LBTestClick
      ExplicitLeft = 2
      ExplicitTop = 30
    end
  end
  object PanelRight: TPanel
    Left = 351
    Top = 41
    Width = 1040
    Height = 864
    Align = alClient
    TabOrder = 2
    ExplicitLeft = 944
    ExplicitTop = 88
    ExplicitWidth = 417
    ExplicitHeight = 769
    object Splitter2: TSplitter
      Left = 1
      Top = 471
      Width = 1038
      Height = 6
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 444
    end
    object MemoSQL: TMemo
      Left = 1
      Top = 774
      Width = 1038
      Height = 89
      Align = alBottom
      ScrollBars = ssBoth
      TabOrder = 0
      ExplicitLeft = -88
      ExplicitTop = 680
      ExplicitWidth = 505
    end
    object BIGrid1: TBIGrid
      Left = 1
      Top = 477
      Width = 1038
      Height = 297
      Align = alBottom
      UseDockManager = False
      ParentBackground = False
      ParentColor = False
      TabOrder = 1
      Alternate.Enabled = True
      ExplicitLeft = -88
      ExplicitTop = 440
      ExplicitWidth = 505
    end
    object BIComposer1: TBIComposer
      Left = 1
      Top = 1
      Width = 1038
      Height = 470
      Align = alClient
      UseDockManager = False
      ParentBackground = False
      TabOrder = 2
      Groups = <>
      Values = <>
      ExplicitLeft = -88
      ExplicitTop = 50
      ExplicitWidth = 505
      ExplicitHeight = 353
    end
  end
end
