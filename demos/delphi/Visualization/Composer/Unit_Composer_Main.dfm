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
    ExplicitWidth = 1385
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
      Top = 9
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
    ExplicitHeight = 847
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
      ExplicitHeight = 812
    end
  end
  object PanelRight: TPanel
    Left = 351
    Top = 41
    Width = 1040
    Height = 864
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 1034
    ExplicitHeight = 847
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
      ScrollBars = ssVertical
      TabOrder = 0
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
      ExplicitTop = 460
      ExplicitWidth = 1032
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
      ExplicitWidth = 1032
      ExplicitHeight = 453
    end
  end
end
