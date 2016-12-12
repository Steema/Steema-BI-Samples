inherited BISourceEditor: TBISourceEditor
  Width = 390
  Height = 280
  Caption = 'TeeBI Source Editor'
  ExplicitWidth = 390
  ExplicitHeight = 280
  PixelsPerInch = 96
  TextHeight = 13
  inherited Pan: TPanel
    Width = 374
    inherited PanelApply: TPanel
      Left = 285
      inherited BApply: TButton
        OnClick = BApplyClick
      end
    end
  end
  object GroupFields: TScrollBox
    Left = 0
    Top = 66
    Width = 374
    Height = 175
    HelpContext = 1870
    HorzScrollBar.Range = 268
    HorzScrollBar.Visible = False
    VertScrollBar.Range = 25
    Align = alClient
    AutoScroll = False
    BorderStyle = bsNone
    TabOrder = 1
    ExplicitLeft = 8
    ExplicitTop = 152
    ExplicitWidth = 369
    ExplicitHeight = 153
    object LLabels: TLabel
      Left = 52
      Top = 8
      Width = 34
      Height = 13
      Alignment = taRightJustify
      Caption = '&Labels:'
      FocusControl = CBLabelsField
    end
    object CBLabelsField: TComboFlat
      Left = 95
      Top = 4
      Width = 173
      HelpContext = 696
      Style = csDropDown
      TabOrder = 0
      OnChange = CBLabelsFieldChange
    end
  end
  object PanelData: TPanel
    Left = 0
    Top = 32
    Width = 374
    Height = 34
    Align = alTop
    TabOrder = 2
    object LData: TLabel
      Left = 96
      Top = 8
      Width = 28
      Height = 13
      Caption = 'LData'
    end
    object BSelectData: TButton
      Left = 12
      Top = 3
      Width = 75
      Height = 25
      Caption = '&Data...'
      TabOrder = 0
      OnClick = BSelectDataClick
    end
  end
end
