object SplitEditor: TSplitEditor
  Left = 0
  Top = 0
  Caption = 'Data Split Editor'
  ClientHeight = 254
  ClientWidth = 249
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object PanelButtons: TPanel
    Left = 0
    Top = 213
    Width = 249
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 248
    ExplicitWidth = 273
    object PanelOk: TPanel
      Left = 60
      Top = 0
      Width = 189
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 84
      object BOk: TButton
        Left = 14
        Top = 8
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object BCancel: TButton
        Left = 102
        Top = 8
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object PageBy: TPageControl
    Left = 0
    Top = 89
    Width = 249
    Height = 124
    ActivePage = TabCount
    Align = alClient
    TabOrder = 1
    OnChange = PageByChange
    ExplicitLeft = -111
    ExplicitTop = 247
    ExplicitWidth = 289
    ExplicitHeight = 193
    object TabPercent: TTabSheet
      Caption = 'By Percent'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 281
      ExplicitHeight = 165
      object LPercent: TLabel
        Left = 11
        Top = 11
        Width = 41
        Height = 13
        Caption = '&Percent:'
        FocusControl = TBPercent
      end
      object LValue: TLabel
        Left = 189
        Top = 11
        Width = 23
        Height = 13
        Alignment = taRightJustify
        Caption = '50%'
        FocusControl = TBPercent
      end
      object TBPercent: TTrackBar
        Left = 3
        Top = 30
        Width = 217
        Height = 30
        Max = 100
        Frequency = 5
        Position = 50
        TabOrder = 0
        ThumbLength = 14
        OnChange = TBPercentChange
      end
    end
    object TabCount: TTabSheet
      Caption = 'By Count'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 281
      ExplicitHeight = 165
      object ECount: TEdit
        Left = 12
        Top = 16
        Width = 101
        Height = 21
        Alignment = taRightJustify
        NumbersOnly = True
        TabOrder = 0
        Text = '0'
        OnChange = ECountChange
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 249
    Height = 89
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 427
    object RGMode: TRadioGroup
      Left = 16
      Top = 8
      Width = 185
      Height = 73
      Caption = '&Mode:'
      ItemIndex = 0
      Items.Strings = (
        'Random'
        'Sequential')
      TabOrder = 0
    end
  end
end
