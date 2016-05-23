object NumericFromTo: TNumericFromTo
  Left = 0
  Top = 0
  ClientHeight = 137
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 11
    Top = 16
    Width = 28
    Height = 13
    Caption = '&From:'
    FocusControl = TBFrom
  end
  object Label2: TLabel
    Left = 11
    Top = 64
    Width = 16
    Height = 13
    Caption = '&To:'
    FocusControl = TBTo
  end
  object LFrom: TLabel
    Left = 177
    Top = 16
    Width = 5
    Height = 13
    Caption = '?'
  end
  object LTo: TLabel
    Left = 177
    Top = 64
    Width = 5
    Height = 13
    Caption = '?'
  end
  object TBFrom: TTrackBar
    Left = 3
    Top = 35
    Width = 373
    Height = 30
    Max = 100
    Frequency = 2
    TabOrder = 0
    ThumbLength = 14
    OnChange = TBFromChange
  end
  object TBTo: TTrackBar
    Left = 6
    Top = 83
    Width = 373
    Height = 30
    Max = 100
    Frequency = 2
    Position = 100
    TabOrder = 1
    ThumbLength = 14
    OnChange = TBToChange
  end
end
