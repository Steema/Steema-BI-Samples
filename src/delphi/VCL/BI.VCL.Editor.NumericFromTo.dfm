object NumericFromTo: TNumericFromTo
  Left = 0
  Top = 0
  ClientHeight = 185
  ClientWidth = 385
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PanelTracks: TPanel
    Left = 0
    Top = 0
    Width = 385
    Height = 185
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = PanelTracksResize
    ExplicitLeft = 8
    ExplicitTop = 160
    ExplicitWidth = 518
    ExplicitHeight = 249
    object Label1: TLabel
      Left = 11
      Top = 7
      Width = 28
      Height = 13
      Caption = '&From:'
      FocusControl = TBFrom
    end
    object Label2: TLabel
      Left = 11
      Top = 55
      Width = 16
      Height = 13
      Caption = '&To:'
      FocusControl = TBTo
    end
    object LFrom: TLabel
      Left = 177
      Top = 7
      Width = 5
      Height = 13
      Caption = '?'
    end
    object LTo: TLabel
      Left = 177
      Top = 55
      Width = 5
      Height = 13
      Caption = '?'
    end
    object TBFrom: TTrackBar
      Left = 3
      Top = 26
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
      Top = 74
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
end
