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
  OnCreate = FormCreate
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
    object LErrorFrom: TLabel
      Left = 194
      Top = 7
      Width = 3
      Height = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LErrorTo: TLabel
      Left = 194
      Top = 59
      Width = 3
      Height = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object TBFrom: TTrackBar
      Left = 0
      Top = 31
      Width = 373
      Height = 30
      Max = 100
      Frequency = 2
      TabOrder = 0
      ThumbLength = 14
      OnChange = TBFromChange
    end
    object TBTo: TTrackBar
      Left = 0
      Top = 78
      Width = 373
      Height = 30
      Max = 100
      Frequency = 2
      Position = 100
      TabOrder = 1
      ThumbLength = 14
      OnChange = TBToChange
    end
    object EFrom: TEdit
      Left = 69
      Top = 4
      Width = 121
      Height = 21
      TabOrder = 2
      OnChange = EFromChange
    end
    object ETo: TEdit
      Left = 69
      Top = 56
      Width = 121
      Height = 21
      TabOrder = 3
      OnChange = EToChange
    end
    object CBFrom: TCheckBox
      Left = 11
      Top = 6
      Width = 56
      Height = 17
      Caption = 'From:'
      TabOrder = 4
      OnClick = CBFromClick
    end
    object CBTo: TCheckBox
      Left = 11
      Top = 58
      Width = 56
      Height = 17
      Caption = 'To:'
      TabOrder = 5
      OnClick = CBToClick
    end
  end
end
