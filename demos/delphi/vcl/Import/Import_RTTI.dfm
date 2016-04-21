object FromRTTI: TFromRTTI
  Left = 0
  Top = 0
  Caption = 'Import using RTTI'
  ClientHeight = 396
  ClientWidth = 573
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 573
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 200
    ExplicitTop = 200
    ExplicitWidth = 185
    object Label1: TLabel
      Left = 176
      Top = 14
      Width = 348
      Height = 13
      Caption = 
        'Please see the ORM_RTTI.dpr demo for a more advanced usage of RT' +
        'TI'
    end
    object Button1: TButton
      Left = 16
      Top = 10
      Width = 145
      Height = 25
      Caption = 'Import TPerson objects '
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object BIGrid1: TBIGrid
    Left = 0
    Top = 41
    Width = 573
    Height = 355
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 1
    ExplicitLeft = 296
    ExplicitTop = 216
    ExplicitWidth = 100
    ExplicitHeight = 40
  end
end
