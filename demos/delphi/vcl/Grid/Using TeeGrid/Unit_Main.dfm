object FormTeeGrid: TFormTeeGrid
  Left = 0
  Top = 0
  Caption = 'TBIGrid using TeeGrid - Example'
  ClientHeight = 561
  ClientWidth = 662
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
  object BIGrid1: TBIGrid
    Left = 0
    Top = 41
    Width = 662
    Height = 520
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    ExplicitLeft = 40
    ExplicitTop = 96
    ExplicitWidth = 585
    ExplicitHeight = 441
    Origin = '|SQLite_Demo|Customers'
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 662
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 248
    ExplicitTop = 280
    ExplicitWidth = 185
    object Button1: TButton
      Left = 16
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Data...'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
end
