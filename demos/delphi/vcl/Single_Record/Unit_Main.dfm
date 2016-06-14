object SingleRecordDemo: TSingleRecordDemo
  Left = 0
  Top = 0
  Caption = 'TeeBI - Single Record example'
  ClientHeight = 633
  ClientWidth = 1078
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object BIGrid1: TBIGrid
    Left = 321
    Top = 0
    Width = 425
    Height = 633
    Align = alLeft
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    OnDataChange = BIGrid1DataChange
    ExplicitLeft = 0
    Origin = 'BISamples:|SQLite_Demo|Customers'
  end
  object BIGrid2: TBIGrid
    Left = 746
    Top = 0
    Width = 332
    Height = 633
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 1
    ExplicitLeft = 248
    ExplicitTop = 280
    ExplicitWidth = 320
    ExplicitHeight = 120
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 321
    Height = 633
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
  end
end
