object CustomFuncTest: TCustomFuncTest
  Left = 0
  Top = 0
  Caption = 'TeeBI Custom Expression Functions'
  ClientHeight = 603
  ClientWidth = 780
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 12
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object BIGrid1: TBIGrid
    Left = 32
    Top = 40
    Width = 225
    Height = 521
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
  end
  object BIGrid2: TBIGrid
    Left = 288
    Top = 40
    Width = 465
    Height = 521
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 1
  end
end
