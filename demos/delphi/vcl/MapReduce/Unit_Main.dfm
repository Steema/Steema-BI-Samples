object DemoForm: TDemoForm
  Left = 0
  Top = 0
  Caption = 'TeeBI - Map Reduce Example'
  ClientHeight = 677
  ClientWidth = 1085
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
  object Button1: TButton
    Left = 32
    Top = 24
    Width = 75
    Height = 25
    Caption = '&Run !'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 32
    Top = 72
    Width = 321
    Height = 113
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object BIGrid1: TBIGrid
    Left = 32
    Top = 209
    Width = 321
    Height = 460
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 2
  end
  object BIGrid2: TBIGrid
    Left = 376
    Top = 24
    Width = 689
    Height = 645
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 3
  end
  object CBParallel: TCheckBox
    Left = 152
    Top = 28
    Width = 97
    Height = 17
    Caption = '&Parallel'
    TabOrder = 4
    OnClick = CBParallelClick
  end
end
