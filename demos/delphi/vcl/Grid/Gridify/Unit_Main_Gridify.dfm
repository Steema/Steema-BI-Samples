object FromGridify: TFromGridify
  Left = 0
  Top = 0
  Caption = 'TeeBI - Gridify Table'
  ClientHeight = 599
  ClientWidth = 901
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
  object Splitter1: TSplitter
    Left = 481
    Top = 41
    Height = 558
    ExplicitLeft = 456
    ExplicitTop = 272
    ExplicitHeight = 100
  end
  object BIGrid1: TBIGrid
    Left = 161
    Top = 41
    Width = 320
    Height = 558
    Align = alLeft
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
  end
  object BIGrid2: TBIGrid
    Left = 484
    Top = 41
    Width = 417
    Height = 558
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 1
    OnResize = BIGrid2Resize
  end
  object LBTest: TListBox
    Left = 0
    Top = 41
    Width = 161
    Height = 558
    Align = alLeft
    ItemHeight = 13
    Items.Strings = (
      'Happiness,[Year],[Person]'
      #39'Year'#39',['#39'Color'#39'],['#39'Person'#39']'
      #39'Person'#39','#39'Year'#39','#39'Color'#39
      #39'Color'#39','#39'Year'#39','#39'Person'#39
      #39'Color'#39','#39'Person'#39','#39'Year'#39
      #39'Rank'#39','#39'Year'#39','#39'Person'#39
      #39'Rank'#39','#39'Person'#39','#39'Year'#39
      #39'Color'#39','#39'Rank'#39','#39'Person'#39)
    TabOrder = 2
    OnClick = LBTestClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 901
    Height = 41
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 3
    object Label1: TLabel
      Left = 16
      Top = 14
      Width = 31
      Height = 13
      Caption = 'Label1'
    end
    object Label2: TLabel
      Left = 184
      Top = 14
      Width = 64
      Height = 13
      Caption = 'Source table:'
    end
    object Label3: TLabel
      Left = 484
      Top = 14
      Width = 70
      Height = 13
      Caption = 'Gridify output:'
    end
  end
end
