object RTTIDemo: TRTTIDemo
  Left = 0
  Top = 0
  Caption = 'TeeBI ORM Data Mapping Example'
  ClientHeight = 657
  ClientWidth = 1094
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
  object Splitter1: TSplitter
    Left = 0
    Top = 405
    Width = 1094
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 0
    ExplicitWidth = 408
  end
  object BIGrid2: TBIGrid
    Left = 0
    Top = 408
    Width = 1094
    Height = 249
    Align = alBottom
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1094
    Height = 405
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object BIGrid1: TBIGrid
      Left = 0
      Top = 41
      Width = 1094
      Height = 364
      Align = alClient
      UseDockManager = False
      ParentBackground = False
      ParentColor = False
      TabOrder = 0
    end
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 1094
      Height = 41
      Align = alTop
      TabOrder = 1
      object Label1: TLabel
        Left = 16
        Top = 14
        Width = 31
        Height = 13
        Caption = 'Label1'
      end
      object Button1: TButton
        Left = 216
        Top = 10
        Width = 105
        Height = 25
        Caption = 'Remove 1 item'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 336
        Top = 10
        Width = 97
        Height = 25
        Caption = 'Update 1 Item'
        TabOrder = 1
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 456
        Top = 10
        Width = 65
        Height = 25
        Caption = 'Clear'
        TabOrder = 2
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 641
        Top = 10
        Width = 88
        Height = 25
        Caption = 'Add Samples'
        TabOrder = 3
        OnClick = Button4Click
      end
      object Button5: TButton
        Left = 768
        Top = 10
        Width = 89
        Height = 25
        Caption = 'Benchmark Add'
        TabOrder = 4
        OnClick = Button5Click
      end
      object Button6: TButton
        Left = 872
        Top = 10
        Width = 105
        Height = 25
        Caption = 'Benchmark Update'
        TabOrder = 5
        OnClick = Button6Click
      end
      object Button7: TButton
        Left = 544
        Top = 10
        Width = 75
        Height = 25
        Caption = 'Find Item'
        TabOrder = 6
        OnClick = Button7Click
      end
      object Button8: TButton
        Left = 110
        Top = 10
        Width = 75
        Height = 25
        Caption = 'Data...'
        TabOrder = 7
        OnClick = Button8Click
      end
    end
  end
end
