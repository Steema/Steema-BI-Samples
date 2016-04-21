object FromBIStore: TFromBIStore
  Left = 0
  Top = 0
  Caption = 'Load Data from a TeeBI Store'
  ClientHeight = 484
  ClientWidth = 621
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
  object Panel1: TPanel
    Left = 0
    Top = 443
    Width = 621
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Panel2: TPanel
      Left = 518
      Top = 0
      Width = 103
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object Button4: TButton
        Left = 12
        Top = 8
        Width = 75
        Height = 25
        Caption = '&Close'
        TabOrder = 0
        OnClick = Button4Click
      end
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 621
    Height = 443
    ActivePage = TabStores
    Align = alClient
    TabOrder = 1
    object TabStores: TTabSheet
      Caption = 'Stores'
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 185
        Height = 415
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object LBStores: TListBox
          Left = 0
          Top = 0
          Width = 185
          Height = 97
          Align = alTop
          ItemHeight = 13
          TabOrder = 0
          OnClick = LBStoresClick
        end
        object LBData: TListBox
          Left = 0
          Top = 97
          Width = 185
          Height = 318
          Align = alClient
          ItemHeight = 13
          TabOrder = 1
          OnClick = LBDataClick
        end
      end
      object BIGrid1: TBIGrid
        Left = 185
        Top = 0
        Width = 428
        Height = 415
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        ParentColor = False
        TabOrder = 1
        ShowItems = True
      end
    end
    object TabRemote: TTabSheet
      Caption = 'Remote Steema Web'
      ImageIndex = 1
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 613
        Height = 41
        Align = alTop
        TabOrder = 0
        object Button1: TButton
          Left = 16
          Top = 5
          Width = 75
          Height = 25
          Caption = '&Connect'
          TabOrder = 0
          OnClick = Button1Click
        end
      end
      object LBRemoteDatas: TListBox
        Left = 0
        Top = 41
        Width = 121
        Height = 374
        Align = alLeft
        ItemHeight = 13
        TabOrder = 1
        OnClick = LBRemoteDatasClick
      end
      object BIGrid2: TBIGrid
        Left = 121
        Top = 41
        Width = 492
        Height = 374
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        ParentColor = False
        TabOrder = 2
      end
    end
  end
end
