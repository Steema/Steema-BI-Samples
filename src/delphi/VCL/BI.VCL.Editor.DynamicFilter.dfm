object DynamicFilterEditor: TDynamicFilterEditor
  Left = 0
  Top = 0
  Caption = 'Filter Editor'
  ClientHeight = 623
  ClientWidth = 418
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
  object BITree1: TBITree
    Left = 0
    Top = 57
    Width = 418
    Height = 295
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    OnChange = BITree1Change
    ExplicitHeight = 525
  end
  object PanelCustom: TPanel
    Left = 0
    Top = 0
    Width = 418
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = PanelCustomResize
    object SBCustom: TSpeedButton
      Left = 386
      Top = 8
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = SBCustomClick
    end
    object LError: TLabel
      Left = 95
      Top = 38
      Width = 3
      Height = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 89
      Height = 57
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object CBCustom: TComboBox
        Left = 8
        Top = 9
        Width = 65
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 0
        Text = 'and'
        OnChange = CBCustomChange
        Items.Strings = (
          'and'
          'or')
      end
    end
    object ECustom: TEdit
      Left = 95
      Top = 9
      Width = 282
      Height = 21
      TabOrder = 1
      OnChange = ECustomChange
    end
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 582
    Width = 418
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Panel1: TPanel
      Left = 233
      Top = 0
      Width = 185
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object Button1: TButton
        Left = 8
        Top = 8
        Width = 75
        Height = 25
        Caption = 'OK'
        ModalResult = 1
        TabOrder = 0
      end
      object Button2: TButton
        Left = 96
        Top = 8
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        Default = True
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object PageItem: TPageControl
    Left = 0
    Top = 352
    Width = 418
    Height = 230
    ActivePage = TabDateTime
    Align = alBottom
    TabOrder = 3
    Visible = False
    object TabDateTime: TTabSheet
      Caption = 'Date Time'
      ExplicitTop = 6
      ExplicitHeight = 108
    end
  end
end
