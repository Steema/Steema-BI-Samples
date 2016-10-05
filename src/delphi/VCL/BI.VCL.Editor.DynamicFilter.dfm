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
  object PanelButtons: TPanel
    Left = 0
    Top = 582
    Width = 418
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 418
    Height = 582
    ActivePage = TabItems
    Align = alClient
    TabOrder = 1
    object TabData: TTabSheet
      Caption = 'Data'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object BITree1: TBITree
        Left = 0
        Top = 41
        Width = 410
        Height = 513
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        ParentColor = False
        TabOrder = 0
        OnChange = BITree1Change
      end
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 410
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object BAdd: TButton
          Left = 8
          Top = 8
          Width = 75
          Height = 25
          Caption = '&Add'
          Enabled = False
          TabOrder = 0
          OnClick = BAddClick
        end
      end
    end
    object TabItems: TTabSheet
      Caption = 'Items'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Splitter1: TSplitter
        Left = 0
        Top = 301
        Width = 410
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 41
        ExplicitWidth = 226
      end
      object CBItems: TCheckListBox
        Left = 0
        Top = 41
        Width = 410
        Height = 260
        OnClickCheck = CBItemsClickCheck
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnClick = CBItemsClick
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 410
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object CBEnabled: TCheckBox
          Left = 5
          Top = 11
          Width = 97
          Height = 17
          Caption = '&Enabled'
          TabOrder = 0
          OnClick = CBEnabledClick
        end
        object BDelete: TButton
          Left = 120
          Top = 9
          Width = 75
          Height = 25
          Caption = 'Delete'
          Enabled = False
          TabOrder = 1
          OnClick = BDeleteClick
        end
      end
      object PanelItem: TPanel
        Left = 0
        Top = 304
        Width = 410
        Height = 250
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
      end
    end
    object TabCustom: TTabSheet
      Caption = 'Custom'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object PanelCustom: TPanel
        Left = 0
        Top = 0
        Width = 410
        Height = 554
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
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
          Height = 554
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
    end
  end
end
