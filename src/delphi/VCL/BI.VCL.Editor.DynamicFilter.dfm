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
  object PanelCustom: TPanel
    Left = 0
    Top = 0
    Width = 418
    Height = 57
    Align = alTop
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
    TabOrder = 1
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
    Top = 57
    Width = 418
    Height = 525
    ActivePage = TabItems
    Align = alClient
    TabOrder = 2
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
        Height = 456
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
        Top = 264
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
        Height = 223
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
      object PageItem: TPageControl
        Left = 0
        Top = 267
        Width = 410
        Height = 230
        ActivePage = TabNumeric
        Align = alBottom
        TabOrder = 2
        Visible = False
        object TabDateTime: TTabSheet
          Caption = 'Date Time'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
        object TabBoolean: TTabSheet
          Caption = 'Boolean'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object CBTrue: TCheckBox
            Left = 16
            Top = 16
            Width = 97
            Height = 17
            Caption = 'True'
            TabOrder = 0
            OnClick = CBTrueClick
          end
          object CBFalse: TCheckBox
            Left = 16
            Top = 39
            Width = 97
            Height = 17
            Caption = 'False'
            TabOrder = 1
            OnClick = CBFalseClick
          end
        end
        object TabNumeric: TTabSheet
          Caption = 'Numeric'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object PageNumeric: TPageControl
            Left = 0
            Top = 0
            Width = 402
            Height = 202
            ActivePage = TabNumericRange
            Align = alClient
            TabOrder = 0
            object TabNumericRange: TTabSheet
              Caption = 'Range'
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
            end
            object TabNumericSelected: TTabSheet
              Caption = 'Selected'
              ImageIndex = 1
              ExplicitLeft = 1
              ExplicitTop = 30
              ExplicitWidth = 0
              ExplicitHeight = 0
            end
          end
        end
        object TabText: TTabSheet
          Caption = 'Text'
          ImageIndex = 3
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object PageControl2: TPageControl
            Left = 0
            Top = 0
            Width = 402
            Height = 202
            ActivePage = TabIncluded
            Align = alClient
            TabOrder = 0
            object TabIncluded: TTabSheet
              Caption = 'Include'
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
            end
            object TabExcluded: TTabSheet
              Caption = 'Exclude'
              ImageIndex = 1
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
            end
          end
        end
      end
    end
  end
end
