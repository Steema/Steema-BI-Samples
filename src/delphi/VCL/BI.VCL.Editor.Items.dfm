object ItemsEditor: TItemsEditor
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'ItemsEditor'
  ClientHeight = 457
  ClientWidth = 762
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 236
    Top = 0
    Height = 419
    ExplicitLeft = 392
    ExplicitTop = 200
    ExplicitHeight = 100
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 419
    Width = 762
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Panel1: TPanel
      Left = 663
      Top = 0
      Width = 99
      Height = 38
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object BOK: TButton
        Left = 8
        Top = 6
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 236
    Height = 419
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object GroupBox1: TGroupBox
      Left = 0
      Top = 0
      Width = 236
      Height = 419
      Align = alClient
      Caption = 'Fields:'
      Padding.Left = 4
      Padding.Right = 4
      TabOrder = 0
      object LBFields: TListBox
        Left = 6
        Top = 41
        Width = 224
        Height = 297
        Align = alClient
        DragMode = dmAutomatic
        ItemHeight = 13
        TabOrder = 0
        OnClick = LBFieldsClick
        OnDragDrop = LBFieldsDragDrop
        OnDragOver = LBFieldsDragOver
      end
      object Panel3: TPanel
        Left = 6
        Top = 338
        Width = 224
        Height = 79
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object Label24: TLabel
          Left = 16
          Top = 51
          Width = 24
          Height = 13
          Caption = '&Kind:'
          FocusControl = CBKind
        end
        object Label25: TLabel
          Left = 15
          Top = 18
          Width = 31
          Height = 13
          Caption = '&Name:'
          FocusControl = EName
        end
        object LDuplicateName: TLabel
          Left = 167
          Top = 18
          Width = 44
          Height = 13
          Caption = 'Duplicate'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          Visible = False
        end
        object CBKind: TComboBox
          Left = 56
          Top = 48
          Width = 97
          Height = 21
          Style = csDropDownList
          Enabled = False
          TabOrder = 0
          OnChange = CBKindChange
          Items.Strings = (
            'Integer 32bit'
            'Integer 64bit'
            'Single float'
            'Double float'
            'Extended float'
            'Text'
            'Date Time'
            'Boolean')
        end
        object EName: TEdit
          Left = 56
          Top = 15
          Width = 97
          Height = 21
          TabOrder = 1
          OnChange = ENameChange
        end
      end
      object Panel4: TPanel
        Left = 6
        Top = 15
        Width = 224
        Height = 26
        Align = alTop
        BevelOuter = bvNone
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        object SBAddField: TSpeedButton
          Left = 16
          Top = 0
          Width = 23
          Height = 22
          Hint = 'Add new field'
          Caption = '+'
          Flat = True
          OnClick = SBAddFieldClick
        end
        object SBRemoveField: TSpeedButton
          Left = 45
          Top = 0
          Width = 23
          Height = 22
          Hint = 'Remove field'
          Caption = '-'
          Enabled = False
          Flat = True
          OnClick = SBRemoveFieldClick
        end
        object Panel5: TPanel
          Left = 163
          Top = 0
          Width = 61
          Height = 26
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 0
          object SBUpField: TSpeedButton
            Left = 5
            Top = -2
            Width = 23
            Height = 22
            Hint = 'Move field up'
            Caption = '^'
            Enabled = False
            Flat = True
            OnClick = SBUpFieldClick
          end
          object SBDownField: TSpeedButton
            Left = 34
            Top = -1
            Width = 23
            Height = 22
            Hint = 'Move field down'
            Caption = 'v'
            Enabled = False
            Flat = True
            OnClick = SBDownFieldClick
          end
        end
      end
    end
  end
  object PanelGrid: TPanel
    Left = 239
    Top = 0
    Width = 523
    Height = 419
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
  end
end
