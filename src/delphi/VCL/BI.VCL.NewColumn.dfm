object AddColumn: TAddColumn
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'New Column'
  ClientHeight = 400
  ClientWidth = 429
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
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 31
    Height = 13
    Caption = '&Name:'
    FocusControl = EName
  end
  object Label2: TLabel
    Left = 24
    Top = 270
    Width = 56
    Height = 13
    Caption = '&Expression:'
    FocusControl = EExpression
  end
  object LError: TLabel
    Left = 24
    Top = 316
    Width = 3
    Height = 13
  end
  object EName: TEdit
    Left = 24
    Top = 35
    Width = 241
    Height = 21
    TabOrder = 0
    OnChange = ENameChange
  end
  object Panel1: TPanel
    Left = 0
    Top = 359
    Width = 429
    Height = 41
    Align = alBottom
    TabOrder = 1
    object Panel2: TPanel
      Left = 239
      Top = 1
      Width = 189
      Height = 39
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object BOk: TButton
        Left = 14
        Top = 8
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        Enabled = False
        ModalResult = 1
        TabOrder = 0
      end
      object Button2: TButton
        Left = 102
        Top = 8
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object RGStyle: TRadioGroup
    Left = 24
    Top = 86
    Width = 377
    Height = 163
    Caption = '&Style:'
    ItemIndex = 0
    Items.Strings = (
      'Data'
      'Lookup'
      'Detail'
      'Calculated')
    TabOrder = 2
    OnClick = RGStyleClick
  end
  object EExpression: TEdit
    Left = 24
    Top = 289
    Width = 377
    Height = 21
    Enabled = False
    TabOrder = 3
    OnChange = EExpressionChange
  end
  object CBKind: TComboBox
    Left = 112
    Top = 107
    Width = 267
    Height = 21
    Style = csDropDownList
    TabOrder = 4
    OnChange = CBKindChange
  end
  object CBLookup: TComboBox
    Left = 112
    Top = 143
    Width = 267
    Height = 21
    Style = csDropDownList
    Enabled = False
    TabOrder = 5
    OnChange = CBLookupChange
  end
  object CBDetail: TComboBox
    Left = 112
    Top = 180
    Width = 267
    Height = 21
    Style = csDropDownList
    Enabled = False
    TabOrder = 6
    OnChange = CBDetailChange
  end
end
