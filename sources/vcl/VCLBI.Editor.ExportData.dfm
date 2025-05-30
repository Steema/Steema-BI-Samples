object ExportData: TExportData
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Export Data'
  ClientHeight = 411
  ClientWidth = 427
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
    Top = 0
    Width = 427
    Height = 185
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 17
      Top = 13
      Width = 38
      Height = 13
      Caption = '&Format:'
      FocusControl = LBFormat
    end
    object Label2: TLabel
      Left = 216
      Top = 141
      Width = 23
      Height = 13
      Caption = 'Size:'
    end
    object LSize: TLabel
      Left = 216
      Top = 160
      Width = 3
      Height = 13
    end
    object LBFormat: TListBox
      Left = 17
      Top = 32
      Width = 177
      Height = 146
      ItemHeight = 13
      TabOrder = 0
      OnClick = LBFormatClick
    end
    object BCopy: TButton
      Left = 216
      Top = 32
      Width = 137
      Height = 25
      Caption = '&Copy to clipboard'
      Enabled = False
      TabOrder = 1
      OnClick = BCopyClick
    end
    object BSave: TButton
      Left = 216
      Top = 68
      Width = 137
      Height = 25
      Caption = '&Save to file...'
      Enabled = False
      TabOrder = 2
      OnClick = BSaveClick
    end
    object CBSchema: TCheckBox
      Left = 216
      Top = 106
      Width = 105
      Height = 17
      Caption = 'Schema &only'
      TabOrder = 3
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 185
    Width = 427
    Height = 31
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label3: TLabel
      Left = 17
      Top = 6
      Width = 31
      Height = 13
      Caption = 'Items:'
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 370
    Width = 427
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Panel4: TPanel
      Left = 328
      Top = 0
      Width = 99
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object BClose: TButton
        Left = 14
        Top = 8
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cl&ose'
        Default = True
        ModalResult = 2
        TabOrder = 0
      end
    end
  end
  object SaveDialog1: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Export Data to file'
    Left = 104
    Top = 88
  end
end
