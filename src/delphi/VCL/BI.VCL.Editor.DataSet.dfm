object BIDataSetEditor: TBIDataSetEditor
  Left = 0
  Top = 0
  Caption = 'DataSet Editor'
  ClientHeight = 444
  ClientWidth = 522
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 522
    Height = 56
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object RGMode: TRadioGroup
      Left = 9
      Top = 6
      Width = 240
      Height = 43
      Caption = '&Mode:'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'Data'
        'Query'
        'Summary')
      TabOrder = 0
      OnClick = RGModeClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 403
    Width = 522
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object CBPreview: TCheckBox
      Left = 16
      Top = 12
      Width = 97
      Height = 17
      Caption = '&Preview'
      TabOrder = 0
      OnClick = CBPreviewClick
    end
    object Panel4: TPanel
      Left = 337
      Top = 0
      Width = 185
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object Button1: TButton
        Left = 13
        Top = 8
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 101
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
  object PanelMain: TPanel
    Left = 0
    Top = 56
    Width = 522
    Height = 347
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
  end
  object Backup: TBIDataset
    RowNumbers = False
    Left = 256
    Top = 152
  end
end
