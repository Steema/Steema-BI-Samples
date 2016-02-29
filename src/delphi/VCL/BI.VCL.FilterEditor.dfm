object DataFilterEditor: TDataFilterEditor
  Left = 0
  Top = 0
  Caption = 'Filter'
  ClientHeight = 413
  ClientWidth = 521
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 372
    Width = 521
    Height = 41
    Align = alBottom
    TabOrder = 0
    object LError: TLabel
      Left = 16
      Top = 16
      Width = 3
      Height = 13
    end
    object Button1: TButton
      Left = 328
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 424
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 283
    Width = 521
    Height = 89
    Align = alBottom
    TabOrder = 1
    OnChange = Memo1Change
  end
  object TreeView1: TTreeView
    Left = 0
    Top = 0
    Width = 521
    Height = 283
    Align = alClient
    Indent = 19
    TabOrder = 2
  end
end
