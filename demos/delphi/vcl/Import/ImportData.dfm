object ImportDataMain: TImportDataMain
  Left = 0
  Top = 0
  ActiveControl = Button2
  BorderStyle = bsDialog
  Caption = 'TeeBI Importing Data Example'
  ClientHeight = 467
  ClientWidth = 620
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 96
    Top = 32
    Width = 431
    Height = 16
    Caption = 
      'Data can be imported from multiple sources and different formats' +
      '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 144
    Top = 127
    Width = 340
    Height = 13
    Caption = 
      'For maximum flexibility, setting properties to customize import ' +
      'features'
  end
  object Label3: TLabel
    Left = 136
    Top = 207
    Width = 358
    Height = 13
    Caption = 
      'One line of code for easy import of any supported file with defa' +
      'ult settings'
  end
  object Label4: TLabel
    Left = 192
    Top = 287
    Width = 228
    Height = 13
    Caption = 'From Memos, Database Connections, Datasets '
  end
  object Label5: TLabel
    Left = 112
    Top = 54
    Width = 398
    Height = 13
    Caption = 
      'CSV, JSON, XML, Text, Excel, ClientDataset, FireDAC, dbExpress a' +
      'nd many others'
  end
  object Label6: TLabel
    Left = 168
    Top = 367
    Width = 269
    Height = 13
    Caption = 'Get TDataItem objects from a local or remote web Store'
  end
  object Button1: TButton
    Left = 144
    Top = 176
    Width = 337
    Height = 25
    Caption = 'Import data files automatically based on file extension'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 144
    Top = 96
    Width = 337
    Height = 25
    Caption = 'Import data using classes by code'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 144
    Top = 256
    Width = 337
    Height = 25
    Caption = 'Import data from Components'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 426
    Width = 620
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitLeft = 224
    ExplicitTop = 208
    ExplicitWidth = 185
    object Button4: TButton
      Left = 272
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Close'
      TabOrder = 0
      OnClick = Button4Click
    end
  end
  object Button5: TButton
    Left = 144
    Top = 336
    Width = 337
    Height = 25
    Caption = 'Load data from a TeeBI Store'
    TabOrder = 4
    OnClick = Button5Click
  end
end
