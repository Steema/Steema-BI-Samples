object FormCreate: TFormCreate
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Big Data - Create and Save'
  ClientHeight = 477
  ClientWidth = 347
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 86
    Top = 92
    Width = 123
    Height = 15
    Caption = 'Quantity of Customers:'
  end
  object Label2: TLabel
    Left = 96
    Top = 121
    Width = 113
    Height = 15
    Caption = 'Quantity of Products:'
  end
  object Label3: TLabel
    Left = 60
    Top = 150
    Width = 149
    Height = 15
    Caption = 'Average sales per Customer:'
  end
  object Label4: TLabel
    Left = 24
    Top = 28
    Width = 250
    Height = 15
    Caption = 'Create a huge database with One Billion cells:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 40
    Top = 247
    Width = 126
    Height = 15
    Caption = 'Data is saved to this file:'
  end
  object LFileName: TLabel
    Left = 40
    Top = 271
    Width = 58
    Height = 15
    Caption = 'big_data.bi'
  end
  object Button1: TButton
    Left = 64
    Top = 200
    Width = 145
    Height = 25
    Caption = 'Create Database'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ECustomers: TEdit
    Left = 232
    Top = 89
    Width = 65
    Height = 23
    Alignment = taRightJustify
    TabOrder = 1
    Text = '2000000'
  end
  object EProducts: TEdit
    Left = 232
    Top = 118
    Width = 65
    Height = 23
    Alignment = taRightJustify
    TabOrder = 2
    Text = '50000'
  end
  object ESales: TEdit
    Left = 232
    Top = 147
    Width = 65
    Height = 23
    Alignment = taRightJustify
    TabOrder = 3
    Text = '100'
  end
  object Memo1: TMemo
    Left = 40
    Top = 308
    Width = 257
    Height = 89
    TabOrder = 4
  end
  object Button2: TButton
    Left = 134
    Top = 420
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 5
    OnClick = Button2Click
  end
  object ButtonView: TButton
    Left = 222
    Top = 200
    Width = 75
    Height = 25
    Caption = 'View...'
    Enabled = False
    TabOrder = 6
    OnClick = ButtonViewClick
  end
end
