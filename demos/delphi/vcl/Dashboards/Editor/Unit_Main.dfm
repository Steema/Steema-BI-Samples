object Form34: TForm34
  Left = 0
  Top = 0
  Caption = 'Form34'
  ClientHeight = 594
  ClientWidth = 1154
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object BIVisual1: TBIVisual
    Left = 24
    Top = 39
    Width = 945
    Height = 529
    TabOrder = 0
    Dashboards = <
      item
        Name = 'customers'
        Items = <
          item
            Name = 'names'
            Panel = 'names'
            Text = 'CustomerID'
          end
          item
            Name = 'customers'
            Panel = 'customers'
            Text = 'Item'
          end>
      end>
    Data = <
      item
        Name = 'customer names'
      end
      item
        Name = '#customer'
      end
      item
        Name = 'customer items'
      end>
    Layouts = <>
    Panels = <
      item
        Name = 'names'
        Data = 'customer names'
        Kind = List
        Target = '#customer'
      end
      item
        Name = 'customers'
        Data = 'customer items'
        Kind = Grid
      end>
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Edit...'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 120
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Load...'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 1000
    Top = 39
    Width = 112
    Height = 274
    Lines.Strings = (
      'Country Population'
      'Spain   47500000'
      'USA 350000000'
      'Brazil 80000000')
    TabOrder = 3
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.json'
    Filter = 'Template files|*.json'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Load Dashboard Template'
    Left = 192
    Top = 104
  end
end
