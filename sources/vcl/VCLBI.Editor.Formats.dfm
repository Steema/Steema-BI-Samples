object DataFormatEditor: TDataFormatEditor
  Left = 0
  Top = 0
  Caption = 'Data Formats'
  ClientHeight = 325
  ClientWidth = 398
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageFormat: TPageControl
    Left = 0
    Top = 0
    Width = 398
    Height = 325
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabCSV: TTabSheet
      Caption = 'CSV'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label1: TLabel
        Left = 16
        Top = 16
        Width = 45
        Height = 13
        Caption = '&Delimiter:'
        FocusControl = CBDelimiter
      end
      object Label2: TLabel
        Left = 16
        Top = 72
        Width = 34
        Height = 13
        Caption = '&Quote:'
        FocusControl = CBQuote
      end
      object Label3: TLabel
        Left = 16
        Top = 192
        Width = 67
        Height = 13
        Caption = '&Missing Value:'
        FocusControl = EMissing
      end
      object Label4: TLabel
        Left = 16
        Top = 128
        Width = 39
        Height = 13
        Caption = '&Header:'
        FocusControl = CBHeader
      end
      object Label5: TLabel
        Left = 162
        Top = 128
        Width = 33
        Height = 13
        Caption = '&Count:'
        FocusControl = CBHeader
      end
      object CBDelimiter: TComboBox
        Left = 16
        Top = 35
        Width = 129
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 0
        Text = 'Automatic'
        OnChange = CBDelimiterChange
        Items.Strings = (
          'Automatic'
          'Comma'
          'Tab'
          'Custom')
      end
      object ECustomDelimiter: TEdit
        Left = 168
        Top = 35
        Width = 49
        Height = 21
        Enabled = False
        TabOrder = 1
        OnChange = ECustomDelimiterChange
      end
      object CBQuote: TComboBox
        Left = 16
        Top = 91
        Width = 129
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 2
        Text = 'Automatic'
        OnChange = CBQuoteChange
        Items.Strings = (
          'Automatic'
          'Double'
          'Single')
      end
      object EMissing: TEdit
        Left = 16
        Top = 211
        Width = 49
        Height = 21
        TabOrder = 3
        Text = 'NA'
        OnChange = EMissingChange
      end
      object CBHeader: TComboBox
        Left = 16
        Top = 147
        Width = 129
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 4
        Text = 'Automatic'
        OnChange = CBHeaderChange
        Items.Strings = (
          'Automatic'
          'Yes'
          'No')
      end
      object Edit1: TEdit
        Left = 162
        Top = 147
        Width = 57
        Height = 21
        TabOrder = 5
        Text = '1'
        OnChange = Edit1Change
      end
      object UDHeader: TUpDown
        Left = 219
        Top = 147
        Width = 17
        Height = 21
        Associate = Edit1
        Position = 1
        TabOrder = 6
        Thousands = False
      end
    end
    object TabZip: TTabSheet
      Caption = 'Zip'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label6: TLabel
        Left = 16
        Top = 21
        Width = 50
        Height = 13
        Caption = '&Password:'
        FocusControl = EZipPassword
      end
      object EZipPassword: TEdit
        Left = 16
        Top = 40
        Width = 153
        Height = 21
        PasswordChar = '*'
        TabOrder = 0
        OnChange = EZipPasswordChange
      end
    end
    object TabExcel: TTabSheet
      Caption = 'Excel'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label7: TLabel
        Left = 10
        Top = 16
        Width = 39
        Height = 13
        Caption = '&Header:'
        FocusControl = CBHeader
      end
      object EExcelHeader: TEdit
        Left = 10
        Top = 34
        Width = 57
        Height = 21
        TabOrder = 0
        Text = '1'
        OnChange = EExcelHeaderChange
      end
      object UDExcelHeader: TUpDown
        Left = 67
        Top = 34
        Width = 16
        Height = 21
        Associate = EExcelHeader
        Position = 1
        TabOrder = 1
        Thousands = False
      end
    end
    object TabJSON: TTabSheet
      Caption = 'JSON'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object RGJSONStyle: TRadioGroup
        Left = 16
        Top = 16
        Width = 185
        Height = 105
        Caption = '&Import Style:'
        ItemIndex = 0
        Items.Strings = (
          'Flat Table'
          'Hierarchical')
        TabOrder = 0
        OnClick = RGJSONStyleClick
      end
      object RGJSONFormat: TRadioGroup
        Left = 16
        Top = 144
        Width = 185
        Height = 105
        Caption = 'File Format:'
        ItemIndex = 0
        Items.Strings = (
          'Default JSON'
          'Array of JSON lines')
        TabOrder = 1
        OnClick = RGJSONFormatClick
      end
    end
    object TabXML: TTabSheet
      Caption = 'XML'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object RGXMLStyle: TRadioGroup
        Left = 16
        Top = 16
        Width = 185
        Height = 105
        Caption = '&Import Style:'
        ItemIndex = 0
        Items.Strings = (
          'Flat Table'
          'Hierarchical')
        TabOrder = 0
        OnClick = RGXMLStyleClick
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Database'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object CBIncludeViews: TCheckBox
        Left = 16
        Top = 16
        Width = 161
        Height = 17
        Caption = '&Include Views'
        TabOrder = 0
        OnClick = CBIncludeViewsClick
      end
    end
  end
end
