object DatasetSpeed: TDatasetSpeed
  Left = 0
  Top = 0
  Caption = 'Speed BIDataSet vs FDMemTable'
  ClientHeight = 423
  ClientWidth = 592
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 11
    Width = 43
    Height = 13
    Caption = 'Samples:'
  end
  object Label2: TLabel
    Left = 208
    Top = 11
    Width = 34
    Height = 13
    Caption = '&Folder:'
    FocusControl = ETemp
  end
  object BTestFireDAC: TButton
    Left = 33
    Top = 49
    Width = 161
    Height = 25
    Caption = 'Test FDMemTable'
    TabOrder = 0
    OnClick = BTestFireDACClick
  end
  object BTestBIDataset: TButton
    Left = 33
    Top = 95
    Width = 161
    Height = 25
    Caption = 'Test BIDataset'
    TabOrder = 1
    OnClick = BTestBIDatasetClick
  end
  object Memo1: TMemo
    Left = 24
    Top = 144
    Width = 542
    Height = 257
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object Edit1: TEdit
    Left = 73
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '10000000'
    OnChange = Edit1Change
  end
  object ETemp: TEdit
    Left = 248
    Top = 8
    Width = 318
    Height = 21
    TabOrder = 4
    Text = 'c:\temp'
  end
  object RGLoopMode: TRadioGroup
    Left = 208
    Top = 79
    Width = 258
    Height = 41
    Caption = 'Loop using:'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Fields'
      'Data Items')
    TabOrder = 5
  end
  object BIDataset1: TBIDataset
    RowNumbers = False
    Left = 280
    Top = 280
  end
end
