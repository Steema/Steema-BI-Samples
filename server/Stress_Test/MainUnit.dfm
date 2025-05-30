object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'TeeBI - BIWeb server stress test'
  ClientHeight = 247
  ClientWidth = 435
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Label1: TLabel
    Left = 112
    Top = 21
    Width = 43
    Height = 13
    Caption = '&Threads:'
  end
  object Label2: TLabel
    Left = 16
    Top = 64
    Width = 49
    Height = 13
    Caption = 'Requests:'
  end
  object LRequests: TLabel
    Left = 71
    Top = 64
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label3: TLabel
    Left = 256
    Top = 21
    Width = 67
    Height = 13
    Caption = '&Max Workers:'
  end
  object Label4: TLabel
    Left = 120
    Top = 64
    Width = 57
    Height = 13
    Caption = 'Per second:'
  end
  object LPerSecond: TLabel
    Left = 189
    Top = 64
    Width = 6
    Height = 13
    Caption = '0'
  end
  object BRun: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = '&Run !'
    TabOrder = 0
    OnClick = BRunClick
  end
  object UDThreads: TUpDown
    Left = 209
    Top = 18
    Width = 16
    Height = 21
    Associate = EThreads
    Min = 1
    Max = 10000
    Position = 4
    TabOrder = 1
  end
  object EThreads: TEdit
    Left = 161
    Top = 18
    Width = 48
    Height = 21
    TabOrder = 2
    Text = '4'
  end
  object CheckListBox1: TCheckListBox
    Left = 16
    Top = 96
    Width = 169
    Height = 97
    ItemHeight = 13
    Items.Strings = (
      'All Datas (string)'
      'SQLite_Demo (TDataItem)'
      'Summary Chart (PNG)'
      'Query (select)'
      'JSON data')
    TabOrder = 3
    OnClickCheck = CheckListBox1ClickCheck
  end
  object EWorkers: TEdit
    Left = 329
    Top = 18
    Width = 48
    Height = 21
    TabOrder = 4
    Text = '50'
    OnChange = EWorkersChange
  end
  object UDWorkers: TUpDown
    Left = 377
    Top = 18
    Width = 16
    Height = 21
    Associate = EWorkers
    Min = 1
    Max = 10000
    Position = 50
    TabOrder = 5
  end
  object CBCompression: TCheckBox
    Left = 256
    Top = 63
    Width = 97
    Height = 17
    Caption = 'Zip compression'
    TabOrder = 6
    OnClick = CBCompressionClick
  end
  object RGEngine: TRadioGroup
    Left = 256
    Top = 96
    Width = 137
    Height = 97
    Caption = 'Http engine:'
    ItemIndex = 1
    Items.Strings = (
      'Indy'
      'RTL Net')
    TabOrder = 7
    OnClick = RGEngineClick
  end
end
