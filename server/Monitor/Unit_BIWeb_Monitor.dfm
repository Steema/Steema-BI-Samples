object BIWebMonitor: TBIWebMonitor
  Left = 0
  Top = 0
  Caption = 'BIWeb Monitor'
  ClientHeight = 390
  ClientWidth = 438
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 162
    Width = 438
    Height = 228
    Align = alClient
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 438
    Height = 162
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 32
      Top = 56
      Width = 32
      Height = 13
      Caption = '&Every:'
      FocusControl = ESeconds
    end
    object Label2: TLabel
      Left = 135
      Top = 56
      Width = 39
      Height = 13
      Caption = 'seconds'
    end
    object CBCheckRun: TCheckBox
      Left = 16
      Top = 24
      Width = 249
      Height = 17
      Caption = 'Make sure BIWeb server is &running'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBCheckRunClick
    end
    object ESeconds: TEdit
      Left = 70
      Top = 53
      Width = 43
      Height = 21
      TabOrder = 1
      Text = '60'
      OnChange = ESecondsChange
    end
    object UDSeconds: TUpDown
      Left = 113
      Top = 53
      Width = 16
      Height = 21
      Associate = ESeconds
      Min = 1
      Max = 32767
      Position = 60
      TabOrder = 2
    end
    object CBUpdates: TCheckBox
      Left = 16
      Top = 104
      Width = 257
      Height = 17
      Caption = 'Check for BIWeb updates'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = CBUpdatesClick
    end
    object Button1: TButton
      Left = 304
      Top = 51
      Width = 97
      Height = 25
      Caption = 'Check &Now !'
      TabOrder = 4
      OnClick = Button1Click
    end
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 320
    Top = 104
  end
end
