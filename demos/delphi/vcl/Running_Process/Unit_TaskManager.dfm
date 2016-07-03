object FormTaskManager: TFormTaskManager
  Left = 0
  Top = 0
  Caption = 'TeeBI TaskManager Example'
  ClientHeight = 579
  ClientWidth = 1006
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
  object BIGrid1: TBIGrid
    Left = 0
    Top = 41
    Width = 1006
    Height = 538
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    ExplicitLeft = 40
    ExplicitTop = 47
    ExplicitWidth = 585
    ExplicitHeight = 505
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1006
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 248
    ExplicitTop = 288
    ExplicitWidth = 185
    object CBAutoRefresh: TCheckBox
      Left = 16
      Top = 13
      Width = 97
      Height = 17
      Caption = 'Auto Refresh'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CBAutoRefreshClick
    end
    object BRefresh: TButton
      Left = 152
      Top = 9
      Width = 97
      Height = 25
      Caption = 'Refresh Now'
      Enabled = False
      TabOrder = 1
      OnClick = BRefreshClick
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 328
    Top = 296
  end
end
