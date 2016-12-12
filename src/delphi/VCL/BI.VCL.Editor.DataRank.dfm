object DataRankEditor: TDataRankEditor
  Left = 0
  Top = 0
  Caption = 'Data Rank Editor'
  ClientHeight = 47
  ClientWidth = 205
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object CBAscending: TCheckBox
    Left = 16
    Top = 13
    Width = 161
    Height = 17
    Caption = '&Ascending'
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = CBAscendingClick
  end
end
