object FormWorkflow: TFormWorkflow
  Left = 0
  Top = 0
  Caption = 'TeeBI TWorkflow Example'
  ClientHeight = 650
  ClientWidth = 1051
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  WindowState = wsMaximized
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BIWorkflow1: TBIWorkflow
    Items = <
      item
        Origin = '|Animals|Animals'
      end>
    Left = 520
    Top = 328
  end
end
