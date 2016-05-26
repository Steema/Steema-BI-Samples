object FormConstants: TFormConstants
  Left = 0
  Top = 0
  Caption = 'Constants'
  ClientHeight = 406
  ClientWidth = 729
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Header: TMemo
    Left = 8
    Top = 16
    Width = 257
    Height = 369
    Lines.Strings = (
      '<!DOCTYPE html>'
      '<html>'
      '<head>'
      '<style>'
      'table, th, td {'
      '    border: 1px solid black;'
      '    border-collapse: collapse;'
      '}'
      'th, td {'
      '    padding: 5px;'
      '    text-align: left;'
      '}'
      '</style>'
      '</head>'
      '<body>')
    TabOrder = 0
  end
  object Footer: TMemo
    Left = 280
    Top = 16
    Width = 129
    Height = 57
    Lines.Strings = (
      '</body>'
      '</html>')
    TabOrder = 1
  end
end
