object SearchEditor: TSearchEditor
  Left = 0
  Top = 0
  Caption = 'Data Search '
  ClientHeight = 37
  ClientWidth = 384
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
  object SBMenu: TSpeedButton
    Left = 149
    Top = 7
    Width = 23
    Height = 22
    Caption = '...'
    Flat = True
    OnClick = SBMenuClick
  end
  object LHits: TLabel
    Left = 178
    Top = 12
    Width = 3
    Height = 13
  end
  object SBClose: TSpeedButton
    Left = -2
    Top = 8
    Width = 18
    Height = 18
    Hint = 'Close'
    Caption = 'x'
    Flat = True
    ParentShowHint = False
    ShowHint = True
  end
  object SBDown: TSpeedButton
    Left = 232
    Top = 8
    Width = 23
    Height = 22
    Caption = 'v'
    Enabled = False
    Visible = False
    OnClick = SBDownClick
  end
  object SBUp: TSpeedButton
    Left = 261
    Top = 8
    Width = 23
    Height = 22
    Caption = '^'
    Enabled = False
    Visible = False
    OnClick = SBUpClick
  end
  object ESearch: TEdit
    Left = 22
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
    OnChange = ESearchChange
  end
  object PopupMenu1: TPopupMenu
    Left = 30
    object Casesensitive1: TMenuItem
      Caption = '&Case sensitive'
      OnClick = Casesensitive1Click
    end
    object Searchat1: TMenuItem
      Caption = '&Search at'
      object Anywhere1: TMenuItem
        Caption = '&Anywhere'
        Checked = True
        RadioItem = True
        OnClick = Exact1Click
      end
      object Start1: TMenuItem
        Caption = '&Start'
        RadioItem = True
        OnClick = Exact1Click
      end
      object End1: TMenuItem
        Caption = '&End'
        RadioItem = True
        OnClick = Exact1Click
      end
      object Exact1: TMenuItem
        Caption = 'E&xact'
        RadioItem = True
        OnClick = Exact1Click
      end
    end
  end
end
