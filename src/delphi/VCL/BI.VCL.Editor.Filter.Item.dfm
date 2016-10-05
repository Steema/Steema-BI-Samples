object FilterItemEditor: TFilterItemEditor
  Left = 0
  Top = 0
  Caption = 'Filter Item Editor'
  ClientHeight = 336
  ClientWidth = 440
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
  object PageItem: TPageControl
    Left = 0
    Top = 0
    Width = 440
    Height = 336
    ActivePage = TabDateTime
    Align = alClient
    TabOrder = 0
    Visible = False
    object TabDateTime: TTabSheet
      Caption = 'Date Time'
    end
    object TabBoolean: TTabSheet
      Caption = 'Boolean'
      ImageIndex = 1
      object CBTrue: TCheckBox
        Left = 16
        Top = 16
        Width = 97
        Height = 17
        Caption = 'True'
        TabOrder = 0
        OnClick = CBTrueClick
      end
      object CBFalse: TCheckBox
        Left = 16
        Top = 39
        Width = 97
        Height = 17
        Caption = 'False'
        TabOrder = 1
        OnClick = CBFalseClick
      end
    end
    object TabNumeric: TTabSheet
      Caption = 'Numeric'
      ImageIndex = 2
      object PageNumeric: TPageControl
        Left = 0
        Top = 0
        Width = 432
        Height = 308
        ActivePage = TabNumericRange
        Align = alClient
        TabOrder = 0
        object TabNumericRange: TTabSheet
          Caption = 'Range'
        end
        object TabNumericSelected: TTabSheet
          Caption = 'Selected'
          ImageIndex = 1
        end
      end
    end
    object TabText: TTabSheet
      Caption = 'Text'
      ImageIndex = 3
      object PageControl2: TPageControl
        Left = 0
        Top = 0
        Width = 432
        Height = 308
        ActivePage = TabTextOptions
        Align = alClient
        TabOrder = 0
        object TabTextOptions: TTabSheet
          Caption = 'Options'
          ImageIndex = 2
          object Label1: TLabel
            Left = 112
            Top = 16
            Width = 26
            Height = 13
            Caption = '&Text:'
            FocusControl = EText
          end
          object LBTextStyle: TListBox
            Left = 16
            Top = 16
            Width = 78
            Height = 81
            ItemHeight = 13
            Items.Strings = (
              'Contains'
              'Is equal'
              'Starts'
              'Ends'
              'Is empty')
            TabOrder = 0
            OnClick = LBTextStyleClick
          end
          object EText: TEdit
            Left = 112
            Top = 35
            Width = 121
            Height = 21
            TabOrder = 1
            OnChange = ETextChange
          end
          object CBTextCase: TCheckBox
            Left = 112
            Top = 72
            Width = 121
            Height = 17
            Caption = '&Case sensitive'
            TabOrder = 2
            OnClick = CBTextCaseClick
          end
        end
        object TabIncluded: TTabSheet
          Caption = 'Include'
        end
        object TabExcluded: TTabSheet
          Caption = 'Exclude'
          ImageIndex = 1
        end
      end
    end
  end
end
