object DataSelectEditor: TDataSelectEditor
  Left = 373
  Top = 142
  Caption = 'TDataSelect Editor'
  ClientHeight = 399
  ClientWidth = 326
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 326
    Height = 358
    ActivePage = TabItems
    Align = alClient
    TabOrder = 0
    object TabItems: TTabSheet
      Caption = 'Items'
      object Panel2: TPanel
        Left = 0
        Top = 41
        Width = 318
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object BUp: TSpeedButton
          Left = 218
          Top = 8
          Width = 23
          Height = 22
          Caption = '^'
          Enabled = False
          OnClick = BUpClick
        end
        object BDown: TSpeedButton
          Left = 247
          Top = 8
          Width = 23
          Height = 22
          Caption = 'v'
          Enabled = False
          OnClick = BDownClick
        end
        object BAdd: TButton
          Left = 12
          Top = 8
          Width = 75
          Height = 25
          Caption = '&Add...'
          TabOrder = 0
          OnClick = BAddClick
        end
        object BRemoveItem: TButton
          Left = 104
          Top = 8
          Width = 75
          Height = 25
          Caption = '&Remove'
          Enabled = False
          TabOrder = 1
          OnClick = BRemoveItemClick
        end
      end
      object LItems: TCheckListBox
        Left = 0
        Top = 82
        Width = 318
        Height = 129
        OnClickCheck = LItemsClickCheck
        Align = alClient
        DragMode = dmAutomatic
        ItemHeight = 13
        TabOrder = 1
        OnClick = LItemsClick
        OnDragDrop = LItemsDragDrop
        OnDragOver = LItemsDragOver
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 318
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
        object Label1: TLabel
          Left = 144
          Top = 16
          Width = 24
          Height = 13
          Caption = '&Max:'
        end
        object CBDistinct: TCheckBox
          Left = 12
          Top = 15
          Width = 129
          Height = 17
          Caption = '&Distinct'
          TabOrder = 0
          OnClick = CBDistinctClick
        end
        object EMax: TEdit
          Left = 177
          Top = 13
          Width = 83
          Height = 21
          TabOrder = 1
          OnChange = EMaxChange
        end
      end
      object Panel5: TPanel
        Left = 0
        Top = 211
        Width = 318
        Height = 119
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 3
        object Label2: TLabel
          Left = 12
          Top = 6
          Width = 56
          Height = 13
          Caption = '&Expression:'
          FocusControl = EItemExpression
        end
        object LItemError: TLabel
          Left = 12
          Top = 53
          Width = 3
          Height = 13
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label3: TLabel
          Left = 12
          Top = 74
          Width = 31
          Height = 13
          Caption = '&Name:'
          FocusControl = EItemName
        end
        object EItemExpression: TEdit
          Left = 12
          Top = 25
          Width = 221
          Height = 21
          Enabled = False
          TabOrder = 0
          OnChange = EItemExpressionChange
        end
        object EItemName: TEdit
          Left = 12
          Top = 93
          Width = 221
          Height = 21
          Enabled = False
          TabOrder = 1
          OnChange = EItemNameChange
        end
      end
    end
    object TabSort: TTabSheet
      Caption = 'Sort By'
      ImageIndex = 1
      object LBSort: TCheckListBox
        Left = 0
        Top = 41
        Width = 318
        Height = 155
        OnClickCheck = LBSortClickCheck
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnClick = LBSortClick
        OnDragDrop = LBSortDragDrop
        OnDragOver = LBSortDragOver
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 318
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object BUpSort: TSpeedButton
          Left = 218
          Top = 8
          Width = 23
          Height = 22
          Caption = '^'
          Enabled = False
          OnClick = BUpSortClick
        end
        object BDownSort: TSpeedButton
          Left = 247
          Top = 8
          Width = 23
          Height = 22
          Caption = 'v'
          Enabled = False
          OnClick = BDownSortClick
        end
        object BAddSort: TButton
          Left = 12
          Top = 8
          Width = 75
          Height = 25
          Caption = '&Add...'
          TabOrder = 0
          OnClick = BAddSortClick
        end
        object BRemoveSort: TButton
          Left = 104
          Top = 8
          Width = 75
          Height = 25
          Caption = '&Remove'
          Enabled = False
          TabOrder = 1
          OnClick = BRemoveSortClick
        end
      end
      object Panel4: TPanel
        Left = 0
        Top = 196
        Width = 318
        Height = 134
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
        object LSortError: TLabel
          Left = 12
          Top = 112
          Width = 3
          Height = 13
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object LSortExpression: TLabel
          Left = 12
          Top = 65
          Width = 56
          Height = 13
          Caption = '&Expression:'
          FocusControl = ESortExpression
        end
        object ESortExpression: TEdit
          Left = 12
          Top = 84
          Width = 221
          Height = 21
          Enabled = False
          TabOrder = 0
          OnChange = ESortExpressionChange
        end
        object CBIgnoreCase: TCheckBox
          Left = 12
          Top = 38
          Width = 181
          Height = 17
          Caption = '&Ignore Text Case'
          Enabled = False
          TabOrder = 1
          OnClick = CBIgnoreCaseClick
        end
        object CBAscending: TCheckBox
          Left = 12
          Top = 15
          Width = 181
          Height = 17
          Caption = '&Ascending'
          Checked = True
          Enabled = False
          State = cbChecked
          TabOrder = 2
          OnClick = CBAscendingClick
        end
      end
    end
    object TabFilter: TTabSheet
      Caption = 'Filter'
      ImageIndex = 2
      object LFilter: TLabel
        Left = 12
        Top = 56
        Width = 3
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object EFilter: TEdit
        Left = 12
        Top = 16
        Width = 253
        Height = 21
        TabOrder = 0
        OnChange = EFilterChange
      end
    end
    object TabSQL: TTabSheet
      Caption = 'SQL'
      ImageIndex = 3
      object MemoSQL: TMemo
        Left = 0
        Top = 0
        Width = 318
        Height = 330
        Align = alClient
        ReadOnly = True
        TabOrder = 0
      end
    end
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 358
    Width = 326
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    object Panel9: TPanel
      Left = 141
      Top = 0
      Width = 185
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object BOK: TButton
        Left = 9
        Top = 6
        Width = 75
        Height = 25
        Caption = 'OK'
        ModalResult = 1
        TabOrder = 0
      end
      object Button2: TButton
        Left = 99
        Top = 6
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        Default = True
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object PopupSort: TPopupMenu
    Left = 192
    Top = 208
    object Data1: TMenuItem
      Caption = '&Data...'
      OnClick = Data1Click
    end
    object Expression1: TMenuItem
      Caption = '&Expression'
      OnClick = Expression1Click
    end
  end
  object PopupItems: TPopupMenu
    Left = 80
    Top = 152
    object MenuItem1: TMenuItem
      Caption = '&Data...'
      OnClick = MenuItem1Click
    end
    object MenuItem2: TMenuItem
      Caption = '&Expression'
      OnClick = MenuItem2Click
    end
  end
end
