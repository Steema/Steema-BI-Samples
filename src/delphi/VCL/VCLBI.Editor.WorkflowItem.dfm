object WorkflowItemEditor: TWorkflowItemEditor
  Left = 0
  Top = 0
  Caption = 'WorkflowItemEditor'
  ClientHeight = 269
  ClientWidth = 378
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
    Width = 378
    Height = 269
    ActivePage = TabSingleRow
    Align = alClient
    TabOrder = 0
    Visible = False
    object TabRename: TTabSheet
      Caption = 'Rename'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ItemNames: TComboBox
        Left = 88
        Top = 16
        Width = 145
        Height = 21
        Style = csDropDownList
        TabOrder = 0
        OnChange = ItemNamesChange
      end
      object ENewName: TEdit
        Left = 88
        Top = 56
        Width = 145
        Height = 21
        TabOrder = 1
        OnChange = ENewNameChange
      end
    end
    object TabDelete: TTabSheet
      Caption = 'Delete'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object DeleteItemNames: TComboBox
        Left = 96
        Top = 24
        Width = 145
        Height = 21
        Style = csDropDownList
        TabOrder = 0
      end
    end
    object TabAdd: TTabSheet
      Caption = 'Add'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object EAdd: TEdit
        Left = 80
        Top = 16
        Width = 121
        Height = 21
        TabOrder = 0
        OnChange = EAddChange
      end
      object CBKind: TComboBox
        Left = 80
        Top = 56
        Width = 121
        Height = 21
        Style = csDropDownList
        TabOrder = 1
        OnChange = CBKindChange
      end
    end
    object TabQuery: TTabSheet
      Caption = 'Query'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object MemoSQL: TMemo
        Left = 0
        Top = 65
        Width = 370
        Height = 176
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        OnChange = MemoSQLChange
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 370
        Height = 65
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object LError: TLabel
          Left = 11
          Top = 39
          Width = 35
          Height = 16
          Caption = 'LError'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          Visible = False
        end
        object BEditQuery: TButton
          Left = 11
          Top = 8
          Width = 75
          Height = 25
          Caption = '&Edit...'
          TabOrder = 0
          OnClick = BEditQueryClick
        end
      end
    end
    object TabFilter: TTabSheet
      Caption = 'Filter'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object BEditFilter: TButton
        Left = 16
        Top = 16
        Width = 75
        Height = 25
        Caption = '&Edit...'
        TabOrder = 0
        OnClick = BEditFilterClick
      end
    end
    object TabSingleRow: TTabSheet
      Caption = 'Single row'
      ImageIndex = 6
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label1: TLabel
        Left = 16
        Top = 16
        Width = 25
        Height = 13
        Caption = 'Row:'
      end
      object ERow: TEdit
        Left = 16
        Top = 35
        Width = 81
        Height = 21
        TabOrder = 0
        Text = '0'
        OnChange = ERowChange
      end
      object UDRow: TUpDown
        Left = 97
        Top = 35
        Width = 16
        Height = 21
        Associate = ERow
        Max = 1
        TabOrder = 1
      end
      object TBRow: TTrackBar
        Left = 9
        Top = 72
        Width = 216
        Height = 25
        TabOrder = 2
        ThumbLength = 14
        OnChange = TBRowChange
      end
    end
    object TabNeeds: TTabSheet
      Caption = 'TabNeeds'
      ImageIndex = 6
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
end
