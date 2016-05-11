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
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 378
    Height = 269
    ActivePage = TabQuery
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
        Top = 0
        Width = 370
        Height = 121
        Align = alTop
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        OnChange = MemoSQLChange
      end
      object BEditQuery: TButton
        Left = 16
        Top = 144
        Width = 75
        Height = 25
        Caption = '&Edit...'
        TabOrder = 1
        OnClick = BEditQueryClick
      end
    end
    object TabData: TTabSheet
      Caption = 'Data'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object BClear: TButton
        Left = 16
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Clear'
        TabOrder = 0
        OnClick = BClearClick
      end
      object Button3: TButton
        Left = 16
        Top = 56
        Width = 75
        Height = 25
        Caption = 'Change...'
        TabOrder = 1
      end
    end
  end
end
