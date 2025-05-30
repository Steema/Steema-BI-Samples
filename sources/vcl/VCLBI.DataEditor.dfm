inherited DataItemEditor: TDataItemEditor
  Caption = 'DataItemEditor'
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  inherited Splitter1: TSplitter
    Height = 609
    ExplicitHeight = 609
  end
  inherited PanelItems: TPanel
    Height = 609
    ExplicitHeight = 609
    inherited SplitterData: TSplitter
      Top = 220
      ExplicitTop = 220
    end
    inherited PanelData: TPanel
      Top = 223
      ExplicitTop = 223
      inherited PanelDataGrid: TPanel
        inherited PanelNav: TPanel
          inherited Panel4: TPanel
            inherited DBNavigator1: TDBNavigator
              Hints.Strings = ()
            end
          end
        end
      end
    end
    inherited PanelItemsGrid: TPanel
      Height = 220
      ExplicitHeight = 220
      inherited ItemsGrid: TBIGrid
        Height = 190
        ExplicitHeight = 190
      end
      inherited Panel3: TPanel
        inherited DBNavigator2: TDBNavigator
          Hints.Strings = ()
        end
      end
    end
  end
  inherited PanelDatas: TPanel
    Height = 609
    ExplicitHeight = 609
    inherited DataTotals: TStringGrid
      Top = 559
      ExplicitTop = 559
    end
  end
  object PanelButtons: TPanel [4]
    Left = 0
    Top = 650
    Width = 1091
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object Panel7: TPanel
      Left = 904
      Top = 0
      Width = 187
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object BOK: TButton
        Left = 16
        Top = 6
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        Enabled = False
        ModalResult = 1
        TabOrder = 0
        OnClick = BOKClick
      end
      object BCancel: TButton
        Left = 104
        Top = 6
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 1
        TabOrder = 1
      end
    end
  end
  object PopupNode: TPopupMenu
    OnPopup = PopupNodePopup
    Left = 48
    Top = 272
    object Rename1: TMenuItem
      Caption = '&Rename...'
      OnClick = Rename1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object AddField1: TMenuItem
      Caption = 'Add &Field...'
      OnClick = AddField1Click
    end
    object AddTable1: TMenuItem
      Caption = 'Add &Table...'
      OnClick = AddTable1Click
    end
    object AddFolder1: TMenuItem
      Caption = 'Add F&older...'
      OnClick = AddFolder1Click
    end
    object Kind1: TMenuItem
      Caption = '&Kind'
      object Integer32bit1: TMenuItem
        Caption = 'Integer 32bit'
        GroupIndex = 1
        RadioItem = True
        OnClick = Boolean1Click
      end
      object Integer64bit1: TMenuItem
        Tag = 1
        Caption = 'Integer 64bit'
        GroupIndex = 1
        RadioItem = True
        OnClick = Boolean1Click
      end
      object FloatSingle1: TMenuItem
        Tag = 2
        Caption = 'Float Single'
        GroupIndex = 1
        RadioItem = True
        OnClick = Boolean1Click
      end
      object FloatDouble1: TMenuItem
        Tag = 3
        Caption = 'Float Double'
        GroupIndex = 1
        RadioItem = True
        OnClick = Boolean1Click
      end
      object FloatExtended1: TMenuItem
        Tag = 4
        Caption = 'Float Extended'
        GroupIndex = 1
        RadioItem = True
        OnClick = Boolean1Click
      end
      object Text1: TMenuItem
        Tag = 5
        Caption = 'Text'
        GroupIndex = 1
        RadioItem = True
        OnClick = Boolean1Click
      end
      object DateTime1: TMenuItem
        Tag = 6
        Caption = 'Date-Time'
        GroupIndex = 1
        RadioItem = True
        OnClick = Boolean1Click
      end
      object Boolean1: TMenuItem
        Tag = 7
        Caption = 'Boolean'
        GroupIndex = 1
        RadioItem = True
        OnClick = Boolean1Click
      end
    end
  end
end
