object VisualizerEditor: TVisualizerEditor
  Left = 475
  Top = 229
  Caption = 'Data Visualizer Editor'
  ClientHeight = 459
  ClientWidth = 291
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
  object PageItems: TPageControl
    Left = 0
    Top = 0
    Width = 291
    Height = 459
    ActivePage = TabGroups
    Align = alClient
    TabOrder = 0
    object TabGroups: TTabSheet
      Caption = 'Groups'
      object LBGroups: TCheckListBox
        Left = 0
        Top = 38
        Width = 283
        Height = 97
        OnClickCheck = LBGroupsClickCheck
        Align = alTop
        ItemHeight = 13
        TabOrder = 0
        OnClick = LBGroupsClick
      end
      object Panel1: TPanel
        Left = 0
        Top = 135
        Width = 283
        Height = 296
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object PageSettings: TPageControl
          Left = 0
          Top = 0
          Width = 283
          Height = 296
          ActivePage = Group
          Align = alClient
          TabOrder = 0
          Visible = False
          OnChange = PageSettingsChange
          object Group: TTabSheet
            Caption = 'Group'
            ImageIndex = 6
            object Panel4: TPanel
              Left = 0
              Top = 0
              Width = 275
              Height = 268
              Align = alClient
              BevelOuter = bvNone
              Color = clWindow
              ParentBackground = False
              TabOrder = 0
              object LCurrentHeader: TLabel
                Left = 9
                Top = 158
                Width = 41
                Height = 13
                Caption = 'Current:'
              end
              object LCurrent: TLabel
                Left = 89
                Top = 158
                Width = 3
                Height = 13
              end
              object Label9: TLabel
                Left = 9
                Top = 180
                Width = 48
                Height = 13
                Caption = 'Group by:'
              end
              object CBGroupBy: TComboBox
                Left = 89
                Top = 175
                Width = 113
                Height = 21
                Style = csDropDownList
                TabOrder = 0
                OnChange = CBGroupByChange
              end
              object RGClass: TRadioGroup
                Left = 9
                Top = 8
                Width = 185
                Height = 137
                Caption = '&Class:'
                Columns = 2
                TabOrder = 1
                OnClick = RGClassClick
              end
            end
          end
          object TabSelected: TTabSheet
            Caption = 'Selected'
            ImageIndex = 8
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object CBSelected: TCheckListBox
              Left = 0
              Top = 0
              Width = 275
              Height = 268
              OnClickCheck = CBSelectedClickCheck
              Align = alClient
              ItemHeight = 13
              TabOrder = 0
            end
          end
          object TabCombo: TTabSheet
            Caption = 'Combo'
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
          end
          object TabMulti: TTabSheet
            Caption = 'Multi'
            ImageIndex = 1
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object LMultiScroll: TLabel
              Left = 9
              Top = 9
              Width = 29
              Height = 13
              Caption = '&Scroll:'
              FocusControl = CBMultiScroll
            end
            object Label3: TLabel
              Left = 9
              Top = 65
              Width = 44
              Height = 13
              Caption = '&Columns:'
              FocusControl = EColumns
            end
            object Label14: TLabel
              Left = 89
              Top = 65
              Width = 59
              Height = 13
              Caption = 'Row &Height:'
              FocusControl = ERowHeight
            end
            object CBMultiScroll: TComboBox
              Left = 9
              Top = 29
              Width = 89
              Height = 21
              Style = csDropDownList
              ItemIndex = 0
              TabOrder = 0
              Text = 'Automatic'
              OnChange = CBMultiScrollChange
              Items.Strings = (
                'Automatic'
                'Yes'
                'No')
            end
            object EColumns: TEdit
              Left = 9
              Top = 84
              Width = 44
              Height = 21
              TabOrder = 1
              Text = '1'
              OnChange = EColumnsChange
            end
            object UDColumns: TUpDown
              Left = 53
              Top = 84
              Width = 16
              Height = 21
              Associate = EColumns
              Min = 1
              Position = 1
              TabOrder = 2
            end
            object CBExpandLast: TCheckBox
              Left = 9
              Top = 120
              Width = 97
              Height = 17
              Caption = '&Expand Last'
              TabOrder = 3
              OnClick = CBExpandLastClick
            end
            object ERowHeight: TEdit
              Left = 89
              Top = 84
              Width = 44
              Height = 21
              TabOrder = 4
              Text = '0'
              OnChange = ERowHeightChange
            end
            object UDRowHeight: TUpDown
              Left = 133
              Top = 84
              Width = 16
              Height = 21
              Associate = ERowHeight
              Max = 2000
              TabOrder = 5
            end
            object CBSplitters: TCheckBox
              Left = 9
              Top = 152
              Width = 97
              Height = 17
              Caption = 'Spli&tters'
              TabOrder = 6
              OnClick = CBSplittersClick
            end
          end
          object TabList: TTabSheet
            Caption = 'List'
            ImageIndex = 3
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object CBCheckBoxes: TCheckBox
              Left = 9
              Top = 16
              Width = 97
              Height = 17
              Caption = '&Check Boxes'
              TabOrder = 0
              OnClick = CBCheckBoxesClick
            end
          end
          object TabPage: TTabSheet
            Caption = 'Page'
            ImageIndex = 5
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
          end
          object TabTrack: TTabSheet
            Caption = 'Trackbar'
            ImageIndex = 7
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
          end
          object TabControl: TTabSheet
            Caption = 'Control'
            ImageIndex = 10
            ExplicitLeft = 0
            ExplicitTop = 0
            ExplicitWidth = 0
            ExplicitHeight = 0
            object Label12: TLabel
              Left = 17
              Top = 17
              Width = 27
              Height = 13
              Caption = '&Align:'
              FocusControl = CBControlAlign
            end
            object CBControlAlign: TComboBox
              Left = 17
              Top = 36
              Width = 89
              Height = 21
              Style = csDropDownList
              ItemIndex = 0
              TabOrder = 0
              Text = 'Left'
              OnChange = CBControlAlignChange
              Items.Strings = (
                'Left'
                'Top'
                'Right'
                'Bottom')
            end
            object CBControlLabel: TCheckBox
              Left = 17
              Top = 80
              Width = 97
              Height = 17
              Caption = 'Label'
              TabOrder = 1
              OnClick = CBControlLabelClick
            end
          end
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 283
        Height = 38
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
        object Panel3: TPanel
          Left = 212
          Top = 0
          Width = 71
          Height = 38
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 0
          object BUp: TSpeedButton
            Left = 10
            Top = 8
            Width = 23
            Height = 22
            Caption = '^'
            Enabled = False
            OnClick = BUpClick
          end
          object BDown: TSpeedButton
            Left = 39
            Top = 8
            Width = 23
            Height = 22
            Caption = 'v'
            Enabled = False
            OnClick = BDownClick
          end
        end
        object Button1: TButton
          Left = 7
          Top = 7
          Width = 75
          Height = 25
          Caption = '&Best Order'
          TabOrder = 1
          OnClick = Button1Click
        end
        object Button2: TButton
          Left = 104
          Top = 8
          Width = 75
          Height = 25
          Caption = '&Data...'
          TabOrder = 2
          OnClick = Button2Click
        end
      end
    end
    object TabValues: TTabSheet
      Caption = 'Values'
      ImageIndex = 1
      object LBValues: TCheckListBox
        Left = 0
        Top = 38
        Width = 283
        Height = 97
        OnClickCheck = LBValuesClickCheck
        Align = alTop
        ItemHeight = 13
        TabOrder = 0
        OnClick = LBValuesClick
      end
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 283
        Height = 38
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Panel6: TPanel
          Left = 212
          Top = 0
          Width = 71
          Height = 38
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 0
          object SBUpValue: TSpeedButton
            Left = 10
            Top = 8
            Width = 23
            Height = 22
            Caption = '^'
            Enabled = False
            OnClick = SBUpValueClick
          end
          object SBDownValue: TSpeedButton
            Left = 39
            Top = 8
            Width = 23
            Height = 22
            Caption = 'v'
            Enabled = False
            OnClick = SBDownValueClick
          end
        end
      end
      object Panel7: TPanel
        Left = 0
        Top = 135
        Width = 283
        Height = 296
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 2
      end
    end
    object TabGrid: TTabSheet
      Caption = 'Grid'
      ImageIndex = 2
      object Panel8: TPanel
        Left = 0
        Top = 0
        Width = 283
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object SpeedButton2: TSpeedButton
          Left = 8
          Top = 6
          Width = 23
          Height = 22
          OnClick = SpeedButton2Click
        end
        object CBDuplicates: TCheckBox
          Left = 45
          Top = 9
          Width = 129
          Height = 17
          Caption = 'Hide Duplicates'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = CBDuplicatesClick
        end
        object CBColorize: TCheckBox
          Left = 180
          Top = 9
          Width = 97
          Height = 17
          Caption = 'Colorize Grid'
          TabOrder = 1
          OnClick = CBColorizeClick
        end
      end
      object BIGrid1: TBIGrid
        Left = 0
        Top = 41
        Width = 283
        Height = 390
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        ParentColor = False
        TabOrder = 1
      end
    end
  end
end
