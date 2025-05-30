object BIChartEditor: TBIChartEditor
  Left = 210
  Top = 198
  Width = 507
  Height = 436
  ActiveControl = BOK
  Caption = 'BIChart Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 491
    Height = 360
    ActivePage = TabOptions
    Align = alClient
    TabOrder = 0
    OnChange = PageControl1Change
    object TabOptions: TTabSheet
      Caption = 'Options'
      object PageMode: TPageControl
        Left = 129
        Top = 0
        Width = 354
        Height = 332
        ActivePage = Tab3D
        Align = alClient
        TabOrder = 0
        object Tab2D: TTabSheet
          Caption = '2D'
          ImageIndex = 1
          object Panel3: TPanel
            Left = 0
            Top = 0
            Width = 111
            Height = 304
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 0
            object GroupBox2: TLabel
              Left = 8
              Top = 3
              Width = 28
              Height = 13
              Caption = '&Style:'
            end
            object Label11: TLabel
              Left = 8
              Top = 193
              Width = 42
              Height = 13
              Caption = 'Sta&cked:'
              FocusControl = CBStacked
            end
            object LB2D: TListBox
              Left = 8
              Top = 22
              Width = 97
              Height = 134
              ItemHeight = 13
              Items.Strings = (
                'Automatic'
                'Area'
                'Bar'
                'Line'
                'Line and Point'
                'Pie'
                'Point XY'
                'Polar'
                'Radar')
              TabOrder = 0
              OnClick = LB2DClick
            end
            object CBStacked: TComboBox
              Left = 8
              Top = 212
              Width = 97
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              ItemIndex = 0
              TabOrder = 1
              Text = 'Automatic'
              OnChange = CBStackedChange
              Items.Strings = (
                'Automatic'
                'No'
                'Yes'
                '100%'
                'Side'
                'Side All'
                'Self')
            end
            object CBHoriz2D: TComboBox
              Left = 8
              Top = 164
              Width = 97
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              ItemIndex = 0
              TabOrder = 2
              Text = 'Automatic'
              OnChange = CBHoriz2DChange
              Items.Strings = (
                'Automatic'
                'Horizontal'
                'Vertical')
            end
          end
          object Panel4: TPanel
            Left = 111
            Top = 0
            Width = 235
            Height = 304
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            object PanelItems: TPanel
              Left = 0
              Top = 0
              Width = 235
              Height = 102
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 0
              object Label1: TLabel
                Left = 10
                Top = 20
                Width = 10
                Height = 13
                Caption = 'X:'
              end
              object Label2: TLabel
                Left = 10
                Top = 47
                Width = 31
                Height = 13
                Caption = 'Texts:'
              end
              object Label3: TLabel
                Left = 10
                Top = 74
                Width = 33
                Height = 13
                Caption = '&Group:'
              end
              object CBX2D: TComboBox
                Left = 47
                Top = 17
                Width = 121
                Height = 21
                Style = csDropDownList
                ItemHeight = 0
                TabOrder = 0
                OnChange = CBX2DChange
              end
              object CBText: TComboBox
                Left = 47
                Top = 44
                Width = 121
                Height = 21
                Style = csDropDownList
                ItemHeight = 0
                TabOrder = 1
                OnChange = CBTextChange
              end
              object CBColors: TComboBox
                Left = 47
                Top = 71
                Width = 121
                Height = 21
                Style = csDropDownList
                ItemHeight = 0
                TabOrder = 2
              end
            end
            object PanelY: TPanel
              Left = 0
              Top = 102
              Width = 235
              Height = 202
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 1
            end
          end
        end
        object Tab3D: TTabSheet
          Caption = '3D'
          ImageIndex = 2
          object Panel3D: TPanel
            Left = 3
            Top = 163
            Width = 212
            Height = 123
            BevelOuter = bvNone
            TabOrder = 0
            object BSwapXY: TSpeedButton
              Left = 181
              Top = 27
              Width = 23
              Height = 22
              Glyph.Data = {
                36050000424D3605000000000000360400002800000010000000100000000100
                08000000000000010000C40E0000C40E00000001000000000000000000000000
                80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
                A6000020400000206000002080000020A0000020C0000020E000004000000040
                20000040400000406000004080000040A0000040C0000040E000006000000060
                20000060400000606000006080000060A0000060C0000060E000008000000080
                20000080400000806000008080000080A0000080C0000080E00000A0000000A0
                200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
                200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
                200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
                20004000400040006000400080004000A0004000C0004000E000402000004020
                20004020400040206000402080004020A0004020C0004020E000404000004040
                20004040400040406000404080004040A0004040C0004040E000406000004060
                20004060400040606000406080004060A0004060C0004060E000408000004080
                20004080400040806000408080004080A0004080C0004080E00040A0000040A0
                200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
                200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
                200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
                20008000400080006000800080008000A0008000C0008000E000802000008020
                20008020400080206000802080008020A0008020C0008020E000804000008040
                20008040400080406000804080008040A0008040C0008040E000806000008060
                20008060400080606000806080008060A0008060C0008060E000808000008080
                20008080400080806000808080008080A0008080C0008080E00080A0000080A0
                200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
                200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
                200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
                2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
                2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
                2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
                2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
                2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
                2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
                2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFF00000000000000FFFFFF00FFFFFFFFFF00FFFFFFFF
                FF00FFFF0000FFFFFFFFFF00FFFFFFFFFF00FF000000000000FFFF00FFFFFFFF
                FF00FFFF0000FFFF00FFFF00000000000000FFFFFF00FFFF00FFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FF
                FFFF00000000000000FFFF00FFFF0000FFFF00FFFFFFFFFF00FFFF0000000000
                00FF00FFFFFFFFFF00FFFFFFFFFF0000FFFF00FFFFFFFFFF00FFFFFFFFFF00FF
                FFFF00000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
              OnClick = BSwapXYClick
            end
            object BSwapYZ: TSpeedButton
              Left = 181
              Top = 70
              Width = 23
              Height = 22
              Glyph.Data = {
                36050000424D3605000000000000360400002800000010000000100000000100
                08000000000000010000C40E0000C40E00000001000000000000000000000000
                80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
                A6000020400000206000002080000020A0000020C0000020E000004000000040
                20000040400000406000004080000040A0000040C0000040E000006000000060
                20000060400000606000006080000060A0000060C0000060E000008000000080
                20000080400000806000008080000080A0000080C0000080E00000A0000000A0
                200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
                200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
                200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
                20004000400040006000400080004000A0004000C0004000E000402000004020
                20004020400040206000402080004020A0004020C0004020E000404000004040
                20004040400040406000404080004040A0004040C0004040E000406000004060
                20004060400040606000406080004060A0004060C0004060E000408000004080
                20004080400040806000408080004080A0004080C0004080E00040A0000040A0
                200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
                200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
                200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
                20008000400080006000800080008000A0008000C0008000E000802000008020
                20008020400080206000802080008020A0008020C0008020E000804000008040
                20008040400080406000804080008040A0008040C0008040E000806000008060
                20008060400080606000806080008060A0008060C0008060E000808000008080
                20008080400080806000808080008080A0008080C0008080E00080A0000080A0
                200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
                200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
                200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
                2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
                2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
                2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
                2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
                2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
                2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
                2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFF00000000000000FFFFFF00FFFFFFFFFF00FFFFFFFF
                FF00FFFF0000FFFFFFFFFF00FFFFFFFFFF00FF000000000000FFFF00FFFFFFFF
                FF00FFFF0000FFFF00FFFF00000000000000FFFFFF00FFFF00FFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FF
                FFFF00000000000000FFFF00FFFF0000FFFF00FFFFFFFFFF00FFFF0000000000
                00FF00FFFFFFFFFF00FFFFFFFFFF0000FFFF00FFFFFFFFFF00FFFFFFFFFF00FF
                FFFF00000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
              OnClick = BSwapYZClick
            end
            object BSwapXZ: TSpeedButton
              Left = 2
              Top = 50
              Width = 23
              Height = 22
              Glyph.Data = {
                36050000424D3605000000000000360400002800000010000000100000000100
                08000000000000010000C40E0000C40E00000001000000000000000000000000
                80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
                A6000020400000206000002080000020A0000020C0000020E000004000000040
                20000040400000406000004080000040A0000040C0000040E000006000000060
                20000060400000606000006080000060A0000060C0000060E000008000000080
                20000080400000806000008080000080A0000080C0000080E00000A0000000A0
                200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
                200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
                200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
                20004000400040006000400080004000A0004000C0004000E000402000004020
                20004020400040206000402080004020A0004020C0004020E000404000004040
                20004040400040406000404080004040A0004040C0004040E000406000004060
                20004060400040606000406080004060A0004060C0004060E000408000004080
                20004080400040806000408080004080A0004080C0004080E00040A0000040A0
                200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
                200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
                200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
                20008000400080006000800080008000A0008000C0008000E000802000008020
                20008020400080206000802080008020A0008020C0008020E000804000008040
                20008040400080406000804080008040A0008040C0008040E000806000008060
                20008060400080606000806080008060A0008060C0008060E000808000008080
                20008080400080806000808080008080A0008080C0008080E00080A0000080A0
                200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
                200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
                200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
                2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
                2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
                2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
                2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
                2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
                2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
                2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFF00000000000000FFFFFF00FFFFFFFFFF00FFFFFFFF
                FF00FFFF0000FFFFFFFFFF00FFFFFFFFFF00FF000000000000FFFF00FFFFFFFF
                FF00FFFF0000FFFF00FFFF00000000000000FFFFFF00FFFF00FFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFF00FF
                FFFF00000000000000FFFF00FFFF0000FFFF00FFFFFFFFFF00FFFF0000000000
                00FF00FFFFFFFFFF00FFFFFFFFFF0000FFFF00FFFFFFFFFF00FFFFFFFFFF00FF
                FFFF00000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
              OnClick = BSwapXZClick
            end
            object Shape1: TShape
              Left = 13
              Top = 10
              Width = 1
              Height = 31
            end
            object Shape2: TShape
              Left = 13
              Top = 82
              Width = 1
              Height = 31
            end
            object Label4: TLabel
              Left = 33
              Top = 11
              Width = 10
              Height = 13
              Caption = 'X:'
            end
            object Label5: TLabel
              Left = 33
              Top = 53
              Width = 10
              Height = 13
              Caption = 'Y:'
            end
            object Label6: TLabel
              Left = 33
              Top = 99
              Width = 10
              Height = 13
              Caption = 'Z:'
            end
            object CBX: TComboBox
              Left = 54
              Top = 8
              Width = 121
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              TabOrder = 0
              OnChange = CBXChange
            end
            object CBY: TComboBox
              Left = 54
              Top = 50
              Width = 121
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              TabOrder = 1
              OnChange = CBYChange
            end
            object CBZ: TComboBox
              Left = 54
              Top = 96
              Width = 121
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              TabOrder = 2
              OnChange = CBZChange
            end
          end
          object LB3D: TListBox
            Left = 113
            Top = 11
            Width = 98
            Height = 94
            ItemHeight = 13
            Items.Strings = (
              'Automatic'
              'Contour'
              'Grid'
              'Surface'
              'Tower'
              'Tri Surface')
            TabOrder = 1
            OnClick = LB3DClick
          end
          object LBXYZ: TListBox
            Left = 113
            Top = 11
            Width = 98
            Height = 65
            ItemHeight = 13
            Items.Strings = (
              'Automatic'
              'Point 3D'
              'Ternary'
              'Tri Surface')
            TabOrder = 2
            OnClick = LBXYZClick
          end
          object RG3D: TRadioGroup
            Left = 8
            Top = 6
            Width = 90
            Height = 122
            Caption = '&Mode:'
            Items.Strings = (
              'Automatic'
              'Table'
              'Grid'
              'XYZ')
            TabOrder = 3
            OnClick = RG3DClick
          end
          object CB3D: TCheckBox
            Left = 9
            Top = 142
            Width = 128
            Height = 17
            Caption = '&Automatic'
            Checked = True
            State = cbChecked
            TabOrder = 4
          end
          object PanelYTable: TPanel
            Left = 9
            Top = 165
            Width = 202
            Height = 128
            BevelOuter = bvNone
            TabOrder = 5
          end
        end
        object TabFinancial: TTabSheet
          Caption = 'Financial'
          ImageIndex = 2
          object Label7: TLabel
            Left = 10
            Top = 40
            Width = 30
            Height = 13
            Caption = '&Open:'
          end
          object Label8: TLabel
            Left = 10
            Top = 67
            Width = 30
            Height = 13
            Caption = '&Close:'
          end
          object Label9: TLabel
            Left = 10
            Top = 94
            Width = 25
            Height = 13
            Caption = '&High:'
          end
          object Label10: TLabel
            Left = 10
            Top = 121
            Width = 23
            Height = 13
            Caption = '&Low:'
          end
          object Label12: TLabel
            Left = 10
            Top = 154
            Width = 38
            Height = 13
            Caption = '&Volume:'
          end
          object CBOpen: TComboBox
            Left = 71
            Top = 37
            Width = 121
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 0
            OnChange = CBOpenChange
          end
          object CBAutoFinancial: TCheckBox
            Left = 10
            Top = 9
            Width = 151
            Height = 17
            Caption = '&Automatic'
            Checked = True
            State = cbChecked
            TabOrder = 1
          end
          object CBClose: TComboBox
            Left = 71
            Top = 64
            Width = 121
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 2
            OnChange = CBCloseChange
          end
          object CBHigh: TComboBox
            Left = 71
            Top = 91
            Width = 121
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 3
            OnChange = CBHighChange
          end
          object CBLow: TComboBox
            Left = 71
            Top = 118
            Width = 121
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 4
            OnChange = CBLowChange
          end
          object LBFinancial: TListBox
            Left = 71
            Top = 194
            Width = 121
            Height = 49
            ItemHeight = 13
            Items.Strings = (
              'Automatic'
              'Candle'
              'Point and Figure')
            TabOrder = 5
            OnClick = LBFinancialClick
          end
          object CBVolume: TComboBox
            Left = 71
            Top = 151
            Width = 121
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 6
            OnChange = CBVolumeChange
          end
        end
        object TabGeo: TTabSheet
          Caption = 'Geographic'
          ImageIndex = 3
          object CBFlags: TCheckBox
            Left = 14
            Top = 134
            Width = 137
            Height = 17
            Caption = 'Flags'
            TabOrder = 0
            OnClick = CBFlagsClick
          end
          object CBStates: TCheckBox
            Left = 14
            Top = 157
            Width = 137
            Height = 17
            Caption = 'States'
            TabOrder = 1
            OnClick = CBStatesClick
          end
          object GroupBox3: TGroupBox
            Left = 14
            Top = 16
            Width = 185
            Height = 89
            Caption = 'Map:'
            TabOrder = 2
            object CBAutoMap: TCheckBox
              Left = 16
              Top = 24
              Width = 166
              Height = 17
              Caption = 'Automatic'
              TabOrder = 0
              OnClick = CBAutoMapClick
            end
            object CBMap: TComboBox
              Left = 16
              Top = 47
              Width = 145
              Height = 21
              Style = csDropDownList
              Enabled = False
              ItemHeight = 0
              TabOrder = 1
              OnChange = CBMapChange
            end
          end
          object CBCities: TCheckBox
            Left = 14
            Top = 181
            Width = 137
            Height = 17
            Caption = '&Cities'
            TabOrder = 3
            OnClick = CBCitiesClick
          end
          object CBMap3D: TCheckBox
            Left = 14
            Top = 204
            Width = 97
            Height = 17
            Caption = '3D'
            TabOrder = 4
            OnClick = CBMap3DClick
          end
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 129
        Height = 332
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 1
        object GroupBox1: TGroupBox
          Left = 3
          Top = 3
          Width = 112
          Height = 149
          Caption = '&Mode:'
          TabOrder = 0
          object RBAuto: TRadioButton
            Left = 8
            Top = 19
            Width = 88
            Height = 17
            Caption = '&Automatic'
            TabOrder = 0
            OnClick = RBAutoClick
          end
          object RBXY: TRadioButton
            Left = 8
            Top = 43
            Width = 66
            Height = 17
            Caption = '&2D'
            TabOrder = 1
            OnClick = RBXYClick
          end
          object RB3D: TRadioButton
            Left = 8
            Top = 68
            Width = 89
            Height = 17
            Caption = '&3D'
            TabOrder = 2
            OnClick = RB3DClick
          end
          object RBFinancial: TRadioButton
            Left = 8
            Top = 94
            Width = 89
            Height = 17
            Caption = '&Financial'
            TabOrder = 3
            OnClick = RBFinancialClick
          end
          object RBGeo: TRadioButton
            Left = 8
            Top = 119
            Width = 88
            Height = 17
            Caption = '&Geographic'
            TabOrder = 4
            OnClick = RBGeoClick
          end
        end
        object RGDirection: TRadioGroup
          Left = 3
          Top = 162
          Width = 112
          Height = 89
          Caption = 'Direction:'
          Items.Strings = (
            'Automatic'
            'By Rows'
            'By Columns')
          TabOrder = 1
          OnClick = RGDirectionClick
        end
      end
    end
    object TabView: TTabSheet
      Caption = 'View'
      ImageIndex = 3
      object RGView: TRadioGroup
        Left = 11
        Top = 16
        Width = 134
        Height = 137
        Caption = '&Render:'
        ItemIndex = 0
        Items.Strings = (
          'Automatic'
          '2D'
          '2D Orthogonal'
          '3D')
        TabOrder = 0
        OnClick = RGViewClick
      end
      object CBOpenGL: TCheckBox
        Left = 19
        Top = 166
        Width = 110
        Height = 17
        Caption = '&Open GL'
        TabOrder = 1
        OnClick = CBOpenGLClick
      end
      object RGLegend: TRadioGroup
        Left = 160
        Top = 16
        Width = 105
        Height = 89
        Caption = '&Legend:'
        Items.Strings = (
          'Automatic'
          'Show'
          'Hide')
        TabOrder = 2
        OnClick = RGLegendClick
      end
      object RGMarks: TRadioGroup
        Left = 160
        Top = 111
        Width = 105
        Height = 90
        Caption = '&Marks:'
        Items.Strings = (
          'Automatic'
          'Show'
          'Hide')
        TabOrder = 3
        OnClick = RGMarksClick
      end
    end
    object TabChart: TTabSheet
      Caption = 'Chart'
      ImageIndex = 1
    end
    object TabData: TTabSheet
      Caption = 'Data'
      ImageIndex = 2
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 483
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Button1: TButton
          Left = 16
          Top = 8
          Width = 75
          Height = 25
          Caption = '&Change...'
          TabOrder = 0
          OnClick = Button1Click
        end
      end
    end
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 360
    Width = 491
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Panel9: TPanel
      Left = 388
      Top = 0
      Width = 103
      Height = 37
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object BOK: TButton
        Left = 9
        Top = 6
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Close'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
    end
  end
end
