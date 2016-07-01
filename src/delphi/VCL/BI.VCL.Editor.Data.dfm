object DataEditor: TDataEditor
  Left = 220
  Top = 210
  Caption = 'Data Definition Editor'
  ClientHeight = 436
  ClientWidth = 518
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TabSources: TPageControl
    Left = 0
    Top = 0
    Width = 518
    Height = 395
    ActivePage = TabDatabase
    Align = alClient
    TabOrder = 0
    object TabFiles: TTabSheet
      Caption = 'Files'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object PageControlFile: TPageControl
        Left = 0
        Top = 0
        Width = 510
        Height = 367
        ActivePage = TabFile
        Align = alClient
        TabOrder = 0
        object TabFile: TTabSheet
          Caption = 'Single File'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object Label2: TLabel
            Left = 16
            Top = 16
            Width = 55
            Height = 13
            Caption = '&File or URL:'
            FocusControl = EFile
          end
          object LFileMissing: TLabel
            Left = 16
            Top = 72
            Width = 87
            Height = 13
            Caption = 'File does not exist'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clMaroon
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            Visible = False
          end
          object EFile: TEdit
            Left = 16
            Top = 35
            Width = 345
            Height = 21
            TabOrder = 0
            OnChange = EFileChange
          end
          object Button3: TButton
            Left = 367
            Top = 33
            Width = 26
            Height = 25
            Caption = '...'
            TabOrder = 1
            OnClick = Button3Click
          end
        end
        object TabFolder: TTabSheet
          Caption = 'Folder'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object Label3: TLabel
            Left = 16
            Top = 16
            Width = 34
            Height = 13
            Caption = '&Folder:'
            FocusControl = EFolder
          end
          object LFolderMissing: TLabel
            Left = 16
            Top = 62
            Width = 101
            Height = 13
            Caption = 'Folder does not exist'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clMaroon
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            Visible = False
          end
          object Label12: TLabel
            Left = 16
            Top = 200
            Width = 74
            Height = 13
            Caption = '&Excluded mask:'
          end
          object EFolder: TEdit
            Left = 16
            Top = 35
            Width = 345
            Height = 21
            TabOrder = 0
            OnChange = EFolderChange
          end
          object Button4: TButton
            Left = 367
            Top = 33
            Width = 26
            Height = 25
            Caption = '...'
            TabOrder = 1
            OnClick = Button4Click
          end
          object CBAllFolder: TCheckBox
            Left = 16
            Top = 96
            Width = 201
            Height = 17
            Caption = '&All files from folder'
            TabOrder = 2
            OnClick = CBAllFolderClick
          end
          object CBRecursive: TCheckBox
            Left = 16
            Top = 168
            Width = 249
            Height = 17
            Caption = '&Recursive subfolders'
            TabOrder = 3
            OnClick = CBRecursiveClick
          end
          object EFileMask: TEdit
            Left = 16
            Top = 128
            Width = 101
            Height = 21
            TabOrder = 4
            Text = '*.*'
            OnChange = EFileMaskChange
          end
          object CBFileType: TComboBox
            Left = 136
            Top = 128
            Width = 145
            Height = 21
            Style = csDropDownList
            ItemIndex = 0
            TabOrder = 5
            Text = 'Custom'
            OnChange = CBFileTypeChange
            Items.Strings = (
              'Custom'
              'CSV'
              'Text'
              'Microsoft Excel'
              'Embarcadero ClientDataset'
              'JSON'
              'XML')
          end
          object Button5: TButton
            Left = 16
            Top = 256
            Width = 161
            Height = 25
            Caption = '&List of included files...'
            TabOrder = 6
            OnClick = Button5Click
          end
          object EExcluded: TEdit
            Left = 16
            Top = 219
            Width = 101
            Height = 21
            TabOrder = 7
            OnChange = EExcludedChange
          end
        end
        object TabFTP: TTabSheet
          Caption = 'FTP'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object Label18: TLabel
            Left = 20
            Top = 16
            Width = 36
            Height = 13
            Caption = '&Server:'
            FocusControl = EFTPServer
          end
          object Label19: TLabel
            Left = 22
            Top = 64
            Width = 24
            Height = 13
            Caption = '&Port:'
            FocusControl = EFTPPort
          end
          object Label20: TLabel
            Left = 20
            Top = 120
            Width = 26
            Height = 13
            Caption = '&User:'
            FocusControl = EFTPUser
          end
          object Label21: TLabel
            Left = 20
            Top = 168
            Width = 50
            Height = 13
            Caption = 'P&assword:'
            FocusControl = EFTPPassword
          end
          object LFTPStatus: TLabel
            Left = 22
            Top = 271
            Width = 3
            Height = 13
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clMaroon
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object BFTPTest: TButton
            Left = 20
            Top = 240
            Width = 141
            Height = 25
            Caption = '&Test Connection...'
            Enabled = False
            TabOrder = 0
            OnClick = BFTPTestClick
          end
          object EFTPServer: TEdit
            Left = 20
            Top = 35
            Width = 241
            Height = 21
            TabOrder = 1
            OnChange = EFTPServerChange
          end
          object EFTPPort: TEdit
            Left = 20
            Top = 83
            Width = 65
            Height = 21
            TabOrder = 2
            Text = '0'
            OnChange = EFTPPortChange
          end
          object UDFTPPort: TUpDown
            Left = 85
            Top = 83
            Width = 16
            Height = 21
            Associate = EFTPPort
            Max = -1
            TabOrder = 3
            Thousands = False
          end
          object EFTPUser: TEdit
            Left = 20
            Top = 139
            Width = 141
            Height = 21
            TabOrder = 4
            OnChange = EFTPUserChange
          end
          object EFTPPassword: TEdit
            Left = 20
            Top = 187
            Width = 141
            Height = 21
            PasswordChar = '*'
            TabOrder = 5
            OnChange = EFTPPasswordChange
          end
        end
      end
    end
    object TabDatabase: TTabSheet
      Caption = 'Database'
      ImageIndex = 1
      object PageControlDBItems: TPageControl
        Left = 0
        Top = 0
        Width = 510
        Height = 367
        ActivePage = SQL
        Align = alClient
        TabOrder = 0
        object TabConnection: TTabSheet
          Caption = 'Connection'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object Label7: TLabel
            Left = 16
            Top = 8
            Width = 33
            Height = 13
            Caption = '&Driver:'
            FocusControl = CBDBDriver
          end
          object Label8: TLabel
            Left = 18
            Top = 64
            Width = 36
            Height = 13
            Caption = '&Server:'
            FocusControl = EDBServer
          end
          object Label9: TLabel
            Left = 18
            Top = 112
            Width = 50
            Height = 13
            Caption = '&Database:'
            FocusControl = EDBDatabase
          end
          object Label10: TLabel
            Left = 18
            Top = 168
            Width = 55
            Height = 13
            Caption = '&User name:'
            FocusControl = EDBUser
          end
          object Label11: TLabel
            Left = 18
            Top = 216
            Width = 50
            Height = 13
            Caption = '&Password:'
            FocusControl = EDBPassword
          end
          object Label17: TLabel
            Left = 192
            Top = 64
            Width = 24
            Height = 13
            Caption = 'P&ort:'
            FocusControl = EDBPort
          end
          object CBDBDriver: TComboBox
            Left = 16
            Top = 27
            Width = 153
            Height = 21
            Style = csDropDownList
            TabOrder = 0
            OnChange = CBDBDriverChange
          end
          object BDBTest: TButton
            Left = 192
            Top = 25
            Width = 145
            Height = 25
            Caption = '&Test Connection...'
            Enabled = False
            TabOrder = 1
            OnClick = BDBTestClick
          end
          object EDBServer: TEdit
            Left = 18
            Top = 83
            Width = 151
            Height = 21
            TabOrder = 2
            OnChange = EDBServerChange
          end
          object EDBDatabase: TEdit
            Left = 18
            Top = 131
            Width = 319
            Height = 21
            TabOrder = 3
            OnChange = EDBDatabaseChange
          end
          object EDBUser: TEdit
            Left = 18
            Top = 187
            Width = 151
            Height = 21
            TabOrder = 4
            OnChange = EDBUserChange
          end
          object EDBPassword: TEdit
            Left = 18
            Top = 235
            Width = 151
            Height = 21
            PasswordChar = '*'
            TabOrder = 5
            OnChange = EDBPasswordChange
          end
          object CBLogin: TCheckBox
            Left = 18
            Top = 262
            Width = 151
            Height = 17
            Caption = '&Login prompt'
            TabOrder = 6
            OnClick = CBLoginClick
          end
          object MemoDBTest: TMemo
            Left = 0
            Top = 285
            Width = 502
            Height = 54
            Align = alBottom
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 7
            Visible = False
          end
          object EDBPort: TEdit
            Left = 192
            Top = 83
            Width = 57
            Height = 21
            TabOrder = 8
            OnChange = EDBPortChange
          end
          object Button6: TButton
            Left = 343
            Top = 129
            Width = 26
            Height = 25
            Caption = '...'
            TabOrder = 9
            OnClick = Button6Click
          end
        end
        object SQL: TTabSheet
          Caption = 'Items'
          ImageIndex = 1
          object PageControl3: TPageControl
            Left = 0
            Top = 0
            Width = 502
            Height = 339
            ActivePage = TabItemAllDB
            Align = alClient
            TabOrder = 0
            object TabItemAllDB: TTabSheet
              Caption = 'All'
              object LDBExclude: TLabel
                Left = 16
                Top = 96
                Width = 68
                Height = 13
                Caption = '&Exclude mask:'
                Enabled = False
                FocusControl = EDBExclude
              end
              object LDBInclude: TLabel
                Left = 16
                Top = 45
                Width = 66
                Height = 13
                Caption = '&Include mask:'
                Enabled = False
                FocusControl = EDBInclude
              end
              object CBAllItems: TCheckBox
                Left = 16
                Top = 16
                Width = 101
                Height = 17
                Caption = '&All Items'
                Checked = True
                State = cbChecked
                TabOrder = 0
                OnClick = CBAllItemsClick
              end
              object EDBInclude: TEdit
                Left = 16
                Top = 64
                Width = 101
                Height = 21
                Enabled = False
                TabOrder = 1
                OnChange = EDBIncludeChange
              end
              object EDBExclude: TEdit
                Left = 16
                Top = 115
                Width = 101
                Height = 21
                Enabled = False
                TabOrder = 2
                OnChange = EExcludedChange
              end
              object Button1: TButton
                Left = 16
                Top = 168
                Width = 153
                Height = 25
                Caption = '&List of included items...'
                TabOrder = 3
                OnClick = Button1Click
              end
              object CBDBSystem: TCheckBox
                Left = 200
                Top = 16
                Width = 137
                Height = 17
                Caption = '&System Items'
                TabOrder = 4
                OnClick = CBDBSystemClick
              end
              object CBDBViews: TCheckBox
                Left = 200
                Top = 39
                Width = 137
                Height = 17
                Caption = 'Include &Views'
                TabOrder = 5
                OnClick = CBDBViewsClick
              end
            end
            object TabSQL: TTabSheet
              Caption = 'Custom SQL'
              ImageIndex = 1
              ExplicitLeft = 0
              ExplicitTop = 0
              ExplicitWidth = 0
              ExplicitHeight = 0
              object MemoSQL: TMemo
                Left = 0
                Top = 0
                Width = 494
                Height = 311
                Align = alClient
                Lines.Strings = (
                  'select * from')
                ScrollBars = ssBoth
                TabOrder = 0
                OnChange = MemoSQLChange
              end
            end
          end
        end
        object TabDBOptions: TTabSheet
          Caption = 'Options'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object CBParallel: TCheckBox
            Left = 16
            Top = 16
            Width = 153
            Height = 17
            Caption = 'Use multiple threads'
            TabOrder = 0
            OnClick = CBParallelClick
          end
          object CBStats: TCheckBox
            Left = 16
            Top = 48
            Width = 153
            Height = 17
            Caption = 'Precalculate Statistics'
            TabOrder = 1
            OnClick = CBStatsClick
          end
        end
      end
    end
    object TabWeb: TTabSheet
      Caption = 'BI Web'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LayoutWebData: TPanel
        Left = 0
        Top = 257
        Width = 510
        Height = 110
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        OnResize = LayoutWebDataResize
        object Label6: TLabel
          Left = 8
          Top = 2
          Width = 27
          Height = 13
          Caption = '&Data:'
        end
        object Label1: TLabel
          Left = 8
          Top = 52
          Width = 23
          Height = 13
          Caption = '&URL:'
          FocusControl = EWebURL
        end
        object CBWebData: TComboBox
          Left = 8
          Top = 21
          Width = 177
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = CBWebDataChange
          OnDropDown = CBWebDataDropDown
        end
        object EWebURL: TEdit
          Left = 8
          Top = 71
          Width = 492
          Height = 21
          TabOrder = 1
          OnChange = EWebURLChange
        end
      end
      object PageControlWeb: TPageControl
        Left = 0
        Top = 0
        Width = 510
        Height = 257
        ActivePage = TabHttpServer
        Align = alTop
        TabOrder = 1
        object TabHttpServer: TTabSheet
          Caption = 'Server'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object Label4: TLabel
            Left = 12
            Top = 8
            Width = 26
            Height = 13
            Caption = '&Host:'
            FocusControl = EWebServer
          end
          object Label5: TLabel
            Left = 14
            Top = 56
            Width = 24
            Height = 13
            Caption = '&Port:'
            FocusControl = EPort
          end
          object LWebTest: TLabel
            Left = 20
            Top = 190
            Width = 3
            Height = 13
          end
          object Label22: TLabel
            Left = 118
            Top = 56
            Width = 42
            Height = 13
            Caption = 'T&imeout:'
            FocusControl = ETimeout
          end
          object Label23: TLabel
            Left = 204
            Top = 78
            Width = 28
            Height = 13
            Caption = 'msec.'
            FocusControl = ETimeout
          end
          object BWebTest: TButton
            Left = 12
            Top = 159
            Width = 177
            Height = 25
            Caption = '&Test Connection...'
            Enabled = False
            TabOrder = 0
            OnClick = BWebTestClick
          end
          object CBZip: TCheckBox
            Left = 12
            Top = 112
            Width = 177
            Height = 17
            Caption = '&Compression'
            Checked = True
            State = cbChecked
            TabOrder = 1
            OnClick = CBZipClick
          end
          object EPort: TEdit
            Left = 12
            Top = 75
            Width = 65
            Height = 21
            TabOrder = 2
            Text = '0'
            OnChange = EPortChange
          end
          object EWebServer: TEdit
            Left = 12
            Top = 27
            Width = 241
            Height = 21
            TabOrder = 3
            OnChange = EWebServerChange
          end
          object UDPort: TUpDown
            Left = 77
            Top = 75
            Width = 16
            Height = 21
            Associate = EPort
            Max = -1
            TabOrder = 4
            Thousands = False
          end
          object ETimeout: TEdit
            Left = 116
            Top = 75
            Width = 65
            Height = 21
            TabOrder = 5
            Text = '2000'
            OnChange = ETimeoutChange
          end
          object UDTimeout: TUpDown
            Left = 181
            Top = 75
            Width = 16
            Height = 21
            Associate = ETimeout
            Max = 65535
            Position = 2000
            TabOrder = 6
            Thousands = False
          end
        end
        object TabHttpProxy: TTabSheet
          Caption = 'Proxy'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object Label13: TLabel
            Left = 12
            Top = 16
            Width = 26
            Height = 13
            Caption = '&Host:'
            FocusControl = EProxyHost
          end
          object Label14: TLabel
            Left = 198
            Top = 16
            Width = 24
            Height = 13
            Caption = '&Port:'
            FocusControl = EProxyPort
          end
          object Label15: TLabel
            Left = 12
            Top = 64
            Width = 55
            Height = 13
            Caption = '&User name:'
            FocusControl = EProxyUser
          end
          object Label16: TLabel
            Left = 12
            Top = 112
            Width = 50
            Height = 13
            Caption = '&Password:'
            FocusControl = EProxyPassword
          end
          object EProxyHost: TEdit
            Left = 12
            Top = 35
            Width = 169
            Height = 21
            TabOrder = 0
            OnChange = EProxyHostChange
          end
          object EProxyPort: TEdit
            Left = 196
            Top = 35
            Width = 65
            Height = 21
            TabOrder = 1
            Text = '0'
            OnChange = EProxyPortChange
          end
          object UDProxyPort: TUpDown
            Left = 261
            Top = 35
            Width = 16
            Height = 21
            Associate = EProxyPort
            Max = -1
            TabOrder = 2
            Thousands = False
          end
          object EProxyUser: TEdit
            Left = 12
            Top = 83
            Width = 169
            Height = 21
            TabOrder = 3
            OnChange = EProxyUserChange
          end
          object EProxyPassword: TEdit
            Left = 12
            Top = 131
            Width = 169
            Height = 21
            PasswordChar = '*'
            TabOrder = 4
            OnChange = EProxyPasswordChange
          end
        end
      end
    end
    object TabUnknown: TTabSheet
      Caption = 'Style'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object LabelUnknown: TLabel
        Left = 24
        Top = 24
        Width = 123
        Height = 13
        Caption = 'Missing data definition file'
      end
      object RGKind: TRadioGroup
        Left = 24
        Top = 56
        Width = 225
        Height = 161
        Caption = '&Import Style:'
        ItemIndex = 0
        Items.Strings = (
          'Files, Folders or URL'
          'Database Server'
          'BI Web Server')
        TabOrder = 0
        Visible = False
        OnClick = RGKindClick
      end
    end
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 395
    Width = 518
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Panel1: TPanel
      Left = 333
      Top = 0
      Width = 185
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object BOK: TButton
        Left = 8
        Top = 6
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
        OnClick = BOKClick
      end
      object Button2: TButton
        Left = 96
        Top = 6
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.txt'
    Filter = 
      'ClientDataSet files|*.cds|CSV files|*.csv|Text files|*.txt|JSON ' +
      'files|*.json|XML files|*.xml|Microsoft Excel files|*.xls;*.xlsx|' +
      'All files|*.cds;*.csv;*.txt;*.json;*.xml;*.xls;*.xlsx'
    FilterIndex = 7
    Options = [ofReadOnly, ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Select file to import'
    Left = 352
    Top = 208
  end
  object OpenDialogDatabase: TOpenDialog
    Options = [ofReadOnly, ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Select Database to import'
    Left = 352
    Top = 272
  end
end
