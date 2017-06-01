object BIDataSetEditor: TBIDataSetEditor
  Left = 0
  Top = 0
  Caption = 'DataSet Editor'
  ClientHeight = 444
  ClientWidth = 412
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 403
    Width = 412
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object CBPreview: TCheckBox
      Left = 16
      Top = 12
      Width = 97
      Height = 17
      Caption = '&Preview'
      TabOrder = 0
      OnClick = CBPreviewClick
    end
    object Panel4: TPanel
      Left = 227
      Top = 0
      Width = 185
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object BOK: TButton
        Left = 13
        Top = 8
        Width = 75
        Height = 25
        Caption = 'OK'
        Enabled = False
        ModalResult = 1
        TabOrder = 0
        OnClick = BOKClick
      end
      object BCancel: TButton
        Left = 101
        Top = 8
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        Default = True
        ModalResult = 2
        TabOrder = 1
      end
    end
    object Button1: TButton
      Left = 119
      Top = 8
      Width = 75
      Height = 25
      Caption = '&New...'
      TabOrder = 2
      OnClick = Button1Click
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 412
    Height = 403
    ActivePage = TabOptions
    Align = alClient
    TabOrder = 1
    OnChange = PageControl1Change
    object TabOptions: TTabSheet
      Caption = 'Options'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 404
        Height = 81
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label1: TLabel
          Left = 3
          Top = 63
          Width = 49
          Height = 13
          Caption = 'Structure:'
        end
        object CBActive: TCheckBox
          Left = 12
          Top = 12
          Width = 125
          Height = 17
          Caption = '&Active'
          TabOrder = 0
          OnClick = CBActiveClick
        end
        object CBReadonly: TCheckBox
          Left = 12
          Top = 35
          Width = 125
          Height = 17
          Caption = '&Read only'
          TabOrder = 1
          OnClick = CBReadonlyClick
        end
      end
      object BITree1: TBITree
        Left = 0
        Top = 81
        Width = 404
        Height = 294
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        ParentColor = False
        TabOrder = 1
      end
    end
    object TabData: TTabSheet
      Caption = 'Data'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
  object Backup: TBIDataset
    RowNumbers = False
    Left = 256
    Top = 224
  end
end
