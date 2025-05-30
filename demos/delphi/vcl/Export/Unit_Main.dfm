object ExportDemo: TExportDemo
  Left = 0
  Top = 0
  Caption = 'TeeBI Export Demo'
  ClientHeight = 623
  ClientWidth = 978
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 265
    Top = 41
    Height = 582
    ExplicitLeft = 496
    ExplicitTop = 280
    ExplicitHeight = 100
  end
  object PageControl1: TPageControl
    Left = 268
    Top = 41
    Width = 710
    Height = 582
    ActivePage = TabExport
    Align = alClient
    TabOrder = 0
    OnChange = PageControl1Change
    object TabGrid: TTabSheet
      Caption = 'Grid'
      object DBNavigator1: TDBNavigator
        Left = 0
        Top = 529
        Width = 702
        Height = 25
        VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbRefresh]
        Align = alBottom
        TabOrder = 0
      end
      object BIGrid1: TBIGrid
        Left = 0
        Top = 0
        Width = 702
        Height = 529
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        ParentColor = False
        TabOrder = 1
      end
    end
    object TabExport: TTabSheet
      Caption = 'Export'
      ImageIndex = 1
      object Memo1: TMemo
        Left = 0
        Top = 41
        Width = 702
        Height = 513
        Align = alClient
        Lines.Strings = (
          'Memo1')
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 702
        Height = 41
        Align = alTop
        TabOrder = 1
        object RGExport: TRadioGroup
          Left = 8
          Top = 1
          Width = 249
          Height = 35
          Columns = 4
          DragMode = dmAutomatic
          ItemIndex = 0
          Items.Strings = (
            'JSON'
            'XML'
            'CSV'
            'HTML')
          TabOrder = 0
          OnClick = RGExportClick
        end
        object Button3: TButton
          Left = 272
          Top = 8
          Width = 113
          Height = 25
          Caption = 'Copy to clipboard'
          TabOrder = 1
          OnClick = Button3Click
        end
      end
    end
    object TabHTML: TTabSheet
      Caption = 'HTML'
      ImageIndex = 2
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 702
        Height = 41
        Align = alTop
        TabOrder = 0
        object Button1: TButton
          Left = 8
          Top = 8
          Width = 113
          Height = 25
          Caption = '&Open in Browser...'
          TabOrder = 0
          OnClick = Button1Click
        end
      end
    end
    object TabPDF: TTabSheet
      Caption = 'PDF'
      ImageIndex = 3
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 702
        Height = 41
        Align = alTop
        TabOrder = 0
        object Button2: TButton
          Left = 8
          Top = 8
          Width = 113
          Height = 25
          Caption = '&Open PDF file...'
          TabOrder = 0
          OnClick = Button2Click
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 41
    Width = 265
    Height = 582
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 978
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 16
      Top = 14
      Width = 80
      Height = 13
      Caption = 'Number of rows:'
    end
    object LRows: TLabel
      Left = 112
      Top = 14
      Width = 6
      Height = 13
      Caption = '0'
    end
  end
end
