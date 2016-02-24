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
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
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
    ActivePage = TabPDF
    Align = alClient
    TabOrder = 0
    OnChange = PageControl1Change
    object TabGrid: TTabSheet
      Caption = 'Grid'
      ExplicitWidth = 1101
      ExplicitHeight = 455
      object DBNavigator1: TDBNavigator
        Left = 0
        Top = 529
        Width = 702
        Height = 25
        VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbRefresh]
        Align = alBottom
        TabOrder = 0
        ExplicitTop = 0
        ExplicitWidth = 1101
      end
      object BIGrid1: TBIGrid
        Left = 0
        Top = 0
        Width = 702
        Height = 529
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        TabOrder = 1
        ExplicitTop = 25
        ExplicitWidth = 1101
        ExplicitHeight = 180
      end
    end
    object TabExport: TTabSheet
      Caption = 'Export'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
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
        ExplicitLeft = 2
        ExplicitWidth = 705
        ExplicitHeight = 554
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 702
        Height = 41
        Align = alTop
        TabOrder = 1
        ExplicitWidth = 1101
        object RGExport: TRadioGroup
          Left = 8
          Top = 4
          Width = 249
          Height = 31
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
      ExplicitWidth = 970
      ExplicitHeight = 595
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 702
        Height = 41
        Align = alTop
        TabOrder = 0
        ExplicitLeft = 256
        ExplicitTop = 256
        ExplicitWidth = 185
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
      ExplicitLeft = 3
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 702
        Height = 41
        Align = alTop
        TabOrder = 0
        ExplicitTop = 8
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
    ExplicitTop = 0
    ExplicitHeight = 623
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 978
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 408
    ExplicitTop = 312
    ExplicitWidth = 185
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
