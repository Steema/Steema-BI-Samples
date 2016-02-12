object ImportDemoForm: TImportDemoForm
  Left = 0
  Top = 0
  Caption = 'TeeBI Import Example'
  ClientHeight = 637
  ClientWidth = 885
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 596
    Width = 885
    Height = 41
    Align = alBottom
    TabOrder = 0
    object BNext: TButton
      Left = 136
      Top = 9
      Width = 75
      Height = 25
      Caption = '&Next >'
      Enabled = False
      TabOrder = 0
      OnClick = BNextClick
    end
    object Panel2: TPanel
      Left = 778
      Top = 1
      Width = 106
      Height = 39
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object Button2: TButton
        Left = 16
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Close'
        TabOrder = 0
        OnClick = Button2Click
      end
    end
    object BPrevious: TButton
      Left = 32
      Top = 9
      Width = 75
      Height = 25
      Caption = '< &Previous'
      Enabled = False
      TabOrder = 2
      OnClick = BPreviousClick
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 885
    Height = 41
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 14
      Width = 225
      Height = 13
      Caption = 'Select a sample data and click Next to continue'
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 41
    Width = 885
    Height = 555
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 20
    Padding.Top = 20
    Padding.Right = 20
    Padding.Bottom = 20
    TabOrder = 2
    object Samples: TTreeView
      Left = 20
      Top = 20
      Width = 845
      Height = 515
      Align = alClient
      Indent = 19
      TabOrder = 0
      OnChange = SamplesChange
      OnDblClick = SamplesDblClick
    end
    object PageControl1: TPageControl
      Left = 20
      Top = 20
      Width = 845
      Height = 515
      ActivePage = TabStructure
      Align = alClient
      TabOrder = 1
      Visible = False
      OnChange = PageControl1Change
      object TabSheet1: TTabSheet
        Caption = 'Data'
        object BIGrid1: TBIGrid
          Left = 0
          Top = 0
          Width = 837
          Height = 487
          Align = alClient
          UseDockManager = False
          ParentBackground = False
          TabOrder = 0
        end
      end
      object TabStructure: TTabSheet
        Caption = 'Structure'
        ImageIndex = 1
      end
    end
  end
end
