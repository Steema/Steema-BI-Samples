object Form35: TForm35
  Left = 0
  Top = 0
  Caption = 'BIGrid Custom Filtering'
  ClientHeight = 640
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object BIGrid1: TBIGrid
    Left = 302
    Top = 70
    Width = 333
    Height = 570
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    ExplicitTop = 41
    ExplicitHeight = 599
  end
  object Panel5: TPanel
    Left = 0
    Top = 70
    Width = 302
    Height = 570
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 41
    ExplicitHeight = 599
    object Splitter1: TSplitter
      Left = 0
      Top = 321
      Width = 302
      Height = 3
      Cursor = crVSplit
      Align = alTop
      ExplicitLeft = -6
      ExplicitTop = 312
    end
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 302
      Height = 321
      ActivePage = TabDate
      Align = alTop
      TabOrder = 0
      OnChange = PageControl1Change
      object TabDate: TTabSheet
        Caption = 'Date'
        ExplicitWidth = 281
        ExplicitHeight = 299
        object LBDateExamples: TListBox
          Left = 0
          Top = 0
          Width = 294
          Height = 293
          Align = alClient
          ItemHeight = 13
          Items.Strings = (
            'Date=Yesterday'
            'Date=Today'
            'Date=Tomorrow'
            'Date=Last 5 days'
            'Date=Next 5 days'
            'This Day'
            'This Month'
            'Last Month'
            'Next Month'
            'This Year'
            'Last Year'
            'Next Year'
            'Aprils'
            'This Year April'
            'Saturdays'
            'This Week'
            'Last Week'
            'Next Week'
            'Day = 13'
            'From >= <= To')
          TabOrder = 0
          OnClick = LBDateExamplesClick
          ExplicitWidth = 281
          ExplicitHeight = 299
        end
      end
      object TabNumber: TTabSheet
        Caption = 'Number'
        ImageIndex = 1
        ExplicitWidth = 281
        ExplicitHeight = 299
        object LBNumberExamples: TListBox
          Left = 0
          Top = 0
          Width = 294
          Height = 293
          Align = alClient
          ItemHeight = 13
          Items.Strings = (
            '= 1234'
            '<> 1234'
            '>= 0'
            '> 200'
            '> -500 and < 500'
            '>= -300 and <= 300')
          TabOrder = 0
          OnClick = LBNumberExamplesClick
          ExplicitWidth = 281
          ExplicitHeight = 299
        end
      end
      object TabText: TTabSheet
        Caption = 'Text'
        ImageIndex = 2
        ExplicitWidth = 281
        ExplicitHeight = 299
        object LBTextExamples: TListBox
          Left = 0
          Top = 0
          Width = 294
          Height = 293
          Align = alClient
          ItemHeight = 13
          Items.Strings = (
            'contains "nter"'
            'is equal "Summer"'
            'starts "Win"'
            'ends "ing"'
            'is empty')
          TabOrder = 0
          OnClick = LBTextExamplesClick
          ExplicitWidth = 281
          ExplicitHeight = 299
        end
      end
    end
    object Panel2: TPanel
      Left = 0
      Top = 324
      Width = 302
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitTop = 291
      object CBEnabled: TCheckBox
        Left = 16
        Top = 10
        Width = 74
        Height = 17
        Caption = 'Enabled'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = CBEnabledClick
      end
      object CBInverted: TCheckBox
        Left = 96
        Top = 10
        Width = 97
        Height = 17
        Caption = 'Inverted'
        TabOrder = 1
        OnClick = CBInvertedClick
      end
    end
    object PanelEditor: TPanel
      Left = 0
      Top = 365
      Width = 302
      Height = 205
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitTop = 368
      ExplicitHeight = 231
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 70
    Align = alTop
    TabOrder = 2
    object Label1: TLabel
      Left = 16
      Top = 42
      Width = 28
      Height = 13
      Caption = '&Filter:'
    end
    object LRows: TLabel
      Left = 144
      Top = 12
      Width = 30
      Height = 13
      Caption = 'Rows:'
    end
    object Button1: TButton
      Left = 15
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Edit...'
      TabOrder = 0
      OnClick = Button1Click
    end
    object EFilter: TEdit
      Left = 50
      Top = 38
      Width = 567
      Height = 21
      TabOrder = 1
    end
  end
end
