object DateTimeFilterEditor: TDateTimeFilterEditor
  Left = 0
  Top = 0
  Caption = 'DateTime Filter Editor'
  ClientHeight = 352
  ClientWidth = 465
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 465
    Height = 352
    ActivePage = TabCommon
    Align = alClient
    TabOrder = 0
    OnChange = PageControl1Change
    object TabCommon: TTabSheet
      Caption = 'Common'
      object LBCommon: TListBox
        Left = 0
        Top = 0
        Width = 145
        Height = 324
        Align = alLeft
        ItemHeight = 13
        Items.Strings = (
          'All time'
          'Today'
          'Yesterday'
          'This week'
          'Last 7 days'
          'Last 14 days'
          'This month'
          'This quarter'
          'This year'
          'Last week'
          'Last month'
          'Last quarter'
          'Last year'
          'Last decade'
          'Tomorrow'
          'Next week'
          'Next month'
          'Next quarter'
          'Next year')
        TabOrder = 0
        OnClick = LBCommonClick
        ExplicitLeft = 16
        ExplicitTop = 16
        ExplicitHeight = 265
      end
    end
    object TabCustom: TTabSheet
      Caption = 'Custom'
      ImageIndex = 1
    end
    object TabIncluded: TTabSheet
      Caption = 'Include'
      ImageIndex = 2
      object Splitter1: TSplitter
        Left = 153
        Top = 0
        Height = 324
        ExplicitLeft = 216
        ExplicitTop = 64
        ExplicitHeight = 100
      end
      object PanelMonths: TPanel
        Left = 0
        Top = 0
        Width = 153
        Height = 324
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitLeft = 8
      end
      object PanelWeeks: TPanel
        Left = 156
        Top = 0
        Width = 301
        Height = 324
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitWidth = 285
        ExplicitHeight = 232
      end
    end
  end
end
