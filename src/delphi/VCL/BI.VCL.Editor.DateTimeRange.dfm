object DateTimeRangeEditor: TDateTimeRangeEditor
  Left = 0
  Top = 0
  Caption = 'DateTime Range Editor'
  ClientHeight = 260
  ClientWidth = 449
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
    Width = 449
    Height = 260
    ActivePage = TabRange
    Align = alClient
    TabOrder = 0
    OnChange = PageControl1Change
    object TabRange: TTabSheet
      Caption = 'Range'
      object Label1: TLabel
        Left = 6
        Top = 6
        Width = 28
        Height = 13
        Caption = 'From:'
      end
      object Label2: TLabel
        Left = 6
        Top = 50
        Width = 16
        Height = 13
        Caption = 'To:'
      end
      object CBDay: TComboBox
        Left = 6
        Top = 23
        Width = 51
        Height = 21
        Style = csDropDownList
        DropDownCount = 31
        TabOrder = 0
        OnChange = CBDayChange
      end
      object CBMonth: TComboBox
        Left = 75
        Top = 23
        Width = 110
        Height = 21
        Style = csDropDownList
        DropDownCount = 13
        TabOrder = 1
        OnChange = CBMonthChange
      end
      object CBYear: TComboBox
        Left = 203
        Top = 23
        Width = 70
        Height = 21
        Style = csDropDownList
        DropDownCount = 13
        TabOrder = 2
        OnChange = CBYearChange
      end
      object CBDayTo: TComboBox
        Left = 6
        Top = 66
        Width = 51
        Height = 21
        Style = csDropDownList
        DropDownCount = 31
        TabOrder = 3
        OnChange = CBDayToChange
      end
      object CBMonthTo: TComboBox
        Left = 75
        Top = 66
        Width = 110
        Height = 21
        Style = csDropDownList
        DropDownCount = 13
        TabOrder = 4
        OnChange = CBMonthToChange
      end
      object CBYearTo: TComboBox
        Left = 203
        Top = 66
        Width = 70
        Height = 21
        Style = csDropDownList
        DropDownCount = 13
        TabOrder = 5
        OnChange = CBYearToChange
      end
      object CBPeriod: TComboBox
        Left = 6
        Top = 104
        Width = 115
        Height = 21
        Style = csDropDownList
        TabOrder = 6
        OnChange = CBPeriodChange
        Items.Strings = (
          'Custom'
          'All time'
          'Today'
          'Yesterday'
          'This'
          'Last'
          'Next')
      end
      object CBPeriod2: TComboBox
        Left = 134
        Top = 104
        Width = 115
        Height = 21
        Style = csDropDownList
        Enabled = False
        TabOrder = 7
        OnChange = CBPeriod2Change
        Items.Strings = (
          'Week'
          'Month'
          'Quarter'
          'Year')
      end
    end
    object TabCalendar: TTabSheet
      Caption = 'Calendar'
      ImageIndex = 1
      object Label3: TLabel
        Left = 5
        Top = 3
        Width = 28
        Height = 13
        Caption = '&From:'
        FocusControl = CalendarFrom
      end
      object Label4: TLabel
        Left = 192
        Top = 3
        Width = 16
        Height = 13
        Caption = '&To:'
        FocusControl = CalendarTo
      end
      object CalendarFrom: TMonthCalendar
        Left = 3
        Top = 20
        Width = 170
        Height = 143
        Date = 42509.510965185180000000
        ShowToday = False
        ShowTodayCircle = False
        TabOrder = 0
        OnClick = CalendarFromClick
      end
      object CalendarTo: TMonthCalendar
        Left = 192
        Top = 20
        Width = 169
        Height = 143
        Date = 42509.510965185180000000
        ShowToday = False
        ShowTodayCircle = False
        TabOrder = 1
        OnClick = CalendarFromClick
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Selected'
      ImageIndex = 2
      object PanelMonths: TPanel
        Left = 0
        Top = 0
        Width = 185
        Height = 232
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
      end
      object PanelWeeks: TPanel
        Left = 185
        Top = 0
        Width = 185
        Height = 232
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 1
      end
    end
    object TabFromTo: TTabSheet
      Caption = 'From / To'
      ImageIndex = 3
    end
  end
end
