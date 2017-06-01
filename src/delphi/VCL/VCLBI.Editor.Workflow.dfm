object BIWorkflowEditor: TBIWorkflowEditor
  Left = 0
  Top = 0
  Caption = 'TeeBI - Data Workflow'
  ClientHeight = 630
  ClientWidth = 1021
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 289
    Top = 0
    Height = 630
    ExplicitLeft = 520
    ExplicitTop = 288
    ExplicitHeight = 100
  end
  object PanelSelector: TPanel
    Left = 0
    Top = 0
    Width = 289
    Height = 630
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    Visible = False
  end
  object PanelMain: TPanel
    Left = 292
    Top = 0
    Width = 729
    Height = 630
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object SplitterPreview: TSplitter
      Left = 326
      Top = 41
      Height = 589
      ExplicitLeft = 633
      ExplicitTop = 33
    end
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 729
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object SBSelector: TSpeedButton
        Left = 6
        Top = 13
        Width = 23
        Height = 22
        Caption = '>>'
        Flat = True
        OnClick = SBSelectorClick
      end
      object BAdd: TButton
        Left = 120
        Top = 10
        Width = 58
        Height = 25
        Caption = '&Add...'
        Enabled = False
        TabOrder = 0
        OnClick = BAddClick
      end
      object BDelete: TButton
        Left = 184
        Top = 10
        Width = 65
        Height = 25
        Caption = 'Delete'
        Enabled = False
        TabOrder = 1
        OnClick = BDeleteClick
      end
      object BNew: TButton
        Left = 55
        Top = 10
        Width = 59
        Height = 25
        Caption = '&New...'
        TabOrder = 2
        OnClick = BNewClick
      end
      object PanelPreviewButton: TPanel
        Left = 648
        Top = 0
        Width = 81
        Height = 41
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 3
        object BPreview: TButton
          Left = 8
          Top = 9
          Width = 65
          Height = 25
          Caption = '&Preview'
          TabOrder = 0
          OnClick = BPreviewClick
        end
      end
    end
    object PanelTree: TPanel
      Left = 0
      Top = 41
      Width = 326
      Height = 589
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object SplitterEditor: TSplitter
        Left = 0
        Top = 340
        Width = 326
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        Visible = False
        ExplicitTop = 332
      end
      object BITree1: TBITree
        Left = 0
        Top = 0
        Width = 326
        Height = 340
        Align = alClient
        UseDockManager = False
        ParentBackground = False
        ParentColor = False
        TabOrder = 0
        OnDragDrop = Tree1DragDrop
        OnDragOver = Tree1DragOver
        OnChange = BITree1Change
        OnDeleting = BITree1Deleting
      end
      object PanelEditor: TPanel
        Left = 0
        Top = 343
        Width = 326
        Height = 246
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        Visible = False
      end
    end
    object PanelPreview: TPanel
      Left = 329
      Top = 41
      Width = 400
      Height = 589
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      object SplitterChart: TSplitter
        Left = 0
        Top = 337
        Width = 400
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 0
        ExplicitWidth = 340
      end
      object PanelChart: TPanel
        Left = 0
        Top = 340
        Width = 400
        Height = 249
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        Visible = False
      end
    end
  end
  object PopupAdd: TPopupMenu
    OnPopup = PopupAddPopup
    Left = 328
    Top = 56
    object Clone1: TMenuItem
      Caption = '&Clone'
      OnClick = Clone1Click
    end
    object Filter1: TMenuItem
      Caption = '&Filter'
      OnClick = Filter1Click
    end
    object Gridify1: TMenuItem
      Caption = '&Gridify...'
      OnClick = Gridify1Click
    end
    object Groupby1: TMenuItem
      Caption = 'Group &by...'
      OnClick = Groupby1Click
    end
    object Column1: TMenuItem
      Caption = '&Items'
      object Add1: TMenuItem
        Caption = '&Add...'
        OnClick = Add1Click
      end
      object Change1: TMenuItem
        Caption = '&Convert...'
        OnClick = Change1Click
        object Integer32bit1: TMenuItem
          Caption = '&Integer 32bit'
          RadioItem = True
          OnClick = Boolean1Click
        end
        object Integer64bit1: TMenuItem
          Caption = 'Integer &64bit'
          RadioItem = True
          OnClick = Boolean1Click
        end
        object Singlefloat1: TMenuItem
          Caption = '&Single float'
          RadioItem = True
          OnClick = Boolean1Click
        end
        object Doublefloat1: TMenuItem
          Caption = '&Double float'
          RadioItem = True
          OnClick = Boolean1Click
        end
        object Extendedfloat1: TMenuItem
          Caption = '&Extended float'
          RadioItem = True
          OnClick = Boolean1Click
        end
        object ext1: TMenuItem
          Caption = '&Text'
          RadioItem = True
          OnClick = Boolean1Click
        end
        object DateTime1: TMenuItem
          Caption = 'D&ate Time'
          RadioItem = True
          OnClick = Boolean1Click
        end
        object Boolean1: TMenuItem
          Caption = '&Boolean'
          RadioItem = True
          OnClick = Boolean1Click
        end
      end
      object Delete1: TMenuItem
        Caption = '&Delete...'
        OnClick = Delete1Click
      end
      object Duplicate1: TMenuItem
        Caption = 'D&uplicate...'
        OnClick = Duplicate1Click
      end
      object Rename1: TMenuItem
        Caption = '&Rename...'
        OnClick = Rename1Click
      end
      object Reorder1: TMenuItem
        Caption = 'Re&order...'
        OnClick = Reorder1Click
      end
    end
    object Normalize1: TMenuItem
      Caption = '&Normalize'
      OnClick = Normalize1Click
    end
    object Rank1: TMenuItem
      Caption = '&Rank...'
      OnClick = Rank1Click
    end
    object Shuffle1: TMenuItem
      Caption = 'Sh&uffle'
      OnClick = Shuffle1Click
    end
    object Singlerow1: TMenuItem
      Caption = 'Si&ngle row'
      OnClick = Singlerow1Click
    end
    object Sort1: TMenuItem
      Caption = '&Sort...'
      OnClick = Sort1Click
    end
    object Split1: TMenuItem
      Caption = 'S&plit...'
      OnClick = Split1Click
    end
    object ranspose1: TMenuItem
      Caption = '&Transpose'
      OnClick = ranspose1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Function1: TMenuItem
      Caption = '&Function...'
      OnClick = Function1Click
    end
    object MachineLearning1: TMenuItem
      Caption = '&Machine Learning'
    end
    object Regression1: TMenuItem
      Caption = '&Regression...'
      OnClick = Regression1Click
    end
    object CrossTableConfusionMatrix1: TMenuItem
      Caption = 'Confusion Matrix'
      OnClick = CrossTableConfusionMatrix1Click
    end
  end
  object PopupNew: TPopupMenu
    OnPopup = PopupAddPopup
    Left = 328
    Top = 128
    object MenuItem34: TMenuItem
      Caption = '&Import from'
      object MenuItem35: TMenuItem
        Caption = '&BIWeb Server...'
        OnClick = BIWebServer1Click
      end
      object MenuItem36: TMenuItem
        Caption = '&Database Server...'
        OnClick = DatabaseServer1Click
      end
      object MenuItem37: TMenuItem
        Caption = '&Files...'
        OnClick = Files1Click
      end
    end
    object MenuItem38: TMenuItem
      Caption = 'Custom &Data...'
      OnClick = NewCustomData1Click
    end
    object MenuItem29: TMenuItem
      Caption = '-'
    end
    object MenuItem2: TMenuItem
      Caption = '&Compare...'
      OnClick = Compare1Click
    end
    object MenuItem20: TMenuItem
      Caption = '&Merge'
      object Join1: TMenuItem
        Caption = '&Join...'
        OnClick = Join1Click
      end
      object Common1: TMenuItem
        Caption = '&Common...'
        OnClick = Common1Click
      end
      object Different1: TMenuItem
        Caption = '&Different...'
        OnClick = Different1Click
      end
    end
    object MenuItem22: TMenuItem
      Caption = '&Query...'
      OnClick = Query1Click
    end
  end
  object PopupPreview: TPopupMenu
    Left = 952
    Top = 72
    object Grid1: TMenuItem
      Caption = '&Grid'
      Checked = True
      OnClick = Grid1Click
    end
    object Chart1: TMenuItem
      Caption = '&Chart'
      OnClick = Chart1Click
    end
  end
end
