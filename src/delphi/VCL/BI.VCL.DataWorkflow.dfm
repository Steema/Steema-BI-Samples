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
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 289
    Top = 41
    Height = 589
    ExplicitLeft = 520
    ExplicitTop = 288
    ExplicitHeight = 100
  end
  object Splitter2: TSplitter
    Left = 624
    Top = 41
    Height = 589
    Align = alRight
    ExplicitLeft = 633
    ExplicitTop = 33
  end
  object BIGrid1: TBIGrid
    Left = 627
    Top = 41
    Width = 394
    Height = 589
    Align = alRight
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    ShowItems = True
  end
  object PanelSelector: TPanel
    Left = 0
    Top = 41
    Width = 289
    Height = 589
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object Panel3: TPanel
      Left = 0
      Top = 392
      Width = 289
      Height = 197
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 1021
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object LError: TLabel
      Left = 496
      Top = 16
      Width = 35
      Height = 16
      Caption = 'LError'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object BAdd: TButton
      Left = 96
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Add...'
      Enabled = False
      TabOrder = 0
      OnClick = BAddClick
    end
    object Button1: TButton
      Left = 194
      Top = 10
      Width = 75
      Height = 25
      Caption = '&Import...'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 295
      Top = 10
      Width = 75
      Height = 25
      Caption = '&Query'
      TabOrder = 2
      OnClick = Button2Click
    end
    object BDelete: TButton
      Left = 392
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Delete'
      TabOrder = 3
      OnClick = BDeleteClick
    end
  end
  object BITree1: TBITree
    Left = 292
    Top = 41
    Width = 332
    Height = 589
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    TabOrder = 3
    OnDragDrop = Tree1DragDrop
    OnDragOver = Tree1DragOver
    OnChange = BITree1Change
  end
  object PopupMenu1: TPopupMenu
    Left = 328
    Top = 56
    object Filter1: TMenuItem
      Caption = '&Filter'
    end
    object Column1: TMenuItem
      Caption = '&Items'
      object Add1: TMenuItem
        Caption = '&Add...'
        OnClick = Add1Click
      end
      object Change1: TMenuItem
        Caption = '&Change...'
      end
      object Delete1: TMenuItem
        Caption = '&Delete...'
        OnClick = Delete1Click
      end
      object Rename1: TMenuItem
        Caption = '&Rename...'
        OnClick = Rename1Click
      end
      object Reorder1: TMenuItem
        Caption = 'Re&order'
      end
    end
    object Sort1: TMenuItem
      Caption = '&Sort'
    end
    object ranspose1: TMenuItem
      Caption = '&Transpose'
      OnClick = ranspose1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Algorithm1: TMenuItem
      Caption = '&Algorithm'
      object Function1: TMenuItem
        Caption = '&Function'
        OnClick = Function1Click
      end
      object MachineLearning1: TMenuItem
        Caption = '&Machine-Learning'
      end
    end
  end
  object PopupMenu2: TPopupMenu
    Left = 424
    Top = 56
    object Files1: TMenuItem
      Caption = '&Files...'
      OnClick = Files1Click
    end
    object Database1: TMenuItem
      Caption = '&Database...'
      OnClick = Database1Click
    end
    object Web1: TMenuItem
      Caption = '&BI Web...'
      OnClick = Web1Click
    end
  end
end
