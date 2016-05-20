object ExpressionEditor: TExpressionEditor
  Left = 0
  Top = 0
  ActiveControl = BCancel
  Caption = 'Expression Editor'
  ClientHeight = 543
  ClientWidth = 745
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 233
    Top = 0
    Height = 502
    ExplicitLeft = 376
    ExplicitTop = 240
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 233
    Height = 502
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object BITree1: TBITree
      Left = 0
      Top = 0
      Width = 233
      Height = 502
      Align = alClient
      UseDockManager = False
      DragMode = dmAutomatic
      ParentBackground = False
      ParentColor = False
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 236
    Top = 0
    Width = 509
    Height = 502
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = Panel2Resize
    object BITree2: TBITree
      Left = 0
      Top = 113
      Width = 509
      Height = 348
      Align = alClient
      UseDockManager = False
      ParentBackground = False
      ParentColor = False
      TabOrder = 0
      OnDragDrop = BITree2DragDrop
      OnDragOver = BITree2DragOver
      OnChange = BITree2Change
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 509
      Height = 113
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Label1: TLabel
        Left = 16
        Top = 10
        Width = 56
        Height = 13
        Caption = '&Expression:'
        FocusControl = EditExp
      end
      object LError: TLabel
        Left = 16
        Top = 56
        Width = 3
        Height = 13
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object LValue: TLabel
        Left = 64
        Top = 80
        Width = 3
        Height = 13
      end
      object Label2: TLabel
        Left = 16
        Top = 80
        Width = 30
        Height = 13
        Caption = 'Value:'
      end
      object EditExp: TEdit
        Left = 16
        Top = 29
        Width = 473
        Height = 21
        TabOrder = 0
        OnChange = EditExpChange
      end
    end
    object Panel4: TPanel
      Left = 0
      Top = 461
      Width = 509
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      object BDelete: TButton
        Left = 16
        Top = 6
        Width = 75
        Height = 25
        Caption = '&Delete'
        Enabled = False
        TabOrder = 0
        OnClick = BDeleteClick
      end
    end
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 502
    Width = 745
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Panel9: TPanel
      Left = 560
      Top = 0
      Width = 185
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object BOK: TButton
        Left = 9
        Top = 6
        Width = 75
        Height = 25
        Caption = 'OK'
        Enabled = False
        ModalResult = 1
        TabOrder = 0
      end
      object BCancel: TButton
        Left = 99
        Top = 6
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        Default = True
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
end
