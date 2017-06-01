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
  OnShow = FormShow
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
  object PanelFunctions: TPanel
    Left = 0
    Top = 0
    Width = 233
    Height = 502
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    Visible = False
    object BITree1: TBITree
      Left = 0
      Top = 28
      Width = 233
      Height = 474
      Align = alClient
      UseDockManager = False
      DragMode = dmAutomatic
      ParentBackground = False
      ParentColor = False
      TabOrder = 0
      ExplicitLeft = 1
      ExplicitTop = -6
      ExplicitHeight = 502
    end
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 233
      Height = 28
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Label1: TLabel
        Left = 13
        Top = 7
        Width = 50
        Height = 13
        Caption = 'Functions:'
      end
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
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 509
      Height = 502
      ActivePage = TabExpression
      Align = alClient
      TabOrder = 0
      OnChange = PageControl1Change
      ExplicitLeft = 112
      ExplicitTop = 152
      ExplicitWidth = 289
      ExplicitHeight = 193
      object TabExpression: TTabSheet
        Caption = 'Expression'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 281
        ExplicitHeight = 165
        object PanelError: TPanel
          Left = 0
          Top = 445
          Width = 501
          Height = 29
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 0
          ExplicitTop = 136
          ExplicitWidth = 281
          object LError: TLabel
            Left = 16
            Top = 8
            Width = 3
            Height = 13
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clMaroon
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
        end
        object EditExp: TMemo
          Left = 0
          Top = 0
          Width = 501
          Height = 445
          Align = alClient
          TabOrder = 1
          OnChange = EditExpChange
          OnKeyDown = EditExpKeyDown
          OnKeyPress = EditExpKeyPress
          ExplicitLeft = -192
          ExplicitTop = 8
          ExplicitWidth = 473
          ExplicitHeight = 69
        end
      end
      object TabTree: TTabSheet
        Caption = 'Tree'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 281
        ExplicitHeight = 165
        object BITree2: TBITree
          Left = 0
          Top = 0
          Width = 501
          Height = 433
          Align = alClient
          UseDockManager = False
          ParentBackground = False
          ParentColor = False
          TabOrder = 0
          OnDragDrop = BITree2DragDrop
          OnDragOver = BITree2DragOver
          OnChange = BITree2Change
          ExplicitTop = 113
          ExplicitWidth = 509
          ExplicitHeight = 348
        end
        object Panel4: TPanel
          Left = 0
          Top = 433
          Width = 501
          Height = 41
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          ExplicitTop = 461
          ExplicitWidth = 509
          object Label2: TLabel
            Left = 108
            Top = 11
            Width = 30
            Height = 13
            Caption = 'Value:'
          end
          object LValue: TLabel
            Left = 156
            Top = 11
            Width = 3
            Height = 13
          end
          object BDelete: TButton
            Left = 19
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
    ExplicitLeft = -8
    ExplicitTop = 504
    object SBToogle: TSpeedButton
      Left = 13
      Top = 9
      Width = 23
      Height = 22
      AllowAllUp = True
      GroupIndex = 1
      Caption = '<<'
      OnClick = SBToogleClick
    end
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
