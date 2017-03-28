{*********************************************}
{  TeeBI Software Library                     }
{  Layouts Designer                           }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Designer.Template.Layouts;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.VCL.Dashboard, Vcl.ExtCtrls,
  Vcl.Menus, Vcl.StdCtrls, Vcl.ComCtrls, BI.VCL.Tree,
  BI.Dashboard, BI.Dashboard.Layouts;

type
  TLayoutDesigner = class(TForm)
    Splitter1: TSplitter;
    GalleryTemplate: TMemo;
    PanelLeft: TPanel;
    Gallery: TBIVisual;
    LBLayouts: TListBox;
    Splitter2: TSplitter;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    BIVisual1: TBIVisual;
    TabSheet2: TTabSheet;
    MemoTemplate: TMemo;
    Panel1: TPanel;
    BClear: TButton;
    BDelete: TButton;
    LAlign: TLabel;
    CBAlign: TComboBox;
    Panel2: TPanel;
    Button1: TButton;
    PopupMenu1: TPopupMenu;
    Captions1: TMenuItem;
    Colors1: TMenuItem;
    Frames1: TMenuItem;
    Splitters1: TMenuItem;
    Label1: TLabel;
    EPadding: TEdit;
    UDPadding: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BClearClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure Splitters1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Colors1Click(Sender: TObject);
    procedure Captions1Click(Sender: TObject);
    procedure Frames1Click(Sender: TObject);
    procedure EPaddingChange(Sender: TObject);
    procedure LBLayoutsClick(Sender: TObject);
  private
    { Private declarations }

    FOnItemChange : TNotifyEvent;

    Tree : TBITree;

    Hover : TShape;

    procedure AddControls(const AParent:TBITreeNode; const AControl:TWinControl);
    procedure CreateHover;

    //procedure FillGallery;
    procedure FillTree(const ALayout:TLayoutItem);

    procedure BIClick(Sender: TObject);
    procedure BIDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure BIDragDrop(Sender, Source: TObject; X, Y: Integer);

    function FindNode(const AControl:TControl):TBITreeNode;

    procedure RecreateLayout(const ALayout:TLayoutItem);
    procedure RefreshEditor(const APanel:TPanel);

    function ScreenRender:TScreenRender;

    function SelectedPanel:TPanel;
    procedure SetEvents(const AControl:TControl);
    procedure SetPanelBackColor(Sender:TControl);
    procedure SetPanelEvents(const ARender:TScreenRender);
    procedure SetSplitterColor(Sender:TControl);
    procedure ShowHideSplitter(Sender:TControl);
    procedure TreeChange(Sender: TObject);
  public
    { Public declarations }

    class function Edit(const AOwner:TComponent;
                        const ALayout:TLayoutItem):Boolean; static;

    class function Embedd(const AOwner:TComponent;
                          const AParent:TWinControl):TLayoutDesigner; static;

    procedure Refresh(const ALayout:TLayoutItem);
    function SelectedItem:TPanel;

    property OnItemChange:TNotifyEvent read FOnItemChange write FOnItemChange;
  end;

implementation
