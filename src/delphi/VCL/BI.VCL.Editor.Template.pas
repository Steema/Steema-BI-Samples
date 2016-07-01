{*********************************************}
{  TeeBI Software Library                     }
{  TBITemplate Dashboard Editor               }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Editor.Template;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Menus,

  BI.Data, BI.Persist,
  BI.Dashboard, BI.VCL.Dashboard, BI.VCL.DataControl, BI.VCL.Grid, BI.VCL.Tree;

type
  TTemplateEditor = class(TForm)
    Panel1: TPanel;
    BIVisual1: TBIVisual;
    BIGrid1: TBIGrid;
    BITree1: TBITree;
    PageProperties: TPageControl;
    TabData: TTabSheet;
    Splitter1: TSplitter;
    Label2: TLabel;
    LData: TLabel;
    BChange: TButton;
    Splitter2: TSplitter;
    TabPanel: TTabSheet;
    Label3: TLabel;
    CBPanelKind: TComboBox;
    TabDashboard: TTabSheet;
    CBSplitters: TCheckBox;
    CBTitles: TCheckBox;
    TabItem: TTabSheet;
    Label7: TLabel;
    CBPanelData: TComboBox;
    Panel2: TPanel;
    BDelete: TButton;
    PageControl1: TPageControl;
    TabPreview: TTabSheet;
    TabTemplate: TTabSheet;
    MemoJSON: TMemo;
    BEditProvider: TButton;
    BAdd: TButton;
    PopupData: TPopupMenu;
    Data1: TMenuItem;
    Query1: TMenuItem;
    Import1: TMenuItem;
    Files1: TMenuItem;
    DatabaseServer1: TMenuItem;
    BIWebServer1: TMenuItem;
    PopupItem: TPopupMenu;
    AddItem1: TMenuItem;
    RemoveItem1: TMenuItem;
    BSelectLayout: TButton;
    LLayout: TLabel;
    Rename1: TMenuItem;
    PageControl2: TPageControl;
    TabItemOptions: TTabSheet;
    TabItemSize: TTabSheet;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    CBPanels: TComboBox;
    CBPosition: TComboBox;
    CBItemKind: TComboBox;
    CBAutoPosition: TCheckBox;
    LRealKind: TLabel;
    Label1: TLabel;
    EWidth: TEdit;
    CBWidthUnits: TComboBox;
    Label8: TLabel;
    EHeight: TEdit;
    CBHeightUnits: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure BITree1Change(Sender: TObject);
    procedure BChangeClick(Sender: TObject);
    procedure CBDataKindChange(Sender: TObject);
    procedure CBPanelKindChange(Sender: TObject);
    procedure CBSplittersClick(Sender: TObject);
    procedure CBTitlesClick(Sender: TObject);
    procedure CBItemKindChange(Sender: TObject);
    procedure CBPanelsChange(Sender: TObject);
    procedure CBPositionChange(Sender: TObject);
    procedure CBAutoPositionClick(Sender: TObject);
    procedure CBPanelDataChange(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure BEditProviderClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BAddClick(Sender: TObject);
    procedure Data1Click(Sender: TObject);
    procedure PopupItemPopup(Sender: TObject);
    procedure AddItem1Click(Sender: TObject);
    procedure RemoveItem1Click(Sender: TObject);
    procedure BSelectLayoutClick(Sender: TObject);
    procedure Query1Click(Sender: TObject);
    procedure Files1Click(Sender: TObject);
    procedure DatabaseServer1Click(Sender: TObject);
    procedure BIWebServer1Click(Sender: TObject);
    procedure Rename1Click(Sender: TObject);
  private
    { Private declarations }

    FTemplate: TBITemplate;

    IData,
    IPanels,
    IDashboards: TBITreeNode;

    procedure AddNewData(const AData:TDataItem);
    procedure ChangeDataLabel(const AData:TDataItem);
    function ChooseData(out AData:TVisualData):Boolean;
    function ChoosePanel(out APanel:TBIPanel):Boolean;
    function DoSetProperties(const AItem: TObject):Boolean;
    function HasPanels:Boolean;
    function NewOwner:TComponent;

    procedure RefreshDashboard(const ADashboard:TDashboard);
    procedure RefreshDashboardItem(const AItem:TDashboardItem);
    procedure RefreshData(const AData:TVisualData);
    procedure RefreshItem(const AItem:TDashboardItem);
    procedure RefreshPanel(const APanel:TBIPanel);

    procedure SetAddCaption(const AObject:TObject);
    procedure SetProperties(const AData:TVisualData); overload;
    procedure SetProperties(const APanel:TBIPanel); overload;
    procedure SetProperties(const AItem:TDashboardItem); overload;
    procedure SetDashboardProperties(const ADashboard:TDashboard);

    procedure TryAddNewItem(const AItems:TDashboardItems;
                            const AParent:TBITreeNode);
    procedure TryImport(const AKind:TDataDefinitionKind);
    procedure TryNewPanel;
  public
    { Public declarations }

    class function Edit(const AOwner:TComponent; const ATemplate:TBITemplate):Boolean; static;

    procedure Refresh(const ATemplate:TBITemplate);
  end;

implementation
