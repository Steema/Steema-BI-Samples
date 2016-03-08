unit BI.VCL.Dashboard;
{.$DEFINE FMX}

interface

uses
  System.Classes, System.UITypes,
  {$IFDEF FMX}
  FMX.Controls, FMX.Layouts, FMX.ExtCtrls, FMX.TabControl, FMX.Graphics,
  FMX.StdCtrls, FMX.Types, BI.FMX.Chart,
  {$ELSE}
  VCL.Controls, VCL.ExtCtrls, VCL.Graphics, VCL.ComCtrls, BI.VCL.Chart,
  {$ENDIF}
  BI.Data, BI.Dashboard;

type
  {$IFDEF FMX}
  TWinControl=TControl;
  TPageControl=TTabControl;
  TPanel=TLayout;
  TAlign=TAlignLayout;
  TTabSheet=TTabItem;
  TGraphic=TBitmap;
  {$ENDIF}

  TScreenRender=class(TRender)
  private
    {$IFDEF AUTOREFCOUNT}
    [Weak]
    {$ENDIF}
    FControl : TWinControl;

    FTabs : TPageControl;

    FExtra,
    FExtra1,
    FExtra2 : TPanel;

    procedure AddControl(const AControl:TControl; const APosition:Integer; const ATitle:String='');

    procedure ChangedVariable(const Sender:TChangeListener; const AValue:String);
    procedure CheckPanelSize(const APanel:TBIPanel; const AControl:TControl);
    procedure Clear;

    procedure ComboChange(Sender:TObject);
    procedure ControlClick(Sender: TObject);

    function CreatePanel(const ABack:TAlphaColor):TPanel; overload;
    function CreatePanel:TPanel; overload; inline;

    procedure EmitDashboard(const AItem:TDashboard; const APosition:Integer);
    function EmitPanel(const AItem:TDashboardItem; const AKind:String; const APosition:Integer):TControl;

    procedure CheckBoxChange(Sender:TObject);
    procedure ListChange(Sender: TObject);

    function ParentRender:TScreenRender;

    procedure SetControl(const Value: TWinControl);
    class procedure SetFont(const AVCLFont:TFont; const AFont:TBIFont);

    procedure TrackBarChange(Sender: TObject);
  protected
    procedure AddListener(const AName:String; const ADataIndex:Integer); override;
    function Variables:TVariables; override;
  public
    Constructor Create; override;
    Destructor Destroy; override;

    procedure Emit(const AItem:TDashboardItem; const AKind:String; const APosition:Integer); override;
    procedure Init(const ADashboard:TDashboard; const ALayout:String='';
                   const AParams:TStrings=nil); override;
    procedure Finish; override;

    class function NewChart(const AOwner:TComponent; const AItem:TDashboardItem;
                            const AData:TDataItem;
                            const AFormat:String):TBIChart; static;

    property Control:TWinControl read FControl write SetControl;
  end;

  TBIVisual=class(TPanel)
  private
    FTemplate : TBITemplate;

    procedure SetTemplate(const Value: TBITemplate);
    procedure TryFreeTemplate;
  public
    Render : TScreenRender;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Generate(const ADashboard:TDashboard; const ALayout:String='');

    property Template:TBITemplate read FTemplate write SetTemplate;
  end;

implementation
