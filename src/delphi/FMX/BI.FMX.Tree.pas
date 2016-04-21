unit BI.FMX.Tree;
{$DEFINE FMX}

interface

uses
  System.Classes,
  {$IFDEF FMX}
  FMX.Controls, FMX.TreeView,
  {$ELSE}
  VCL.Controls,
  {$ENDIF}
  BI.Arrays, BI.Data;

type
  TBITreeNode=TObject;

  TBITreePlugin=class abstract
  public
  type
    TNodeData=class
    public
      Index : TInteger;

      //{$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
      //Node : Integer;

      {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
      Tag : TObject;
    end;

  private
    IDatas : Array of TNodeData;

  protected
    procedure Clear; virtual; abstract;
    procedure ClearData;
//    function FindData(const ANode:Integer):TNodeData;
    function GetControl:TControl; virtual; abstract;
    function GetCount:Integer; virtual; abstract;
    function NewNodeData(const ATag:TObject; const AIndex:TInteger):TNodeData;

    function GetOnChange: TNotifyEvent; virtual; abstract;
    function GetSelected:TBITreeNode; virtual; abstract;
    function GetSelectedData:TBITreePlugin.TNodeData; virtual; abstract;
    procedure SetOnChange(const Value: TNotifyEvent); virtual; abstract;
    procedure SetSelected(const Value: TBITreeNode); virtual; abstract;
  public
    Constructor Create(const AOwner:TComponent); virtual; abstract;
    Destructor Destroy; override;

    procedure Expand(const AIndex:Integer); virtual; abstract;
    function Find(const ATag:TObject; const AIndex:Integer=-1):TBITreeNode; virtual; abstract;

    function NewNode(const AParent:TBITreeNode; const AText:String;
              const ATag:TObject=nil;
              const AIndex:TInteger=-1):TBITreeNode; virtual; abstract;

    property Count:Integer read GetCount;
    property Selected:TBITreeNode read GetSelected write SetSelected;
    property SelectedData:TBITreePlugin.TNodeData read GetSelectedData;

    property OnChange:TNotifyEvent read GetOnChange write SetOnChange;
  end;

  TBITreePluginClass=class of TBITreePlugin;

  // Generic Tree control that fills nodes from TDataItem relationships

  {$IF CompilerVersion>=23}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32
              {$IF CompilerVersion>=25}or pidiOSSimulator or pidiOSDevice{$ENDIF}
              {$IF CompilerVersion>=26}or pidAndroid{$ENDIF}
              {$IF CompilerVersion>=29}or pidiOSDevice64{$ENDIF}
              )]
  {$ENDIF}
  TBITree=class({$IFDEF FMX}TControl{$ELSE}TWinControl{$ENDIF})
  private
    {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
    FData: TDataItem;

    procedure SetDataItem(const Value: TDataItem);
    function GetOnChange: TNotifyEvent;
    procedure SetOnChange(const Value: TNotifyEvent);
  public
    class var
      Engine : TBITreePluginClass;

    var
      Plugin : TBITreePlugin;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    procedure Clear;

    procedure Fill(const AData:TDataItem); overload;
    procedure Fill(const AMaster,ADetail:TDataItem); overload;
    procedure FillParent(const AKey,AParent:TDataItem; const ADisplay:TDataItem=nil);

    procedure FillStore(const AStore:String);
    procedure FillStores;
  published
    property Data:TDataItem read FData write SetDataItem;

    property OnChange:TNotifyEvent read GetOnChange write SetOnChange;
  end;

implementation
