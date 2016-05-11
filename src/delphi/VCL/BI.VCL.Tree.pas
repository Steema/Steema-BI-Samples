{*********************************************}
{  TeeBI Software Library                     }
{  TBITree control for VCL and FireMonkey     }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Tree;
{.$DEFINE FMX}

interface

// This unit implements a visual control (TBITree), capable of displaying data
// from TDataItem instances in hierarchical layout, using a TreeView or any other
// compatible control using the "Plugin" property.

// TBITree "Fill..." methods offer several modes to add tree nodes.

uses
  System.Classes,
  {$IFDEF FMX}
  FMX.Controls, FMX.TreeView, BI.FMX.DataControl,
  {$ELSE}
  VCL.Controls, VCL.Graphics, BI.VCL.DataControl,
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
    function GetControl:TControl; virtual; abstract;
    function GetCount:Integer; virtual; abstract;
    function GetData(const ANode:TBITreeNode):TObject; virtual; abstract;
    function GetNode(const AIndex:Integer):TBITreeNode; virtual; abstract;
    function GetOnChange: TNotifyEvent; virtual; abstract;
    function GetSelected:TBITreeNode; virtual; abstract;
    function GetSelectedData:TBITreePlugin.TNodeData; virtual; abstract;
    function NewNodeData(const ATag:TObject; const AIndex:TInteger):TNodeData;
    procedure SetOnChange(const Value: TNotifyEvent); virtual; abstract;
    procedure SetSelected(const Value: TBITreeNode); virtual; abstract;
  public
    Constructor Create(const AOwner:TComponent); virtual; abstract;
    Destructor Destroy; override;

    procedure Expand(const AIndex:Integer); overload; virtual; abstract;
    procedure Expand(const ANode:TBITreeNode; const DoExpand:Boolean=True); overload; virtual; abstract;
    function Find(const ATag:TObject; const AIndex:Integer=-1):TBITreeNode; virtual; abstract;

    function NewNode(const AParent:TBITreeNode; const AText:String;
              const ATag:TObject=nil;
              const AIndex:TInteger=-1):TBITreeNode; virtual; abstract;

    function ParentOf(const ANode:TBITreeNode):TBITreeNode; virtual; abstract;
    procedure SetData(const ANode:TBITreeNode; const AData:TObject); virtual; abstract;

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
  TBITree=class(TBIDataControl)
  public
    type
      TBITreeNodes=class
      private
        {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
        ITree : TBITree;

        function Get(const Index: Integer): TBITreeNode;
      public
        function Count:Integer;
        property Items[const Index:Integer]:TBITreeNode read Get; default;
      end;

  private
    {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}
    FData: TDataItem;

    FNodes : TBITreeNodes;

    function GetOnChange: TNotifyEvent;
    function GetSelected: TBITreeNode;
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetSelected(const Value: TBITreeNode);
  protected
    function GetDataItem:TDataItem; override;
    procedure Loaded; override;
    procedure SetDataItem(const Value: TDataItem); override;
  public
    class var
      Engine : TBITreePluginClass;

    var
      Plugin : TBITreePlugin;

    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;

    function Add(const AParent:TBITreeNode; const AText:String; const AObject:TObject=nil):TBITreeNode; overload;
    function Add(const AText:String; const AObject:TObject=nil):TBITreeNode; overload; inline;

    procedure Clear;
    function DataOf(const ANode:TBITreeNode):TObject;

    procedure Expand(const ANode:TBITreeNode; const DoExpand:Boolean=True);

    procedure Fill(const AData:TDataItem); overload;
    procedure Fill(const AMaster,ADetail:TDataItem); overload;
    procedure FillParent(const AKey,AParent:TDataItem; const ADisplay:TDataItem=nil);

    procedure FillStore(const AStore:String);
    procedure FillStores;

    function ParentOf(const ANode:TBITreeNode):TBITreeNode;
    procedure SetNodeData(const ANode:TBITreeNode; const AData:TObject);

    property Nodes:TBITreeNodes read FNodes;
    property Selected:TBITreeNode read GetSelected write SetSelected;
  published
    {$IFNDEF FMX}
    property Color default clWhite;
    {$ENDIF}

    property OnChange:TNotifyEvent read GetOnChange write SetOnChange;
  end;

implementation
