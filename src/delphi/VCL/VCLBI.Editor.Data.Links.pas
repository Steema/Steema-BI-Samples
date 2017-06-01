unit VCLBI.Editor.Data.Links;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  VCLBI.DataControl, VCLBI.Grid, Vcl.ExtCtrls, BI.Persist,
  BI.DataItem;

type
  TDataLinksEditor = class(TForm)
    Panel2: TPanel;
    BAdd: TButton;
    BDelete: TButton;
    BIGrid1: TBIGrid;
    PanelLink: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    EMaster: TEdit;
    EDetail: TEdit;
    procedure BIGrid1DataChange(Sender: TObject);
    procedure BAddClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure EDetailChange(Sender: TObject);
    procedure EMasterChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    IChanging : Boolean;

    IData : TDataDefinition;
    ILinks : TDataRelations;

    function ChangedField(const ANum:Integer; const AValue:String):Boolean;
    function LinksToData:TDataItem;
    procedure RefreshGrid;
    procedure SaveLinks;
    function TryChangeLink(const ARoot:TDataItem; const S:String):String;
  public
    { Public declarations }

    class function Embed(const AOwner:TComponent;
                         const AParent:TWinControl):TDataLinksEditor; static;

    procedure Refresh(const AData:TDataDefinition);
  end;

implementation
