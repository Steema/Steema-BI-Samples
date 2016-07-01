unit BI.VCL.Editor.NumericFromTo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  BI.Data, Vcl.ExtCtrls;

type
  TNumericFromTo = class(TForm)
    PanelTracks: TPanel;
    TBFrom: TTrackBar;
    TBTo: TTrackBar;
    EFrom: TEdit;
    LErrorFrom: TLabel;
    LErrorTo: TLabel;
    ETo: TEdit;
    CBFrom: TCheckBox;
    CBTo: TCheckBox;
    procedure TBFromChange(Sender: TObject);
    procedure TBToChange(Sender: TObject);
    procedure PanelTracksResize(Sender: TObject);
    procedure EFromChange(Sender: TObject);
    procedure EToChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBFromClick(Sender: TObject);
    procedure CBToClick(Sender: TObject);
  private
    { Private declarations }

    IChanging : Boolean;

    Min,
    Max,
    Range : Extended;

    FOnChange : TNotifyEvent;

    function AsString(const AValue:Extended):String;
    procedure DoChanged;
    procedure EnableFrom;
    procedure EnableTo;
    function GetFrom: Extended;
    function GetTo: Extended;
    function PositionOf(const AValue:Extended):Integer;
    procedure SetFrom(const Value: Extended);
    procedure SetTo(const Value: Extended);
    function TrySetTrack(const S:String; const ATrack:TTrackBar):Boolean;
    function ValueOf(const APosition:Integer):Extended;
  protected
    procedure HideTo;
  public
    { Public declarations }

    Float : Boolean;
    DateTime : Boolean;

    class function Embedd(const AOwner:TComponent;
                          const AParent:TWinControl;
                          const OnChange:TNotifyEvent):TNumericFromTo; static;

    function EnabledFrom:Boolean;
    function EnabledTo:Boolean;

    property FromValue:Extended read GetFrom write SetFrom;
    property ToValue:Extended read GetTo write SetTo;

    procedure Refresh(const AData:TDataItem); overload;
    procedure Refresh(const AMin,AMax,AFrom,ATo:Extended); overload;
  end;

implementation
