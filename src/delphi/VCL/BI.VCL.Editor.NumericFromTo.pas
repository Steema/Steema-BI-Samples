unit BI.VCL.Editor.NumericFromTo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  BI.Data;

type
  TNumericFromTo = class(TForm)
    Label1: TLabel;
    TBFrom: TTrackBar;
    Label2: TLabel;
    TBTo: TTrackBar;
    LFrom: TLabel;
    LTo: TLabel;
    procedure TBFromChange(Sender: TObject);
    procedure TBToChange(Sender: TObject);
  private
    { Private declarations }

    Min,
    Max,
    Range : Extended;

    FOnChange : TNotifyEvent;

    function AsString(const AValue:Extended):String;
    procedure DoChanged;
    function PositionOf(const AValue:Extended):Integer;
    function ValueOf(const APosition:Integer):Extended;
  public
    { Public declarations }

    DateTime : Boolean;

    class function Embedd(const AOwner:TComponent;
                          const AParent:TWinControl;
                          const OnChange:TNotifyEvent):TNumericFromTo; static;

    function FromValue:Extended;
    function ToValue:Extended;

    procedure Refresh(const AData:TDataItem); overload;
    procedure Refresh(const AMin,AMax,AFrom,ATo:Extended); overload;
  end;

implementation
