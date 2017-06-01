{*********************************************}
{  TeeBI Software Library                     }
{  Generic dialog to edit numeric ranges      }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLBI.Editor.NumericFromTo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  BI.DataItem, Vcl.ExtCtrls;

(*
  TNumericFromTo dialog is used by TDynamicFilterEditor dialog to edit
  numeric ranges.

  It can also be used standalone without a TDataItem.

*)

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
    CBFromEqual: TComboBox;
    CBToEqual: TComboBox;
    procedure TBFromChange(Sender: TObject);
    procedure TBToChange(Sender: TObject);
    procedure PanelTracksResize(Sender: TObject);
    procedure EFromChange(Sender: TObject);
    procedure EToChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBFromClick(Sender: TObject);
    procedure CBToClick(Sender: TObject);
    procedure CBFromEqualChange(Sender: TObject);
    procedure CBToEqualChange(Sender: TObject);
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
    function GetFromEqual: Boolean;
    function GetToEqual: Boolean;
    procedure SetFromEqual(const Value: Boolean);
    procedure SetToEqual(const Value: Boolean);
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

    property FromEqual:Boolean read GetFromEqual write SetFromEqual;
    property FromValue:Extended read GetFrom write SetFrom;

    property ToEqual:Boolean read GetToEqual write SetToEqual;
    property ToValue:Extended read GetTo write SetTo;

    procedure Refresh(const AData:TDataItem); overload;
    procedure Refresh(const AMin,AMax,AFrom,ATo:Extended); overload;
  end;

implementation
