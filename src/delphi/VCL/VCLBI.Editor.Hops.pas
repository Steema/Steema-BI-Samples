unit VCLBI.Editor.Hops;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  BI.Expressions, BI.Query, BI.DataItem;

type
  THopsViewer = class(TForm)
    LBHops: TListBox;
    LMain: TLabel;
    LCount: TLabel;
    LBItems: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    LParent: TLabel;
    procedure FormShow(Sender: TObject);
    procedure LBHopsClick(Sender: TObject);
  private
    { Private declarations }

    FHops : TDataHops;

    procedure AddHopItems(const AHop:THops);
    procedure AddHops;
  public
    { Public declarations }

    class function HopsFrom(const AProvider:TDataProvider):TDataHops; overload; static;
    class function HopsFrom(const AQuery:TBIQuery):TDataHops; overload; static;

    class procedure View(const AOwner:TComponent; const AHops:TDataHops); overload; static;
    class procedure View(const AOwner:TComponent; const AQuery:TBIQuery); overload; static;
  end;

implementation
