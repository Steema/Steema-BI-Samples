unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  BI.VCL.Grid, BI.Data;

type
  TFormSpeed = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    Grid : TBIGrid;

    Results : TDataItem;

    procedure Bench(const AName:String; const Times:Integer; const Run:TProc);
    procedure Test;
  public
    { Public declarations }
  end;

var
  FormSpeed: TFormSpeed;

implementation

{$R *.dfm}

uses
  System.Diagnostics;

procedure TFormSpeed.Button1Click(Sender: TObject);
begin
  Screen.Cursor:=crHourGlass;
  try
    Results.Resize(0);

    Test;

    Grid.BindTo(nil);
    Grid.BindTo(Results);
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TFormSpeed.FormCreate(Sender: TObject);
begin
  Grid:=TBIGrid.Create(Self);
  Grid.Align:=alClient;
  Grid.Parent:=Self;

  Results:=TDataItem.Create(True);
  Results.Items.Add('Description',TDataKind.dkText);
  Results.Items.Add('Times',TDataKind.dkInt32);
  Results.Items.Add('Milliseconds',TDataKind.dkInt64);

  Grid.Data:=Results;
end;

procedure TFormSpeed.FormDestroy(Sender: TObject);
begin
  Results.Free;
end;

procedure TFormSpeed.Bench(const AName:String; const Times:Integer; const Run:TProc);
var t1 : TStopwatch;
    Pos : Integer;
    Elapsed : Int64;
begin
  t1:=TStopwatch.StartNew;

  Run;

  Elapsed:=t1.ElapsedMilliseconds;

  Pos:=Results.Count;
  Results.Resize(Pos+1);

  Results[0].TextData[Pos]:=AName;
  Results[1].Int32Data[Pos]:=Times;
  Results[2].Int64Data[Pos]:=Elapsed;
end;

procedure TFormSpeed.Test;
var Persons : TDataItem;
begin
  Bench('Create and Destroy Table',100000,
    procedure
    var Data : TDataItem;
        t : Integer;
    begin
      for t:=1 to 100000 do
      begin
        Data:=TDataItem.Create(True);
          Data.Items.Add('ID',TDataKind.dkInt32);
          Data.Items.Add('Name',TDataKind.dkText);
          Data.Items.Add('Price',TDataKind.dkSingle);

        Data.Free;
      end;
    end);

  Persons:=TDataItem.Create(True);
        Persons.Items.Add('ID',TDataKind.dkInt32);
        Persons.Items.Add('Name',TDataKind.dkText);
        Persons.Items.Add('Salary',TDataKind.dkSingle);

  Bench('Add Records',1000000,
    procedure
    var t : Integer;
    begin
      Persons.Resize(1000000);

      for t:=1 to 1000000 do
      begin
        Persons[0].Int32Data[t]:=t;
        Persons[1].TextData[t]:='Sam';
        Persons[2].SingleData[t]:=456.789;
      end;
    end);

  Persons.Free;
end;

end.
