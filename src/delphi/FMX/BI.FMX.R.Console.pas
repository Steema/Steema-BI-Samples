unit BI.FMX.R.Console;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, 

  {$IF CompilerVersion<=27}
  {$DEFINE HASFMX20}
  {$ENDIF}

  {$IFNDEF HASFMX20}
  FMX.Graphics, FMX.Controls.Presentation,
  {$ENDIF}

  FMX.Dialogs, FMX.Edit, FMX.StdCtrls, FMX.Layouts, 

  {$IF CompilerVersion<=28}
  {$DEFINE HASFMX21}
  {$ENDIF}

  {$IFNDEF HASFMX21}
  FMX.ScrollBox,
  {$ENDIF}

  FMX.Memo;

// Simple console window to execute R Language statements and print their
// return values

type
  TBIRConsole = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    BGo: TButton;
    EStatement: TEdit;
    Memo: TMemo;
    Button1: TButton;
    procedure EStatementChangeTracking(Sender: TObject);
    procedure BGoClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure EStatementKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }

    procedure Add(const Value: TStrings);
  end;

implementation
