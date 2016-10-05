{*********************************************}
{  TeeBI Software Library                     }
{  Expression Completion ListBox              }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Editor.Completion;

interface

uses
  System.Classes, System.Types, VCL.Controls, VCL.StdCtrls;

type
  TExpressionCompletion=class;

  TSizeListBox=class(TListBox)
  private
    ICompletion : TExpressionCompletion;
  protected
    {$IFNDEF FPC}
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF}
    procedure Resize; override;
  end;

  TCompleteEvent=procedure(Sender:TObject; const Text:String) of object;

  TExpressionCompletion=class
  private
    FControl : TSizeListBox;

    FFields,
    FItems : TStrings;

    FOnComplete : TCompleteEvent;
    FOnGetFields: TCompleteEvent;
    FOnResize : TNotifyEvent;

    FParent : TWinControl;

    HiddenByReturn : Boolean;

    function CaretPosition:TPoint; // in pixels
    procedure CheckControl;
    procedure ControlExit(Sender: TObject);
    procedure ControlKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    function LineHeight:Integer;
    function GetControl: TSizeListBox; // in pixels
    function WordBeforeCaret:String;
  public
    Constructor Create(const AParent:TWinControl);
    Destructor Destroy; override;

    procedure AddItems(const FieldsOnly:Boolean);
    function Handle(var Key:Char):Boolean; overload;
    function Handle(var Key: Word; Shift: TShiftState):Boolean; overload;

    procedure Hide;
    procedure Show(const FieldsOnly:Boolean);
    function Visible:Boolean;

    property Fields:TStrings read FFields;
    property Items:TStrings read FItems;
    property Control:TSizeListBox read GetControl;

    property OnComplete:TCompleteEvent read FOnComplete write FOnComplete;
    property OnGetFields:TCompleteEvent read FOnGetFields write FOnGetFields;
    property OnResize:TNotifyEvent read FOnResize write FOnResize;
  end;

implementation
