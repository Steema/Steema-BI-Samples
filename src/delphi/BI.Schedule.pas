{*********************************************}
{  TeeBI Software Library                     }
{  Scheduler class for repetitive tasks       }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Schedule;

interface

uses
  System.Classes;

type
  TWork={$IFNDEF FPC}reference to{$ENDIF} procedure(const Item:TObject);

  TScheduler=class;

  TWorkItem=class
  private
    FDueTime : TDateTime;
    FTag : TObject;
    FWork : TWork;

    IScheduler : TScheduler;

    procedure SetDueTime(const Value: TDateTime);
  public
    Asynchronous : Boolean;

    Destructor Destroy; override;

    procedure Work;
    property DueTime:TDateTime read FDueTime write SetDueTime;
  end;

  TScheduler=class(TComponent)
  private
    function IndexOf(const Work:TWorkItem):Integer;
    procedure Sort;
  public
    Items : TArray<TWorkItem>;

    Destructor Destroy; override;

    function Add(const DueTime:TDateTime; const Tag:TObject; const Work:TWork):TWorkItem;
    procedure Clear;
    function Count:Integer;
    procedure Remove(const Work:TWorkItem);

    procedure Work; // <-- must be called at some repeated interval, for example every minute
  end;

implementation
