{*********************************************}
{  TeeBI Software Library                     }
{  Expressions by Row and Column              }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.Expressions;

interface

uses
  BI.Data, BI.Expression, BI.Summary;

type
  // Expression to Aggregate data using all items in a row
  TRowFunction=class(TColumnExpression)
  protected
    procedure Calculate(const Hops:TDataHops; const Dest:TDataItem); override;
    function KindOf:TDataKind; override;
    class function TryParse(const S:String):TColumnExpression; override;
  public
    Operand : TAggregate;
    MissingAsZero : Boolean;

    function ToString:String; override;
    function Value:Variant; override;
  end;

  TColumnOperand=(Percent,PercentChange,Cumulative,MovingAverage);

  TColumnOperandHelper=record helper for TColumnOperand
  public
    class function FromString(const S:String; out Operand:TColumnOperand):Boolean; static;
    function ToString:String;
  end;

  // Expression to Aggregate data using all items in a column
  TColumnFunction=class(TColumnExpression)
  protected
    procedure Calculate(const Hops:TDataHops; const Dest:TDataItem); override;
    function KindOf:TDataKind; override;
    class function TryParse(const S:String):TColumnExpression; override;
  public
    Operand : TColumnOperand;

    function Value:Variant; override;
    function ToString:String; override;
  end;

implementation
