{*********************************************}
{  TeeBI Software Library                     }
{  Example of a custom function Expression    }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Expression.Custom;

interface

{
 The following "THypot" class implements a custom function expression.

 This function can be parsed and evaluated either by code or forming part of
 a string expression.

 It is also usable at several TeeBI components like SQL queries or pivot-tables.
}

uses
  BI.Expression;

type
  THypot=class(TParameterExpression)
  private
    const
      Name='Hypot';
  protected
    function ResultClass: TExpressionClass; override;
  public
    class function FromString(const S:String):TParameterExpression; override;
    function ToString:String; override;
    function Value:TData; override;
  end;

implementation

uses
  System.SysUtils;

{ THypot }

// Our name
class function THypot.FromString(const S: String): TParameterExpression;
begin
  if SameText(S,Name) then
     result:=Self.Create
  else
     result:=nil;
end;

// We return float results
function THypot.ResultClass: TExpressionClass;
begin
  result:=TFloatExpression;
end;

// Hypot(x,y)
function THypot.ToString: String;
begin
  result:=Name+inherited;
end;

// Calculate hypotenuse
function THypot.Value: TData;
var x,
    y : Double;
begin
  x:=Sqr(Parameters[0].Value);
  y:=Sqr(Parameters[1].Value);

  result:=Sqrt(x+y);
end;

initialization
  TParameterExpression.Registered.Add(THypot);
finalization
  TParameterExpression.Registered.Remove(THypot);
end.
