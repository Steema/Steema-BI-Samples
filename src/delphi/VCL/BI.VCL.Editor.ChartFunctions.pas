{*********************************************}
{  TeeBI Software Library                     }
{  TeeChart TeeFunctions Editor               }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.VCL.Editor.ChartFunctions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, BI.VCL.ChartFunctions;

type
  TDataFunctionEditor = class(TForm)
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FFunction : TDataFunction;
  public
    { Public declarations }
  end;

implementation
