{*********************************************}
{  TeeBI Software Library                     }
{  Web Server (VCL) constants                 }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Unit_Constants;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormConstants = class(TForm)
    Header: TMemo;
    Footer: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormConstants: TFormConstants;

implementation

{$R *.dfm}

end.
