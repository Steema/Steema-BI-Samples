{*********************************************}
{  TeeBI Software Library                     }
{  Master-Detail relationships discovery      }
{  Copyright (c) 2015-2016 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.GuessLinks;

interface

uses
  BI.Data;

type
  TGuessLinks=class
  public
    class function Guess(const Data:TDataItem):TDataItem;
  end;

implementation
