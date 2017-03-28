{*********************************************}
{  TeeBI Software Library                     }
{  Master-Detail relationships discovery      }
{  Copyright (c) 2015-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.Data.GuessLinks;

interface

uses
  BI.Data;

// Returns a TDataItem table with all items that are "linked" to Data parameter.
// "Linked" means master-detail relationships between items.

type
  TGuessLinks=class
  public
    class function Guess(const Data:TDataItem):TDataItem;
  end;

implementation
