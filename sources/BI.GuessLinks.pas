{*********************************************}
{  TeeBI Software Library                     }
{  Master-Detail relationships discovery      }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.GuessLinks;

interface

uses
  BI.DataItem;

// Returns a TDataItem table with all items that are "linked" to Data parameter.
// "Linked" means master-detail relationships between items.

type
  TGuessLinks=class
  public
    class function Guess(const Data:TDataItem):TDataItem;
  end;

implementation

uses
  System.SysUtils, BI.Arrays;

{ TGuessLinks }

type
  TDataItemAccess=class(TDataItem);

class function TGuessLinks.Guess(const Data: TDataItem): TDataItem;

  procedure GuessItem(const AItem:TDataItem; const AData:TDataItem);
  var t : Integer;
      Col : TDataItem;
  begin
    for t:=0 to Data.Items.Count-1 do
    begin
      if Data.Items[t]<>AData then
      begin
        for Col in Data.Items[t].Items.AsArray do
        begin
          if Col.DataMap<>nil then
          if Col.DataMap.Sorted<>TDataOrder.None then
          if SameText(Col.Name,AItem.Name) then
          begin
            result.Items[0].TextData.Append(AData.Name);
            result.Items[1].TextData.Append(AItem.Name);

            result.Items[2].TextData.Append(Data.Items[t].Name);
            result.Items[3].TextData.Append(Col.Name);

            Inc(TDataItemAccess(result).FCount);

            Exit;
          end;
        end;
      end;
    end;
  end;

  procedure GuessData(const AData:TDataItem);
  var c : TDataItem;
  begin
    for c in AData.Items.AsArray do
      if c.Master=nil then
         if not c.Primary then
            if c.DataMap<>nil then
               if c.DataMap.Sorted=TDataOrder.None then
                  GuessItem(c,AData);
  end;

var Item : TDataItem;
begin
  result:=TDataItem.Create(True);

  Data.Items.Add(result);

  result.Name:='Guess links of '+Data.Name;

  result.Items.Add('Detail',dkText);
  result.Items.Add('Detail Item',dkText);
  result.Items.Add('Master',dkText);
  result.Items.Add('Master Item',dkText);

  for Item in Data.Items.AsArray do
      GuessData(Item);
end;

end.
