unit BI.Web.Tests;

interface

uses
  BI.Data;

type
  TBIWebTest=record
  public
    Description,
    URL : String;

    ReturnsData : Boolean;
  end;

  TBIWebTests=record
  private
    class function CheckHost(const AHost: String):String; static;
    class procedure Init; static;
  public
    class var
      Tests : Array of TBIWebTest;

    class function GetData(const AURL,AHost:String):TDataItem; static;
    class procedure Run(const AHost:String=''); static;
    class procedure SaveToHTML(const AFileName:String; const AHost:String=''); static;
    class function URL(const ATest:TBIWebTest; const AHost:String=''):String; static;
  end;

implementation

uses
  BI.Web, System.Classes, System.SysUtils;

class procedure TBIWebTests.Init;

  procedure Add(const ADescription:String; const AURL:String=''; const ReturnsData:Boolean=True);
  var L : Integer;
  begin
    L:=Length(Tests);
    SetLength(Tests,L+1);

    Tests[L].Description:=ADescription;
    Tests[L].URL:=AURL;
    Tests[L].ReturnsData:=ReturnsData;
  end;

begin
  Add('Home');

  Add('History','history=1&format=.htm');
  Add('History (JSON)','history=1&format=.json');
  Add('History (XML)','history=1&format=.xml');
  Add('History (CSV)','history=1&format=.csv');

  Add('Data Access');

  Add('SQLite Import Definition','def=SQLite_demo&format=.htm',False);
  Add('SQLite Data Items','data=SQLite_demo&format=.htm');
  Add('SQLite Data Info','data=SQLite_demo&info=1&format=.htm');

  Add('SQLite Products Data','data=SQLite_demo|Products&format=.htm');
  Add('SQLite Products with no CSS style','data=SQLite_demo|Products&format=.htm&css=no');

  Add('SQLite just Products QuantityPerUnit data','data=SQLite_demo|Products|QuantityPerUnit&format=.htm');

  Add('SQLite first 5 Products','data=SQLite_demo|Products&max=5&format=.htm');
  Add('SQLite last 5 Products','data=SQLite_demo|Products&start=-5&format=.htm');

  Add('Products items only: ProductName and CategoryID','data=SQLite_demo|Products&format=.htm&items=ProductName,CategoryID');

  Add('Products filter: SupplierID = 11','data=SQLite_demo|Products&format=.htm&filter=SupplierID=11');

  Add('Products with extra expression column: (UnitsInStock+UnitsOnOrder)','data=SQLite_demo|Products&format=.htm&items=ProductName,UnitsInStock,UnitsOnOrder,UnitsInStock%2BUnitsOnOrder');

  Add('Sorting');

  Add('Products sorted by UnitsInStock (ascending)','data=SQLite_demo|Products&format=.htm&sort=UnitsInStock');

  Add('Products sorted by UnitsInStock (descending)','data=SQLite_demo|Products&format=.htm&sort=UnitsInStock:descending');

  Add('Products sorted by UnitsInStock+UnitsOnOrder','data=SQLite_demo|Products&format=.htm&sortexp=UnitsInStock%2BUnitsOnOrder');

  Add('Sort Restaurants by subcolumn "address.building"','data=|restaurants|mongo_restaurants&format=.htm&sort=address.building&start=1300');

  Add('Queries');

  Add('Orders OrderID and OrderDate','data=SQLite_demo|Orders&format=.htm&query=OrderID,OrderDate');

  Add('Orders EmployeeID and ShipCountry','data=SQLite_demo|Orders&format=.htm&query=EmployeeID,ShipCountry&sort=EmployeeID');

  Add('Orders DISTINCT EmployeeID and ShipCountry','data=SQLite_demo|Orders&format=.htm&query=EmployeeID,ShipCountry&sort=EmployeeID&distinct=true');

  Add('SQL select * from Customers','data=SQLite_demo&format=.htm&sql=select * from Customers');

  Add('Summaries');

  Add('Count of Orders by ShortMonth(OrderDate)','data=SQLite_demo|Orders&format=.htm&summary=count(OrderID);ShortMonth(OrderDate)');

  Add('Sum of Products UnitsInStock','data=SQLite_demo|Products&format=.htm&summary=sum(UnitsInStock);');

  Add('Average of "Order Details" UnitPrice and sum of Quantity','data=SQLite_demo|"Order Details"&format=.htm&summary=average(UnitPrice),sum(Quantity)');

  Add('Sum of "Order Details" Quantity by Categories CategoryName','data=SQLite_demo|"Order Details"&format=.htm&summary=sum(Quantity);Categories.CategoryName');

  Add('Sum of "Order Details" having Quantity&gt;5000 by Categories CategoryName','data=SQLite_demo|"Order Details"&format=.htm&summary=sum(Quantity);Categories.CategoryName&having={Sum of Quantity}>5000');

  Add('Charts');

  Add('Chart (HTML5 Live Javascript) of Sum of "Order Details" Quantity by Shippers ShipperID','data=SQLite_demo|"Order Details"&format=.html5&summary=sum(Quantity);Shippers.ShipperID');

  Add('HTML5 Height=200,Width=300,FontZoom=60','data=SQLite_demo|"Order Details"&format=.html5&summary=sum(Quantity);Shippers.ShipperID&width=300&height=200&fontzoom=60');

  Add('Chart (png) of Sum of "Order Details" Quantity by Shippers ShipperID','data=SQLite_demo|"Order Details"&format=.png&summary=sum(Quantity);Shippers.ShipperID');

  Add('Chart (jpg) of Sum of "Order Details" Quantity by Shippers ShipperID','data=SQLite_demo|"Order Details"&format=.jpg&summary=sum(Quantity);Shippers.ShipperID');

  Add('Chart 2D and "Flat" theme 1280x720','data=SQLite_demo|"Order Details"&format=.png&view3d=false&theme=flat&width=1280&height=720&amp;summary=sum(Quantity);Shippers.ShipperID');

  Add('Chart of Sum of "Order Details" Quantity by Shippers ShipperID, Quarter and Year of Orders.OrderDate','data=SQLite_demo|"Order Details"&format=.htm&summary=sum(Quantity);Shippers.CompanyName,Quarter(Orders.OrderDate),Year(Orders.OrderDate)');
end;

class function TBIWebTests.CheckHost(const AHost: String):String;
begin
  result:=Trim(AHost);

  if result='' then
     result:='steema.cat:15015';
end;

function RemoveTag(const AURL,ATag:String):String;
var i : Integer;
    tmp : String;
begin
  i:=Pos(ATag+'=',AURL);

  if i=0 then
     result:=AURL
  else
  begin
    result:=Copy(AURL,1,i-1);

    if Copy(result,Length(result),1)='&' then
       Delete(result,Length(result),1);

    tmp:=Copy(AURL,i+6,Length(AURL));

    if tmp<>'' then
    begin
      i:=Pos('&',tmp);

      if i>0 then
         result:=result+Copy(tmp,i,Length(tmp));
    end;
  end;
end;

class function TBIWebTests.URL(const ATest:TBIWebTest; const AHost:String):String;
begin
  result:='http://'+CheckHost(AHost)+'?'+ATest.URL;
end;

class function TBIWebTests.GetData(const AURL,AHost:String):TDataItem;
var tmp : String;
begin
  tmp:=RemoveTag(AURL,'format');

  if tmp='' then
     result:=nil
  else
     result:=TBIWebClient.FromURL('http://'+AHost+'?'+tmp);
end;

class procedure TBIWebTests.Run(const AHost: String);
var tmp : TBIWebTest;
    tmpData : TDataItem;
    tmpHost : String;
begin
  tmpHost:=CheckHost(AHost);

  for tmp in Tests do
  begin
    if tmp.ReturnsData then
    begin
      tmpData:=GetData(tmp.URL,tmpHost);
      try
      finally
        tmpData.Free;
      end;
    end;
  end;
end;

class procedure TBIWebTests.SaveToHTML(const AFileName, AHost: String);
var tmp : TStrings;
    tmpTest : TBIWebTest;
    tmpHost : String;
begin
  tmp:=TStringList.Create;
  try
    tmp.Add('<html>');
    tmp.Add('<body>');

    tmp.Add('<h2>TeeBI Remote Web Server Tests</h2>');
    tmp.Add('<p>');

    tmpHost:=CheckHost(AHost);

    for tmpTest in Tests do
        if tmpTest.URL='' then
           tmp.Add('<b>'+tmpTest.Description+'</b></br></br>')
        else
           tmp.Add('<a href=''http://'+AHost+'?'+tmpTest.URL+'''>'+tmpTest.Description+'</a></br></br>');

    tmp.Add('</p>');

    tmp.Add('</body>');
    tmp.Add('</html>');

    tmp.SaveToFile(AFileName);
  finally
    tmp.Free;
  end;
end;

initialization
  TBIWebTests.Init;
finalization
  TBIWebTests.Tests:=nil;
end.
