## Google Gemini AI Demo

### Ask the AI agent your question using Delphi and TeeBI

First get a free personal private Google Gemini API key here, if you don't have one:
[aistudio.google.com/app/apikey](https://aistudio.google.com/app/apikey)

You can then send the request in one line of code:

```pascal
uses
   BI.AI, BI.DataItem;
   
procedure TForm_AI_Demo.Button1Click(Sender: TObject);
var Data1 : TDataItem;
begin
  // One line of code, call Gemini and get a data structure:
  Data1:=TBIAI.From('your key', 
  
      'Give me the list of the highest 10 mountains by elevation in csv format, just the list', 
      
      TBIAI.GoogleGemini);

  // Visualize it:
  BIGrid1.Data:=Data1;   // in the grid
  BIChart1.Data:=Data1;  // in the chart
end;
```

![](https://raw.github.com/Steema/BI/master/docs/img/TeeBI_AI_Google_Gemini_api.png)
