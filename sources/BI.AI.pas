{*********************************************}
{  TeeBI Software Library                     }
{  AI Agents API                              }
{  Copyright (c) 2025-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit BI.AI;

interface

{
  This unit implements classes to import data from AI agents like
  Google Gemini, ChatGPT and others.

  You need to provide your personal API Key and a question.
  The AI agent will return a response and TeeBI will try to visualize these
  results.

  Don't have a Google Gemini key yet?

  Get a free personal PRIVATE Google Gemini API key here:

  https://aistudio.google.com/app/apikey

  Example code:

  var Data1 : TDataItem;

  // One line of code, call Gemini and get a data structure:
  Data1:=TBIAI.From('your_api_key',
     'Give me the list of mountains by elevation in csv format, just the list',
     TBIAI.GoogleGemini);

  // Visualize it:
  BIGrid1.Data:=Data1;
  BIChart1.Data:=Data1;


}


uses
  System.SysUtils, System.Classes,

  BI.Arrays, BI.DataItem, BI.DataSource;

type
  EBIAI=class(EBIException);

  TBIAI=class(TBISource)
  public
    const
      GoogleGemini='Gemini';

    class function From(const Key,Question,Agent:String):TDataItem; static;
  end;

implementation


uses
  System.Net.HttpClient, System.JSON, System.IOUtils,

  // Units used to avoid "inline" not-expansion hints:
  System.Generics.Collections, System.Net.URLClient, System.NetConsts;

{ TBIAI }

procedure DoError(const AMessage:String);
begin
  raise EBIAI.Create(AMessage);
end;

class function TBIAI.From(const Key, Question, Agent: String): TDataItem;

  function GeminiOutput(const JSON:String):String;
  const
    csvFlag='```csv'+#10;
    jsonFlag='```json'+#10;

  var tmp,
      tmpContent : TJSONObject;
      Arr : TJSONArray;
      tmpS : String;
  begin
    tmp:=TJSONObject.ParseJSONValue(JSON) as TJSONObject;

    if tmp<>nil then
    try
      if tmp.TryGetValue('candidates', Arr) and (not Arr.IsEmpty) then
         if Arr.Items[0].TryGetValue('content', tmpContent) then
            if tmpContent.TryGetValue('parts', Arr) and (not Arr.IsEmpty) then
               if Arr.Items[0].TryGetValue('text', tmpS) then
               begin
                 // Cleanup:
                 if tmpS.StartsWith(CSVFlag) then
                    tmpS:=tmpS.Substring(CSVFlag.Length)
                 else
                 if tmpS.StartsWith(JSONFlag) then
                    tmpS:=tmpS.Substring(JSONFlag.Length);

                 if tmpS.EndsWith('```') then
                    tmpS:=tmpS.Remove(tmpS.Length-3,3)
                 else
                 if tmpS.EndsWith('```'#10) then
                    tmpS:=tmpS.Remove(tmpS.Length-4,4);

                 Exit(tmpS);
               end;
    finally
      tmp.Free;
    end;

    DoError('AI Google Gemini error: Wrong response');
  end;

const
  GeminiURL='https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent';

var
  Response : IHTTPResponse;
  Body : TStringStream;
  Http : THTTPClient;
begin
  result:=nil;

  if SameText(Agent,GoogleGemini) then
  begin
    if Key.Trim='' then
       DoError('AI API Key missing');

    if Question.Trim='' then
       DoError('AI Question missing');

    { DEBUG: if TFile.Exists('Gemini_Output.json') then
       result:=TBIFileSource.FromText(GeminiOutput(TFile.ReadAllText('Gemini_Output.json')))
    else
    }
    begin
      Body:=TStringStream.Create('{"contents": [{"parts": [{"text": "'+Question.Trim+'"}]}]}');
      try
        Http:=THTTPClient.Create;
        try
          Http.ContentType:='application/json';
          Http.CustomHeaders['X-goog-api-key']:=Key;

          Response:=Http.Post(GeminiURL, Body);

          // DEBUG: TBITextExport.SaveToFile(Response.ContentAsString,'Gemini_Output.json');

          result:=TBIFileSource.FromText(GeminiOutput(Response.ContentAsString));
        finally
          Http.Free;
        end;
      finally
        Body.Free;
      end;
    end;
  end
  else
     DoError('AI Agent: '+Agent+' not supported');
end;

end.
