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

  Don't have a AI key yet?

  Get a free personal PRIVATE key here:

  Google Gemini     : https://aistudio.google.com/app/apikey
  Microsoft Copilot :
  OpenAI ChatGPT    :
  X Grok            :

  Example code:

  var Data1 : TDataItem;

  // One line of code, call Gemini and get a data structure:
  Data1:=TBIAI.From('your_api_key',
     'Give me the list of mountains by elevation in csv format, just the list',
     TBIAI.TAgent.Gemini);

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
    type
      TAgent=(Gemini,Copilot,ChatGPT,Grok);

    class function From(const Question:String; const Agent:TAgent;
                        const Key:String):TDataItem; static;
  end;

implementation


uses
  {$IFDEF FPC}
  fpHttpClient, jsonParser
  {$ELSE}
  System.Net.HttpClient, System.JSON, System.IOUtils,

  // Units used to avoid "inline" not-expansion hints:
  System.Generics.Collections, System.Net.URLClient, System.NetConsts
  {$ENDIF}
  ;

{ TBIAI }

procedure DoError(const AMessage:String);
begin
  raise EBIAI.Create(AMessage);
end;

class function TBIAI.From(const Question:String; const Agent:TAgent; const Key:String): TDataItem;

const
  GeminiURL  ='https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent';
  CopilotURL ='https://??';
  ChatGPTURL ='https://api.openai.com/v1/chat/completions'; // 'https://api.openai.com/v1/responses';
  GrokURL    ='http://api.x.ai/v1/chat/completions';

{$IFDEF FPC}
// Work in progress
begin
  result:=nil;
end;
{$ELSE}

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

var
  Response : IHTTPResponse;
  Body : TStringStream;
  Http : THTTPClient;
  tmpOutput : String;
begin
  result:=nil;

  if Key.Trim='' then
     DoError('AI API Key missing');

  if Question.Trim='' then
     DoError('AI Question missing');

  { DEBUG: if TFile.Exists('AI_Output.txt') then
     result:=TBIFileSource.FromText(TFile.ReadAllText('AI_Output.txt')))
  else
  }

  Http:=THTTPClient.Create;
  try
    Http.ContentType:='application/json';

    case Agent of
      Gemini :
      begin
        Http.CustomHeaders['X-goog-api-key']:=Key;

        Body:=TStringStream.Create('{"contents": [{"parts": [{"text": "'+Question.Trim+'"}]}]}');
        try
          Response:=Http.Post(GeminiURL, Body);
          tmpOutput:=GeminiOutput(Response.ContentAsString);
        finally
          Body.Free;
        end;
      end;

      Copilot:
      begin
        tmpOutput:='Pending to implement';
      end;

      ChatGPT:
      begin
        Http.CustomHeaders['Authorization']:='Bearer '+Key;

        { Models:

          gpt-4.1
          gpt-4.1-mini
          gpt-4.1-nano
          gpt-4.5-preview
          gpt-4o
          gpt-4o-mini
          o1
          o1-mini
          o1-pro
          o3
          o3-mini
          o4-mini
        }

        Body:=TStringStream.Create('{"model": "gpt-4.1-mini", "messages": [{"role": "user", "content": "'+Question.Trim+'"}] }');
        try
          Response:=Http.Post(ChatGPTURL, Body);
          tmpOutput:=Response.ContentAsString;
        finally
          Body.Free;
        end;
      end;

      Grok:
      begin
        Http.CustomHeaders['Authorization']:='Bearer '+Key;

        Body:=TStringStream.Create('{"messages": [{"role": "user", "content": "'+
            Question.Trim+'"}], "model": "grok-3-latest", "stream": false, "temperature": 0 }');
        try
          Response:=Http.Post(GrokURL, Body);
          tmpOutput:=Response.ContentAsString;
        finally
          Body.Free;
        end;
      end;
    end;

  finally
    Http.Free;
  end;

  // DEBUG: TBITextExport.SaveToFile(tmpOutput,'AI_Output.txt');

  if tmpOutput.Trim='' then
     DoError('Empty response from AI');

  result:=TBIFileSource.FromText(tmpOutput);
end;
{$ENDIF}

end.
