unit Unit_Icon_Generator;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  System.Types,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  PNGImage, VCLTee.TeeGDIPlus, WinAPI.GDIPAPI, WinAPI.GDIPOBJ,gifimg;

type
  TFormIconGenerator = class(TForm)
    LBIcons: TListBox;
    EPath: TEdit;
    BLoad: TButton;
    ImageOriginal: TImage;
    Label1: TLabel;
    Image16x16: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Image32x32: TImage;
    Image24x24: TImage;
    Label4: TLabel;
    Button1: TButton;
    LBQuality: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure BLoadClick(Sender: TObject);
    procedure LBIconsClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LBQualityClick(Sender: TObject);
  private
    { Private declarations }

    function CreateBitmap(const ASource:TBitmap; const ASize:Integer):TBitmap;
  public
    { Public declarations }
  end;

var
  FormIconGenerator: TFormIconGenerator;

implementation

{$R *.dfm}

uses
  System.IOUtils;

const
  PNGExtension='.png';

procedure TFormIconGenerator.BLoadClick(Sender: TObject);
var s : String;
begin
  LBIcons.Items.BeginUpdate;
  try
    LBIcons.Clear;

    for s in TDirectory.GetFiles(EPath.Text,'*'+PNGExtension) do
        LBIcons.Items.Add(TPath.GetFileNameWithoutExtension(s));

  finally
    LBIcons.Items.EndUpdate;
  end;
end;

procedure TFormIconGenerator.FormCreate(Sender: TObject);
begin
  LBQuality.ItemIndex := 0;
  BLoadClick(Self);
end;

procedure TFormIconGenerator.FormShow(Sender: TObject);
begin
  if LBIcons.Count>0 then
  begin
    LBIcons.ItemIndex:=0;
    LBIconsClick(Self);
  end;
end;

procedure TFormIconGenerator.Button1Click(Sender: TObject);
var t : Integer;
    tmp : String;
begin
  for t:=0 to LBIcons.Count-1 do
  begin
    LBIcons.ItemIndex:=t;
    LBIconsClick(Self);

    tmp:=LBIcons.Items[t];

    Image16x16.Picture.Bitmap.SaveToFile(TPath.Combine(EPath.Text,tmp+'16.bmp'));
    Image24x24.Picture.Bitmap.SaveToFile(TPath.Combine(EPath.Text,tmp+'.bmp')); // <-- no size = default 24x24
    Image32x32.Picture.Bitmap.SaveToFile(TPath.Combine(EPath.Text,tmp+'32.bmp'));
  end;
end;

Procedure ReduceTo8Bit(var bmp:TBitmap; ColorReduction: TColorReduction; DitherMode: TDitherMode);
var
 GI:TGifImage;
begin
   GI:=TGifImage.Create;
   try
     GI.DitherMode := DitherMode;
     GI.ColorReduction := ColorReduction;
     GI.Assign(bmp);
     bmp.Assign(GI.Bitmap);
   finally
     GI.Free;
   end;
end;

procedure ResizeBitmap(const Source,Dest:TBitmap; const AMode:InterpolationMode);
var tmpGraphics : TGPGraphics;
    tmpDC       : HDC;
    tmpImage    : TGPImage;
    tmpDestRect : TRect;
begin
  tmpDC:=Dest.Canvas.Handle;

  tmpGraphics:=TGPGraphics.Create(tmpDC);
  try
    tmpImage:=TGPBitmap.Create(Source.Handle, Source.Palette);
    try
      tmpDestRect:=TRect.Create(0,0,Dest.Width,Dest.Height);

      tmpGraphics.SetInterpolationMode(AMode); //InterpolationModeHighQualityBicubic best but rewrites background color (need 248,248,248).
                                               //InterpolationModeDefault tests OK

      tmpGraphics.DrawImage(tmpImage,MakeRect(tmpDestRect));
    finally
      tmpImage.Free;
    end;
  finally
    tmpGraphics.Free;
  end;
end;

function TFormIconGenerator.CreateBitmap(const ASource:TBitmap; const ASize:Integer):TBitmap;
begin
  result:=TBitmap.Create;
  result.SetSize(ASize,ASize);

  result.PixelFormat:=ASource.PixelFormat;

  {TGDIPlusCanvas.} ResizeBitmap(ASource,result,InterpolationMode(LBQuality.ItemIndex));
  //TGDIPlusCanvas.ResizeBitmap(ASource,result); //,InterpolationMode(LBQuality.ItemIndex));

  result.TransparentColor:= Rgb(248,248,248);
  result.TransparentMode:=TTransparentMode.tmFixed;

  ReduceTo8Bit(result,rmQuantize,dmStucki);
  result.PixelFormat:=vcl.Graphics.TPixelFormat.pf8bit; //let's be sure we're on 8bit.
end;

function OriginalBitmap(const AGraphic:TGraphic):TBitmap;
begin
  result:=TBitmap.Create;
  result.SetSize(AGraphic.Width,AGraphic.Height);
  result.Canvas.Draw(0,0,AGraphic);
end;

procedure TFormIconGenerator.LBIconsClick(Sender: TObject);

  procedure SetNewImage(const AImage:TImage; const ASource:TBitmap; const ASize:Integer);
  begin
    AImage.Picture.Bitmap.Free;
    AImage.Picture.Bitmap:=CreateBitmap(ASource,ASize);
  end;

var tmp : String;
    tmpSource : TBitmap;
begin
  tmp:=TPath.Combine(EPath.Text,LBIcons.Items[LBIcons.ItemIndex]+PNGExtension);
  ImageOriginal.Picture.LoadFromFile(tmp);

  tmpSource:=OriginalBitmap(ImageOriginal.Picture.Graphic);
  try
    SetNewImage(Image16x16,tmpSource,16);
    SetNewImage(Image24x24,tmpSource,24);
    SetNewImage(Image32x32,tmpSource,32);
  finally
    tmpSource.Free;
  end;
end;

procedure TFormIconGenerator.LBQualityClick(Sender: TObject);
begin
  LBIconsClick(Self);
end;

end.
