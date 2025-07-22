## Use 7z compression api with TeeBI data.

**Setup**:

- Clone or download the [Delphi 7zip repository](https://github.com/geoffsmith82/d7zip)
- [Download 7z](https://www.7-zip.org/)  for Windows, 32bit and 64bit if necessary.
- Once 7zip is installed, locate the 7z.dll (not 7-z.dll) and copy it so your exes can locate it.
- Run this demo test project.

### How it works?

The BI.Compression unit contains code to compress and decompress streams with TeeBI Data items.
The Delphi RTL System.Zip engine is used by default.

You can change to your custom engine just using the unit and a line of code, for example with 7zip:

```pascal
uses BI.Compression, BI.Compression.d7zip;
...
  TCompression.Plugin := Td7zipCompression;  // <--- use 7zip
...
  // Back to system, if necessary:
  TCompression.Plugin := TSystemCompression;  // <--- use the default System.Zip

```

Compressing data using the current engine:

```pascal
var S : TStream;
    S := TStore.DataToStream('TechProducts',True);  // <-- returns a compressed stream
```

Decompressing:

```pascal
var S2 : TStream;
  S.Position := 0;  // reset stream to start
  S2 := TCompression.DeCompress(S,''TechProducts'');

var Data1 : TDataItem;
  Data1 := TPersistence.Load(S2);

```

Optional methods enable compressing a single TDataItem, an array, a Store full of data, a file, etc.

Other plugins are available, like [TSynzip and TSynLZ](https://github.com/Steema/TeeBI/tree/master/demos/3rd_party/SynLZ).

The [BIWeb server](https://github.com/Steema/TeeBI/wiki/biweb) optionally uses compression to transmit the Data items streams via http / https.


