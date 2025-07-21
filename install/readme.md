## How to install TeeBI?

First clone this repository and extract the TeeBIRecompile.zip content.

## Automatic tool to recompile and install TeeBI packages

- TeeBIRecompile.exe
  
This tool:

- Detects the supported Delphi / C++ RAD Studio IDEs installed in your system.

- For each ide, it checks if you have TeeChart installed, and if its the
  Lite, Standard and/or Pro versions.

- Enables selecting and recompiling plaftorms.
  Warning: For Mac and iOS, Linux, RAD sdks should be already present.

- Recompiles the TeeBI packages from the packages\Temp subfolder.

- Installs the design-time DCLVCLxxx and DCLFMXxxx compiled packages.

- Copies the runtime *.bpl packages to Windows\Syswow64 and/or System32 folders.

- Sets the appropiate paths to enable compiling projects (Search, Library and Browsing paths)

Note:

It should be run in Administrator mode due to copying files to Windows subfolders.

### Lazarus v4 FreePascal 3.2.2

- Units are copied to sources\Lazarus folder before compiling the TeeBI.lpk Lazarus package.

- Some units not compatible or not ported to Lazarus yet, are not copied.
  
- When rebuilding the TeeBI package inside Lazarus, the "Sources" path might be necessary to add at ide Options
  
![](https://github.com/Steema/TeeBI/blob/master/docs/img/teebirecompile.png)

