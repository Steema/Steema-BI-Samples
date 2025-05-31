## Automatic tool to recompile and install TeeBI packages

- TeeBIRecompile.exe
  
This tool:

- Detects the supported Delphi / C++ RAD Studio IDEs
- Checks if you have TeeChart installed, Lite, Standard or Pro versions
- Enables selecting and recompiling plaftorms
     Warning: For Mac and iOS, sdks should be present
- Installs the design-time DCLxxx packages
- Copies the runtime *.bpl packages to Windows\Syswow64 and System32
- Sets the appropiate paths to source code (Search, Library and Browsing paths)

Note:

It should be run in Administrator mode due to copying files to Windows subfolders.
