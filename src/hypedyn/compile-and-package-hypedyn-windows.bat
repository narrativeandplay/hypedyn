::
:: build hypedyn
::

:: clear everything
rmdir /S /Q bin
mkdir bin
rmdir /S /Q build
mkdir build

::
:: build the editor
::

:: build the directory structure
mkdir bin\hypedyn-windows
mkdir bin\hypedyn-windows\lib

:: compile
java kawa.repl -d build --warn-invoke-unknown-method --warn-undefined-variable  --main -C runhypedyn.scm
cd build
copy ..\Manifest.txt .
jar -cmf Manifest.txt hypedyn.jar ./ 
cd ..

:: copy the jar files
copy build\hypedyn.jar bin\hypedyn-windows\lib
copy ..\..\..\kawa\kawa-1.11.jar bin\hypedyn-windows\lib\kawa.jar
copy ..\..\lib\AppleJavaExtensions.jar bin\hypedyn-windows\lib

:: copy the .exe file
copy hypedyn.exe bin\hypedyn-windows

:: copy the help file
copy hypedyn.txt bin\hypedyn-windows\hypedyn.txt

:: copy the examples and tutorials
mkdir bin\hypedyn-windows\examples
copy examples\*.dyn bin\hypedyn-windows\examples
mkdir bin\hypedyn-windows\tutorials
copy ..\..\doc\*.pdf bin\hypedyn-windows\tutorials

::
:: build the reader
::

:: build the directory structure
mkdir bin\hypedyn-reader-windows
mkdir bin\hypedyn-reader-windows\lib

:: compile
del /F /Q build\*.*
java kawa.repl -d build --warn-invoke-unknown-method --warn-undefined-variable  --main -C htreader.scm
cd build
copy ..\Manifest-htreader.txt .\Manifest.txt
jar -cmf Manifest.txt hypedyn-reader.jar ./ 
cd ..

:: copy the jar files
copy build\hypedyn-reader.jar bin\hypedyn-reader-windows\lib
copy ..\..\..\kawa\kawa-1.11.jar bin\hypedyn-reader-windows\lib\kawa.jar
copy ..\..\lib\AppleJavaExtensions.jar bin\hypedyn-reader-windows\lib

:: copy the .exe file
copy hypedyn-reader.exe bin\hypedyn-reader-windows

::
:: build the applet reader
::

:: build the directory structure
mkdir bin\hypedyn-reader-applet

:: compile
del /F /Q build\*.*
java kawa.repl -d build --warn-invoke-unknown-method --warn-undefined-variable  --applet -C htapplet.scm
cd build
copy ..\Manifest.txt .
jar -cmf Manifest.txt htapplet.jar ./
cd ..

:: copy the jar files
copy build\htapplet.jar bin\hypedyn-reader-applet
copy ..\..\lib\kawa-applet.jar bin\hypedyn-reader-applet\kawa-applet.jar
copy ..\..\lib\AppleJavaExtensions.jar bin\hypedyn-reader-applet\AppleJavaExtensions.jar

:: copy the html file
copy htapplet.html bin\hypedyn-reader-applet\htapplet.html

::
:: assemble the export folder
:: 

:: build the export folder
mkdir bin\hypedyn-windows\lib\export

:: copy in the common jar files
copy bin\hypedyn-windows\lib\*.jar bin\hypedyn-windows\lib\export
copy bin\hypedyn-reader-windows\lib\hypedyn-reader.jar bin\hypedyn-windows\lib\export

:: copy in the windows files
copy hypedyn-reader-standalone.exe bin\hypedyn-windows\lib\export\hypedyn-reader.exe

:: copy in the linux files
copy hypedyn-reader-standalone bin\hypedyn-windows\lib\export\hypedyn-reader

:: copy in the macos bundle skeleton
xcopy /S /I "HypeDynReader-standalone_package.app" "bin\hypedyn-windows\lib\export\HypeDynReader.app"

:: copy in the applet files
copy bin\hypedyn-reader-applet\htapplet.html bin\hypedyn-windows\lib\export
copy bin\hypedyn-reader-applet\kawa-applet.jar bin\hypedyn-windows\lib\export
copy bin\hypedyn-reader-applet\htapplet.jar bin\hypedyn-windows\lib\export

::
:: copy lib dir to source folder for testing purposes
::
rmdir /S /Q lib
xcopy /S /I bin\hypedyn-windows\lib lib
copy bin\hypedyn-reader-windows\lib\hypedyn-reader.jar lib

::
:: cleanup
::

rmdir /S /Q build

:: done
