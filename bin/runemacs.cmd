::#!/bin/sh
::export HOME=C:\\cygwin64\\home\\bernh
set HOME=C:\cygwin64\home\bernh
set MYEMACSDIR=C:\Users\bernh\sw\emacs-26.2-x86_64
set MYEMACSDIR=c:/msys64/mingw64
set PATH=%PATH%;%MYEMACSDIR%\bin
set PATH=%PATH%;%HOME%\bin
set my_unix=mingw64
if %my_unix%==cygwin ( set "PATH=c:\cygwin64\bin;%PATH%" )
if %my_unix%==msys64 ( set "PATH=C:\msys64\usr\bin;%PATH%" )
if %my_unix%==mingw64 (
  ::set "PATH=%PATH%;C:\msys64\usr\bin;c:\msys64\mingw64\bin"
  set "PATH=c:\msys64\mingw64\bin;C:\msys64\usr\bin;C:\msys64\bin;%PATH%"
  set DICPATH=c:/Program Files/LibreOffice/share/extensions/dict-de
)
set PATH=c:\Program Files\ut\7-Zip;%PATH%
set EDITOR=emacsclientw.exe -f ~/.emacs.d/server/windows-nt
%MYEMACSDIR%\bin\runemacs.exe
::c:/msys64/usr/bin/emacs.exe