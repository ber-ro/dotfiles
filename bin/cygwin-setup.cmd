@echo off
setlocal
set https_proxy=127.0.0.1:3128
set setup=C:\Users\%USERNAME%\Downloads\setup-x86_64.exe
@echo on
c:\cygwin64\bin\wget.exe -v https://cygwin.com/setup-x86_64.exe -O %setup%
icacls %setup% /reset
%setup% -M -B
@echo off
goto :end

:end
endlocal
