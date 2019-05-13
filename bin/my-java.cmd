set JAVA_HOME="/cygdrive/c/Program Files (x86)/JavaSoft/jre/1.8.0_144"
set JAVA_HOME="/cygdrive/c/Users/$USER/sw/zulu8.34.0.1-ca-jdk8.0.201-win_x64/"
set JAVA_HOME=c:\Users\%USER%\sw\zulu8.36.0.1-ca-fx-jdk8.0.202-win_x64
set DEBUG=-agentlib:jdwp=transport=dt_socket,server=y,address=5005,suspend=n
"%JAVA_HOME%\bin\java" %DEBUG% -jar %*
