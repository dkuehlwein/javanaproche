################################################################
# Setup the environment for compiling and running JPL-linked Java code.
# Should never be called directly.
# Is called by run_xxx.sh Scripts like run_Example.sh.
#
# Required setup:
# 
# 	* The directory holding java and javac must be in $PATH
# 	* JPL must be installed
# 	* Prolog must be available as one of "swi-prolog", "swipl"
#	  or "pl" in $PATH
#
################################################################

findexe()
{ oldifs="$IFS"
  IFS=:
  for d in $PATH; do
    if [ -x $d/$1 ]; then
       IFS="$oldifs"
       return 0
    fi
  done
  IFS="$oldifs"
  return 1
}

for f in swi-prolog swipl pl; do
  if [ -z "$PL" ]; then
     if findexe $f; then
        PL="$f"
     fi
  fi
done

if findexe java; then
  true
elif [ -x "$JAVA_HOME"/bin/java ]; then
  PATH="$PATH:$JAVA_HOME/bin"
else
  echo "ERROR: Cannot find java.  Please ensure JAVA_HOME is set"
  echo "ERROR: properly or java is in $PATH"
  exit 1
fi

if findexe javac; then
  true
else
  echo "ERROR: Cannot find javac.  This demo requires the SDK to"
  echo "ERROR: be installed and accessible through JAVA_HOME"
  echo "ERROR: or PATH"
  exit 1
fi

################################################################
# Setup the environment
################################################################

eval `$PL -dump-runtime-variables`

JPLJAR="java/lib/jpl.jar"
# JPLJAR="$PLBASE/lib/jpl.jar"

PLLIBDIR="$PLBASE/lib/$PLARCH"
if [ -z "$LD_LIBRARY_PATH" ]; then
   LD_LIBRARY_PATH="$PLLIBDIR";
else
   LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$PLLIBDIR"
fi

   CLASSPATH="$CLASSPATH:$JPLJAR"

export LD_LIBRARY_PATH CLASSPATH
echo $CLASSPATH
echo $LD_LIBRARY_PATH

echo "Compiling Javanaproche"
javac java/net/naproche/GUI/*.java -d .
javac java/net/naproche/preparser/*.java -d .


################################################################
# run Class
# 
# Compiles Class if the .class file does not exsist and runs it
# Note that some systems (Linux, ...) find the libjpl.xxx from
# LD_LIBRARY_PATH.  MacOS finds this only when named libjpl.jnilib
# and using -Djava.library.path=<Path>.  We pass both, hoping to
# satisfy most systems ...
################################################################

run()
{
   echo "Javanaproche Module: $1"
   java -Djava.library.path=.:$PLLIBDIR net/naproche/$1
}
