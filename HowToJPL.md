# Abstract #

The SWI-Prolog compiler needs to be recompiled (on most Linux distributions) for JPL to work, since JPL is not in the default-configuration and it needs libswipl compiled as a shared library.

However: Online documentation indicates that recompilation may be not necessary on AMD64/x86\_64 platforms. Only compiling JPL may suffice.


# Steps #

Get the source: http://www.swi-prolog.org/download/stable/src/pl-5.10.1.tar.gz

Extract the tarball and edit build.templ as follows:
Set PREFIX:
  * PREFIX=/usr
  * SUDO="sudo"
Select Packages:
  * export DISABLE\_PKGS="ssl odbc xpce zlib"
Additional packages may be disabled (as long as JPL stays enabled, of course) or any of the 4 above may be enabled. However xpce and zlib are known to cause problems in some environments and are not needed for naproche anyway.
And create a shared library. Add:
  * EXTRACFG+=" --enable-shared"

Make sure every one of the options above is uncommented.

Then run build.templ. (Despite setting SUDO in most cases the script needs to be called as root)

Afterwards there should be a "jpl.jar" in /usr/lib/swipl-5.10.1/lib and libswipl.so in /usr/lib/swipl-5.10.1/lib/i686-linux

For compilation of the java-sources the jpl.jar needs to be in the Classpath and java needs to be called as "java -Djava.library.path=.:/usr/lib/swipl-5.10.1/lib/i686-linux" (path to libswpl.so)
This is most easily done by using the provided "env.sh" script, called by the various run.sh scripts. When using an IDE (like Eclipse) manual work is required to set the variables (see below).

Please note: While this will (probably) not overwrite any SWI-Prolog packages which were installed via package management, it will replace the symbolic links to swipl, swipl-ld and swipl-rc in /usr/bin.

Tested on:
  * Arch Linux, i686
  * Ubuntu Linux, i686

# Troubleshooting #

Most erros when using JPL are caused by incorrectly set Classpath and java.library.path variables.

"java: symbol lookup error: libjpl.so: undefined symbol: PL\_is\_initialised" indicates that the java.library.path does not point to libswipl.

When using Eclipse you need to explicitly reference the jpl.jar. Also the "native library location" (the path to libswipl.so) must be known to Eclipse. This can be done as follows:
Right-click on your project, select Properties -> Java Build Path -> Libraries. Expand the "jpl.jar"-entry, select "native library location" and give it the path to libswipl.so (most likely "/usr/lib/swipl-5.10.1/lib/i686-linux")

# Links #
http://comments.gmane.org/gmane.comp.ai.prolog.swi/12597