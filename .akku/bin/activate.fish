set --export CHEZSCHEMELIBDIRS "$PWD/.akku/lib::$PWD/.akku/libobj"
set --erase CHEZSCHEMELIBEXTS
set --export GUILE_LOAD_PATH "$PWD/.akku/lib"
set --export GUILE_LOAD_COMPILED_PATH "$PWD/.akku/libobj"
set --export IKARUS_LIBRARY_PATH "$PWD/.akku/lib"
set --export MOSH_LOADPATH "$PWD/.akku/lib"
set --export PLTCOLLECTS ":$PWD/.akku/lib"
set --export SAGITTARIUS_LOADPATH "$PWD/.akku/lib"
set --export VICARE_SOURCE_PATH "$PWD/.akku/lib"
set --export YPSILON_SITELIB "$PWD/.akku/lib"
set --export LARCENY_LIBPATH "$PWD/.akku/lib"
set --export IRONSCHEME_LIBRARY_PATH "$PWD/.akku/lib"
set --export LOKO_LIBRARY_PATH "$PWD/.akku/lib"
set --export CHIBI_MODULE_PATH "$PWD/.akku/lib"
set --export GAUCHE_LOAD_PATH "$PWD/.akku/lib"
set --export --prepend PATH $PWD/.akku/bin
set --export --prepend LD_LIBRARY_PATH $PWD/.akku/ffi
set --export --prepend DYLD_LIBRARY_PATH $PWD/.akku/ffi
