# Load this with "source .akku/bin/.akku/bin/activate.fish" in Fish    -*-sh-*-
set --export CHEZSCHEMELIBDIRS "$PWD/.akku/lib::$PWD/.akku/libobj"
set --erase CHEZSCHEMELIBEXTS
set --export GUILE_LOAD_PATH "$PWD/.akku/lib"
set --export IKARUS_LIBRARY_PATH "$PWD/.akku/lib"
set --export MOSH_LOADPATH "$PWD/.akku/lib"
set --export PLTCOLLECTS ":$PWD/.akku/lib"
set --export SAGITTARIUS_LOADPATH "$PWD/.akku/lib"
set --export VICARE_SOURCE_PATH "$PWD/.akku/lib"
set --export YPSILON_SITELIB "$PWD/.akku/lib"
set --export LARCENY_LIBPATH "$PWD/.akku/lib"
set --export CHIBI_MODULE_PATH "$PWD/.akku/lib"
set --prepend PATH $PWD/.akku/bin
if set --query LD_LIBRARY_PATH
  set --export --prepend LD_LIBRARY_PATH $PWD/.akku/ffi
else
  set --export LD_LIBRARY_PATH $PWD/.akku/ffi
end
if set --query DYLD_LIBRARY_PATH
  set --export --prepend DYLD_LIBRARY_PATH $PWD/.akku/ffi
else
  set --export DYLD_LIBRARY_PATH $PWD/.akku/ffi
end
