#/usr/bin/julia --startup-file=no -e 'using PackageCompiler; compile_incremental(:Plots, :DataStructures, :Roots, :Glob, :Dierckx, :DataFrames, :CSV, :SpecialFunctions, :Parameters, :ArgCheck, :PyCall, :PyPlot)'
#/usr/bin/julia --startup-file=no -e 'using PackageCompiler; compile_incremental(:Plots, :DataStructures, :Roots, :Glob, :Dierckx, :DataFrames, :CSV, :SpecialFunctions, :Parameters, :ArgCheck, :PyCall)'
/usr/bin/julia --startup-file=no -e 'using PackageCompiler; compile_incremental(:Plots, :DataStructures, :Roots, :Glob, :Dierckx, :DataFrames, :CSV, :SpecialFunctions, :Parameters, :ArgCheck)'
#julia --startup-file=no -e 'using PackageCompiler; compile_package(:Plots, :DataStructures, :Roots, :Glob, :Dierckx, :DataFrames, :CSV, :SpecialFunctions, :Parameters, :ArgCheck)'
