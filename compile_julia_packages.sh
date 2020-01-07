#/usr/bin/julia --startup-file=no -e 'using PackageCompiler; compile_incremental(:Plots, :DataStructures, :Roots, :Glob, :Dierckx, :DataFrames, :CSV, :SpecialFunctions, :Parameters, :ArgCheck, :PyCall, :PyPlot)'
#/usr/bin/julia --startup-file=no -e 'using PackageCompiler; compile_incremental(:Plots, :DataStructures, :Roots, :Glob, :Dierckx, :DataFrames, :CSV, :SpecialFunctions, :Parameters, :ArgCheck, :PyCall)'
#/usr/bin/julia --startup-file=no -e 'using PackageCompiler; compile_incremental(:Plots, :DataStructures, :Roots, :Glob, :Dierckx, :DataFrames, :CSV, :SpecialFunctions, :Parameters, :ArgCheck)'
#julia --startup-file=no -e 'using PackageCompiler; compile_package(:Plots, :DataStructures, :Roots, :Glob, :Dierckx, :DataFrames, :CSV, :SpecialFunctions, :Parameters, :ArgCheck)'

# /usr/bin/julia --startup-file=no -e 'using PackageCompiler; compile_incremental(:Revise, :OhMyREPL, :Constants, :DanUtils, :Plots, :DataStructures, :Dierckx, :DataFrames, :CSV, :SpecialFunctions)'
# /usr/bin/julia --startup-file=no -e 'using PackageCompiler; compile_incremental(:OhMyREPL, :Constants, :DanUtils, :Plots, :DataStructures, :Dierckx, :CSV, :SpecialFunctions, :Debugger)'
/usr/bin/julia --startup-file=no -e 'using PackageCompiler; compile_incremental(:OhMyREPL, :Constants, :DanUtils, :Plots, :Debugger)'
