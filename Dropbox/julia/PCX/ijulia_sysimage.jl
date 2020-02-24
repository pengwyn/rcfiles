ENV["PLOTS_DEFAULT_BACKEND"] = ""

using Revise, OhMyREPL, Plots, Debugger, DanUtils, Constants, MacroTools
using IJulia
#using Interact, Blink, WebIO

using PackageCompilerX

create_sysimage([:Revise, :OhMyREPL, :Plots, :Debugger, :DanUtils, :Constants, :IJulia, :MacroTools,
                 #:Interact, :Blink, :WebIO
                 ], sysimage_path=expanduser("~/.julia/config/ijulia_sysimage.so"), precompile_statements_file="OMRPlotsDebugger_trace.jl", precompile_execution_file="precompile_plots.jl")
