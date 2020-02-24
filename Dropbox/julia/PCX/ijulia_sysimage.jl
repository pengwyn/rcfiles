ENV["PLOTS_DEFAULT_BACKEND"] = "gr"

using Revise, OhMyREPL, Debugger, DanUtils, Constants, MacroTools
using Plots, GR
using IJulia
#using Interact, Blink, WebIO

using PackageCompilerX

create_sysimage([:Revise, :OhMyREPL, :Debugger, :DanUtils, :Constants, :IJulia, :MacroTools,
                 :Plots, :GR,
                 #:Interact, :Blink, :WebIO
                 ], sysimage_path=expanduser("~/.julia/config/ijulia_sysimage.so"), precompile_statements_file="OMRPlotsDebugger_trace.jl", precompile_execution_file="precompile_plots.jl")
