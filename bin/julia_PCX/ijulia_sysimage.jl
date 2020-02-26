# ENV["PLOTS_DEFAULT_BACKEND"] = "unicodeplots"

packages = [:Revise, :OhMyREPL, :Debugger, :MacroTools,
            :IJulia,
            #:Plots, :GR,
            :PyCall,
            # :UnicodePlots,
            # :GR,
            :Plots,
            # :PyPlot,
            #:Interact, :Blink, :WebIO,

# All of my util packages
            :AndExport,
			:AutoParameters,
			:AutoNamedTuples,
			:Constants,
            :DanUtils,
			]


for package in packages
	@eval import $package
end

using PackageCompiler

# create_sysimage(packages, sysimage_path=expanduser("~/.julia/config/ijulia_sysimage.so"), precompile_statements_file="OMR_trace.jl", precompile_execution_file="precompile_plots.jl")
create_sysimage(packages, sysimage_path=expanduser("~/.julia/config/ijulia_sysimage.so"), precompile_statements_file="OMR_trace.jl")
