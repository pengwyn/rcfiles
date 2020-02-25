ENV["PLOTS_DEFAULT_BACKEND"] = "gr"

packages = [:Revise, :OhMyREPL, :Debugger, :IJulia, :MacroTools,
                 :Plots, :GR,
                 #:Interact, :Blink, :WebIO,
# All of my util packages
            :AndExport,
			:AutoParameters,
			:AutoNamedTuples,
			:DanUtils, :Constants,
			]


for package in packages
	@eval using $package
end

using PackageCompiler

create_sysimage(packages, sysimage_path=expanduser("~/.julia/config/ijulia_sysimage.so"), precompile_statements_file="OMR_trace.jl")#, precompile_execution_file="precompile_plots.jl")
