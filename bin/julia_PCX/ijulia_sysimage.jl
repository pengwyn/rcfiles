packages = [:OhMyREPL, :Debugger, :MacroTools,
            :IJulia,
            :Plots,
            :PyCall,
            # :UnicodePlots,
            # :GR,
            # :PyPlot,
			# :Atom,

# All of my util packages
            :AndExport,
			:AutoParameters,
			:AutoNamedTuples,
			:Constants,
            :DanUtils,

            :LanguageServer,

# Revise should come last to not cause problems loading new packages
			:Revise, 
			]

if gethostname() == "mixologist"
	println("Adding in extra mixologist packages")
	pushfirst!(packages, :SwarmMC)
end


for package in packages
	@eval import $package
end

using PackageCompiler

# create_sysimage(packages, sysimage_path=expanduser("~/.julia/config/ijulia_sysimage.so"), precompile_statements_file="OMR_trace.jl", precompile_execution_file="precompile_plots.jl")
create_sysimage(packages,
                sysimage_path=expanduser("~/.julia/config/ijulia_sysimage.so"),
                # precompile_statements_file="OMR_trace.jl",
                # precompile_execution_file="snoop.jl"
                # precompile_statements_file="traced.jl",
                precompile_statements_file="comb_traced.jl",
                )
