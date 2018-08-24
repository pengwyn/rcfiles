import Distributed

#if isinteractive()
if true
	if Distributed.nprocs() == 1
		push!(LOAD_PATH, "/home/pengwyn/work5/julia")
		push!(LOAD_PATH, ".")
	else
		@eval Distributed.@everywhere push!(LOAD_PATH, "/home/pengwyn/work5/julia")
		@eval Distributed.@everywhere push!(LOAD_PATH, ".")
	end


#ENV["PLOTS_DEFAULT_BACKEND"] = "PlotlyJS"
#ENV["PLOTS_DEFAULT_BACKEND"] = "PyPlot"


#			for pkg in ["Glob",
#						"PyPlot",
#						"PlotlyJS",
#						"GR",
#						"Plots",
#						"StaticArrays",
#						"DataFrames",
#						"JLD2",
#						"LsqFit",
#						"QuadGK",
#						"MAT",
#						"DifferentialEquations",
#						"SpecialFunctions",
#						"IJulia",
#						"Roots",
#						"Revise",
#						"Polynomials",
#						"Dierckx",
#						"EllipsisNotation",
#                        "OffsetArrays",
#                        "MicroLogging",
#                        "NamedTuples",
#                        "Optim",
#                        "Reexport",
#                        "NNLS",
#						]
	try
		@eval using Revise
		# Turn on Revise's automatic-evaluation behavior
		Revise.async_steal_repl_backend()
	catch err
		@warn "Could not load Revise."
	end

	using Generic
end
