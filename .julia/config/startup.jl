import Distributed

if Distributed.nprocs() == 1
	push!(LOAD_PATH, "/home/pengwyn/work5/julia")
	push!(LOAD_PATH, ".")
else
	@eval Distributed.@everywhere push!(LOAD_PATH, "/home/pengwyn/work5/julia")
	@eval Distributed.@everywhere push!(LOAD_PATH, ".")
end


#ENV["PLOTS_DEFAULT_BACKEND"] = "PlotlyJS"
#ENV["PLOTS_DEFAULT_BACKEND"] = "PyPlot"


function PackageCheck()
	if !("NO_JULIA_PACKAGE_CHECK" in keys(ENV))
		println("Doing package check")
		withenv("NO_JULIA_PACKAGE_CHECK" => "STOP") do
			firsttime = true
			for pkg in ["Glob",
						"PyPlot",
						"PlotlyJS",
						"GR",
						"Plots",
						"StaticArrays",
						"DataFrames",
						"JLD2",
						"LsqFit",
						"QuadGK",
						"MAT",
						"DifferentialEquations",
						"SpecialFunctions",
						"IJulia",
						"Roots",
						"Revise",
						"Polynomials",
						"Dierckx",
						"EllipsisNotation",
                        "OffsetArrays",
                        "MicroLogging",
                        "NamedTuples",
                        "Optim",
                        "Reexport",
                        "NNLS",
						]
				if Pkg.installed(pkg) == nothing
					if firsttime
						print("Do I want to update packages now? ")
						ans = chomp(readline())
						if ans == "yes"
							print_with_color(:green, "Updating package database before installing $pkg:\n", bold=true)
							Pkg.update()
							println("About to change firstime to false: $(getpid())")
							firsttime = false
						elseif ans == "no"
							println("Aborting")
							break
						else
							error("Unknown answer - should be 'yes' or 'no'")
						end
					end
					print_with_color(:green, "Install package $pkg:\n", bold=true)
					Pkg.add(pkg)
					Pkg.build(pkg)
				end
			end
		end
		println("Done package check")
	end
end

if false
	PackageCheck()
end

try
    @eval using Revise
    # Turn on Revise's automatic-evaluation behavior
    Revise.async_steal_repl_backend()
catch err
    @warn "Could not load Revise."
end
# atreplinit() do REPL
#     using Distributed
#     Distributed.@schedule begin
#         sleep(0.1)
#         try
# 			if !("NO_REVISE" in keys(ENV))
# 				# Distributed.@everywhere @eval using Revise
# 				@eval using Revise
# 				println("Loaded Revise")
# 			end
# 			@eval using Generic
# 			println("Loaded Generic")
# 			#@eval using Plots
# 			#println("Loaded Plots")
#         catch err
#             warn("Could not load Revise.")
#         end
#     end
# end

using Generic
