@everywhere push!(LOAD_PATH, "/home/pengwyn/work5/julia")

if false #isinteractive()
	@schedule begin
		if @eval isinteractive()
			sleep(0.1)
			#println("At begin")
			@eval using Revise
			#println("At mid")
			#@everywhere using Plots
			#println("At end")
		end
	end
end

if true
	@everywhere push!(LOAD_PATH, ".")
	@everywhere using Generic

    ENV["PLOTS_DEFAULT_BACKEND"] = "PlotlyJS"
end


if true && !("NO_JULIA_PACKAGE_CHECK" in keys(ENV))
	withenv("NO_JULIA_PACKAGE_CHECK" => "STOP") do
		firsttime = true
		for pkg in ["Glob",
					"PyPlot",
					"PlotlyJS",
					"GR",
					"Plots",
					"StaticArrays",
					"DataFrames",
					"JLD",
					"LsqFit",
					"QuadGK",
					"MAT",
					"DifferentialEquations",
					"SpecialFunctions",
					"IJulia",
					"Roots"]
			if Pkg.installed(pkg) == nothing
				if firsttime
					print_with_color(:green, "Updating package database before installing $pkg:\n", bold=true)
					Pkg.update()
					println("About to change firstime to false: $(getpid())")
					firsttime = false
				end
				print_with_color(:green, "Install package $pkg:\n", bold=true)
				Pkg.add(pkg)
				Pkg.build(pkg)
			end
		end
	end
end
