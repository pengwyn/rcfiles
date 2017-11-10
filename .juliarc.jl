@everywhere push!(LOAD_PATH, "/home/pengwyn/work5/julia")

if true #isinteractive()
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
	@everywhere push!(LOAD_PATH, ".")
	@everywhere using Generic
	#println("Before plots")
	#@eval using Plots
	#println("After plots")
end


for pkg in ["Glob", "PyPlot", "PlotlyJS", "Plots", "StaticArrays", "DataFrames", "JLD", "LsqFit", "QuadGK", "MAT", "DifferentialEquations"]
    if Pkg.installed(pkg) == nothing
        print_with_color(:green, "Install package $pkg:\n", bold=true)
        Pkg.add(pkg)
        Pkg.build(pkg)
    end
end
