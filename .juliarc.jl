@everywhere push!(LOAD_PATH, "/home/pengwyn/work5/julia")

if true
	@everywhere push!(LOAD_PATH, ".")

    #ENV["PLOTS_DEFAULT_BACKEND"] = "PlotlyJS"
    ENV["PLOTS_DEFAULT_BACKEND"] = "PyPlot"
end


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
						"JLD",
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
						"EllipsisNotation"
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


atreplinit() do REPL
    @schedule begin
        sleep(0.1)
        try
			if !("NO_REVISE" in keys(ENV))
				@eval using Revise
				println("Loaded Revise")
			end
			@eval using Generic
			println("Loaded Generic")
			#@eval using Plots
			#println("Loaded Plots")
        catch err
            warn("Could not load Revise.")
        end
    end
end
