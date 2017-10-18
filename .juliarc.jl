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

