import Distributed

if Distributed.nprocs() == 1
	pushfirst!(LOAD_PATH, "/home/pengwyn/work5/julia")
	#pushfirst!(LOAD_PATH, "/home/pengwyn/work5/julia_packages")
	pushfirst!(LOAD_PATH, ".")
else
	@eval Distributed.@everywhere pushfirst!(LOAD_PATH, "/home/pengwyn/work5/julia")
	#@eval Distributed.@everywhere pushfirst!(LOAD_PATH, "/home/pengwyn/work5/julia_packages")
	@eval Distributed.@everywhere pushfirst!(LOAD_PATH, ".")
end

atreplinit() do repl
    try
		@async begin
			Revise.wait_steal_repl_backend()
		end
    catch exc
		@error "Exception in startup.jl" exc
    end
end

    
try
	@time using Revise
	@time using OhMyREPL
	@time using Constants
	@time using DanUtils
catch exc
	@error "Exception loading modules" exc
end
