import Distributed

if Distributed.nprocs() == 1
	pushfirst!(LOAD_PATH, "/home/pengwyn/work5/julia")
	pushfirst!(LOAD_PATH, "/home/pengwyn/work5/julia/packages")
	pushfirst!(LOAD_PATH, ".")
else
	@eval Distributed.@everywhere pushfirst!(LOAD_PATH, "/home/pengwyn/work5/julia")
	@eval Distributed.@everywhere pushfirst!(LOAD_PATH, "/home/pengwyn/work5/julia/packages")
	@eval Distributed.@everywhere pushfirst!(LOAD_PATH, ".")
end

atreplinit() do repl
    try
        @time @eval using Revise
		@time @eval using Constants
		@async begin
			Revise.wait_steal_repl_backend()
			@time @eval using DanUtils
		end
    catch
    end
end

    

