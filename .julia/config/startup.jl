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
        @eval using Revise
        @async Revise.wait_steal_repl_backend()
    catch
    end
end

atreplinit() do repl
    @eval using DanUtils
end
    

