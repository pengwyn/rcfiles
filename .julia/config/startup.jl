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
        @time @eval using Revise
		@time @eval using Constants
		@time @eval using DanUtils
		@async begin
			Revise.wait_steal_repl_backend()
		end
    catch
    end
end

    

