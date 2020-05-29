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
    OhMyREPL.enable_autocomplete_brackets(false)
	@time using Constants
	@time using DanUtils
    @time using Debugger
    @time using Plots
	@time using UnitfulRecipes
catch exc
	@error "Exception loading modules" exc
end

try
    using PkgTemplates
    pkg_template = Template(dir="~/work5/julia_packages",
                            plugins=[Git(ssh=true), TravisCI(), Documenter{TravisCI}()])

catch exc
    @error "Error in PkgTemplates"
end
