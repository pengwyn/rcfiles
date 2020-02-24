import IJulia

ijulia_kernel_file = joinpath(dirname(pathof(IJulia)), "kernel.jl")
include(ijulia_kernel_file)
