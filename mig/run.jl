doc = """

Homeownership, Regional Shocks and Migration (Oswald, 2017)

    | Welcome to the run file of my paper. Please
    | see below how to run the code. 
    | Thanks,
    | Florian

Usage:
    run.jl -h | --help
    run.jl --version
    run.jl estim (BGP|slices) [--nworkers=<nw>] [--maxiter=<maxit>] [--cluster=<c>]
    run.jl test 
    run.jl experiment noMove [--yshock=<ys>] [--pshock=<ps>] [--nosave] [--nworkers=<nw>] [--cluster=<c>]
    run.jl experiment shockRegion [--nosave] [--on_impact] [--nworkers=<nw>] [--cluster=<c>]
    run.jl experiment (moneyMC|decomp) [--nosave]

Options:
    -h --help           Show this screen.
    --nworkers=<nw>     use <nw> of workers for task. [default: 1]
    --cluster=<c>       name of cluster to use [default: cumulus]
    --maxiter=<maxit>   max number of iterations in estimation [default: 500].
    --nosave            don't save experiment output
    --yshock=<ys>       shock applied to regional income [default: 1.0]
    --pshock=<ps>       shock applied to regional price [default: 1.0]
    --on_impact         measure experiment in year of impact only
    --version           show version

"""
using DocOpt
args = docopt(doc, version=v"0.9")


if args["estim"]
    using mig
    info("Running estimation: ")
    nwork = parse(Int,args["--nworkers"])
    maxit = parse(Int,args["--maxit"])
    if args["BGP"]
        info("      BGP estimation algorithm on $nwork workers and for $maxit iterations.")
        mig.estimate(maxit,nwork)
    elseif args["slices"]
        info("      compute slices on $nwork workers.")
        mig.slices(nwork)
    end
elseif args["test"]
    info("Running tests")
elseif args["experiment"]
    info("Running experiments:")
    nosave = args["--nosave"]
    on_impact = args["--on_impact"]
    nwork = parse(Int,args["--nworkers"])
    _ys = parse(Float64,args["--yshock"])
    _ps = parse(Float64,args["--pshock"])
    if args["noMove"]
        if nwork > 1
            if args["--cluster"]=="cumulus"
                addprocs([("vm$i-8core",round(Int,nwork/7)) for i in 3:10])
            elseif args["--cluster"]=="local"
                addprocs(nwork)
            end
        end
        using mig
        info("      noMove experiment, with nosave=$nosave")
        info("      applying ys=$_ys, ps=$_ps")
        mig.exp_Nomove(do_ctax=true,save=!nosave,ys=_ys,ps=_ps)
    elseif args["shockRegion"]
        if nwork > 1
            if args["--cluster"]=="cumulus"
                addprocs([("vm$i-8core",round(Int,nwork/7)) for i in 3:10])
            elseif args["--cluster"]=="local"
                addprocs(nwork)
            end
        end
        using mig
        info("      shockRegion experiment, with nosave=$nosave, on_impact=$on_impact, on $(length(workers())) cores")
        @time mig.shockRegions_scenarios(on_impact,save=!nosave)
    elseif args["moneyMC"]
        info("      monetize the moving costs, with nosave=$nosave")
    elseif args["decomp"]
        info("      decompose the moving costs, with nosave=$nosave")
    end

end