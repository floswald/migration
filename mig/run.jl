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
    --nosave            don't save experiment output. If you set it, it doesn't save. 
    --yshock=<ys>       shock applied to regional income [default: 1.0]
    --pshock=<ps>       shock applied to regional price [default: 1.0]
    --on_impact         measure experiment in year of impact only
    --version           show version

"""
using DocOpt
args = docopt(doc, version=v"0.9.5")

cumulus = vcat("10.20.35.11",
"10.20.35.21",
"10.20.35.26",
"10.20.35.27",
"10.20.35.30",
"10.20.35.31",
"10.20.35.32",
"10.20.35.33",
"10.20.35.35",
"10.20.35.36")


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
                if nwork > 10
                    error("only 10 workers on cumulus")
                end
                addprocs([cumulus[i] for i in 1:nwork])
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
                if nwork > 10
                    error("only 10 workers on cumulus")
                end
                addprocs([cumulus[i] for i in 1:nwork])
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
        using mig
        mig.decompose_MC_owners()
    end

end