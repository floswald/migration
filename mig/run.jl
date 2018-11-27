doc = """

The Effect of Homeownership on the Option
Value of Regional Migration (Oswald, 2018)

    | Welcome to the run file of my paper. Please
    | see below how to run the code. 
    | Thanks,
    | Florian

Usage:
    run.jl -h | --help
    run.jl --version
    run.jl estim (bgp|slices) [--nworkers=<nw>] [--maxiter=<maxit>] 
    run.jl test 
    run.jl experiment (elasticity|ownersWTP|ownersWTP2|moneyMC|decomp) [--nworkers=<nw>] [--nosave] 
    run.jl experiment moversWTP [--nworkers=<nw>] [--nosave] [--region=<reg>] 
    run.jl experiment noMove [--yshock=<ys>] [--pshock=<ps>] [--nosave] [--nworkers=<nw>] 

Options:
    -h --help           Show this screen.
    --nworkers=<nw>     use <nw> of workers for task. [default: 1]
    --maxiter=<maxit>   max number of iterations in estimation [default: 500].
    --region=<reg>      in which region to run experiment [default: 1].
    --nosave            don't save experiment output. If you set it, it doesn't save. 
    --yshock=<ys>       shock applied to regional income [default: 1.0]
    --pshock=<ps>       shock applied to regional price [default: 1.0]
    --version           show version

"""

using DocOpt
args = docopt(doc, version=v"0.9.7")

if args["estim"]
    info("Running estimation: ")
    nwork = parse(Int,args["--nworkers"])
    maxit = parse(Int,args["--maxiter"])
    if args["bgp"]
        info("      BGP estimation algorithm on $nwork workers and for $maxit iterations.")
        addprocs(nwork)
        using mig
        mig.estimate(maxit,nwork)
    elseif args["slices"]
        info("      compute slices on $nwork workers.")
        using mig
        mig.slices(nwork)
    end
elseif args["test"]
    info("Running tests")
elseif args["experiment"]
    info("Running experiments:")
    nosave = args["--nosave"]
    nwork = parse(Int,args["--nworkers"])
    reg = parse(Int,args["--region"])
    _ys = parse(Float64,args["--yshock"])
    _ps = parse(Float64,args["--pshock"])
    if args["noMove"]
        addprocs(nwork)
        using mig
        info("      noMove experiment, with nosave=$nosave")
        info("      applying ys=$_ys, ps=$_ps")
        mig.exp_Nomove(do_ctax=true,save=!nosave,ys=_ys,ps=_ps)
    elseif args["moneyMC"]
        info("      monetize the moving costs, with nosave=$nosave")
        using mig
        mig.moneyMC(nosave)
    elseif args["ownersWTP"]
        info("      computing owners WTP to become renter again, with nosave=$nosave")
        addprocs(nwork)
        using mig
        mig.ownersWTP(nosave)
    elseif args["ownersWTP2"]
        info("      computing owners WTP v2 to become renter again, with nosave=$nosave")
        addprocs(nwork)
        using mig
        mig.ownersWTP2(nosave)
    elseif args["moversWTP"]
        info("      computing movers WTP in region $reg with nosave=$nosave")
        addprocs(nwork)
        using mig
        mig.moversWTP(reg,nosave)
    elseif args["elasticity"]
        info("      computing elasticity wrt 10% income shock, with nosave=$nosave")
        addprocs(nwork)
        using mig
        mig.elasticity(nosave)
    elseif args["decomp"]
        info("      decompose the moving costs, with nosave=$nosave")
        using mig
        mig.decompose_MC_owners(nosave)
    end

end