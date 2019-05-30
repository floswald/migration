doc = """

The Effect of Homeownership on the Option
Value of Regional Migration (Oswald, 2019)

    | Welcome to the run file of my paper. Please
    | see below how to run the code. 
    | Thanks,
    | Florian

Usage:
    run.jl -h | --help
    run.jl --version
    run.jl estim (bgp|grad|slices|stderrors|thomas) [--nworkers=<nw>] [--maxiter=<maxit>][--npoints=<npts>]  
    run.jl test 
    run.jl experiment (elasticity|ownersWTP|ownersWTP2|moneyMC|decomp) [--nworkers=<nw>] [--shock=<sh>] [--nosave]  [--neg]
    run.jl experiment moversWTP [--nworkers=<nw>] [--nosave] [--region=<reg>] 
    run.jl experiment noMove [--yshock=<ys>] [--pshock=<ps>] [--nosave] [--nworkers=<nw>] 
    run.jl experiment scenarios [--nworkers=<nw>] [--nosave] 

Options:
    -h --help           Show this screen.
    --nworkers=<nw>     use <nw> of workers for task. [default: 1]
    --npoints=<npts>    number of points to use [default: 10].
    --maxiter=<maxit>   max number of iterations in estimation [default: 500].
    --region=<reg>      in which region to run experiment [default: 1].
    --nosave            don't save experiment output. If you set it, it doesn't save. 
    --neg               perform negative elasticity shock. If you set it, it does negative.
    --shock=<sh>        type of shock to apply [default: q]
    --yshock=<ys>       shock applied to regional income [default: 1.0]
    --pshock=<ps>       shock applied to regional price [default: 1.0]
    --version           show version

"""

using DocOpt
args = docopt(doc, version=v"1.1")

addprocs1(n) = n>1 ? addprocs(n) : nothing

if args["estim"]
    info("Running estimation: ")
    nwork = parse(Int,args["--nworkers"])
    npoints = parse(Int,args["--npoints"])
    maxit = parse(Int,args["--maxiter"])
    if args["bgp"]
        info("      BGP estimation algorithm on $nwork workers and for $maxit iterations.")
        addprocs1(nwork)
        using mig
        mig.estimate(maxit)
    elseif args["grad"]
        info("      grad descent estimation algorithm on $nwork workers")
        addprocs1(nwork)
        using mig
        # mig.estimate(maxit,npoints=npoints,method=:grad,keep=["eta","MC0","xi1","xi2"])
        mig.estimate(maxit,npoints=npoints,method=:grad)
    elseif args["stderrors"]
        info("      computing std errors with $nwork workers")
        addprocs1(nwork)
        using mig
        # mig.estimate(maxit,npoints=npoints,method=:grad,keep=["eta","MC0","xi1","xi2"])
        mig.stdErrors()
    elseif args["thomas"]
        info("      computing mom func gradients with $nwork workers")
        addprocs1(nwork)
        using mig
        # mig.estimate(maxit,npoints=npoints,method=:grad,keep=["eta","MC0","xi1","xi2"])
        mig.gradMoments()
        info("      computing ATE gradients with $nwork workers")
        mig.gradNoMoveATE()
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
    neg = args["--neg"]
    shock = args["--shock"]
    nwork = parse(Int,args["--nworkers"])
    reg = parse(Int,args["--region"])
    _ys = parse(Float64,args["--yshock"])
    _ps = parse(Float64,args["--pshock"])
    if args["noMove"]
        if gethostname()=="magi3"
            using ParallelTest
            wrkers = ParallelTest.machines()  # does addprocs
        elseif contains(gethostname(),"ip-")  # on aws
            machine_ip = readlines(`qconf -sel`)
            mach_spec = [(i,1) for i in machine_ip]
            addprocs(mach_spec)
        else
            addprocs1(nwork)
        end
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
        addprocs1(nwork)
        using mig
        mig.ownersWTP(nosave)
    elseif args["ownersWTP2"]
        info("      computing owners WTP v2 to become renter again, with nosave=$nosave")
        if gethostname()=="magi3"
            using ParallelTest
            wrkers = ParallelTest.machines()  # does addprocs
        elseif contains(gethostname(),"ip-")  # on aws
            machine_ip = readlines(`qconf -sel`)
            mach_spec = [(i,1) for i in machine_ip]
            addprocs(mach_spec)
        else
            addprocs1(nwork)
        end
        using mig
        mig.ownersWTP2(nosave)
    elseif args["moversWTP"]
        info("      computing movers WTP in region $reg with nosave=$nosave")
        addprocs1(nwork)
        using mig
        mig.moversWTP(reg,nosave)
    elseif args["elasticity"]
        info("      computing elasticity wrt 10% $shock shock, with nosave=$nosave, neg=$neg")
        if gethostname()=="magi3"
            using ParallelTest
            wrkers = ParallelTest.machines()  # does addprocs
        elseif contains(gethostname(),"ip-")  # on aws
            machine_ip = readlines(`qconf -sel`)
            mach_spec = [(i,1) for i in machine_ip]
            addprocs(mach_spec)
        else
            addprocs1(nwork)
        end
        using mig
        mig.elasticity(shock=shock,nosave=nosave,neg=neg)
    elseif args["decomp"]
        info("      decompose the moving costs, with nosave=$nosave")
        using mig
        mig.decompose_MC_owners(nosave)
    elseif args["scenarios"]
        if gethostname()=="magi3"
            using ParallelTest
            wrkers = ParallelTest.machines()  # does addprocs
        elseif contains(gethostname(),"ip-")  # on aws
            machine_ip = readlines(`qconf -sel`)
            mach_spec = [(i,1) for i in machine_ip]
            addprocs(mach_spec)
        else
            addprocs1(nwork)
        end
        using mig
        info("      running shockRegions_scenarios nosave=$nosave")
        mig.shockRegions_scenarios(save=!(nosave),qrange=0.05,prange=0.05)
    end

end