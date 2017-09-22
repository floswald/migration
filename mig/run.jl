# migration project run file

using mig
using DocOpt

function main()
    doc = """

        Usage:
           run.jl -h | --help 
           run.jl --test=<test>
           run.jl --estim=<estimation> [--nworkers=<nw>] [--maxiter=<maxit>]
           run.jl --exper=<exper> [--reg=<region>] [--year=<year>]

        Options:
            -h --help       Show this screen.
            --test=<test>   run test  
                            tests:
                                sol             Compute solution to DP
                                sim             Compute simulation
                                runtests        Run all unit tests
                                test_<name>.jl  Run unit tests in test/test_<name>.jl
            --estim=<estim>  run estimation estimation
                                estimations:
                                BGP         estimate model via BGP algorithm
                                slices      compute model slices along parameters
            --nworkers=<nw>    run estimation on num_workers [default: 1]
            --maxiter=<maxit>  run estimation for maxiter iterations [default: 500]
            --exper=<exper>    run experiment 
                                    experiments:
                                        highMC        Perform highMC experiment in <region>
                                        noMove        Shutdown moving everywhere      
                                        moneyMC       Monetize moving cost      
                                        ypshock       shock y and p in region in year
                                        pshock        shock p in region in year
                                        yshock        shock y in region in year
                                        decomp        decompose owner's moving cost
            --save=<true/false>  save the results? [default: false]
            --reg=<region>  run experiment in region region [default: 8]
            --year=<year>   run experiment in starting in year [default: 2000]
 
        """

    dir = dirname(@__FILE__)

    args = docopt(doc)
    for (k,v) in args
        if (v==nothing) | (v=="--help")
        else
            if k=="--test"
                Base.info("Running test: $v")
                if v=="sol"
                    mig.runSol()
                elseif v=="sim"
                    mig.runSim()
                elseif v=="runtests"
                    include(joinpath("dir","test","runtests.jl"))
                elseif contains(v,"test_")
                    include(joinpath("dir","test",v))
                end
            elseif k=="--estim"
                Base.info("Running estimation: $v")
                if v=="BGP"
                    # run estimation with BGP
                    mig.estimate(args["--maxiter"],args["--nworkers"])
                elseif v=="slices"
                    # run slices
                # which estim
                end
            elseif k=="--exper"
                Base.info("Running experiment: $v")
                save = parse(Bool,args["--save"])
                if save
                    Base.info("will save results to disk")
                else
                    Base.info("will NOT save results to disk")
                end
                reg = parse(Int,args["--reg"])
                if v=="highMC"
                    Base.info("in region: $reg")
                    e = mig.exp_value_mig_base(reg,save=true,ctax=false)   # true recomputes all cons taxes.
                if v=="noMove"
                    info("shutting down moving in all regions")
                    e = mig.exp_value_mig_base(0,save=true,ctax=false)   # true recomputes all cons taxes.
                elseif v=="moneyMC"
                    mig.moneyMC()
                elseif (v=="ypshock") | (v=="yshock") | (v=="pshock")
                    mig.runExperiment(v,reg,parse(Int,args["--year"]))
                elseif v=="decomp"
                    mig.decompose_MC_owners()
                end
            end
        end
    end

end

main()

