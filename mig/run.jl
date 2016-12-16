# migration project run file

using mig
using DocOpt

function main()
    doc = """

        Usage:
           run.jl -h | --help 
           run.jl [--test=<test>]
           run.jl [--estim=<estimation>]
           run.jl [--exper=<experiment>] [--reg=<region>] [--year=<year>]

        Options:
            -h --help               Show this screen.
            --test=<test>           run test <test> [default: sol].
                                    <test>:
                                        sol             Compute solution to DP
                                        sim             Compute simulation
                                        runtests        Run all unit tests
                                        test_<name>.jl  Run unit tests in test/test_<name>.jl

            --estim=<estim>         run estimation <estimation> [default: BGP]
                                    <estimation>
                                        BGP         estimate model via BGP algorithm
                                        slices      compute model slices along parameters

            --exper=<experiment>    run experiment <experiment> [default: noMove]
                                    <experiment>:
                                        noMove        Perform noMove experiment      
                                        moneyMC       Monetize moving cost      
                                        ypshock       shock y and p in <region> in <year>
                                        pshock        shock p in <region> in <year>
                                        yshock        shock y in <region> in <year>

            --reg=<region>          run <experiment> in region <region> [default: 8]
            --year=<year>           run <experiment> in starting in year <year> [default: 2000]
 
        """

    args = docopt(doc)

    # parse args

