


using mig

which = string(ARGS[1])
region = parse(Int,ARGS[2])
year = parse(Int,ARGS[3])
revert = parse(Int,ARGS[4])

e = runExperiment(which,region,year,revert)



