

# master makefile
fit = ~/Dropbox/research/mobility/output/model/fit
out = ~/Dropbox/research/mobility/output/model/data_repo/out_data_jl

ucl-slices:
	ssh ucl "julia ~/git/migration/mig/src/cluster/ucl-econ_slices.jl"

plot-moms: ${fit}/moms.json
	Rscript -e "library(migration);plot_moment_fit()"

${fit}/moms.json: 
	julia -e "using mig; runObj(true,false)"

obj:
	julia -e "using mig; runObj()"

plot-elas: $(out)/shockReg/exp_region8_ypshock.jld
	julia -e 'using mig; mig.plotShockRegion("ypshock",8)'

estim:
	julia5 --depwarn=no ~/git/migration/mig/src/cluster/examples/estimation.jl 500 30

estim-test:
	julia5 --depwarn=no ~/git/migration/mig/src/cluster/examples/estimation.jl 4 3
