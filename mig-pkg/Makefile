

CMDinstall:
	R CMD INSTALL --no-multiarch ../mig-pkg

install:
	Rscript -e "library(methods);library(devtools);document('.',roclets=c('collate','rd'));install('.')"

sipp.stats:
	Rscript -e "source('examples/sipp-sumstats.r')"

test:
	Rscript -e "library(testthat); test_dir('tests/testthat/')"

logit30:
	Rscript -e "library(migration); example(runMNLogit)"

buildLogit:
	Rscript -e "library(migration); example(buildLogit)"

plotHPI:
	Rscript -e "library(migration); example(plot.FHFAindex); example(plot.LincolnHomeValues)"
