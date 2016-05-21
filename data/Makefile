
# send data to remote computers
# puts data into a folder ~/data_repo/mig/in_data_jl

dbi = ~/Dropbox/research/mobility/output/model/data_repo/in_data_jl
db_remote = ~/data_repo/mig/in_data_jl

setup_all: setup_cs setup_sherlock

setup_cs: put_cs
	ssh cs "cd ${db_remote} && tar -xzf data.tar.gz"

setup_ucl: put_ucl
	ssh ucl "cd ${db_remote} && tar -xzf data.tar.gz"

setup_sherlock: put_sherlock
	ssh cs "cd ${db_remote} && tar -xzf data.tar.gz"

put: put_sherlock put_cs put_ucl

put_sherlock: tardat
	cd ${dbi}; scp data.tar.gz sherlock:${db_remote}

put_cs: cleancs tardat
	cd ${dbi}; scp data.tar.gz cs:${db_remote}

put_ucl: cleanucl tardat
	cd ${dbi}; scp data.tar.gz ucl:${db_remote}
	 
tardat: cleanhere
	cd ${dbi}; tar -zcvf data.tar.gz *.rda *.csv

cleanhere:
	cd ${dbi}; rm -rf data.tar.gz

cleancs:
	ssh cs "cd ${db_remote} && rm -rf *"

cleanucl:
	ssh ucl "cd ${db_remote} && rm -rf *"

cleandat: cleanhere cleancs cleanucl


