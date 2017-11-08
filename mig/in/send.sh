
all: sherlock cs 

sherlock: tarup
	scp data.tar.gz iridis:~/data_repo/mig/in_data_jl

cs: tarup
	scp data.tar.gz cs:~/data_repo/mig/in_data_jl

tarup: clean
	tar -zcvf data.tar.gz *.rda *.csv

clean:
	rm -rf data.tar.gz
