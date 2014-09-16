module test_accelerator


using mig
using FactCheck


facts("testing searchsorted1step") do

	n = 11
	y = iround(linspace(1,11,n))
	x = 8.9
	i = 1
	@fact mig.searchsorted1step(y,x,i,n) => 8

	n = 13
	y = linspace(1,11,n)
	x = 0.9
	i = 4
	@fact mig.searchsorted1step(y,x,i,n) => 3

	n = 15
	y = linspace(1,15,n)
	x = 1.999
	i = 1
	@fact mig.searchsorted1step(y,x,i,n) => 1

	x = 2.999
	@fact mig.searchsorted1step(y,x,i,n) => 2
	x = 8.999
	@fact mig.searchsorted1step(y,x,i,n) => 8

end


facts("testing linearapprox with accelerator and 1 step bracket search") do

acc = mig.Accelerator(1)
n = 18
x = linspace(1,18,n)
y = 2.29 .* x

@fact mig.linearapprox(x,y,0.0,n,acc) => 0.0
@fact mig.linearapprox(x,y,100.0,n,acc) => roughly(100*2.29)

@fact mig.linearapprox(x,y,1.9,n,acc) => 2.29 * 1.9
@fact acc.i => 1

@fact mig.linearapprox(x,y,8.9,n,acc) => 2.29 * 8.9
@fact acc.i => 8

@fact mig.linearapprox(x,y,13.1,n,acc) => 2.29 * 13.1
@fact acc.i => 13

@fact mig.linearapprox(x,y,17.1,n,acc) => roughly(2.29 * 17.1)
@fact acc.i => 17
@fact mig.linearapprox(x,y,18.1,n,acc) => roughly(2.29 * 18.1)
@fact acc.i => 17

end
end