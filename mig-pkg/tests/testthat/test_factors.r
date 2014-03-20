



# transformations

context("test transformations")

test_that("ztransform throws error",{

		  expect_error( z2y.transform(x,high=1,low=3)  )

})

test_that("ztransform bounds original values",{

		  x <- runif(n=100,min=-2,20)
		  hi=1
		  lo <- -1
		  z <- z2y.transform(x,hi,lo)

		  expect_that( min(z) >= -2 ,is_true() )
		  expect_that( max(z) <=  20 ,is_true() )

})

test_that("ztransform maps into correct range",{

		  x <- runif(n=100,min=-2,20)
		  hi=1
		  lo <- -1
		  z <- z2y.transform(x,hi,lo)

		  expect_that( min(z) >= lo ,is_true() )
		  expect_that( max(z) <=  hi ,is_true() )

})


test_that("inverse of ztransform returns initial values",{


		  x <- runif(n=100,min=-2,20)
		  hi=1
		  lo <- -1
		  z <- z2y.transform(x,hi,lo)

		  y <- y2z.transform(z,hi,lo)

		  expect_that( all.equal(y,x) ,is_true() )

})


context("test inverse difference function")

test_that("inverse difference is correct",{


		  x <- runif(n=10)
		  dx <- diff(x)

		  expect_that( idiff(x0=5,dx)[1], equals(5) )

		  expect_that( all.equal(idiff(x0=x[1],dx), x), is_true() )

})


context("test percentage growth function")

test_that("percentage growth is correct",{


		  x <- runif(n=10)
		  dx <- diff(x) / x[-length(x)]

		  expect_that( ipercent(x0=5,dx)[1], equals(5) )
		  expect_that( ipercent(x0=5,dx)[2], equals(5*(1+dx[1])) )

})

