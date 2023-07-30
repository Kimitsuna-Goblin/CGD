#### This file is for testing CGD$nls.freq and CGD$nls.freq.all.

library( "cgd" )
library( "testthat" )

a<-CGD$new()
dev.new(); plot.new()

diff.check <- function( a, x, freq, total )
{
	diff <- sqrt( ( a$d( x ) - cgd::get.d( x, freq, total ) )^2 )
	summary <- c( min( diff ), max( diff ), mean( diff ), sd( diff ), cor( a$d( x ), cgd::get.d( x, freq, total ) ) )
	names( summary ) <- c( "diff.min", "diff.max", "diff.mean", "diff.sd", "cor" )

	list( diff = diff, summary = summary )
}

plot.freq.and.d <- function( a, x, freq, total )
{
	xlim <- c( min( x ), max( x ) )
	ylim <- c( 0, max( cgd::get.d( x, freq, total ) ) * 1.2 )
	plot( x, cgd::get.d( x, freq, total ), xlim = xlim, ylim = ylim, ylab = "" )
	par( new = T )
	plot( seq( min( x ), max( x ), 0.01 ), a$d( seq( min( x ), max( x ), 0.01 ) ), type = "l", xlim = xlim, ylim = ylim, xlab = "" )
}

#### normal distribution base
# normal test
x <- seq( -2, 2, 0.2 )
freq <- ( pnorm( x + 0.1, 0, 0.5 ) - pnorm( x - 0.1, 0, 0.5 ) ) * ( 1000 + sin( x * 10 + 0.5 ) * 50 )
total <- sum( freq )

a$nls.freq( x, freq, total, normal = TRUE )
expect_equal( a$type1.type, 1 )
diff.check( a, x, freq, total )

a <- nls.freq( x, freq, total, normal = TRUE )
expect_equal( a$type1.type, 2 )
diff.check( a, x, freq, total )

########
## This function should be run after each non-error test to confirm the result.
show.results <- function( a, x, freq, total )
{
	print( a )
	plot.freq.and.d( a, x, freq, total )
}

## Instead of the above function, you can each of these commands.
a
plot.freq.and.d( a, x, freq, total )
########

# Error case
expect_error( a$nls.freq( x[1:length( x ) - 1], freq, total, normal = TRUE ), "The lengths of x" )
expect_error( a$nls.freq( x, freq[1:length( freq ) - 1], total, normal = TRUE ), "The lengths of x" )
expect_error( a$nls.freq( x, freq, floor( sum( freq ) ), normal = TRUE ), "total is smaller" )
expect_error( a$nls.freq( x[1:2], freq[1:2], floor( sum( freq[1:2] ) ), normal = TRUE ), "The lengths of x" )
expect_error( a$nls.freq( -x, -freq, floor( sum( freq ) ), normal = TRUE ), "x must have been sorted" )
expect_error( a$nls.freq( x, freq, total, this.type1.type = 0 ), "0 is not allowed for nls.freq" )
expect_error( a$nls.freq( x, freq, total, this.type1.type = 5 ), "5 is not allowed for nls.freq" )
expect_error( a$nls.freq( x, freq, total, start = list( mean = -10, sqrt.sd = 10 ), normal = TRUE ), "nls has failed." )

# normal test
a$nls.freq( x, freq, total, normal = TRUE, this.type1.type = 1, trace = TRUE )

# Error case
freq <- floor( dnorm( x, 0, 0.5 ) * 1000 )
total <- sum( freq ) * 1.1
expect_error( a$nls.freq( x, freq, total, normal = TRUE ), "nls has failed." )

# normal test
a$nls.freq( x, freq, total, normal = TRUE, set.by.start = TRUE )
expect_equal( a$kind, "Normal Distribution" )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

# normal test
freq <- dnorm( x, 0, 0.5 ) * 1000 + sin( x + 0.05 ) * 10
total <- sum( freq ) * 1.1
a$nls.freq( x, freq, total, normal = TRUE, this.type1.type = 3 )
expect_equal( a$type1.type, 1 )
expect_equal( a$kind, "Normal Distribution" )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

# normal test
a$set.waypoints(
	data.frame( p = c( 0.3, 0.5, 0.7 ), q = c( qnorm( 0.3, -0.2, 1 ), 0, qnorm( 0.7, 0.2, 0.8 ) ) ),
	this.type1.type = 2, continuous = TRUE )
a$nls.freq( x, freq, total, normal = TRUE )
expect_equal( a$type1.type, 2 )
expect_equal( a$kind, "Normal Distribution" )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

# normal test
a$set.waypoints(
	data.frame( p = c( 0.1, 0.4, 0.5 ), q = c( qnorm( 0.1, 0, 1.2 ), qnorm( 0.4, 0, 0.9 ), 0 ) ),
	this.type1.type = 3 )
a$nls.freq( x, freq, total, normal = TRUE )
expect_equal( a$type1.type, 1 )
expect_equal( a$kind, "Normal Distribution" )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )


#### sharp center base
x <- seq( -2, 2, 0.2 )
seed<-CGD$new()
seed$set.waypoints(
	data.frame( p = c( 0.3, 0.5, 0.9 ), q = c( qnorm( 0.3, -0.2, 0.7 ), -0.2, qnorm( 0.9, -0.2, 1 ) ) ),
	this.type1.type = 2, symmetric = TRUE )
freq <- ( seed$p( x + 0.1 ) - seed$p( x - 0.1 ) ) * ( 1000 + sin( x * 10 + 0.5 ) * 100 )
total <- sum( freq )
plot( x, cgd::get.d( x, freq, total ) )

# Error case
expect_error( a$nls.freq( x, freq, total, symmetric = TRUE ), "nls has failed." )

# Warning case
expect_warning( a$nls.freq( x, freq, total, control = list( warnOnly = TRUE ), symmetric = TRUE ), "minFactor" )
a$type1.type == 2 # TRUE
a$is.symmetric() # TRUE
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, normal = TRUE )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, this.type1.type = 1 )
expect_equal( a$type1.type, 1 )
expect_equal( a$is.symmetric(), TRUE )
expect_equal( a$kind.index > 1, TRUE )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, set.by.start = TRUE, symmetric = TRUE )
expect_equal( a$type1.type, 2 )
expect_equal( a$is.symmetric(), TRUE )
expect_match( a$kind, "Horizontal Gradational Distribution" )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, v.grad = TRUE )
expect_equal( a$type1.type, 3 )
expect_equal( a$is.v.grad(), TRUE )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, uni.sigma = TRUE )
expect_equal( a$type1.type, 3 )
expect_equal( a$is.v.grad(), FALSE )
expect_equal( a$is.uni.sigma(), TRUE )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

#### nls.freq.all

# normal test
expect_equal( cgd.kind( 1:16 ), cgd:::kinds )
expect_equal( cgd.kind.index( 1:16 ), 1:16 )
expect_equal( cgd.kind.index( c( -1, 0, 1, 2, NA, Inf, NaN, 3, 4, 5, 15, 16, 17 ) ),
			  c( NA, NA, 1, 2, NA, NA, NA, 3, 4, 5, 15, 16, NA ) )
expect_equal( cgd.kind.index( c( NA, "Normal Distribution", "Null Distribution" ) ), c( NA, 1 ) )

# normal test (with 4 times warnings)
expect_warning( expect_warning( expect_warning( expect_warning(
				cgds <- nls.freq.all( x, freq, total ) ) ) ) )
cgds
cgds$cgd
expect_equal( cgds$best$kind.index, 15 )
expect_equal( cgds$best.cor > 0.996, TRUE )
cgds$cor
for ( i in 1:length( cgds$cgd ) ) print( paste( i, ":", cgds$cgd[[i]]$kind ) )

plot.freq.and.d( cgds$best, x, freq, total )

# normal test
expect_equal( cgd.kind( cgds$cgd ), cgd:::kinds[1:15] )
expect_equal( cgd.kind.index( cgds$cgd ), 1:15 )
expect_equal( cgd.kind( cgds$cgd[[1]] ), cgds$cgd[[1]]$kind )
expect_equal( cgd.kind.index( cgds$cgd[[1]] ), cgds$cgd[[1]]$kind.index )

## nls.start.template (a revenge of cgds$cgd[[13]])
expect_equal( unname( diff.check( cgds$cgd[[10]], x, freq, total )$summary["cor"] ), 0.9945, tolerance = 1e-4 )
expect_equal( unname( diff.check( cgds$cgd[[13]], x, freq, total )$summary["cor"] ), 0.8879, tolerance = 1e-4 )
expect_equal( unname( diff.check( cgds$cgd[[13]], x, freq, total )$summary["cor"] <
					  diff.check( cgds$cgd[[10]], x, freq, total )$summary["cor"] ), TRUE )

expect_equal( cgds$cgd[[10]]$kind, "3-Median-Differed-Sigma-Equaled Vertical Gradational Distribution" )
expect_equal( cgds$cgd[[13]]$kind, "Median-Differed-Sigma-Equaled Vertical-Horizontal Gradational Distribution" )

expect_equal( cgds$cgd[[10]]$intervals.mean(), c( -0.6709839, -0.1977604, 0.2928316 ), tolerance = 1e-5 )
expect_equal( cgds$cgd[[10]]$intervals.sd(), c( 0.6400899, 0.6400899, 0.6400899 ), tolerance = 1e-5 )

start.list <- nls.start.template( cgds$cgd[[13]]$kind )
start.list

start.list$mean.1.1 <- -0.6709839
start.list$mean.1.2 <- -0.1977604
start.list$mean.2.1 <- 0.2928316
start.list$mean.2.2 <- -0.1977604
start.list$sqrt.sd <- sqrt( 0.64 ) # sqrt( 0.64 ) == 0.8
start.list

a$nls.freq( x, freq, total, start = start.list, kind = cgds$cgd[[13]] )
expect_equal( a$kind, cgds$cgd[[13]]$kind )
show.results( a, x, freq, total )
expect_equal( unname( diff.check( a, x, freq, total )$summary[5]
					  > diff.check( cgds$cgd[[10]], x, freq, total )$summary[5] ), TRUE )
# a revenge is done.

# then retrying nls.freq.all
start.lists <- init.start.lists()
expect_equal( length( start.lists ), 15 )
start.lists # All elements are NULL.

start.lists[[13]] <- nls.start.template( cgds$cgd[[13]]$kind )

start.lists[[13]]$mean.1.1 <- -0.6709839
start.lists[[13]]$mean.1.2 <- -0.1977604
start.lists[[13]]$mean.2.1 <- 0.2928316
start.lists[[13]]$mean.2.2 <- -0.1977604
start.lists[[13]]$sqrt.sd <- sqrt( 0.64 ) # sqrt( 0.64 ) == 0.8
start.lists

# (3 times warnings)
expect_warning( expect_warning( expect_warning(
	cgds <- nls.freq.all( x, freq, total, start.lists ) ) ) )
cgds
cgds$cgd
expect_equal( cgds$best$kind.index, 15 )
expect_equal( cgds$best.cor, 0.9964, tolerance = 1e-4 )
cgds$cor
expect_equal( cgds$cor[10], 0.9945, tolerance = 1e-4 )
expect_equal( cgds$cor[13], 0.9956, tolerance = 1e-4 )

#### dent center base
seed$set.waypoints(
data.frame( p = c( 0.6, 0.9, 0.5 ), q = c( qnorm( 0.6, 0, 1.2 ), qnorm( 0.9, 0, 1.05 ), 0 ) ),
this.type1.type = 2, symmetric = TRUE )
freq <- ( seed$p( x + 0.1 ) - seed$p( x - 0.1 ) ) * ( 1000 + sin( x * 10 + 0.5 ) * 100 )
total <- sum( freq )


## set.by.start
expect_equal( a$nls.freq( x, freq, total, set.by.start = TRUE, normal = TRUE ), NULL )
expect_equal( a$kind, "Normal Distribution" )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

expect_equal( a$nls.freq( x, freq, total, set.by.start = TRUE, this.type1.type = 1 ), NULL )
expect_equal( a$kind.index, 2 )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

expect_equal( a$nls.freq( x, freq, total, set.by.start = TRUE, symmetric = TRUE ), NULL )
expect_equal( a$kind.index, 3 )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

expect_equal( a$nls.freq( x, freq, total, set.by.start = TRUE, this.type1.type = 1, uni.sigma = TRUE ), NULL )
expect_equal( a$type1.type, 1 )
expect_equal( a$is.uni.sigma(), FALSE )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

expect_equal( a$nls.freq( x, freq, total, set.by.start = TRUE, this.type1.type = 2 ), NULL )
expect_equal( a$type1.type, 2 )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

expect_equal( a$nls.freq( x, freq, total, set.by.start = TRUE, uni.sigma = TRUE ), NULL )
expect_equal( a$type1.type, 2 )
expect_equal( a$is.uni.sigma(), TRUE )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

expect_equal( a$nls.freq( x, freq, total, set.by.start = TRUE, this.type1.type = 2, uni.mean = FALSE ), NULL )
expect_equal( a$type1.type, 2 )
expect_equal( a$is.uni.mean(), TRUE ) # uni.mean = FALSE is ignored because nls is not run.
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

expect_equal( a$nls.freq( x, freq, total, set.by.start = TRUE, v.grad = TRUE ), NULL )
expect_equal( a$type1.type, 3 )
expect_equal( a$is.v.grad(), TRUE )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

expect_equal( a$nls.freq( x, freq, total, set.by.start = TRUE, v.grad = TRUE, uni.mean = FALSE ), NULL )
expect_equal( a$type1.type, 3 )
expect_equal( a$is.v.grad(), TRUE )
expect_equal( a$is.uni.mean(), TRUE ) # uni.mean = FALSE is ignored because nls is not run.
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

expect_equal( a$nls.freq( x, freq, total, set.by.start = TRUE, this.type1.type = 3, uni.sigma = TRUE ), NULL )
expect_equal( a$type1.type, 3 )
expect_equal( a$is.uni.sigma(), TRUE )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

expect_equal( a$nls.freq( x, freq, total, set.by.start = TRUE, this.type1.type = 3 ), NULL )
expect_equal( a$type1.type, 3 )
expect_equal( a$is.uni.sigma(), FALSE )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

expect_equal( a$nls.freq( x, freq, total, set.by.start = TRUE, this.type1.type = 3, uni.mean = FALSE ), NULL )
expect_equal( a$type1.type, 3 )
expect_equal( a$is.uni.mean(), TRUE ) # uni.mean = FALSE is ignored because nls is not run.
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

expect_equal( a$nls.freq( x, freq, total, set.by.start = TRUE, this.type1.type = 4, uni.sigma = TRUE ), NULL )
expect_equal( a$type1.type, 4 )
expect_equal( a$kind, "Normal Distribution" ) # nls has not been not run.
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

expect_equal( a$nls.freq( x, freq, total, set.by.start = TRUE, this.type1.type = 4 ), NULL )
expect_equal( a$type1.type, 4 )
expect_equal( a$is.uni.sigma(), FALSE )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

expect_equal( a$nls.freq( x, freq, total, set.by.start = TRUE, this.type1.type = 4, uni.mean = FALSE ), NULL )
expect_equal( a$type1.type, 4 )
expect_equal( a$is.uni.mean(), TRUE ) # uni.mean = FALSE is ignored because nls is not run.
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

## nls test
# normal test
a$nls.freq( x, freq, total, this.type1.type = 2, uni.sigma = TRUE )
expect_equal( a$type1.type, 2 )
expect_equal( a$is.uni.sigma(), TRUE )
expect_equal( a$is.uni.mean(), FALSE )
expect_equal( a$intervals.mean(), c( 1.161789, -1.161223 ), tolerance = 1e-5 )
expect_equal( a$intervals.sd(), c( 0.8834894, 0.8834894 ), tolerance = 1e-5 )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

# Warning case
expect_warning( a$nls.freq( x, freq, total, control = list( warnOnly = TRUE ), this.type1.type = 1 ), "minFactor" )
expect_equal( a$type1.type, 1 )
expect_equal( a$is.uni.mean(), TRUE )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, this.type1.type = 2 )
expect_equal( a$type1.type, 2 )
expect_equal( a$is.uni.sigma(), FALSE )
expect_equal( a$is.uni.mean(), TRUE )
expect_equal( all( a$intervals.mean() == a$intervals.mean()[1] ), TRUE )
expect_equal( a$intervals.sd(), c( 1.084905, 1.087016 ), tolerance = 1e-5 )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, uni.mean = FALSE, this.type1.type = 2 )
expect_equal( a$type1.type, 2 )
expect_equal( a$is.uni.sigma(), FALSE )
expect_equal( a$is.uni.mean(), FALSE )
expect_equal( a$intervals.mean(), c( -0.2743978, 0.2750668 ), tolerance = 1e-5 )
expect_equal( a$intervals.sd(), c( 0.6652693, 0.6667470 ), tolerance = 1e-5 )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, this.type1.type = 3 )
expect_equal( a$type1.type, 3 )
expect_equal( all( a$intervals.mean() == a$intervals.mean()[1] ), TRUE )
expect_equal( a$intervals.sd(), c( 0.8854747, 1.1734463, 0.8907395 ), tolerance = 1e-5 )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, uni.mean = FALSE, this.type1.type = 3 )
expect_equal( a$type1.type, 3 )
expect_equal( a$is.uni.mean(), FALSE )
expect_equal( a$intervals.mean(), c( 0.147964498, 0.007134283, -0.225932696 ), tolerance = 1e-5 )
expect_equal( a$intervals.sd(), c( 0.9219731, 1.2638195, 0.9800101 ), tolerance = 1e-5 )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

# Warning case
expect_warning( a$nls.freq( x, freq, total, control = list( warnOnly = TRUE ), v.grad = TRUE ), "minFactor" )
expect_equal( a$type1.type, 3 )
expect_equal( a$is.v.grad(), TRUE )
expect_equal( all( a$intervals.mean() == a$intervals.mean()[1] ), TRUE )
expect_equal( a$intervals.sd(), c( 0.8881116, 1.1734366, 0.8881116 ), tolerance = 1e-5 )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, v.grad = TRUE, uni.mean = FALSE )
expect_equal( a$type1.type, 3 )
expect_equal( a$is.v.grad(), TRUE )
expect_equal( all( a$intervals.mean() == a$intervals.mean()[1] ), FALSE )
expect_equal( a$intervals.sd(), c( 0.8881109, 1.1734375, 0.8881109 ), tolerance = 1e-5 )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

# Warning case
expect_warning( a$nls.freq( x, freq, total, control = list( warnOnly = TRUE ), this.type1.type = 4 ), "minFactor" )
expect_equal( a$type1.type, 4 )
expect_equal( all( a$intervals.mean() == a$intervals.mean()[1] ), TRUE )
expect_equal( a$intervals.sd(),
			  c( 0.8890329, 1.1712108, 0.8890329, 0.8871634, 1.1756816, 0.8871634 ), tolerance = 1e-5 )
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, control = list( maxiter = 250 ), uni.mean = FALSE, this.type1.type = 4 )
expect_equal( a$type1.type, 4 )
expect_equal( a$intervals.mean(), c( -0.5235105, -0.2758431, -0.5235105, -0.2828730, 0.5360592, -0.2828730 ), tolerance = 1e-5 )
expect_equal( a$intervals.mean()[1], a$intervals.mean()[3] ) # [1] and [3] are the mean of the lower-outer normal distribution
expect_equal( a$intervals.mean()[4], a$intervals.mean()[6] ) # [4] and [6] are the mean of the upper-outer normal distribution
expect_equal( a$intervals.mean()[1] != a$intervals.mean()[2], TRUE ) # [2] is the mean of the lower-inner normal distribution
expect_equal( a$intervals.mean()[4] != a$intervals.mean()[5], TRUE ) # [5] is the mean of the upper-inner normal distribution
expect_equal( a$intervals.sd(), c( 0.6690816, 0.7251914, 0.6690816, 0.4712487, 1.0446081, 0.4712487 ), tolerance = 1e-5 )
expect_equal( a$intervals.sd()[1], a$intervals.sd()[3] ) # ([1] and [3] are the sd of the lower-outer normal distribution)
expect_equal( a$intervals.sd()[4], a$intervals.sd()[6] ) # ([4] and [6] are the sd of the upper-outer normal distribution)
expect_equal( a$intervals.sd()[1] != a$intervals.sd()[2], TRUE ) # [2] is the sd of the lower-inner normal distribution
expect_equal( a$intervals.sd()[4] != a$intervals.sd()[5], TRUE ) # [5] is the sd of the upper-inner normal distribution
show.results( a, x, freq, total )
diff.check( a, x, freq, total )

#### nls.freq.all
# normal test (with 4 times warnings)
expect_warning( expect_warning( expect_warning( expect_warning(
	cgds <- nls.freq.all( x, freq, total ) ) ) ) )
cgds
cgds$cgd
cgds$best
expect_equal( cgds$best$kind.index, 12 )
expect_equal( cgds$best.cor, 0.9853436, tolerance = 1e-5 )
cgds$cor
for ( i in 1:length( cgds$cgd ) ) print( paste( i, ":", cgds$cgd[[i]]$kind ) )

for ( i in 1:length( cgds$cgd ) )
{
	print( paste( "i:", i ) )
	print( paste( "tex.p:", i ) )
	cgds$cgd[[i]]$tex.p()
	print( paste( "tex.d:", i ) )
	cgds$cgd[[i]]$tex.d()
}

for ( i in 1:length( cgds$cgd ) )
{
	print( paste( "i:", i ) )
	print( paste( "tex:", i ) )
	cgds$cgd[[i]]$tex()
}

cgds$cgd[[10]]$tex.p( 3, TRUE )
cgds$cgd[[10]]$tex.p( 8, FALSE )
cgds$cgd[[10]]$tex.d( 3, TRUE )
cgds$cgd[[10]]$tex.d( 8, FALSE )
cgds$cgd[[10]]$tex( 3, TRUE )
cgds$cgd[[10]]$tex( 8, FALSE )

plot.freq.and.d( cgds$cgd[[1]], x, freq, total )
plot.freq.and.d( cgds$cgd[[2]], x, freq, total )
plot.freq.and.d( cgds$cgd[[3]], x, freq, total )
plot.freq.and.d( cgds$cgd[[4]], x, freq, total )
plot.freq.and.d( cgds$cgd[[5]], x, freq, total )
plot.freq.and.d( cgds$cgd[[6]], x, freq, total )
plot.freq.and.d( cgds$cgd[[7]], x, freq, total )
plot.freq.and.d( cgds$cgd[[8]], x, freq, total )
plot.freq.and.d( cgds$cgd[[9]], x, freq, total )
plot.freq.and.d( cgds$cgd[[10]], x, freq, total )
plot.freq.and.d( cgds$cgd[[11]], x, freq, total )
plot.freq.and.d( cgds$cgd[[12]], x, freq, total )
plot.freq.and.d( cgds$cgd[[13]], x, freq, total )
plot.freq.and.d( cgds$cgd[[14]], x, freq, total )
plot.freq.and.d( cgds$cgd[[15]], x, freq, total )

plot.freq.and.d( cgds$best, x, freq, total )

## nls.start.template
# normal test
expect_equal( nls.start.template( 1 ), list( mean = 0, sqrt.sd = 1 ) )
expect_equal( nls.start.template( 15 ),
			  list( mean.1.1 = 0, mean.1.2 = 0, sqrt.sd.1.1 = 1, sqrt.sd.1.2 = 1,
			  		mean.2.1 = 0, mean.2.2 = 0, sqrt.sd.2.1 = 1, sqrt.sd.2.2 = 1 ) )
expect_equal( nls.start.template( "Mean of Median-Equaled-Sigma-Differed 2 Normal Distributions" ),
			  list( mean = 0, sqrt.sd.1 = 1, sqrt.sd.2 = 1 ) )

# Warning case
expect_warning( nls.start.template( 0 ), "not suitable for cgd.kind.index" )
expect_warning( nls.start.template( 16 ), "not suitable for cgd.kind.index" )

start.lists <- init.start.lists()
start.lists[[ 1]] <- list( mean = 0,					sqrt.sd = sqrt( 1 ) )
start.lists[[ 2]] <- list( mean = 0,					sqrt.sd.1 = sqrt( 2 ), sqrt.sd.2 = sqrt( 1 ) )
start.lists[[ 3]] <- list( mean = 0,					sqrt.sd.1 = sqrt( 2 ), sqrt.sd.2 = sqrt( 1 ) )
start.lists[[ 4]] <- list( mean.1 = -1, mean.2 = 1,		sqrt.sd = sqrt( 1 ) )
start.lists[[ 5]] <- list( mean = 0,					sqrt.sd.1 = sqrt( 1 ), sqrt.sd.2 = sqrt( 1 ) )
start.lists[[ 6]] <- list( mean.1 = -1, mean.2 = 1,		sqrt.sd.1 = sqrt( 1 ), sqrt.sd.2 = sqrt( 1 ) )
start.lists[[ 7]] <- list( mean.1 = 0, mean.2 = 0,		sqrt.sd = sqrt( 1 ) )
start.lists[[ 8]] <- list( mean = 0,					sqrt.sd.1 = sqrt( 2 ), sqrt.sd.2 = sqrt( 1 ) )
start.lists[[ 9]] <- list( mean.1 = 0, mean.2 = 0,		sqrt.sd.1 = sqrt( 2 ), sqrt.sd.2 = sqrt( 1 ) )
start.lists[[10]] <- list( mean.1 = 0, mean.2 = 0, mean.3 = 0,
														sqrt.sd = sqrt( 1 ) )
start.lists[[11]] <- list( mean = 0,					sqrt.sd.1 = sqrt( 2 ), sqrt.sd.2 = sqrt( 1 ), sqrt.sd.3 = sqrt( 2 ) )
start.lists[[12]] <- list( mean.1 = 0, mean.2 = 0, mean.3 = 0,
														sqrt.sd.1 = sqrt( 2 ), sqrt.sd.2 = sqrt( 1 ), sqrt.sd.3 = sqrt( 2 ) )
start.lists[[13]] <- list( mean.1.1 = 0, mean.1.2 = 0, mean.2.1 = 0, mean.2.2 = 0,
														sqrt.sd = sqrt( 1 ) )
start.lists[[14]] <- list( mean = 0,					sqrt.sd.1.1 = sqrt( 2 ), sqrt.sd.1.2 = sqrt( 1 ),
														sqrt.sd.2.1 = sqrt( 2 ), sqrt.sd.2.2 = sqrt( 1 ) )
start.lists[[15]] <- list( mean.1.1 = 0, mean.1.2 = 0,	sqrt.sd.1.1 = sqrt( 2 ), sqrt.sd.1.2 = sqrt( 1 ),
						  mean.2.1 = 0, mean.2.2 = 0,	sqrt.sd.2.1 = sqrt( 2 ), sqrt.sd.2.2 = sqrt( 1 ) )

# (9 times warnings)
expect_warning( expect_warning( expect_warning( expect_warning( expect_warning(
expect_warning( expect_warning( expect_warning( expect_warning(
	cgds <- nls.freq.all( x, freq, total, start.lists ) ) ) ) ) ) ) ) ) )
cgds
cgds$cgd
cgds$best
expect_equal( cgds$best$kind.index, 13 )
expect_equal( cgds$best.cor, 0.9849939, tolerance = 1e-5 )
cgds$cor
plot.freq.and.d( cgds$best, x, freq, total )

# normal and error test
cgds <- nls.freq.all( x, freq, total, control = list( maxiter = 50, warnOnly = FALSE ) )
cgds
cgds$cgd
cgds$best
expect_equal( cgds$best$kind.index, 12 )
expect_equal( cgds$best.cor, 0.9853436, tolerance = 1e-5 )
cgds$cor
plot.freq.and.d( cgds$best, x, freq, total )

# normal test (with 4 times warnings)
expect_warning( expect_warning( expect_warning( expect_warning(
	cgds <- nls.freq.all( x, freq, total, method = "spearman", trace = TRUE ) ) ) ) )
cgds
cgds$cgd
expect_equal( cgds$best$kind.index, 15 )
expect_equal( cgds$best.cor, 0.9532468, tolerance = 1e-5 )
cgds$cor
plot.freq.and.d( cgds$best, x, freq, total )
