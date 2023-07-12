#### This file is for testing mainly CGD$set.waypoints and trace.q.

library( "cgd" )
library( "testthat" )

a<-CGD$new()
dev.new(); plot.new()

## If you want to wait a moment before drawing a new graph
## to watch the last-drawn graph on a real-time test,
## set TRUE this option.
wait.before.new.graph <- FALSE
#wait.before.new.graph <- TRUE

## for control option test
control <- list( ftol = 1e-4 )

## A function to plot a function within a range of x.
plot.f <- function( f, range = seq( -8, 8, 0.02 ) )
{
	y <- vapply( range, f, 0 )
	plot( range, y, type = "l" )
}

## It's recomended to run this plot to compare integration a$d() to a$p().
## Remark: Numerical integral is sometimes not so accurate.
plot.f( function( x ) { integrate( f <- function( t ) { a$d( t ) }, -Inf, x )$value - a$p( x ) } )

mean.via.integrate <- function( a, lower = -Inf, upper = Inf )
{
	integrate( f <- function( t ) { t * a$d( t ) }, lower, upper )$value
}

v.via.integrate <- function( a, lower = -Inf, upper = Inf )
{
	integrate( f <- function( t ) { ( t - a$mean )^2 * a$d( t ) }, lower, upper )$value
}

sd.via.integrate <- function( a )
{
	sqrt( v.via.integrate( a ) )
}

## This function should be run after each non-error test to confirm the result.
show.results <- function( obj = a, plot.range = 3, sumple.num = 1000, is.extreme.case = FALSE )
{
	if ( wait.before.new.graph )
	{
		# Waiting a moment for the last-drawn graph to check on a real-time test.
		Sys.sleep( 3 )
	}
	cat( "d:" )
	print( system.time( plot( seq( -plot.range, plot.range, 0.01 ),
							  obj$d( seq( -plot.range, plot.range, 0.01 ) ), type = "l" ) ) )

	if ( wait.before.new.graph )
	{
		# Waiting a moment for the last-drawn graph to check on a real-time test.
		Sys.sleep( 0.3 )
	}
	cat( "p:" )
	print( system.time( plot( seq( -plot.range, plot.range, 0.01 ),
							  obj$p( seq( -plot.range, plot.range, 0.01 ) ), type = "l" ) ) )

	if ( wait.before.new.graph )
	{
		# Waiting a moment for the last-drawn graph to check on a real-time test.
		Sys.sleep( 0.3 )
	}
	cat( "q:" )
	print( system.time( plot( seq( 0, 1, 0.01 ), obj$q( seq( 0, 1, 0.01 ) ), type = "l" ) ) )

	if ( wait.before.new.graph )
	{
		# Waiting a moment for the last-drawn graph to check on a real-time test.
		Sys.sleep( 0.3 )
	}
	cat( "r:" )
	print( system.time( sample <- obj$r( sumple.num ) ) ); hist( sample )

	print( obj )

	## These values are for reference
	## because the accuracy guarantees of mean and sd
	## using the integrate {stats} function have been performed in tests/test.integ.R.
	## So in most cases the cause of a high difference value will be in the integrate function.
	## Note, however, if there is grater than 0.1 of difference in a less extreme case,
	## you should doubt that something wrongs may be in CGD.R.
	cat ( "\nDifference vs integrate (for reference):\n" )

	mean.diff <- abs( obj$mean - mean.via.integrate( obj ) )
	cat( paste( "mean:", mean.diff, "\n" ) )

	sd.diff <- abs( obj$sd - sd.via.integrate( obj ) )
	cat( paste( "sd:  ", sd.diff, "\n" ) )

	if ( !is.extreme.case )
	{
		expect_equal( mean.diff < 0.1, TRUE )
		expect_equal( sd.diff < 0.1, TRUE )
	}
	else
	{
		cat( "** Differences have not been checked because is.extreme.case = TRUE. **\n" )
	}
}

## Instead of the above function, you can each of these commands.
system.time( plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" ) )
system.time( plot( seq( -3, 3, 0.01 ), a$p( seq( -3, 3, 0.01 ) ), type = "l" ) )
system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
system.time( sample <- a$r( 1000 ) ); hist( sample )
a
print( paste( "mean:", abs( a$mean - mean.via.integrate( a ) ) ) )
print( paste( "sd:  ", abs( a$sd - sd.via.integrate( a ) ) ) )
expect_equal( abs( a$mean - mean.via.integrate( a ) ) < 0.1, TRUE )
expect_equal( abs( a$sd - sd.via.integrate( a ) ) < 0.1, TRUE )

## Plotting waypoints and a$p().
plot.waypoints.and.p <- function( a, p, q, xlim = NULL )
{
	if ( is.null( xlim ) )
	{
		xlim <- c( min( q ) - abs( q[2] - q[1] ), max( q ) + abs( q[length( q )] - q[length( q - 1 )] ) )
	}
	ylim <- c( 0, 1 )
	plot( q, p, xlim = xlim, ylim = ylim, xlab = "", ylab = "" )
	par( new = TRUE )
	plot( seq( xlim[1], xlim[2], 0.01 ), a$p( seq( xlim[1], xlim[2], 0.01 ) ), type = "l", xlim = xlim, ylim = ylim )
}

# normal test
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), this.type1.type = 1 )
expect_equal( a$is.ind.p( c( 0, 0.1, 0.3, 0.5, 0.6, 0.65, 0.7, 1 ) ),
			  list( bool = c( TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE ),
			  		i = c( 1, 1, 1, 1, 1, NaN, 2, 2 ) ) )
expect_equal( a$is.conn.p( c( 0, 0.1, 0.3, 0.5, 0.6, 0.65, 0.7, 1 ) ),
			  list( bool = c( FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE ),
			  		i.1 = c( 1, 1, 1, 1, 1, 1, 2, 2 ),
			  		i.2 = c( NaN, NaN, NaN, NaN, NaN, 2, NaN, NaN ) ) )
expect_equal( a$is.ind.q( c( -Inf, -3, 0, 0.2533471, 0.2533472, 0.49, 0.5, 0.52440, 0.52441, 1, Inf ) ),
			  list( bool = c( TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE ),
			  		i = c( 1, 1, 1, 1, NaN, NaN, 2, 2, 2, 2, 2 ) ) )
expect_equal( a$is.conn.q( c( -Inf, -3, 0, 0.2533471, 0.2533472, 0.49, 0.5, 0.52440, 0.52441, 1, Inf ) ),
			  list( bool = c( FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE ),
			  i.1 = c( 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2 ),
			  i.2 = c( NaN, NaN, NaN, NaN, 2, 2, NaN, NaN, NaN, NaN, NaN ) ) )
expect_equal( a$p( c( 0.253347, 0.253348, 0.5, 0.52440, 0.52441 ) ),
			  c( 0.6000000, 0.6000004, 0.7000000, 0.7088373, 0.7088409 ), tolerance = 5e-7 )
expect_equal( ( a$p( 0.253347 ) == pnorm( 0.253347, 0, a$intervals[[1]]$sd ) ), TRUE )
expect_equal( ( a$p( 0.253348 ) > pnorm( 0.253348, 0, a$intervals[[1]]$sd ) ), TRUE )
expect_equal( ( a$p( 0.49 ) < pnorm( 0.49, 0, a$intervals[[2]]$sd ) ), TRUE )
expect_equal( ( a$p( 0.5 ) == pnorm( 0.5, 0, a$intervals[[2]]$sd ) ), TRUE )
expect_equal( ( a$p( 0.52440 ) == pnorm( 0.52440, 0, a$intervals[[2]]$sd ) ), TRUE )
expect_equal( ( a$median == 0 ), TRUE )
expect_equal( a$mean, -0.01719301, tolerance = 5e-7 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9772327, 0.9863357, 0.9680440 ), tolerance = 5e-7 )
plot.waypoints.and.p( a, c( 0.1, 0.3, 0.5, 0.6, 0.7 ), c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ), c( -3, 3 ) )
show.results()

# Error case
expect_error( a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), this.type1.type = -1 ),
	"type1.type -1 is undefined." )

# Error case
expect_error( a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), this.type1.type = 5 ),
	"type1.type 5 is undefined." )

# Error case
a$type1.type <- -1
expect_error( a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ) ),
	"type1.type -1 is undefined." )

# Error case
a$type1.type <- 5
expect_error( a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ) ),
	"type1.type 5 is undefined." )

# normal test
a <- trace.q(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), type1.type = 1 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9772327, 0.9863357, 0.9680440 ), tolerance = 5e-7 )
plot.waypoints.and.p( a, c( 0.1, 0.3, 0.5, 0.6, 0.7 ), c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ), c( -3, 3 ) )
show.results()

# normal test
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), this.type1.type = 0 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9643554, 0.9796490, 0.9488153 ), tolerance = 5e-7 )
plot.waypoints.and.p( a, c( 0.1, 0.3, 0.5, 0.6, 0.7 ), c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ), c( -3, 3 ) )
show.results()

# normal test
a <- trace.q(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), type1.type = 0 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9643554, 0.9796490, 0.9488153 ), tolerance = 5e-7 )
plot.waypoints.and.p( a, c( 0.1, 0.3, 0.5, 0.6, 0.7 ), c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ), c( -3, 3 ) )
show.results()

# Error case
expect_error( a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.55, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), this.type1.type = 1 ),
	"The point of p = 0.5 must be given" )

# Error case
expect_error( a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.55, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), this.type1.type = 2 ),
	"The point of p = 0.5 must be given" )

# normal test (with some computational errors of qnorm)
a$set.waypoints(
	data.frame(
	p = seq( 0.1, 0.9, 0.1 ),
	q = c( qnorm( seq( 0.1, 0.9, 0.1 ), 0.3, 1 ) ) ), this.type1.type = 1 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.00053, 1.00106, 1.00000 ), tolerance = 5e-7 )
plot.waypoints.and.p( a, seq( 0.1, 0.9, 0.1 ), qnorm( seq( 0.1, 0.9, 0.1 ), 0.3, 1 ), c( -3, 3 ) )
show.results()
expect_equal( a$intervals.mean() - 0.3, rep( 0, length( a$intervals ) ) )
expect_equal( all( a$intervals.sd() == rep( 1, length( a$intervals ) ) ), FALSE )

# normal test
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.55, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), this.type1.type = 0 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9654052, 0.9680747, 0.9627283 ), tolerance = 5e-7 )
plot.waypoints.and.p( a, c( 0.1, 0.3, 0.55, 0.6, 0.7 ), c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ), c( -3, 3 ) )
show.results()

# Error case
expect_error( a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), this.type1.type = 1, diff.mean = TRUE ),
	"Illegal options for diff.mean = TRUE" )

# Error case
expect_error( a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), this.type1.type = 2, diff.mean = TRUE ),
	"Illegal options for diff.mean = TRUE" )

# Error case
expect_error( a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.5 ),
	q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6, 0.51 ), 0, 1 ) ), this.type1.type = 1 ) ),
	"Order of q is not along with that of p" )

# normal test
a <- trace.q(
	data.frame(
	p = c( 0.3, 0.5, 0.1, 0.6, 0.7 ),
	q = c( qnorm( c( 0.3, 0.5, 0.1, 0.6 ), 0, 1 ), 0.5 ) ), type1.type = 1 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9772327, 0.9863357, 0.9680440 ), tolerance = 5e-7 )
plot.waypoints.and.p( a, c( 0.3, 0.5, 0.1, 0.6, 0.7 ), c( qnorm( c( 0.3, 0.5, 0.1, 0.6 ), 0, 1 ), 0.5 ), c( -3, 3 ) )
show.results()

# normal test
a <- trace.q(
	data.frame(
	p = c( 0.3, 0.5, 0.1, 0.6, 0.7 ),
	q = c( qnorm( c( 0.3, 0.5, 0.1, 0.6 ), 0, 1 ), 0.5 ) ), type1.type = 0 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9643554, 0.9796490, 0.9488153 ), tolerance = 5e-7 )
plot.waypoints.and.p( a, c( 0.3, 0.5, 0.1, 0.6, 0.7 ), c( qnorm( c( 0.3, 0.5, 0.1, 0.6 ), 0, 1 ), 0.5 ), c( -3, 3 ) )
show.results()

# normal test
a$set.waypoints(
	data.frame(
	p = c( 0, 0.3, 0.5, 0.6, 1 ),
	q = c( -Inf, qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ), Inf ) ), this.type1.type = 1 )
expect_equal( a$sd, 1 )
expect_equal( a$type1.type, 1 )
expect_equal( a$kind, "Normal Distribution" )
show.results()

# normal test
a$set.waypoints(
	data.frame(
	p = c( 0, 0.3, 0.5, 0.6, 1 ),
	q = c( -Inf, qnorm( c( 0.3, 0.5, 0.6 ), 0, 2 ), Inf ) ), this.type1.type = 2 )
expect_equal( a$sd, 2 )
expect_equal( a$type1.type, 2 )
expect_equal( a$kind, "Normal Distribution" )
show.results()

# normal test
a$set.waypoints(
	data.frame(
	p = c( 0, 0.3, 0.5, 0.6, 1 ),
	q = c( -Inf, qnorm( c( 0.3, 0.5, 0.6 ), 0, 1.5 ), Inf ) ), this.type1.type = 0 )
expect_equal( a$sd, 1.5 )
expect_equal( a$type1.type, 0 )
expect_equal( a$kind, "Normal Distribution" )
show.results()

# warning test
expect_warning( a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( -Inf, qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ), Inf ) ), this.type1.type = 1 ),
	"There is a row which q is infinite" )
expect_equal( a$kind, "Normal Distribution" )
show.results()

# warning test
expect_warning( a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( -Inf, qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ), Inf ) ), this.type1.type = 0 ),
	"There is a row which q is infinite" )
expect_equal( a$kind, "Normal Distribution" )
show.results()

# warning test
expect_warning( a$set.waypoints(
	data.frame(
	p = c( -0.1, 0.3, 0.5, 0.6, 1.5 ),
	q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), this.type1.type = 1 ),
	"Probability -0.1 is out of range" )
expect_equal( a$kind, "Normal Distribution" )
show.results()

# warning test
expect_warning( a$set.waypoints(
	data.frame(
	p = c( -0.1, 0.3, 0.5, 0.6, 1.5 ),
	q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), this.type1.type = 0 ),
	"Probability -0.1 is out of range" )
expect_equal( a$kind, "Normal Distribution" )
show.results()

# normal test (type 1)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
	q = c( qnorm( c( 0.1, 0.25, 0.5, 0.75 ), 1, 1.1 ), qnorm( 0.9, 1, 0.9 ) ) ), this.type1.type = 1 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.0213423, 1.0599484, 0.9812185 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
		c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
		c( qnorm( c( 0.1, 0.25, 0.5, 0.75 ), 1, 1.1 ), qnorm( 0.9, 1, 0.9 ) ),
		c( -3, 3 ) )
show.results()

# normal test (set.intervals, type 1, type 2)
b<-CGD$new( type1.type = 1 )
b$set.intervals( a$intervals )
expect_equal( c( b$sd, b$lsd, b$usd ), c( 1.0213423, 1.0599484, 0.9812185 ), tolerance = 5e-7 )
show.results( b )

b$set.intervals( a$intervals, this.type1.type = 2 )
expect_equal( c( b$sd, b$lsd, b$usd ), c( 1.0207011, 1.0594369, 0.9804361 ), tolerance = 5e-7 )
show.results( b )

# Error case
expect_error( b$set.intervals( a$intervals, this.type1.type = -1 ) )

# Error case
expect_error( b$set.intervals( a$intervals, this.type1.type = 5 ) )

# normal test (set.intervals, intervals[[1]]$q.ind[2] == intervals[[2]]$q.ind[1]; Normal Distribution)
a.1<-CGD$new()
a.1$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.4, 0.5 ),
	q = c( qnorm( c( 0.1, 0.25 ), 0, 1 ),
		   qnorm( c( 0.4, 0.5 ), 0, 1.2 ) ) ), this.type1.type = 1 )
a.2<-CGD$new()
a.2$set.waypoints(
	data.frame(
	p = c( 0.1, 0.2, 0.25, 0.5 ),
	q = c( qnorm( c( 0.1, 0.2 ), 0, 1.2 ),
		   qnorm( c( 0.25, 0.5 ), 0, 1 ) ) ), this.type1.type = 1 )
expect_equal( a.1$intervals.p.ind()[1, 2] == a.2$intervals.p.ind()[2, 1], TRUE )
expect_equal( a.1$intervals.q.ind()[1, 2] == a.2$intervals.q.ind()[2, 1], TRUE )
expect_equal( length( a.2$intervals ) == 2, TRUE )

a$set.intervals( list( a.1$intervals[[1]], a.2$intervals[[2]] ),
				 this.type1.type = 1 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1, 1, 1 ) )
plot.waypoints.and.p( a,
		c( 0.1, 0.25, 0.5 ),
		qnorm( c( 0.1, 0.25, 0.5 ), 0, 1 ),
		c( -3, 3 ) )
show.results()
expect_equal( a$kind, "Normal Distribution" )

# normal test
# (set.intervals, intervals[[1]]$q.ind[2] == intervals[[2]]$q.ind[1]; Disconnected Distribution)
a.2$set.waypoints(
	data.frame(
	p = c( 0.1, 0.2, 0.25, 0.5, 0.6, 0.75 ),
	q = c( qnorm( c( 0.1, 0.2 ), 0, 1.2 ),
		   qnorm( c( 0.25, 0.5, 0.6 ), 0, 1 ),
		   qnorm( 0.75, 0, 0.75 ) ) ),
		   this.type1.type = 1 )
expect_equal( a.1$intervals.p.ind()[1, 2] == a.2$intervals.p.ind()[2, 1], TRUE )
expect_equal( a.1$intervals.q.ind()[1, 2] == a.2$intervals.q.ind()[2, 1], TRUE )
expect_equal( length( a.2$intervals ) == 3, TRUE )

a$set.intervals( list( a.1$intervals[[1]], a.2$intervals[[2]], a.2$intervals[[3]] ),
				 this.type1.type = 1 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.8820963, 0.9293987, 0.8321092 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
		c( 0.1, 0.2, 0.25, 0.5, 0.6, 0.75 ),
		c( qnorm( c( 0.1, 0.2, 0.25, 0.5, 0.6 ), 0, 1 ),
		   qnorm( 0.75, 0, 0.75 ) ),
		c( -3, 3 ) )
show.results()
expect_equal( a$kind, "Discontinuous Connected Gaussian Distribution" )

# Remove temporary objects
rm( a.1, a.2, b )

# normal test (type 1)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), this.type1.type = 2 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9772270, 0.9863249, 0.9680437 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ),
		c( -3, 3 ) )
show.results()

# normal test (type 2)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.7 ) ), this.type1.type = 2 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.169650, 1.100779, 1.234685 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.7 ),
		c( -3, 3 ) )
show.results()

# normal test (type 2)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 1.2 ) ), this.type1.type = 2 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.688993, 1.415815, 1.923762 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 1.2 ),
		c( -3, 3 ) )
show.results()

# normal test (type1.type = 0)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 1.2 ) ), this.type1.type = 0 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 2.127395, 1.647591, 2.517352 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 1.2 ),
		c( -3, 3 ) )
show.results()

# normal test (type 2)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( -5, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ) ), this.type1.type = 2 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 2.477120, 3.070707, 1.686120 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( -5, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ),
		c( -3, 3 ) )
show.results()

# normal test (type1.type = 0)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( -5, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ) ), this.type1.type = 0 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 2.810767, 3.498392, 1.887345 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( -5, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ),
		c( -3, 3 ) )
show.results()

# normal test (type 3a)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3 ), 0, 1 ), 0, qnorm( c( 0.6, 0.7 ), 0, 1.1 ) ) ), this.type1.type = 2 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.056852, 1.032227, 1.080917 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( qnorm( c( 0.1, 0.3 ), 0, 1 ), 0, qnorm( c( 0.6, 0.7 ), 0, 1.1 ) ),
		c( -3, 3 ) )
show.results()

# normal test (type1.type = 0)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3 ), 0, 1 ), 0, qnorm( c( 0.6, 0.7 ), 0, 1.1 ) ) ), this.type1.type = 0 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.050433, 1.032119, 1.068433 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( qnorm( c( 0.1, 0.3 ), 0, 1 ), 0, qnorm( c( 0.6, 0.7 ), 0, 1.1 ) ),
		c( -3, 3 ) )
show.results()

# normal test (type 3b)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3 ), 0, 1.1 ), 0, qnorm( c( 0.6, 0.7 ), 0, 1 ) ) ), this.type1.type = 2 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.053889, 1.070640, 1.036868 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( qnorm( c( 0.1, 0.3 ), 0, 1.1 ), 0, qnorm( c( 0.6, 0.7 ), 0, 1 ) ),
		c( -3, 3 ) )
show.results()

# normal test (type1.type = 0)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( qnorm( c( 0.1, 0.3 ), 0, 1.1 ), 0, qnorm( c( 0.6, 0.7 ), 0, 1 ) ) ), this.type1.type = 0 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.050433, 1.068433, 1.032119 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( qnorm( c( 0.1, 0.3 ), 0, 1.1 ), 0, qnorm( c( 0.6, 0.7 ), 0, 1 ) ),
		c( -3, 3 ) )
show.results()

# normal test (type 1)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( -1.1, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ) ), this.type1.type = 2 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9408820, 0.9119680, 0.9689335 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( -1.1, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ),
		c( -3, 3 ) )
show.results()

# normal test (type1.type = 0)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( -1.1, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ) ), this.type1.type = 0 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9303524, 0.8954867, 0.9639579 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( -1.1, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ),
		c( -3, 3 ) )
show.results()

# normal test (type 2)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( -1.4, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ) ), this.type1.type = 2 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.038014, 1.056303, 1.019397 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( -1.4, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ),
		c( -3, 3 ) )
show.results()

# normal test (type1.type = 0)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( -1.4, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ) ), this.type1.type = 0 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.047796, 1.071136, 1.023923 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( -1.4, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ),
		c( -3, 3 ) )
show.results()

# normal test (very high abs of q.conn.prev/next with type1.type = 2)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.7, 1 - 1e-13, 1 - 9.99e-14 ),
	q = c( -1.1, qnorm( c( 0.3, 0.5, 0.7, 1 - 1e-12, 1 - 9.998e-13 ), 0, 1 ) ) ),
		this.type1.type = 2 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9255395, 0.9043759, 0.9462298 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
		c( 0.1, 0.2, 0.2 + 1e-10, 0.3, 0.5, 0.6, 0.7, 1 - 1e-11, 1 - 9.99e-12 ),
		c( -1.1, qnorm( c( 0.2, 0.2 + 1e-10 - 1e-15, 0.3, 0.5, 0.6, 0.7, 1 - 1e-11, 1 - 9.9899e-12 ), 0, 1 ) ),
		c( -3, 8 ) )
show.results()

# normal test
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
	q = c( -0.5, -0.04, 0, 1, 2 ) ), this.type1.type = 1 )
expect_equal( a$intervals[[2]]$p.ind[1] - 0.3, 0 )
expect_equal( a$is.ind.p( c( 0, 0.05, 0.1, 0.2, 0.3, a$intervals[[2]]$p.ind[1], 0.4, 0.5, 0.6, 0.65, 0.7, 0.8 ) ),
			  list( bool = c( TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE ),
					i = c( 1, 1, 1, NaN, 2, 2, NaN, NaN, 3, NaN, 4, 4 ) ) )
expect_equal( a$is.conn.p( c( 0, 0.05, 0.1, 0.2, 0.3, a$intervals[[2]]$p.ind[1], 0.4, 0.5, 0.6, 0.65, 0.7, 0.8 ) ),
			  list( bool = c( FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE ),
			  		i.1 = c( 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 4 ),
			  		i.2 = c( NaN, NaN, NaN, 2, NaN, NaN, 3, 3, NaN, 4, NaN, NaN ) ) )
expect_equal( a$is.ind.q( c( -1, -0.48, -0.04, 0, 0.5, 1, 1.5, 2 ) ),
			  list( bool = c( TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE ),
			  		i = c( 1, NaN, 2, NaN, NaN, 3, NaN, 4 ) ) )
expect_equal( a$is.conn.q( c( -1, -0.48, -0.04, 0, 0.5, 1, 1.5, 2 ) ),
			  list( bool = c( FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE ),
			  		i.1 = c( 1, 1, 2, 2, 2, 3, 3, 4 ),
			  		i.2 = c( NaN, 2, NaN, 3, 3, NaN, 4, NaN ) ) )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 2.323496, 1.720156, 2.799702 ), tolerance = 5e-7 )

plot( seq( -1, 0.05, 0.01 ), a$d( seq( -1, 0.05, 0.01 ) ), type = "l" )
plot( seq( 0.05, 10, 0.01 ), a$d( seq( 0.05, 10, 0.01 ) ), type = "l" )
show.results()


# warning test
expect_warning( a$set.waypoints(
	data.frame(
	p = c( 0.5 ),
	q = c( 1 ) ),
	this.type1.type = 1 ),
	"No waypoints other than" )
expect_equal( a$type1.type, 1 )
expect_equal( a$kind, character(0) )

# Error case
expect_warning( expect_error( a$set.waypoints(
	data.frame(
	p = c( 0.5 ),
	q = c( 1 ) ),
	this.type1.type = 0 ),
	"2 and more quantiles are needed" ),
	"No waypoints other than" )


# normal test
a <- CGD$new( 0 )
expect_equal( a$type1.type, 0 )
expect_equal( a$kind, "Normal Distribution" )
show.results()

# normal test
a <- CGD$new( 2 )
expect_equal( a$type1.type, 2 )
expect_equal( a$kind, "Normal Distribution" )
show.results()

# normal test
a <- CGD$new( 3 )
expect_equal( a$type1.type, 3 )
expect_equal( a$kind, "Normal Distribution" )
show.results()

# normal test
a <- CGD$new( 4 )
expect_equal( a$type1.type, 4 )
expect_equal( a$kind, "Normal Distribution" )
show.results()

# warning test
expect_warning( a <- CGD$new( -1 ), "type1.type -1 is undefined." )
expect_equal( a$type1.type, 1 )
expect_equal( a$kind, "Normal Distribution" )
show.results()

# warning test
expect_warning( a <- CGD$new( 5 ), "type1.type 5 is undefined." )
expect_equal( a$type1.type, 1 )
expect_equal( a$kind, "Normal Distribution" )
show.results()


# normal test
a$set.waypoints(
	data.frame(
	p = c( 0.5, 0.7 ),
	q = c( 0, qnorm( 0.7, 0, 1 ) ) ),
	this.type1.type = 1 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1, 1, 1 ) )
show.results()

# normal test
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ),
	q = c( -9, -8.9, -8.7, -5.4, -1, 0, 0.3, 0.5, 0.77 ) ),
	this.type1.type = 1 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 4.942252, 5.541424, 4.259615 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
		c( 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ),
		c( -9, -8.9, -8.7, -5.4, -1, 0, 0.3, 0.5, 0.77 ),
		c( -12, 2 ) )
plot( seq( -12, 2, 0.01 ), a$d( seq( -12, 2, 0.01 ) ), type = "l" )
show.results( is.extreme.case = TRUE )

# normal test
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ),
	q = c( -9, -8.9, -8.7, -5.4, -1, 0, 0.3, 0.5, 0.77 ) ),
	this.type1.type = 2 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 4.941505, 5.540824, 4.258663 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
		c( 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ),
		c( -9, -8.9, -8.7, -5.4, -1, 0, 0.3, 0.5, 0.77 ),
		c( -12, 2 ) )
plot( seq( -12, 2, 0.01 ), a$d( seq( -12, 2, 0.01 ) ), type = "l" )
show.results( is.extreme.case = TRUE )

# normal test
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ),
	q = c( -9, -8.9, -8.7, -5.4, -1, 0, 0.3, 0.5, 0.77 ) ),
	this.type1.type = 0 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 4.238081, 4.508632, 3.949036 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
		c( 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ),
		c( -9, -8.9, -8.7, -5.4, -1, 0, 0.3, 0.5, 0.77 ),
		c( -12, 2 ) )
plot( seq( -12, 2, 0.01 ), a$d( seq( -12, 2, 0.01 ) ), type = "l" )
show.results()

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.6, 0, 0.75 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.5, 0.6 ),
	q = qs ),
	this.type1.type = 1, continuous = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.111522, 1.111522, 1.111522 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( qs ), c( 0.1, 0.5, 0.6 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.6 ), qs, c( -3, 3 ) )
show.results()

## Where symmetric distribution,
## cgd:::v.cont value with get.lv/get.uv = TRUE is "worse" than the lsd/usd field value.
## The theoretical maximum error of cgd:::v.cont (vs lsd/usd) is sqrt( .Machine$double.eps ).
## However, almost always, all values of them are equal.
expect_equal( abs( a$lsd - sqrt( cgd:::v.cont( 1, a$intervals.mean(), a$intervals.sd(), get.lv = TRUE ) * 2 ) ) <=
				sqrt( .Machine$double.eps ), TRUE )
expect_equal( abs( a$usd - sqrt( cgd:::v.cont( 1, a$intervals.mean(), a$intervals.sd(), get.uv = TRUE ) * 2 ) ) <=
				sqrt( .Machine$double.eps ), TRUE )

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.5, 0.6 ),
		q = qs ),
		this.type1.type = 0, continuous = TRUE ),
		"Illegal number of quantiles" )

# normal test (control option)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.5, 0.6 ),
	q = qs ),
	this.type1.type = 1, continuous = TRUE, control = control )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.110976, 1.110976, 1.110976 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( qs ), c( 0.0999377, 0.5000000, 0.5999828 ), tolerance = 5e-7 )
show.results()

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.5, 0.6 ),
		q = qs ),
		this.type1.type = 1, continuous = TRUE, diff.mean = TRUE ) )

# normal test
qs <- c( qnorm( 0.1, -0.5, 1.9 ), -0.5, qnorm( 0.7, -0.5, 1.8 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.5, 0.7 ),
	q = qs ),
	this.type1.type = 1, symmetric = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.986118, 1.986118, 1.986118 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( qs ), c( 0.1, 0.5, 0.7 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.7 ), qs, c( -3, 3 ) )
show.results()

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.5, 0.7 ),
		q = qs ),
		this.type1.type = 0, symmetric = TRUE ),
		"Illegal number of quantiles" )

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.5, 0.6 ),
		q = c( qnorm( 0.1, 0, 0.9 ), 0, qnorm( 0.6, 0, 1 ) ) ),
		this.type1.type = 1, continuous = TRUE ),
		"Failed to construct" )

# normal test for v2.3.0, and up
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.8 ), qnorm( 0.6, 0, 0.8 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.4, 0.6 ), qs ),
	this.type1.type = 1, continuous = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.104407, 1.104407, 1.104407 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( qs ), c( 0.1, 0.4, 0.6 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.6 ), qs, c( -3, 3 ) )
show.results()

# normal test (for bug-fix)
qs <- c( qnorm( 0.1, 0, 1.9 ), 0, qnorm( 0.7, 0, 0.4 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.5, 0.7 ),
	q = c( qnorm( 0.1, 0, 1.9 ), 0, qnorm( 0.7, 0, 0.4 ) ) ),
	this.type1.type = 1, symmetric = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 2.04997, 2.04997, 2.04997 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( qs ), c( 0.1, 0.5, 0.7 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.7 ), qs, c( -3, 3 ) )
show.results()


# normal test
qs <- c( qnorm( 0.3, -0.2, 1 ), 0, qnorm( 0.7, 0.2, 0.8 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.3, 0.5, 0.7 ),
	q = qs ),
	this.type1.type = 2, continuous = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.324899, 1.412494, 1.231088 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.3, 0.5, 0.7 ), tolerance = 5e-7 )
expect_equal( a$is.uni.mean(), TRUE )
plot.waypoints.and.p( a,
	c( 0.3, 0.5, 0.7 ), qs, c( -3, 3 ) )
show.results()

# normal test (control option)
a$set.waypoints(
	data.frame(
	p = c( 0.3, 0.5, 0.7 ),
	q = qs ),
	this.type1.type = 2, continuous = TRUE, control = control )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.324821, 1.412394, 1.231034 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.2999898, 0.5000000, 0.7000054 ), tolerance = 5e-7 )
expect_equal( a$is.uni.mean(), TRUE )
show.results()

# normal test (diff.mean)
a$set.waypoints(
	data.frame(
	p = c( 0.3, 0.5, 0.7 ),
	q = qs ),
	this.type1.type = 2, continuous = TRUE, diff.mean = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.381321, 1.488917, 1.264602 ), tolerance = 5e-7 )
a$p( qs )
expect_equal( a$is.uni.mean(), FALSE )
plot.waypoints.and.p( a,
	c( 0.3, 0.5, 0.7 ), qs, c( -3, 3 ) )
show.results()

# normal test (diff.mean, control option)
a$set.waypoints(
	data.frame(
	p = c( 0.3, 0.5, 0.7 ),
	q = qs ),
	this.type1.type = 2, continuous = TRUE, diff.mean = TRUE, control = control )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.378619, 1.485238, 1.263033 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.3000307, 0.5000591, 0.7000246 ), tolerance = 5e-7 )
expect_equal( a$is.uni.mean(), FALSE )
show.results()

# normal test
qs <- c( qnorm( 0.3, 0.2, 0.7 ), 0.2, qnorm( 0.9, 0.2, 1.1 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.3, 0.5, 0.9 ),
	q = qs ),
	this.type1.type = 2, continuous = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.8858638, 0.7846082, 0.9766777 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.3, 0.5, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.3, 0.5, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test
a$set.waypoints(
	data.frame(
	p = c( 0.3, 0.6, 0.9 ),
	q = qs ),
	this.type1.type = 2, continuous = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.8666972, 0.7011611, 1.0053364 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.3, 0.6, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.3, 0.6, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test
a$set.waypoints(
	data.frame(
	p = c( 0.52, 0.6, 0.9 ),
	q = qs ),
	this.type1.type = 2, continuous = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.821245, 1.944652, 1.688844 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.52, 0.6, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.52, 0.6, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test - sharp center
qs <- c( qnorm( 0.3, -0.2, 0.7 ), -0.2, qnorm( 0.9, -0.2, 1 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.3, 0.5, 0.9 ),
	q = qs ),
	this.type1.type = 2, symmetric = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9863575, 0.9863575, 0.9863575 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.3, 0.5, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.3, 0.5, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test (control option)
a$set.waypoints(
	data.frame(
	p = c( 0.3, 0.5, 0.9 ),
	q = qs ),
	this.type1.type = 2, symmetric = TRUE, control = control )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9864258, 0.9864258, 0.9864258 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.2999911, 0.5000000, 0.8999857 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.3, 0.5, 0.9 ), qs, c( -3, 3 ) )
show.results()

## Where symmetric distribution,
## cgd:::v.cont value with get.lv/get.uv = TRUE is "worse" than the lsd/usd field value.
## The theoretical maximum error of cgd:::v.cont (vs lsd/usd) is sqrt( .Machine$double.eps ).
## However, almost always, all values of them are equal.
expect_equal( abs( a$lsd - sqrt( cgd:::v.cont( 2, a$intervals.mean(), a$intervals.sd(),
									symmetric = TRUE, get.lv = TRUE ) * 2 ) ) <= sqrt( .Machine$double.eps ), TRUE )
expect_equal( abs( a$usd - sqrt( cgd:::v.cont( 2, a$intervals.mean(), a$intervals.sd(),
									symmetric = TRUE, get.uv = TRUE ) * 2 ) ) <= sqrt( .Machine$double.eps ), TRUE )

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.3, 0.49, 0.9 ),
		q = qs ),
		this.type1.type = 2, symmetric = TRUE ),
		"The point of p = 0.5 must be given" )

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.3, 0.5, 0.9 ),
		q = qs ),
		this.type1.type = 2, symmetric = TRUE, diff.mean = TRUE ),
		"Illegal options for diff.mean = TRUE" )

# normal test - dent center
qs <- c( qnorm( 0.6, 0, 1.2 ), qnorm( 0.9, 0, 1.05 ), 0 )
a$set.waypoints(
	data.frame(
	p = c( 0.6, 0.9, 0.5 ),
	q = qs ),
	this.type1.type = 2, symmetric = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.044687, 1.044687, 1.044687 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.6, 0.9, 0.5 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.6, 0.9, 0.5 ), qs, c( -3, 3 ) )
show.results()

# normal test (control option)
a$set.waypoints(
	data.frame(
	p = c( 0.6, 0.9, 0.5 ),
	q = qs ),
	this.type1.type = 2, symmetric = TRUE, control = control )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.044729, 1.044729, 1.044729 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.6000152, 0.8999913, 0.5000000 ), tolerance = 5e-7 )
show.results()

# Error case - bug fix (for v1.3.7, and up)
qs <- c( qnorm( 0.1, 0, 1.8 ), 0, qnorm( 0.4, 0, 1.05 ) )
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.4, 0.5 ),
		q = qs ),
		this.type1.type = 2, symmetric = TRUE ),
		"Order of q is not along with that of p" )

# normal test - sharp center - bug fix (for v1.3.7, and up)
qs <- c( qnorm( 0.1, 0, 1.8 ), 0, qnorm( 0.4, 0, 1.05 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.5, 0.4 ),
	q = qs ),
	this.type1.type = 2, symmetric = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.76902, 1.76902, 1.76902 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.5, 0.4 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.4 ), qs, c( -3, 3 ) )
show.results()

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.5, 0.6, 0.9 ),
		q = c( 0, 0.9, 1.9 ) ),
		this.type1.type = 2, symmetric = TRUE ),
		"Failed to construct" )


# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.5, 0.6 ),
		q = c( 0, 0.9 ) ),
		this.type1.type = 3 ),
		"Illegal number of quantiles" )

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.2, 0.4, 0.5, 0.6, 0.8, 0.9 ),
		q = c( -1.2, -1, -0.35, 0, 0.34, 0.979, 1.14 ) ),
		this.type1.type = 3 ),
		"Illegal number of quantiles" )

# normal test
qs <- c( qnorm( 0.1, 0, 1.2 ), qnorm( 0.4, 0, 0.9 ), 0 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.4, 0.5 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.0850150, 1.1821218, 0.9783164 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5 ), qs, c( -3, 3 ) )
show.results()

# normal test (control option)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.4, 0.5 ),
	q = qs ),
	this.type1.type = 3, control = control )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.085022, 1.182105, 0.978352 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.09999746, 0.40000590, 0.50000000 ), tolerance = 5e-7 )
show.results()

# normal test
qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.4, 0, 0.7 ), 0 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.4, 0.5 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.8228291, 0.8877917, 0.7522776 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )
expect_equal( a$is.uni.mean(), TRUE )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( 0, qnorm( 0.6, 0, 0.7 ), qnorm( 0.9, 0, 0.9 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.5, 0.6, 0.9 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.8228291, 0.7522776, 0.8877917 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.5, 0.6, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.5, 0.6, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( 0, qnorm( 0.6, 0, 0.9 ), qnorm( 0.9, 0, 1.2 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.5, 0.6, 0.9 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.0850150, 0.9783164, 1.1821218 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.5, 0.6, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.5, 0.6, 0.9 ), qs, c( -3, 3 ) )
show.results()


# normal test for v2.3.0, and up
qs <- c( qnorm( 0.4, 0, 0.7 ), qnorm( 0.6, 0, 0.7 ), qnorm( 0.9, 0, 0.9 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.4, 0.6, 0.9 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.7928474, 0.7043016, 0.8724524 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.4, 0.6, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.4, 0.6, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test for v2.3.0, and up
qs <- c( 0, qnorm( 0.6, 0, 0.9 ), qnorm( 0.9, 0, 1.2 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.4, 0.6, 0.9 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9763717, 0.8543389, 1.0847619 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.4, 0.6, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.4, 0.6, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test for v2.3.0, and up
qs <- c( qnorm( 0.1, 0, 1.5 ), qnorm( 0.3, 0, 1 ), qnorm( 0.6, 0, 0.8 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.6 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.567369, 1.560106, 1.574599 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.3, 0.6 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.6 ), qs, c( -3, 3 ) )
show.results()


# normal test
qs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.7, 0, 0.9 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.5, 0.7 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.022428, 1.022428, 1.022428 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( qs ), c( 0.1, 0.5, 0.7 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.7 ), qs, c( -3, 3 ) )
show.results()

# normal test (control option)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.5, 0.7 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE, control = control )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.02251, 1.02251, 1.02251 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( qs ), c( 0.1000117, 0.5000000, 0.700004 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
show.results()

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.9 ), qnorm( 0.4, 0, 0.85 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.4 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.102772, 1.076883, 1.128066 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.3, 0.4 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
expect_equal( a$is.uni.mean(), FALSE )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.4 ), qs, c( -3, 3 ) )
show.results()

# normal test (control option)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.4 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE, control = control )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.102147, 1.076373, 1.127331 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.09994926, 0.30000000, 0.40000054 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
expect_equal( a$is.uni.mean(), FALSE )
show.results()

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.9, 0, 1 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.5, 0.9 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1, 1, 1 ) )
expect_equal( a$p( qs ), c( 0.1, 0.5, 0.9 ), tolerance = 5e-7 )
expect_equal( a$kind, "Normal Distribution" )
expect_equal( a$is.v.grad(), TRUE )
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.9 ), qs, c( -3, 3 ) )
show.results()


# normal test for v2.3.0, and up
qs <- c( -0.479, -0.472, -0.215 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.7 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.1543608, 0.1457587, 0.1625082 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.3, 0.7 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.7 ), qs, c( -3, 3 ) )
show.results()

# normal test for v2.3.0, and up
# This is an extreme case that large differences of mean and sd (vs integrate) may occur.
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.32 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 7.395600, 6.996260, 7.774455 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.3, 0.32 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.32 ), qs, c( -3, 3 ) )
show.results( is.extreme.case = TRUE )

# normal test for v2.3.0, and up
qs <- c( 0.215, 0.217, 0.472 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.12, 0.3 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.3273484, 0.3531581, 0.29932145 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.12, 0.3 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.12, 0.3 ), qs, c( -3, 3 ) )
show.results()

# normal test for v2.3.0, and up
a$set.waypoints(
	data.frame(
	p = c( 0.6, 0.62, 0.8 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9436589, 0.5608163, 1.2109787 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.6, 0.62, 0.8 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.6, 0.62, 0.8 ), qs, c( -3, 3 ) )
show.results()

# Error case
qs <- c( -0.479, -0.472, 0.472 )
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.12, 0.2 ),
		q = qs ),
		this.type1.type = 3, v.grad = TRUE ),
		"Failed to construct" )


# Error case
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.9 ), qnorm( 0.4, 0, 0.85 ), 0, qnorm( 0.6, 0, 0.85 )	)
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.3, 0.4, 0.5, 0.6 ),
		q = qs ),
		this.type1.type = 3, v.grad = TRUE ),
		"Illegal number of quantiles" )

# normal test for v2.2.0, and up
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.9 ), 0, qnorm( 0.6, 0, 0.85 )  )
	a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.6 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9424154, 0.9689216, 0.9151417 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.3, 0.5, 0.6 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5, 0.6 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.9 ), qnorm( 0.4, 0, 0.85 ), qnorm( 0.6, 0, 0.85 )  )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.4, 0.6 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.8949887, 0.9356703, 0.8523677 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.3, 0.4, 0.6 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.4, 0.6 ), qs, c( -3, 3 ) )
show.results()

# normal test (control option)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.4, 0.6 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE, control = control )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.8948516, 0.9355167, 0.8522484 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.09998827, 0.30007279, 0.40003980, 0.59996960 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
show.results()

# normal test
qs <- c( qnorm( 0.6, 0, 1 ), qnorm( 0.7, 0, 1.01 ), qnorm( 0.8, 0, 1.1 ), qnorm( 0.9, 0, 1.12 )  )
a$set.waypoints(
	data.frame(
	p = c( 0.6, 0.7, 0.8, 0.9 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.8793983, 0.8118007, 0.9421584 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.6, 0.7, 0.8, 0.9 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
plot.waypoints.and.p( a,
	c( 0.6, 0.7, 0.8, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.85 ), qnorm( 0.6, 0, 0.85 )  )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.4, 0.6 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.083355, 1.063918, 1.102449 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.4, 0.6 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
expect_equal( a$is.uni.mean(), FALSE )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.6 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.85 ), 0 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.4, 0.5 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.029705, 1.029705, 1.029705 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
expect_equal( a$is.uni.mean(), TRUE )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5 ), qs, c( -3, 3 ) )
show.results()

## Where symmetric distribution (type1.type = 3, is.v.grad() = TRUE and is.uni.mean() = TRUE is symmetric),
## cgd:::v.cont value with get.lv/get.uv = TRUE is "worse" than the lsd/usd field value.
## The theoretical maximum error of cgd:::v.cont (vs lsd/usd) is sqrt( .Machine$double.eps ).
## However, almost always, all values of them are equal.
expect_equal( abs( a$lsd - sqrt( cgd:::v.cont( 3, a$intervals.mean(), a$intervals.sd(),
								symmetric = TRUE, get.lv = TRUE ) * 2 ) ) <= sqrt( .Machine$double.eps ), TRUE )
expect_equal( abs( a$usd - sqrt( cgd:::v.cont( 3, a$intervals.mean(), a$intervals.sd(),
								symmetric = TRUE, get.uv = TRUE ) * 2 ) ) <= sqrt( .Machine$double.eps ), TRUE )

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.9 ), 0 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.4, 0.5 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.051783, 1.040166, 1.063274 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
expect_equal( a$is.uni.mean(), FALSE )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.8 ), 0 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.275515, 1.185610, 1.359487 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.3, 0.5 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
expect_equal( a$is.uni.mean(), FALSE )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( 0, qnorm( 0.7, 0, 0.8 ), qnorm( 0.9, 0, 1 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.5, 0.7, 0.9 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.275515, 1.359487, 1.185610 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.5, 0.7, 0.9 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
expect_equal( a$is.uni.mean(), FALSE )
plot.waypoints.and.p( a,
	c( 0.5, 0.7, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( 0, 0.7, 0.9 )
a$set.waypoints(
	data.frame(
	p = c( 0.5, 0.7, 0.9 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.8114439, 0.8587942, 0.7611536 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.5, 0.7, 0.9 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
expect_equal( a$is.uni.mean(), FALSE )
plot.waypoints.and.p( a,
	c( 0.5, 0.7, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( 0, 0.7, 0.9 )
a$clear()
a$set.waypoints(
	data.frame(
	p = c( 0.5, 0.7, 0.9 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.8114439, 0.8587942, 0.7611536 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.5, 0.7, 0.9 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
expect_equal( a$is.uni.mean(), FALSE )
plot.waypoints.and.p( a,
	c( 0.5, 0.7, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.8 ), 0 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.774394, 1.463498, 2.038412 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.3, 0.5 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
expect_equal( a$is.uni.mean(), FALSE )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5 ), qs, c( -3, 3 ) )
show.results()

plot( seq( -8, 8, 0.01 ), a$d( seq( -8, 8, 0.01 ) ), type = "l" )

# normal test
qs <- c( -0.7, -0.5, 0 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.6255090, 0.5859148, 0.6627419 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.3, 0.5 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
expect_equal( a$is.uni.mean(), FALSE )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( -0.7, -0.5, 0 )
a$clear()
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.6255090, 0.5859148, 0.6627419 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.3, 0.5 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
expect_equal( a$is.uni.mean(), FALSE )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( -1.7, -0.5, 0 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.871951, 1.686569, 2.040560 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.3, 0.5 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
expect_equal( a$is.uni.mean(), FALSE )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( -1.7, -0.5, 0 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.396233, 1.396233, 1.396233 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( qs ), c( 0.1, 0.3, 0.5 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
expect_equal( a$is.uni.mean(), TRUE )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.8 ), 0 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.022333, 1.022333, 1.022333 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( qs ), c( 0.1, 0.3, 0.5 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
expect_equal( a$is.uni.mean(), TRUE )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5 ), qs, c( -3, 3 ) )
show.results()


# normal test for v2.3.0, and up
qs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.6, 0, 0.85 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.5, 0.6 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.018605, 1.022299, 1.014897 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.5, 0.6 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), TRUE )
expect_equal( a$is.uni.mean(), FALSE )
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.6 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.7, 0, 0.9 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.5, 0.7 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9273588, 0.9714121, 0.8811057 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.5, 0.7 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), FALSE )
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.7 ), qs, c( -3, 3 ) )
show.results()

# normal test (control option)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.5, 0.7 ),
	q = qs ),
	this.type1.type = 3, control = control )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9273600, 0.9714280, 0.8810906 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1000050, 0.5000000, 0.7000025 ), tolerance = 5e-7 )
expect_equal( a$is.v.grad(), FALSE )
show.results()

# normal test for v1.3.5, and up
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.9 ), qnorm( 0.4, 0, 0.87 ), 0 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.4, 0.5 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9189759, 0.9524046, 0.8842844 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.3, 0.4, 0.5 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.4, 0.5 ), qs, c( -3, 3 ) )
show.results()

# normal test (also)
qs <- c( 0.5, qnorm( 0.6, 0.5, 0.9 ), qnorm( 0.75, 0.52, 1 ), qnorm( 0.9, 0.57, 1.1 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.5, 0.6, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9367179, 0.8612149, 1.0065733 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.5, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.5, 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( -1, -0.1, 0.2, 1.3 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.4, 0.6, 0.9 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9461550, 0.8933198, 0.9961919 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.4, 0.6, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.6, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( qnorm( 0.3, 0, 0.2 ), 0, qnorm( 0.6, 0, 0.7 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.3, 0.5, 0.6 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.8291401, 0.6306881, 0.9885238 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.3, 0.5, 0.6 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.3, 0.5, 0.6 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( -1, 0, 0.5 )
a$set.waypoints(
	data.frame(
	p = c( 0.499, 0.5, 0.6 ),
	q = qs ), this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 486.4020, 581.6453, 367.2364 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.499, 0.5, 0.6 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.499, 0.5, 0.6 ), qs, c( -3, 3 ) )
show.results()

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.0001, 0.5, 0.7 ),
		q = c( -0.01, 0, 0.25 ) ),
		this.type1.type = 3 ),
		"Failed to construct" )

# normal test
qs <- c( -2, -0.5, 0 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.311066, 1.506895, 1.080305 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.3, 0.5 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5 ), qs, c( -3, 3 ) )
show.results()

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.1002, 0.5 ),
		q = c( -2, -0.001, 0 ) ),
		this.type1.type = 3 ),
		"Failed to construct" )

# normal test
qs <- c( 0, 0.2, 1.9 )
a$set.waypoints(
	data.frame(
	p = c( 0.5, 0.6, 0.9 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.2085623, 0.9596986, 1.4142929 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.5, 0.6, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.5, 0.6, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( 1, 1.2, 2.9 )
a$set.waypoints(
	data.frame(
	p = c( 0.5, 0.6, 0.9 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.2085623, 0.9596986, 1.4142929 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.5, 0.6, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.5, 0.6, 0.9 ), qs, c( -3, 3 ) )
show.results()

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.5, 0.6, 0.9 ),
		q = c( 0, 1.2, 2.9 ) ),
		this.type1.type = 3 ),
		"Failed to construct" )

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.4, 0.5 ),
		q = c( -2.9, -1.2, 0 ) ),
		this.type1.type = 3 ),
		"Failed to construct" )

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.5, 0.6 ),
		q = c( -2.9, -1.2, 0 ) ),
		this.type1.type = 3 ),
		"Failed to construct" )

# normal test
qs <- c( -2, -0.5, 0, 0.7 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.8 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.2595387, 1.4798423, 0.9914344 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.3, 0.5, 0.8 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5, 0.8 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( -2, -0.5, 0, 0.5 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.7 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.640214, 1.640214, 1.640213 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.3, 0.5, 0.7 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5, 0.7 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( -4, -2.5, -2, -1.5 )
	a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.3, 0.5, 0.7 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.640214, 1.640214, 1.640213 ), tolerance = 5e-7 )
expect_equal( a$intervals[[1]]$sd == a$intervals[[3]]$sd, FALSE )
expect_equal( a$p( qs ), c( 0.1, 0.3, 0.5, 0.7 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5, 0.7 ), qs, c( -8, 4 ) )
show.results()

# normal test
qs <- c( -1.5, -0.4, 0, 0.5 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.4, 0.5, 0.7 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.119693, 1.120214, 1.119173 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.4, 0.5, 0.7 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5, 0.7 ), qs, c( -3, 3 ) )
show.results()

# This test should be performed after the previous test immediately (to use the previous result).
sd.2 <- a$intervals[[2]]$sd
qs <- c( -1.5, -0.4, 0, qnorm( 0.7, 0, sd.2 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.4, 0.5, 0.7 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.458235, 1.348544, 1.560232 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.4, 0.5, 0.7 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5, 0.7 ), qs, c( -3, 3 ) )
show.results()

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.3, 0.5, 0.7 ),
		q = c( -2, -0.5, 0, 0.7 ) ),
		this.type1.type = 3 ),
		"Failed to construct" )

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.3, 0.5, 0.7 ),
		q = c( -2, -0.5, 0, 0.1 ) ),
		this.type1.type = 3 ),
		"Failed to construct" )

# normal test
qs <- c( -2, 0, 0.1, 2.3 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.5, 0.6, 0.9 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.694588, 1.610757, 1.7744622 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.6, 0.9 ), qs, c( -3, 3 ) )
show.results()

# This test should be performed after the previous test immediately (to use the previous result).
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.5, 0.6, 0.9 ),
	q = qs ),
	this.type1.type = 3, diff.mean = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.692928, 1.644191, 1.740301 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.6, 0.9 ), qs, c( -3, 3 ) )
show.results()

# This test should be performed after the previous test immediately (to use the previous result).
sd.2 <- a$intervals[[2]]$sd
qs <- c( qnorm( 0.1, 0, sd.2 ), 0, 0.1, 2.3 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.5, 0.6, 0.9 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.2550168, 0.7392928, 1.6135614 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.5, 0.6, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.6, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( 2.3, -1.5, 0, 0.3 )
a$set.waypoints(
	data.frame(
	p = c( 0.9, 0.1, 0.5, 0.6 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.559393, 1.338481, 1.752678 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.9, 0.1, 0.5, 0.6 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.9, 0.1, 0.5, 0.6 ), qs, c( -3, 3 ) )
show.results()

# Compare to above test
qs <- c( 0, 0.3, 2.3 )
a$set.waypoints(
	data.frame(
	p = c( 0.5, 0.6, 0.9 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.560729, 1.340740, 1.753330 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.5, 0.6, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.5, 0.6, 0.9 ), qs, c( -3, 3 ) )
show.results()


# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.4, 0.5, 0.6, 0.9 ),
		q = c( -0.2, 0, 0.1, 2.3 ) ),
		this.type1.type = 3 ),
		"Failed to construct" )

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.04, 0.5, 0.6, 0.9 ),
		q = c( -0.2, 0, 0.1, 2.3 ) ),
		this.type1.type = 3 ),
		"Failed to construct" )

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.4, 0.5, 0.9 ),
		q = c( -2.3, -0.1, 0, 0.2 ) ),
		this.type1.type = 3 ),
		"Failed to construct" )

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.4, 0.5, 0.51 ),
		q = c( -2.3, -0.1, 0, 0.2 ) ),
		this.type1.type = 3 ),
		"Failed to construct" )

# normal test- bug fix (for v.2.3.0, and up)
qs <- c( -2.3, -0.2, 0, 0.2 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.4, 0.5, 0.6 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.851449, 1.851450, 1.851448 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.4, 0.5, 0.6 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5, 0.6 ), qs, c( -3, 3 ) )
show.results()

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.2, 0.4, 0.6, 0.9 ),
		q = c( -2.8, -0.1, 0.1, 2.3 ) ),
		this.type1.type = 3 ),
		"Failed to construct" )

## uni.sigma

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.5, 0.6 ),
		q = c( -1, 0, 0.5 ) ),
		this.type1.type = 1, continuous = TRUE, uni.sigma = TRUE ),
		"Illegal number of quantiles" )

# Error case
qs <- c( -2, -0.31, 0.29, 1.9 )
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.4, 0.6, 0.9 ),
		q = qs ),
		this.type1.type = 2, continuous = TRUE, uni.sigma = TRUE ),
		"Illegal number of quantiles" )

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.25, 0.5, 0.75 ),
		q = c( -2, 0, 2.1 ) ),
		this.type1.type = 0, continuous = TRUE, uni.sigma = TRUE ),
		"Illegal number of quantiles" )

# Error case
qs <- c( -2, -0.31, 0.29, 1.9 )
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.4, 0.6, 0.9 ),
		q = qs ),
		this.type1.type = 3, v.grad = TRUE, uni.sigma = TRUE ),
		"Illegal number of quantiles" )

# Error case
qs <- c( -2, -0.31, 0, 0.29, 1.9 )
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.4, 0.5, 0.6, 0.9 ),
		q = qs ),
		this.type1.type = 3, uni.sigma = TRUE ),
		"Illegal number of quantiles" )

# Error case
qs <- c( -2, -1, -0.31, 0.29, 0.95, 1.9 )
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
		q = qs ),
		this.type1.type = 4, uni.sigma = TRUE ),
		"Illegal number of quantiles" )

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.25, 0.5, 0.75 ),
		q = c( -2, 0, 2.1 ) ),
		this.type1.type = 2, continuous = TRUE, uni.sigma = TRUE ),
		"Failed to construct" )

# normal test
qs <- c( -0.584, 0, 0.291 )
a$set.waypoints(
	data.frame(
	p = c( 0.3, 0.5, 0.6 ),
	q = qs ),
	this.type1.type = 2, continuous = TRUE, uni.sigma = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9982686, 0.9982686, 0.9982686 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( qs ), c( 0.3, 0.5, 0.6 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.3, 0.5, 0.6 ), qs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (without uni.sigma; Compare to above)
a$set.waypoints(
	data.frame(
	p = c( 0.3, 0.5, 0.6 ),
	q = qs ),
	this.type1.type = 2, continuous = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.140537, 1.118273, 1.162375 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.3, 0.5, 0.6 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.3, 0.5, 0.6 ), qs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# normal test
qs <- c( -2, 0, 1.8 )
a$set.waypoints(
	data.frame(
	p = c( 0.25, 0.5, 0.7 ),
	q = qs ),
	this.type1.type = 2, continuous = TRUE, uni.sigma = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 2.171117, 2.171117, 2.171117 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( qs ), c( 0.25, 0.5, 0.7 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.25, 0.5, 0.7 ), qs, c( -3, 3 ) )
show.results()

plot( seq( -8, 8, 0.01 ), a$d( seq( -8, 8, 0.01 ) ), type = "l" )
plot( seq( -8, 8, 0.01 ), a$p( seq( -8, 8, 0.01 ) ), type = "l" )
plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" )
sample <- a$r( 1000 ); hist( sample )

# normal test (control option)
a$set.waypoints(
	data.frame(
	p = c( 0.25, 0.5, 0.7 ),
	q = qs ),
	this.type1.type = 2, continuous = TRUE, uni.sigma = TRUE, control = control )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 2.171191, 2.171191, 2.171191 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( qs ), c( 0.2500214, 0.5000000, 0.6999782 ), tolerance = 5e-7 )
show.results()

# normal test
qs <- c( qnorm( 0.25, 0, 1 ), 0, qnorm( 0.7, 0, 1.02 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.25, 0.5, 0.7 ),
	q = qs ),
	this.type1.type = 2, continuous = TRUE, uni.sigma = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9156116, 0.9156116, 0.9156116 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( qs ), c( 0.25, 0.5, 0.7 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.25, 0.5, 0.7 ), qs, c( -3, 3 ) )
show.results()

# normal test for v2.3.0, and up
qs <- c( qnorm( 0.2, 0, 1 ), qnorm( 0.4, 0, 1.01 ), qnorm( 0.8, 0, 1.05 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.2, 0.4, 0.8 ),
	q = qs ),
	this.type1.type = 2, continuous = TRUE, uni.sigma = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9637276, 0.9637276, 0.9637276 ), tolerance = 5e-7 )
expect_equal( c( a$sd, a$sd ), c( a$lsd, a$usd ) )
expect_equal( a$p( qs ), c( 0.2, 0.4, 0.8 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.2, 0.4, 0.8 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( qnorm( 0.2, 0, 1 ), qnorm( 0.4, 0, 1.01 ), qnorm( 0.8, 0, 1.05 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.2, 0.4, 0.8 ),
	q = qs ),
	this.type1.type = 2, continuous = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.025681, 1.011411, 1.039755 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.2, 0.4, 0.8 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.2, 0.4, 0.8 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( qnorm( 0.1, -0.2, 1 ), qnorm( 0.4, 0, 1 ), 0 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.4, 0.5 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE, uni.sigma = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.0172012, 1.0672460, 0.9645634 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5 ), qs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (without uni.sigma; Compare to above)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.4, 0.5 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.186791, 1.186791, 1.186791 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.4, 0.5 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5 ), qs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# normal test
qs <- c( qnorm( 0.1, -0.2, 1 ), qnorm( 0.4, 0, 1 ), qnorm( 0.6, 0.1, 1 ), qnorm( 0.8, 0.4, 1 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.4, 0.6, 0.8 ),
	q = qs ),
	this.type1.type = 3, uni.sigma = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.380724, 1.292676, 1.463485 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.4, 0.6, 0.8 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.6, 0.8 ), qs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# normal test (without uni.sigma; Compare to above)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.4, 0.6, 0.8 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 2.384632, 1.674935, 2.927035 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.4, 0.6, 0.8 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.6, 0.8 ), qs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case (to show that uni.sigma = TRUE option is very hard to convergent)
qs <- c( qnorm( 0.1, -0.2, 0.8 ), qnorm( 0.4, 0, 0.9 ), qnorm( 0.6, 0.1, 1 ), qnorm( 0.8, 0.4, 1.1 ) )
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.4, 0.6, 0.8 ),
		q = qs ),
		this.type1.type = 3, uni.sigma = TRUE ),
		"Failed to construct" )

# normal test (without uni.sigma; Compare to above)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.4, 0.6, 0.8 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 3.558481, 2.123352, 4.562560 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.4, 0.6, 0.8 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.6, 0.8 ), qs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


# normal test
qs <- c( -1.464921, -0.280095, 0.027170, 0.352307, 1.201652 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.4, 0.5, 0.6, 0.8 ),
	q = qs ),
	this.type1.type = 4, uni.sigma = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.312522, 1.255553, 1.367119 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.4, 0.5, 0.6, 0.8 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5, 0.6, 0.8 ), qs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

## intervals has 6 elements where type1.type = 4
expect_equal( a$intervals.p.ind(),
				data.frame( p.ind.1 = c( 0, 0.4990328, 1, 0, 0.4990396, 1 ),
							p.ind.2 = c( 0, 0.4990328, 1, 0, 0.4990396, 1 ) ),
				tolerance = 5e-7 )
expect_equal( a$intervals.p.ind()[c( 1, 3, 4, 6 ), 1], c( 0, 1, 0, 1 ) )
expect_equal( a$intervals.p.ind()[c( 1, 3, 4, 6 ), 2], c( 0, 1, 0, 1 ) )

expect_equal( a$intervals.q.ind(),
				data.frame( q.ind.1 = c( -Inf, 0.1756244, Inf, -Inf, -0.1315700, Inf ),
							q.ind.2 = c( -Inf, 0.1756244, Inf, -Inf, -0.1315700, Inf ) ),
				tolerance = 5e-7 )

# normal test (without uni.sigma; Compare to above)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.4, 0.5, 0.6, 0.8 ),
	q = qs ),
	this.type1.type = 4 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.321844, 1.250002, 1.389979 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.4, 0.5, 0.6, 0.8 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5, 0.6, 0.8 ), qs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()

# Error case (to show that uni.sigma = TRUE option is very hard to convergent)
qs <- round( qs, 3 )
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.4, 0.5, 0.6, 0.8 ),
		q = qs ),
		this.type1.type = 4, uni.sigma = TRUE, control = list( maxit = 2000 ) ),
		"Failed to construct" )

# normal test (without uni.sigma; Compare to above)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.4, 0.5, 0.6, 0.8 ),
	q = qs ),
	this.type1.type = 4 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.322447, 1.250436, 1.390735 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.4, 0.5, 0.6, 0.8 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5, 0.6, 0.8 ), qs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
show.results()


##	4 waypoints for type1.type = 2
# normal test
qs <- c( qnorm( 0.1, 0, 1.1 ), qnorm( 0.25, 0, 1 ), qnorm( 0.75, 0, 1.05 ), qnorm( 0.9, 0, 1.06 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 2, continuous = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.130475, 1.166446, 1.093321 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.25, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test (control option)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 2, continuous = TRUE, control = control )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.130699, 1.166624, 1.093594 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1000118, 0.2500168, 0.7499932, 0.8999589 ), tolerance = 5e-7 )
show.results()

# normal test
qs <- c( -1.7, -0.2, 0.3, 1.9 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 2, continuous = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.523424, 1.533391, 1.513392 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.25, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

plot( seq( 0.4, 0.6, 0.001 ), a$q( seq( 0.4, 0.6, 0.001 ) ), type = "l" )
plot( seq( 0.01, 0.06, 0.001 ), a$q( seq( 0.01, 0.06, 0.001 ) ), type = "l" )

# normal test for v2.3.0, and up
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.5, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 2, continuous = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.547907, 1.432889, 1.654950 ), tolerance = 5e-7 )
expect_equal( a$p( qs ),  c( 0.1, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

## 5 or 6 waypoints for type1.type = 3
# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
		q = c( qnorm( 0.1, 0, 1.12 ), qnorm( 0.25, 0, 1.11 ), qnorm( 0.5, 0, 1 ),
			   qnorm( 0.75, 0, 1.05 ), qnorm( 0.9, 0, 1.06 ) ) ),
		this.type1.type = 3, symmetric = TRUE ),
		"Illegal number of quantiles" )

# normal test
qs <- c( qnorm( 0.1, 0, 1.12 ), qnorm( 0.25, 0, 1.11 ), qnorm( 0.5, 0, 1 ), qnorm( 0.75, 0, 1.05 ), qnorm( 0.9, 0, 1.06 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.092465, 1.096976, 1.087935 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test (control option)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 3, control = control )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.092589, 1.097010, 1.088149 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1000345, 0.2500480, 0.5000082, 0.7500175, 0.8999795 ), tolerance = 5e-7 )
show.results()

# normal test
qs <- c( -1.4, -0.7, 0, 0.75, 1.42 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.115314, 1.138797, 1.091325 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( -0.4, 0.3, 1, 1.75, 2.42 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.115314, 1.138797, 1.091325 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.75, 0.9 ), qs, c( -2, 4 ) )
show.results()

# normal test
qs <- c( -1.412, -0.262, 0.272, 0.756, 1.427 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.4, 0.6, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.106102, 1.126407, 1.085419 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.4, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( -1.4, -0.1, 0.1, 0.75, 1.42 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.45, 0.62, 0.84, 0.92 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.812762, 2.296927, 1.138570 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.45, 0.62, 0.84, 0.92 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.45, 0.62, 0.84, 0.92 ), qs, c( -3, 3 ) )
show.results()

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
		q = qs ),
		this.type1.type = 3 ),
		"Failed to construct" )


# normal test
qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
			qnorm( 0.4, 0, 0.98 ), qnorm( 0.6, 0, 1.03 ),
			qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9896027, 0.9505782, 1.0271455 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test (control option)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 3, control = control )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.9896004, 0.9505648, 1.0271535 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.0999907, 0.2499966, 0.4000068, 0.5999995, 0.7499936, 0.8999972 ), tolerance = 5e-7 )
show.results()

# normal test
qs <- c( qnorm( 0.1, 0, 0.8 ), qnorm( 0.25, 0, 0.85 ),
			qnorm( 0.4, 0, 0.9 ), qnorm( 0.6, 0, 1.2 ),
			qnorm( 0.75, 0, 1.21 ), qnorm( 0.9, 0, 1.23 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 3 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.0203153, 0.9379232, 1.0965340 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

## type1.type == 4

# Error case
expect_error( expect_warning( a$set.waypoints(
		data.frame(
		p = c( 0.5 ),
		q = c( 0 ) ),
		this.type1.type = 4 ),
		"No waypoints other than" ),
		"Illegal number of quantiles" )

# Error case
qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
			qnorm( 0.4, 0, 0.98 ), qnorm( 0.6, 0, 1.03 ),
			qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.25, 0.3, 0.4, 0.75, 0.9 ),
		q = qs ),
		this.type1.type = 4 ),
		"Failed to construct" )

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.25, 0.6, 0.7, 0.75, 0.9 ),
		q = qs ),
		this.type1.type = 4 ),
		"Failed to construct" )

# normal test for v2.3.0, and up
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.5, 0.6, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 4 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.0386813, 0.9576588, 1.1138256 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.25, 0.5, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

# Error case
qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
			qnorm( 0.5, 0, 0.98 ), qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.5, 0.6, 0.75, 0.9 ),
		q = qs ),
		this.type1.type = 4 ),
		"Failed to determine the drop-in parameters of the lower distribution" )

# Error case
qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
			qnorm( 0.5, 0, 0.98 ), qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.25, 0.4, 0.5, 0.9 ),
		q = qs ),
		this.type1.type = 4 ),
		"Failed to determine the drop-in parameters of the upper distribution" )

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.25, 0.4, 0.6, 0.9 ),
		q = qs ),
		this.type1.type = 4 ),
		"Failed to determine the drop-in parameters of the upper distribution" )

# Error case
qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ), qnorm( 0.4, 0, 0.98 ),
		 qnorm( 0.5, 0, 0.98 ),
		 qnorm( 0.6, 0, 1.03 ), qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.25, 0.5, 0.55, 0.6, 0.75, 0.9 ),
		q = qs ),
		this.type1.type = 4 ),
		"Failed to construct" )

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.25, 0.4, 0.45, 0.5, 0.75, 0.9 ),
		q = qs ),
		this.type1.type = 4 ),
		"Failed to construct" )

# Error case
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.25, 0.4, 0.45, 0.55, 0.75, 0.9 ),
		q = qs ),
		this.type1.type = 4 ),
		"Failed to construct" )

# Error case
qs <- c( qnorm( 0.25, 0, 0.9 ), qnorm( 0.5, 0, 0.95 ), qnorm( 0.75, 0, 1.1 ) )
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.25, 0.5, 0.75 ),
		q = qs ),
		this.type1.type = 4 ),
		"Illegal number of quantiles" )

# Error case
qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
			qnorm( 0.6, 0, 1.03 ), qnorm( 0.75, 0, 1.1 ) )
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.25, 0.75, 0.9 ),
		q = qs ),
		this.type1.type = 4 ),
		"Illegal number of quantiles" )

# normal test
qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
			qnorm( 0.5, 0, 1 ),
			qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 4 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.0099747, 0.9593477, 1.0581823 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.75, 0.9 ), qs, c( -3, 3 ) )
expect_equal( a$is.conn.p( c( 0, 0.5, 1 ) ),
				list( bool = c( FALSE, TRUE, FALSE ), i.1 = c( 1, 1, 2 ), i.2 = c( NaN, 2, NaN ) ) )
expect_equal( a$is.ind.p( c( 0, 0.5, 1 ) ),
				list( bool = c( TRUE, FALSE, TRUE ), i = c( 1, NaN, 2 ) ) )
expect_equal( a$is.conn.q( c( -Inf, 0, Inf ) ),
				list( bool = c( FALSE, TRUE, FALSE ), i.1 = c( 1, 1, 2 ), i.2 = c( NaN, 2, NaN ) ) )
expect_equal( a$is.ind.q( c( -Inf, 0, Inf ) ),
				list( bool = c( TRUE, FALSE, TRUE ), i = c( 1, NaN, 2 ) ) )
show.results()

# normal test (control option)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 4, control = control )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.0099933, 0.9593534, 1.0582127 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1000013, 0.2499950, 0.5000000, 0.7500129, 0.8999999 ), tolerance = 5e-7 )
show.results()

# normal test
qs <- c( qnorm( 0.1, 0, 2 ), qnorm( 0.25, 0, 1.5 ),
			qnorm( 0.5, 0, 1 ),
			qnorm( 0.75, 0, 0.9 ), qnorm( 0.9, 0, 0.8 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 4 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.519464, 1.771948, 1.215624 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
expect_equal( a$is.uni.mean(), TRUE )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 4, diff.mean = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.517806, 1.763559, 1.223655 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
expect_equal( a$is.uni.mean(), FALSE )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( qnorm( 0.1, 0, 2 ), qnorm( 0.25, 0, 1.5 ),
			qnorm( 0.5, 0, 1 ),
			qnorm( 0.75, 0, 0.9 ), qnorm( 0.9, 0, 0.792 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 4 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.519549, 1.771140, 1.217014 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( qnorm( 0.1, 0, 0.792 ), qnorm( 0.25, 0, 0.9 ),
			qnorm( 0.5, 0, 1 ),
			qnorm( 0.75, 0, 1.5 ), qnorm( 0.9, 0, 2 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 4 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.519549, 1.217014, 1.771140 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test for v2.2.0, and up
qs <- c( qnorm( 0.1, 0, 2 ), qnorm( 0.25, 0, 1.5 ),
			qnorm( 0.5, 0, 1 ),
			qnorm( 0.75, 0, 0.95 ), qnorm( 0.9, 0, 0.792 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 4 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.535793, 1.810405, 1.199898 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test for v2.2.0, and up
qs <- c( qnorm( 0.1, 0, 0.792 ), qnorm( 0.25, 0, 0.95 ),
			qnorm( 0.5, 0, 1 ),
			qnorm( 0.75, 0, 1.5 ), qnorm( 0.9, 0, 2 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 4 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.513764, 1.179897, 1.786282 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.25, 0.5, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ), qnorm( 0.4, 0, 0.97 ),
			qnorm( 0.6, 0, 1.1 ), qnorm( 0.75, 0, 1.12 ), qnorm( 0.9, 0, 1.2 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 4 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.0561249, 0.9806857, 1.1265234 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

# Error case
qs <- c( qnorm( 0.1, 0, 0.6 ), qnorm( 0.25, 0, 0.95 ), qnorm( 0.4, 0, 0.97 ),
			qnorm( 0.6, 0, 1.1 ), qnorm( 0.75, 0, 1.12 ), qnorm( 0.9, 0, 1.3 ) )
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
		q = qs ),
		this.type1.type = 4 ),
		"Failed to construct" )

# Error case
qs <- c( -1, -0.7, -0.3, 0, 0.32, 0.9, 1.5 )
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
		q = qs ),
		this.type1.type = 4 ),
		"Failed to construct" )

# normal test (Baltan)
qs <- c( -1, -0.8, -0.3, 0, 0.32, 1, 1.2 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 4, control = list( maxit = 1000 ) )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.8389749, 0.8207566, 0.8568058 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

# Error case (This test should be performed after the previous test immediately)
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
		q = qs ),
		this.type1.type = 4, control = list( allowSingular = FALSE ) ),
		"Failed to construct" )

# normal test (sub1 of Baltan)
qs <- c( -1, -0.8, -0.3 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.4 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.0238190, 0.9605979, 1.0833571 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.25, 0.4 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.4 ), qs, c( -3, 3 ) )
show.results()

# normal test (sub2 of Baltan)
qs <- c( 0.32, 1, 1.2 )
a$set.waypoints(
	data.frame(
	p = c( 0.6, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 3, v.grad = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.294718, 1.368588, 1.216370 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

# Error case (Too straight points)
qs <- c( -1.2, -0.83, -0.31, 0, 0.32, 0.85, 1.2 )
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
		q = qs ),
		this.type1.type = 4 ),
		"Failed to construct" )

# normal test (Baltan Rev. by set.intervals)
qs <- c( -1, -0.8, -0.3, 0, 0.32, 1, 1.2 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9),
	q = qs ),
	this.type1.type = 4, control = list( maxit = 1000 ) )
intervals <- list( gen.t3.intervals(
						c( -a$intervals[[2]][[1]]$mean, -a$intervals[[2]][[2]]$mean, -a$intervals[[2]][[3]]$mean ),
						c( a$intervals[[2]][[1]]$sd, a$intervals[[2]][[2]]$sd, a$intervals[[2]][[3]]$sd ) ),
					gen.t3.intervals(
						c( -a$intervals[[1]][[1]]$mean, -a$intervals[[1]][[2]]$mean, -a$intervals[[1]][[3]]$mean ),
						c( a$intervals[[1]][[1]]$sd, a$intervals[[1]][[2]]$sd, a$intervals[[1]][[3]]$sd ) ) )

a$set.intervals( intervals )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 0.8389749, 0.8568058, 0.8207566 ), tolerance = 5e-7 )
a$p( c( -1.2, -1, -0.32, 0, 0.3, 0.8, 1 ) )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), -qs[order( -qs )], c( -3, 3 ) )
show.results()

# normal test (Baltan Rev.) (for success, it needs about 1000 iterations)
qs <- c( -1.2, -1, -0.32, 0, 0.3, 0.8, 1 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
	q = qs ),
this.type1.type = 4, control = list( maxit = 1000 ) )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.122107, 1.095699, 1.147907 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()


# normal test
# This qs is given by qs <- c( qnorm( 0.3, -0.2, 1 ), 0, qnorm( 0.7, 0.2, 0.8 ) );
# a$set.waypoints( data.frame( p = c( 0.3, 0.5, 0.7 ), q = qs ), this.type1.type = 2, continuous = TRUE );
# qs <- a$q( c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ) )
qs <- c( -1.9535092, -0.9569967, -0.3331447, 0, 0.3085147, 0.7858078, 1.4341828 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 4 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.326808, 1.415428, 1.231829 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test (control option)
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 4, control = control )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.327368, 1.416047, 1.232325 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1000971, 0.2500470, 0.4000137, 0.4999966, 0.5999756, 0.7499354, 0.8999094 ), tolerance = 5e-7 )
show.results()


# normal test
qs <- c( -1.4, -0.78, -0.3, 0, 0.32, 0.96, 2 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
	q = qs ),
	this.type1.type = 4 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.352093, 1.229626, 1.464354 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.10, 0.25, 0.40, 0.50, 0.60, 0.75, 0.90 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test (compare to above)
# This test should be performed after the previous test immediately (to use the previous result).
qs.2 <- a$q( c( 0.25, 0.5, 0.75 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.25, 0.5, 0.75 ),
	q = qs.2 ),
	this.type1.type = 2, continuous = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.326105, 1.227620, 1.417765 ), tolerance = 5e-7 )
expect_equal( a$p( qs.2 ), c( 0.25, 0.50, 0.75 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.103, 0.25, 0.402, 0.5, 0.596, 0.75, 0.903 ), tolerance = 1e-3 )
show.results()

# Error case
qs <- c( -1.4, -0.96, -0.61, -0.3, 0.32, 0.715, 1.24, 2 )
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.2, 0.3, 0.5, 0.6, 0.7, 0.8, 0.9 ),
		q = qs ),
		this.type1.type = 4, control = list( maxit = 2000 ) ),
		"Failed to construct" )

# Error case
qs <- c( -1.4, -0.96, -0.61, -0.3, 0.32, 0.715, 1.24, 2 )
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.2, 0.3, 0.35, 0.4, 0.7, 0.8, 0.9 ),
		q = qs ),
		this.type1.type = 4 ),
		"Failed to construct" )

# Error case
qs <- c( -1.4, -0.96, -0.61, -0.3, 0.32, 0.715, 1.24, 2 )
expect_error( a$set.waypoints(
		data.frame(
		p = c( 0.1, 0.2, 0.3, 0.55, 0.6, 0.7, 0.8, 0.9 ),
		q = qs ),
		this.type1.type = 4 ),
		"Failed to construct" )

# normal test
qs <- c( -1.4, -0.96, -0.61, -0.3, 0.32, 0.715, 1.24, 2 )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9 ),
	q = qs ),
	this.type1.type = 4 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.307786, 1.198102, 1.408958 ), tolerance = 5e-7 )
expect_equal( a$p( qs ), c( 0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9 ), qs, c( -3, 3 ) )
show.results()

# normal test
qs <- c( a$q( 0.1 ), a$q( 0.25 ), a$q( 0.4 ), a$q( 0.50 ), a$q( 0.6 ), a$q( 0.7 ), a$q( 0.8 ), a$q( 0.9 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ),
	q = qs ),
	this.type1.type = 4 )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.307786, 1.198102, 1.408958 ), tolerance = 5e-7 )
show.results()

# normal test (compare to above)
# This test should be performed after the previous test immediately (to use the previous result).
qs.2 <- a$q( c( 0.25, 0.5, 0.75 ) )
a$set.waypoints(
	data.frame(
	p = c( 0.25, 0.5, 0.75 ),
	q = qs.2 ),
this.type1.type = 2, continuous = TRUE )
expect_equal( c( a$sd, a$lsd, a$usd ), c( 1.324126, 1.224024, 1.417175 ), tolerance = 5e-7 )
plot.waypoints.and.p( a,
	c( 0.25, 0.5, 0.75 ), qs.2, c( -3, 3 ) )
show.results()


#### Appendix

## Plot of the burden ratio of the lower distribution for type1.type = 3, symmetric.
x<-seq( -5, 5, 0.01 ); plot( x, pnorm( x, 0, 1 ) - pnorm( x, 0, 1/sqrt(2) ) / sqrt(2), type="l" )


## Plots to show pnorm( x, 0, 1/sqrt(2) ) / pnorm( x, 0, 1 ) is bounded on [1, 1.107] where x > 0,
## and ( 1 - pnorm( x, 0, 1/sqrt(2) ) ) / ( 1 - pnorm( x, 0, 1 ) ) is also bounded on [1, 1.107] where x < 0.
x<-seq( 0, 4, 0.01 ); plot( x, pnorm( x, 0, 1/sqrt(2) ) / pnorm( x, 0, 1 ), type="l" )
x<-seq( -4, 0, 0.01 ); plot( x, ( 1 - pnorm( x, 0, 1/sqrt(2) ) ) / ( 1 - pnorm( x, 0, 1 ) ), type="l" )

max.pp <- cgd:::bisection( function(x) dnorm( x, 0, 1/sqrt(2) ) / pnorm( x, 0, 1 ) - pnorm( x, 0, 1/sqrt(2) ) * dnorm( x, 0, 1 ) / pnorm( x, 0, 1 )^2, c( 0, 1 ))
max.pp
pnorm( max.pp, 0, 1/sqrt(2) ) / pnorm( max.pp, 0, 1 )
( 1 - pnorm( -max.pp, 0, 1/sqrt(2) ) ) / ( 1 - pnorm( -max.pp, 0, 1 ) )


## Plot of x which maximize pnorm( x, 0, sigma * 1/sqrt(2) ) / pnorm( x, 0, sigma ) where sigma > 0 (it is linear).
sigma<-seq( 0.1, 2, 0.1 )
x<-vapply( sigma, function(s) { cgd:::bisection( function(y) dnorm( y, 0, s * 1/sqrt(2) ) / pnorm( y, 0, s ) - pnorm( y, 0, s * 1/sqrt(2) ) * dnorm( y, 0, s ) / pnorm( y, 0, s )^2, c( 0, s * 2 ) ) }, 0 )
plot( sigma, x, type="l" )

# Plot of the maximum values (it is a constant).
plot( sigma, pnorm( x, 0, sigma * 1/sqrt(2) ) / pnorm( x, 0, sigma ), type="l" )


## The following plots are of no use to this library, but may be interesting as mathematics.

## Plot of x.max which maximize pnorm( x, 0, sigma ) / pnorm( x, 0, 1 ) where 0 < sigma < 1.
sigma <- c( 0.001, seq( 0.01, 0.99, 0.01 ), 0.999 )
x.max <- vapply( sigma, function(s)
						{ cgd:::bisection( function(y)
											dnorm( y, 0, s ) / pnorm( y, 0, 1 ) -
											pnorm( y, 0, s ) * dnorm( y, 0, 1 ) / pnorm( y, 0, 1 )^2, c( 0, 1 ) ) }, 0 )
plot( sigma, x.max, type="l" )

## Plot of the maximum values.
plot( sigma, pnorm( x.max, 0, sigma ) / pnorm( x.max, 0, 1 ), type="l" )
