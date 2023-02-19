#### This file is for testing mainly CGD$set.waypoints and trace.q.

library( "cgd" )
a<-CGD$new()
dev.new(); plot.new()

# for control option test
control <- list( ftol = 1e-4 )

# There 5 commands should be run after each non-error test to confirm the result.
# You can change the numbers '3' and '1000' of below to any other numbers.
a
system.time( plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" ) )
system.time( plot( seq( -3, 3, 0.01 ), a$p( seq( -3, 3, 0.01 ) ), type = "l" ) )
system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
system.time( sample <- a$r( 1000 ) ); hist( sample )

# It's recomended to run this plot to compare integration a$d() to a$p().
#	Remark: Numerical integral is sometimes not so accurate.
y <- numeric()
i <- 1
for ( x in seq( -8, 8, 0.02 ) )
{
	y[i] <- integrate( f <- function( t ) { a$d( t ) }, -Inf, x )$value - a$p( x )
	i <- i + 1
}
plot( seq( -8, 8, 0.02 ), y, type = "l" )

# Plotting waypoints and a$p().
plot.waypoints.and.p <- function( a, p, q, xlim = NULL )
{
	if ( is.null( xlim ) )
	{
		xlim <- c( min( q ) - abs( q[2] - q[1] ), max( q ) + abs( q[length( q )] - q[length( q - 1 )] ) )
	}
	ylim <- c( 0, 1 )
	plot( q, p, xlim = xlim, ylim = ylim, xlab = "", ylab = "" )
	par( new = T )
	plot( seq( xlim[1], xlim[2], 0.01 ), a$p( seq( xlim[1], xlim[2], 0.01 ) ), type = "l", xlim = xlim, ylim = ylim )
}

# normal test
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), this.type1.type = 1 )
a$is.ind.p( c( 0, 0.1, 0.3, 0.5, 0.6, 0.65, 0.7, 1 ) )
	# bool: T T T T T F T T, i: 1 1 1 1 1 NaN 2 2
a$is.conn.p( c( 0, 0.1, 0.3, 0.5, 0.6, 0.65, 0.7, 1 ) )
	# bool: F F F F F T F F, i.1: 1 1 1 1 1 1 2 2, i.2: NaN NaN NaN NaN NaN 2 NaN NaN
a$is.ind.q( c( -Inf, -3, 0, 0.2533471, 0.2533472, 0.49, 0.5, 0.52440, 0.52441, 1, Inf ) )
	# bool: T T T T F F T T T T T, i: 1 1 1 1 NaN NaN 2 2 2 2 2
a$is.conn.q( c( -Inf, -3, 0, 0.2533471, 0.2533472, 0.49, 0.5, 0.52440, 0.52441, 1, Inf ) )
	# bool: F F F F T T F F F F F, i.1: 1 1 1 1 1 1 2 2 2 2 2, i.2: NaN NaN NaN NaN 2 2 NaN NaN NaN NaN NaN
a$p( c( 0.253347, 0.253348, 0.5, 0.52440, 0.52441 ) )
( a$p( 0.253347 ) == pnorm( 0.253347, 0, a$intervals[[1]]$sd ) ) # TRUE
( a$p( 0.253348 ) > pnorm( 0.253348, 0, a$intervals[[1]]$sd ) ) # TRUE
( a$p( 0.49 ) < pnorm( 0.49, 0, a$intervals[[2]]$sd ) ) # TRUE
( a$p( 0.5 ) == pnorm( 0.5, 0, a$intervals[[2]]$sd ) ) # TRUE
( a$p( 0.52440 ) == pnorm( 0.52440, 0, a$intervals[[2]]$sd ) ) # TRUE
c( a$sd(), a$lsd(), a$usd() ) # 0.9773839 1.0000000 0.9542319 (about)
plot.waypoints.and.p( a, c( 0.1, 0.3, 0.5, 0.6, 0.7 ), c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ), c( -3, 3 ) )

# This test compares returned value of sd() method and numerical integral.
# This test sholud be run after each normal test
# then the result value sholud be small enough (about 1e-5 or less).
#	Remark: Numerical integral is sometimes not so accurate.
abs( 1 - sqrt( integrate( f <- function( x ) { ( x - a$mean )^2 * a$d( x ) }, -Inf, Inf )$value ) / a$sd() )

# normal test
a <- trace.q(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), type1.type = 1 )
plot.waypoints.and.p( a, c( 0.1, 0.3, 0.5, 0.6, 0.7 ), c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ), c( -3, 3 ) )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), this.type1.type = 1, diff.mean = TRUE )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), this.type1.type = 2, diff.mean = TRUE )

# Error case
a$set.waypoints(
	data.frame(
		p = c( 0.1, 0.3, 0.5, 0.6, 0.5 ),
		q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6, 0.51 ), 0, 1 ) ), this.type1.type = 1 ) )

# normal test
a <- trace.q(
data.frame(
p = c( 0.3, 0.5, 0.1, 0.6, 0.7 ),
q = c( qnorm( c( 0.3, 0.5, 0.1, 0.6 ), 0, 1 ), 0.5 ) ), type1.type = 1 )

# warning case
a$set.waypoints(
	data.frame(
		p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		q = c( -Inf, qnorm( c( 0.3, 0.5, 0.6 ), 0, 1 ), Inf ), this.type1.type = 1 ) )
a$kind # "Normal Distribution"

# warning case
a$set.waypoints(
	data.frame(
		p = c( -0.1, 0.3, 0.5, 0.6, 1.5 ),
		q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ), this.type1.type = 1 ) )
a$kind # "Normal Distribution"

# normal test (type 1)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = c( qnorm( c( 0.1, 0.25, 0.5, 0.75 ), 1, 1.1 ), qnorm( 0.9, 1, 0.9 ) ) ), this.type1.type = 1 )
c( a$sd(), a$lsd(), a$usd() ) # 1.0226015 1.1000000 0.9388438 (about)
plot.waypoints.and.p( a,
		c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
		c( qnorm( c( 0.1, 0.25, 0.5, 0.75 ), 1, 1.1 ), qnorm( 0.9, 1, 0.9 ) ),
		c( -3, 3 ) )

# normal test (set.intervals, type 1, type 2)
b<-CGD$new( type1.type = 1 )
b$set.intervals( a$intervals )
c( b$sd(), b$lsd(), b$usd() ) # 1.0226015 1.1000000 0.9388438 (about)
b$set.intervals( a$intervals, this.type1.type = 2 )
c( b$sd(), b$lsd(), b$usd() ) # 1.0219938 1.1000000 0.9375195 (about)


# normal test (type 1)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), this.type1.type = 2 )
c( a$sd(), a$lsd(), a$usd() ) # 0.9773785 1.0000000 0.9542209 (about)
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ),
		c( -3, 3 ) )

# normal test (type 2)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.7 ) ), this.type1.type = 2 )
c( a$sd(), a$lsd(), a$usd() ) # 1.176089 1.000000 1.329049
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.7 ),
		c( -3, 3 ) )

# normal test (type 2)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 1.2 ) ), this.type1.type = 2 )
c( a$sd(), a$lsd(), a$usd() ) # 1.753938 1.000000 2.269934
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 1.2 ),
		c( -3, 3 ) )

# normal test (type 2)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( -5, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ) ), this.type1.type = 2 )
c( a$sd(), a$lsd(), a$usd() ) # 2.596196 3.532770 1.000000
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( -5, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ),
		c( -3, 3 ) )

# normal test (type 3a)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( qnorm( c( 0.1, 0.3 ), 0, 1 ), 0, qnorm( c( 0.6, 0.7 ), 0, 1.1 ) ) ), this.type1.type = 2 )
c( a$sd(), a$lsd(), a$usd() ) # 1.051098 1.000000 1.099824
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( qnorm( c( 0.1, 0.3 ), 0, 1 ), 0, qnorm( c( 0.6, 0.7 ), 0, 1.1 ) ),
		c( -3, 3 ) )

# normal test (type 3b)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( qnorm( c( 0.1, 0.3 ), 0, 1.1 ), 0, qnorm( c( 0.6, 0.7 ), 0, 1 ) ) ), this.type1.type = 2 )
c( a$sd(), a$lsd(), a$usd() ) # 1.050307 1.098312 1.000000
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( qnorm( c( 0.1, 0.3 ), 0, 1.1 ), 0, qnorm( c( 0.6, 0.7 ), 0, 1 ) ),
		c( -3, 3 ) )

# normal test (type 1)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( -1.1, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ) ), this.type1.type = 2 )
c( a$sd(), a$lsd(), a$usd() ) # 0.9417019 0.8795481 1.0000000 (about)
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( -1.1, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ),
		c( -3, 3 ) )

# normal test (type 2)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( -1.4, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ) ), this.type1.type = 2 )
c( a$sd(), a$lsd(), a$usd() ) # 1.038296 1.075228 1.000000
plot.waypoints.and.p( a,
		c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
		c( -1.4, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ),
		c( -3, 3 ) )

# normal test
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( -0.5, -0.04, 0, 1, 2 ) ) )
a$intervals[[2]]$p.ind[1] - 0.3 # 0
a$is.ind.p( c( 0, 0.05, 0.1, 0.2, 0.3, a$intervals[[2]]$p.ind[1], 0.4, 0.5, 0.6, 0.65, 0.7, 0.8 ) )
	# bool: T T T F T T F F T F T T, i: 1 1 1 NaN 2 2 NaN NaN 3 NaN 4 4
a$is.conn.p( c( 0, 0.05, 0.1, 0.2, 0.3, a$intervals[[2]]$p.ind[1], 0.4, 0.5, 0.6, 0.65, 0.7, 0.8 ) )
	# bool: F F F T F F T T F T F F, i.1: 1 1 1 1 2 2 2 2 3 3 4 4, i.2: NaN NaN NaN 2 NaN NaN 3 3 NaN 4 NaN NaN
a$is.ind.q( c( -1, -0.48, -0.04, 0, 0.5, 1, 1.5, 2 ) )
	# bool: T F T F F T F T, i: 1 NaN 2 NaN NaN 3 NaN 4
a$is.conn.q( c( -1, -0.48, -0.04, 0, 0.5, 1, 1.5, 2 ) )
	# bool: F T F T T F T F, i.1: 1 1 2 2 2 3 3 4, i.2: NaN 2 NaN 3 3 NaN 4 NaN
c( a$sd(), a$lsd(), a$usd() ) # 2.7039729 0.3521343 3.8077474 (about)

plot( seq( -1, 0.05, 0.01 ), a$d( seq( -1, 0.05, 0.01 ) ), type = "l" )
plot( seq( 0.05, 10, 0.01 ), a$d( seq( 0.05, 10, 0.01 ) ), type = "l" )


# warning test
a$set.waypoints(
data.frame(
p = c( 0.5 ),
q = c( 1 ) ),
this.type1.type = 1 )
a$type1.type # 1
a$kind # NA

# normal test
a <- CGD$new( 2 )
a$type1.type # 2
a$kind # Normal Distribution

# normal test
a <- CGD$new( 3 )
a$type1.type # 3
a$kind # Normal Distribution

# normal test
a <- CGD$new( 4 )
a$type1.type # 4
a$kind # Normal Distribution


# normal test
a$set.waypoints(
data.frame(
p = c( 0.5, 0.7 ),
q = c( 0, qnorm( 0.7, 0, 1 ) ) ),
this.type1.type = 1 )
c( a$sd(), a$lsd(), a$usd() ) # 1 1 1

# normal test
a$set.waypoints(
data.frame(
p = c( 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ),
q = c( -9, -8.9, -8.7, -5.4, -1, 0, 0.3, 0.5, 0.77 ) ),
this.type1.type = 1 )
c( a$sd(), a$lsd(), a$usd() ) # 5.518816 7.642767 1.582013 (about)
plot.waypoints.and.p( a,
		c( 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ),
		c( -9, -8.9, -8.7, -5.4, -1, 0, 0.3, 0.5, 0.77 ),
		c( -12, 2 ) )
plot( seq( -12, 2, 0.01 ), a$d( seq( -12, 2, 0.01 ) ), type = "l" )

# normal test
a$set.waypoints(
data.frame(
p = c( 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ),
q = c( -9, -8.9, -8.7, -5.4, -1, 0, 0.3, 0.5, 0.77 ) ),
this.type1.type = 2 )
c( a$sd(), a$lsd(), a$usd() ) # 5.518078 7.641861 1.581248 (about)
plot.waypoints.and.p( a,
		c( 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ),
		c( -9, -8.9, -8.7, -5.4, -1, 0, 0.3, 0.5, 0.77 ),
		c( -12, 2 ) )
plot( seq( -12, 2, 0.01 ), a$d( seq( -12, 2, 0.01 ) ), type = "l" )

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.6, 0, 0.75 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.6 ),
q = qs ),
this.type1.type = 1, continuous = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.111522 1.111522 1.111522
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.6 ), qs, c( -3, 3 ) )

# normal test (control option)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.6 ),
q = qs ),
this.type1.type = 1, continuous = TRUE, control = control )
c( a$sd(), a$lsd(), a$usd() )
a$p( qs ) # 0.0999377 0.5000000 0.5999828 (about)

# Error case
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.6 ),
q = qs ),
this.type1.type = 1, continuous = TRUE, diff.mean = TRUE )

# normal test
qs <- c( qnorm( 0.1, -0.5, 1.9 ), -0.5, qnorm( 0.7, -0.5, 1.8 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.7 ),
q = qs ),
this.type1.type = 1, symmetric = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.986118 1.986118 1.986118
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.7 ), qs, c( -3, 3 ) )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.6 ),
q = c( qnorm( 0.1, 0, 0.9 ), 0, qnorm( 0.6, 0, 1 ) ) ),
this.type1.type = 1, continuous = TRUE )

# normal test for v2.3.0 and up
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.8 ), qnorm( 0.6, 0, 0.8 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.6 ), qs ),
this.type1.type = 1, continuous = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.104407 1.104407 1.104407
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.6 ), qs, c( -3, 3 ) )

# normal test (for bug-fix)
qs <- c( qnorm( 0.1, 0, 1.9 ), 0, qnorm( 0.7, 0, 0.4 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.7 ),
q = c( qnorm( 0.1, 0, 1.9 ), 0, qnorm( 0.7, 0, 0.4 ) ) ),
this.type1.type = 1, symmetric = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 2.04997 2.04997 2.04997
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.7 ), qs, c( -3, 3 ) )


# normal test
qs <- c( qnorm( 0.3, -0.2, 1 ), 0, qnorm( 0.7, 0.2, 0.8 ) )
a$set.waypoints(
data.frame(
p = c( 0.3, 0.5, 0.7 ),
q = qs ),
this.type1.type = 2, continuous = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.332352 1.519680 1.113954 (about)
a$p( qs )
a$is.uni.mean()	# TRUE
plot.waypoints.and.p( a,
	c( 0.3, 0.5, 0.7 ), qs, c( -3, 3 ) )

# normal test (control option)
a$set.waypoints(
data.frame(
p = c( 0.3, 0.5, 0.7 ),
q = qs ),
this.type1.type = 2, continuous = TRUE, control = control )
c( a$sd(), a$lsd(), a$usd() )
a$p( qs ) # 0.2999898 0.5000000 0.7000054 (about)
a$is.uni.mean()	# TRUE

# normal test (diff.mean)
a$set.waypoints(
data.frame(
p = c( 0.3, 0.5, 0.7 ),
q = qs ),
this.type1.type = 2, continuous = TRUE, diff.mean = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.390740 1.609658 1.130186 (about)
a$p( qs )
a$is.uni.mean()	# FALSE
plot.waypoints.and.p( a,
	c( 0.3, 0.5, 0.7 ), qs, c( -3, 3 ) )

# normal test (diff.mean, control option)
a$set.waypoints(
data.frame(
p = c( 0.3, 0.5, 0.7 ),
q = qs ),
this.type1.type = 2, continuous = TRUE, diff.mean = TRUE, control = control )
c( a$sd(), a$lsd(), a$usd() )
a$p( qs ) # 0.3000307 0.5000591 0.7000246 (about)
a$is.uni.mean()	# FALSE

# normal test
qs <- c( qnorm( 0.3, 0.2, 0.7 ), 0.2, qnorm( 0.9, 0.2, 1.1 ) )
a$set.waypoints(
data.frame(
p = c( 0.3, 0.5, 0.9 ),
q = qs ),
this.type1.type = 2, continuous = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 0.8988465 0.6552345 1.0892740 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.3, 0.5, 0.9 ), qs, c( -3, 3 ) )

# normal test
a$set.waypoints(
data.frame(
p = c( 0.3, 0.6, 0.9 ),
q = qs ),
this.type1.type = 2, continuous = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 0.9063446 0.4747657 1.1905959 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.3, 0.6, 0.9 ), qs, c( -3, 3 ) )

# normal test
a$set.waypoints(
data.frame(
p = c( 0.52, 0.6, 0.9 ),
q = qs ),
this.type1.type = 2, continuous = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.842416 2.165694 1.448711 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.52, 0.6, 0.9 ), qs, c( -3, 3 ) )

# normal test - sharp center
qs <- c( qnorm( 0.3, -0.2, 0.7 ), -0.2, qnorm( 0.9, -0.2, 1 ) )
a$set.waypoints(
data.frame(
p = c( 0.3, 0.5, 0.9 ),
q = qs ),
this.type1.type = 2, symmetric = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 0.9863575 0.9863575 0.9863575 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.3, 0.5, 0.9 ), qs, c( -3, 3 ) )

# normal test (control option)
a$set.waypoints(
data.frame(
p = c( 0.3, 0.5, 0.9 ),
q = qs ),
this.type1.type = 2, symmetric = TRUE, control = control )
c( a$sd(), a$lsd(), a$usd() )
a$p( qs ) # 0.2999911 0.5000000 0.8999857 (about)
plot.waypoints.and.p( a,
	c( 0.3, 0.5, 0.9 ), qs, c( -3, 3 ) )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.3, 0.49, 0.9 ),
q = qs ),
this.type1.type = 2, symmetric = TRUE )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.3, 0.5, 0.9 ),
q = qs ),
this.type1.type = 2, symmetric = TRUE, diff.mean = TRUE )

# normal test - dent center
qs <- c( qnorm( 0.6, 0, 1.2 ), qnorm( 0.9, 0, 1.05 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.6, 0.9, 0.5 ),
q = qs ),
this.type1.type = 2, symmetric = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.044687 1.044687 1.044687 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.6, 0.9, 0.5 ), qs, c( -3, 3 ) )

# normal test (control option)
a$set.waypoints(
data.frame(
p = c( 0.6, 0.9, 0.5 ),
q = qs ),
this.type1.type = 2, symmetric = TRUE, control = control )
c( a$sd(), a$lsd(), a$usd() )
a$p( qs ) # 0.6000152 0.8999913 0.5000000 (about)

# Error case - bug fix (for v1.3.7 and up)
qs <- c( qnorm( 0.1, 0, 1.8 ), 0, qnorm( 0.4, 0, 1.05 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5 ),
q = qs ),
this.type1.type = 2, symmetric = TRUE )

# normal test - sharp center - bug fix (for v1.3.7 and up)
qs <- c( qnorm( 0.1, 0, 1.8 ), 0, qnorm( 0.4, 0, 1.05 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.4 ),
q = qs ),
this.type1.type = 2, symmetric = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.76902 1.76902 1.76902 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.4 ), qs, c( -3, 3 ) )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.5, 0.6, 0.9 ),
q = c( 0, 0.9, 1.9 ) ),
this.type1.type = 2, symmetric = TRUE )


# normal test
qs <- c( qnorm( 0.1, 0, 1.2 ), qnorm( 0.4, 0, 0.9 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.0900415 1.2567814 0.8926821
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5 ), qs, c( -3, 3 ) )

# normal test (control option)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5 ),
q = qs ),
this.type1.type = 3, control = control )
c( a$sd(), a$lsd(), a$usd() )
a$p( qs ) # 0.09999746 0.40000590 0.50000000 (about)

# normal test
qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.4, 0, 0.7 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 0.8258552 0.9389195 0.6946250
a$p( qs )
a$is.uni.mean() # TRUE
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5 ), qs, c( -3, 3 ) )

# normal test
qs <- c( 0, qnorm( 0.6, 0, 0.7 ), qnorm( 0.9, 0, 0.9 ) )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 0.8258552 0.6946250 0.9389195
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.5, 0.6, 0.9 ), qs, c( -3, 3 ) )

# normal test
qs <- c( 0, qnorm( 0.6, 0, 0.9 ), qnorm( 0.9, 0, 1.2 ) )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.0900415 0.8926821 1.2567814
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.5, 0.6, 0.9 ), qs, c( -3, 3 ) )


# normal test for v2.3.0 and up
qs <- c( qnorm( 0.4, 0, 0.7 ), qnorm( 0.6, 0, 0.7 ), qnorm( 0.9, 0, 0.9 ) )
a$set.waypoints(
data.frame(
p = c( 0.4, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 0.7982611 0.6262522 0.9392814 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.4, 0.6, 0.9 ), qs, c( -3, 3 ) )

# normal test for v2.3.0 and up
qs <- c( 0, qnorm( 0.6, 0, 0.9 ), qnorm( 0.9, 0, 1.2 ) )
a$set.waypoints(
data.frame(
p = c( 0.4, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 0.9806078 0.7884754 1.1408286 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.4, 0.6, 0.9 ), qs, c( -3, 3 ) )

# normal test for v2.3.0 and up
qs <- c( qnorm( 0.1, 0, 1.5 ), qnorm( 0.3, 0, 1 ), qnorm( 0.6, 0, 0.8 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.6 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.567381 1.555877 1.578800 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.6 ), qs, c( -3, 3 ) )


# normal test
qs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.7, 0, 0.9 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.022428 1.022428 1.022428
a$p( qs )
a$is.v.grad() # TRUE
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.7 ), qs, c( -3, 3 ) )

# normal test (control option)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, control = control )
c( a$sd(), a$lsd(), a$usd() )
a$p( qs ) # 0.1000117 0.5000000 0.700004 (about)
a$is.v.grad() # TRUE

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.9 ), qnorm( 0.4, 0, 0.85 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.4 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.103387 1.048196 1.155945 (about)
a$p( qs )
a$is.v.grad() # TRUE
a$is.uni.mean() # FALSE
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.4 ), qs, c( -3, 3 ) )

# normal test (control option)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.4 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, control = control )
c( a$sd(), a$lsd(), a$usd() )
a$p( qs ) # 0.09994926 0.30000000 0.40000054 (about)
a$is.v.grad() # TRUE
a$is.uni.mean() # FALSE

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.9, 0, 1 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.9 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1 1 1
a$p( qs )
a$is.v.grad() # TRUE
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.9 ), qs, c( -3, 3 ) )


# normal test for v2.3.0 and up
qs <- c( -0.479, -0.472, -0.215 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.7 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 0.12619658 0.06875084 0.16469510 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.7 ), qs, c( -3, 3 ) )

# normal test for v2.3.0 and up
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.32 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 6.013542 3.276127 7.848081 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.32 ), qs, c( -3, 3 ) )

# normal test for v2.3.0 and up
qs <- c( 0.215, 0.217, 0.472 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.12, 0.3 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 0.3410867 0.4322945 0.2140135 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.12, 0.3 ), qs, c( -3, 3 ) )

# normal test for v2.3.0 and up
a$set.waypoints(
data.frame(
p = c( 0.6, 0.62, 0.8 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.0138751 0.2176936 1.4172138 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.6, 0.62, 0.8 ), qs, c( -3, 3 ) )

# Error case
qs <- c( -0.479, -0.472, 0.472 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.12, 0.2 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )


# Error case
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.9 ), qnorm( 0.4, 0, 0.85 ), 0, qnorm( 0.6, 0, 0.85 )	)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.4, 0.5, 0.6 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )

# normal test for v2.2.0 and up
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.9 ), 0, qnorm( 0.6, 0, 0.85 )  )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 0.9433543 1.0012696 0.8816427 (about)
a$p( qs )
a$is.v.grad() # TRUE
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5, 0.6 ), qs, c( -3, 3 ) )

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.9 ), qnorm( 0.4, 0, 0.85 ), qnorm( 0.6, 0, 0.85 )  )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.4, 0.6 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 0.8977201 0.9896193 0.7952712 (about)
a$p( qs )
a$is.v.grad() # TRUE
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.4, 0.6 ), qs, c( -3, 3 ) )

# normal test (control option)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.4, 0.6 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, control = control )
c( a$sd(), a$lsd(), a$usd() )
a$p( qs ) # 0.09998827 0.30007279 0.40003980 0.59996960 (about)
a$is.v.grad() # TRUE

# normal test
qs <- c( qnorm( 0.6, 0, 1 ), qnorm( 0.7, 0, 1.01 ), qnorm( 0.8, 0, 1.1 ), qnorm( 0.9, 0, 1.12 )  )
a$set.waypoints(
data.frame(
p = c( 0.6, 0.7, 0.8, 0.9 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 0.8878141 0.7095931 1.0358114 (about)
a$p( qs )
a$is.v.grad() # TRUE
plot.waypoints.and.p( a,
	c( 0.6, 0.7, 0.8, 0.9 ), qs, c( -3, 3 ) )

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.85 ), qnorm( 0.6, 0, 0.85 )  )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.6 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.083659 1.044424 1.121522 (about)
a$p( qs )
a$is.v.grad() # TRUE
a$is.uni.mean() # FALSE
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.6 ), qs, c( -3, 3 ) )

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.85 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.029705 1.029705 1.029705 (about)
a$p( qs )
a$is.v.grad() # TRUE
a$is.uni.mean() # TRUE
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5 ), qs, c( -3, 3 ) )

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.9 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.051916 1.027216 1.076050
a$p( qs )
a$is.v.grad() # TRUE
a$is.uni.mean() # FALSE
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5 ), qs, c( -3, 3 ) )

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.8 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.279438 1.111342 1.427879
a$p( qs )
a$is.v.grad() # TRUE
a$is.uni.mean() # FALSE
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5 ), qs, c( -3, 3 ) )

# normal test
qs <- c( 0, qnorm( 0.7, 0, 0.8 ), qnorm( 0.9, 0, 1 ) )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.7, 0.9 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.279438 1.427879 1.111342
a$p( qs )
a$is.v.grad() # TRUE
a$is.uni.mean() # FALSE
plot.waypoints.and.p( a,
	c( 0.5, 0.7, 0.9 ), qs, c( -3, 3 ) )

# normal test
qs <- c( 0, 0.7, 0.9 )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.7, 0.9 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 0.8121997 0.8874782 0.7291907
a$p( qs )
a$is.v.grad() # TRUE
a$is.uni.mean() # FALSE
plot.waypoints.and.p( a,
	c( 0.5, 0.7, 0.9 ), qs, c( -3, 3 ) )

# normal test
qs <- c( 0, 0.7, 0.9 )
a$clear()
a$set.waypoints(
data.frame(
p = c( 0.5, 0.7, 0.9 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 0.8121997 0.8874782 0.7291907
a$p( qs )
a$is.v.grad() # TRUE
a$is.uni.mean() # FALSE
plot.waypoints.and.p( a,
	c( 0.5, 0.7, 0.9 ), qs, c( -3, 3 ) )

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.8 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.793933 1.292371 2.183155
a$p( qs )
a$is.v.grad() # TRUE
a$is.uni.mean() # FALSE
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5 ), qs, c( -3, 3 ) )

plot( seq( -8, 8, 0.01 ), a$d( seq( -8, 8, 0.01 ) ), type = "l" )

# normal test
qs <- c( -0.7, -0.5, 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 0.6264145 0.5552750 0.6902608
a$p( qs )
a$is.v.grad() # TRUE
a$is.uni.mean() # FALSE
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5 ), qs, c( -3, 3 ) )

# normal test
qs <- c( -0.7, -0.5, 0 )
a$clear()
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 0.6264145 0.5552750 0.6902608
a$p( qs )
a$is.v.grad() # TRUE
a$is.uni.mean() # FALSE
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5 ), qs, c( -3, 3 ) )

# normal test
qs <- c( -1.7, -0.5, 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.881278 1.552203 2.160805
a$p( qs )
a$is.v.grad() # TRUE
a$is.uni.mean() # FALSE
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5 ), qs, c( -3, 3 ) )

# normal test
qs <- c( -1.7, -0.5, 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.396233 1.396233 1.396233
a$p( qs )
a$is.v.grad() # TRUE
a$is.uni.mean() # TRUE
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5 ), qs, c( -3, 3 ) )

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.8 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.022333 1.022333 1.022333
a$p( qs )
a$is.v.grad() # TRUE
a$is.uni.mean() # TRUE
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5 ), qs, c( -3, 3 ) )


# normal test for v2.3.0 and up
qs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.6, 0, 0.85 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.6 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.018618 1.026266 1.010913
a$p( qs )
a$is.v.grad() # TRUE
a$is.uni.mean() # FALSE
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.6 ), qs, c( -3, 3 ) )

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.7, 0, 0.9 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 0.9289980 1.0138648 0.8355553
a$p( qs )
a$is.v.grad() # FALSE
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.7 ), qs, c( -3, 3 ) )

# normal test (control option)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3, control = control )
c( a$sd(), a$lsd(), a$usd() )
a$p( qs ) # 0.1000050 0.5000000 0.7000025 (about)
a$is.v.grad() # FALSE

# normal test for v1.3.5 and up
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.9 ), qnorm( 0.4, 0, 0.87 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.4, 0.5 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 0.9205978 0.9943971 0.8403423 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.4, 0.5 ), qs, c( -3, 3 ) )

# normal test (also)
qs <- c( 0.5, qnorm( 0.6, 0.5, 0.9 ), qnorm( 0.75, 0.52, 1 ), qnorm( 0.9, 0.57, 1.1 ) )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 0.9452409 0.7552078 1.1030059 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.5, 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )

# normal test
qs <- c( -1, -0.1, 0.2, 1.3 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 0.9471925 0.8606191 1.0264902 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.6, 0.9 ), qs, c( -3, 3 ) )

# normal test
qs <- c( qnorm( 0.3, 0, 0.2 ), 0, qnorm( 0.6, 0, 0.7 ) )
a$set.waypoints(
data.frame(
p = c( 0.3, 0.5, 0.6 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 0.8688877 0.4137035 1.1570570
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.3, 0.5, 0.6 ), qs, c( -3, 3 ) )

# normal test
qs <- c( -1, 0, 0.5 )
a$set.waypoints(
data.frame(
p = c( 0.499, 0.5, 0.6 ),
q = qs ), this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 511.7395 683.7288 237.2122
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.499, 0.5, 0.6 ), qs, c( -3, 3 ) )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.0001, 0.5, 0.7 ),
q = c( -0.01, 0, 0.25 ) ),
this.type1.type = 3 )

# normal test
qs <- c( -2, -0.5, 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.3266035 1.6402138 0.9107426
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5 ), qs, c( -3, 3 ) )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.1, 0.1002, 0.5 ),
q = c( -2, -0.001, 0 ) ),
this.type1.type = 3 )

# normal test
qs <- c( 0, 0.2, 1.9 )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.2267938 0.7814673 1.5489851
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.5, 0.6, 0.9 ), qs, c( -3, 3 ) )

# normal test
qs <- c( 1, 1.2, 2.9 )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.2267938 0.7814673 1.5489851
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.5, 0.6, 0.9 ), qs, c( -3, 3 ) )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.5, 0.6, 0.9 ),
q = c( 0, 1.2, 2.9 ) ),
this.type1.type = 3 )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5 ),
q = c( -2.9, -1.2, 0 ) ),
this.type1.type = 3 )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.6 ),
q = c( -2.9, -1.2, 0 ) ),
this.type1.type = 3 )

# normal test
qs <- c( -2, -0.5, 0, 0.7 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.8 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.283321 1.640214 0.776867
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5, 0.8 ), qs, c( -3, 3 ) )

# normal test
qs <- c( -2, -0.5, 0, 0.5 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.640214 1.640214 1.640213
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5, 0.7 ), qs, c( -3, 3 ) )

# normal test
qs <- c( -4, -2.5, -2, -1.5 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.640214 1.640214 1.640213
a$intervals[[1]]$sd - a$intervals[[3]]$sd # 8.227264e-07
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.3, 0.5, 0.7 ), qs, c( -8, 4 ) )

# normal test
qs <- c( -1.5, -0.4, 0, 0.5 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.122981 1.190641 1.050973
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5, 0.7 ), qs, c( -3, 3 ) )

# This test should be performed after the previous test immediately (to use the previous result).
sd.2 <- a$intervals[[2]]$sd
qs <- c( -1.5, -0.4, 0, qnorm( 0.7, 0, sd.2 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.469891 1.190641 1.703976
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5, 0.7 ), qs, c( -3, 3 ) )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.7 ),
q = c( -2, -0.5, 0, 0.7 ) ),
this.type1.type = 3 )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.7 ),
q = c( -2, -0.5, 0, 0.1 ) ),
this.type1.type = 3 )

# normal test
qs <- c( -2, 0, 0.1, 2.3 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.695580 1.575835 1.807409
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.6, 0.9 ), qs, c( -3, 3 ) )

# This test should be performed after the previous test immediately (to use the previous result).
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3, diff.mean = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.693498 1.618187 1.765599
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.6, 0.9 ), qs, c( -3, 3 ) )

# This test should be performed after the previous test immediately (to use the previous result).
sd.2 <- a$intervals[[2]]$sd
qs <- c( qnorm( 0.1, 0, sd.2 ), 0, 0.1, 2.3 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.3075065 0.3904101 1.8074088
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.6, 0.9 ), qs, c( -3, 3 ) )

# normal test
qs <- c( 2.3, -1.5, 0, 0.3 )
a$set.waypoints(
data.frame(
p = c( 0.9, 0.1, 0.5, 0.6 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.572562 1.170029 1.891279
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.9, 0.1, 0.5, 0.6 ), qs, c( -3, 3 ) )

# Compare to above test
qs <- c( 0, 0.3, 2.3 )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.573760 1.173245 1.891279
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.5, 0.6, 0.9 ), qs, c( -3, 3 ) )


# Error case
a$set.waypoints(
data.frame(
p = c( 0.4, 0.5, 0.6, 0.9 ),
q = c( -0.2, 0, 0.1, 2.3 ) ),
this.type1.type = 3 )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.04, 0.5, 0.6, 0.9 ),
q = c( -0.2, 0, 0.1, 2.3 ) ),
this.type1.type = 3 )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5, 0.9 ),
q = c( -2.3, -0.1, 0, 0.2 ) ),
this.type1.type = 3 )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5, 0.51 ),
q = c( -2.3, -0.1, 0, 0.2 ) ),
this.type1.type = 3 )

# normal test- bug fix (for v.2.3.0 and up)
qs <- c( -2.3, -0.2, 0, 0.2 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5, 0.6 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.851449 1.851451 1.851447 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5, 0.6 ), qs, c( -3, 3 ) )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.2, 0.4, 0.6, 0.9 ),
q = c( -2.8, -0.1, 0.1, 2.3 ) ),
this.type1.type = 3 )

## uni.sigma

#Error case
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.6 ),
q = c( -1, 0, 0.5 ) ),
this.type1.type = 1, continuous = TRUE, uni.sigma = TRUE )

# Error case
qs <- c( -2, -0.31, 0.29, 1.9 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.6, 0.9 ),
q = qs ),
this.type1.type = 2, continuous = TRUE, uni.sigma = TRUE )

# Error case
qs <- c( -2, -0.31, 0.29, 1.9 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, uni.sigma = TRUE )

# Error case
qs <- c( -2, -0.31, 0, 0.29, 1.9 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3, uni.sigma = TRUE )

# Error case
qs <- c( -2, -1, -0.31, 0.29, 0.95, 1.9 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4, uni.sigma = TRUE )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.25, 0.5, 0.75 ),
q = c( -2, 0, 2.1 ) ),
this.type1.type = 2, continuous = TRUE, uni.sigma = TRUE )

# normal test
qs <- c( -0.584, 0, 0.291 )
a$set.waypoints(
data.frame(
p = c( 0.3, 0.5, 0.6 ),
q = qs ),
this.type1.type = 2, continuous = TRUE, uni.sigma = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 0.9982686 0.9982686 0.9982686 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.3, 0.5, 0.6 ), qs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )

# normal test (without uni.sigma; Compare to above)
a$set.waypoints(
data.frame(
p = c( 0.3, 0.5, 0.6 ),
q = qs ),
this.type1.type = 2, continuous = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.141039 1.090946 1.189023 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.3, 0.5, 0.6 ), qs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )


# normal test
qs <- c( -2, 0, 1.8 )
a$set.waypoints(
data.frame(
p = c( 0.25, 0.5, 0.7 ),
q = qs ),
this.type1.type = 2, continuous = TRUE, uni.sigma = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 2.171117 2.171117 2.171117 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.25, 0.5, 0.7 ), qs, c( -3, 3 ) )

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
c( a$sd(), a$lsd(), a$usd() )
a$p( qs ) #0.2500214 0.5000000 0.6999782 (about)

# normal test
qs <- c( qnorm( 0.25, 0, 1 ), 0, qnorm( 0.7, 0, 1.02 ) )
a$set.waypoints(
data.frame(
p = c( 0.25, 0.5, 0.7 ),
q = qs ),
this.type1.type = 2, continuous = TRUE, uni.sigma = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 0.0.9156116 0.9156116 0.9156116 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.25, 0.5, 0.7 ), qs, c( -3, 3 ) )

# normal test for v2.3.0 and up
qs <- c( qnorm( 0.2, 0, 1 ), qnorm( 0.4, 0, 1.01 ), qnorm( 0.8, 0, 1.05 ) )
a$set.waypoints(
data.frame(
p = c( 0.2, 0.4, 0.8 ),
q = qs ),
this.type1.type = 2, continuous = TRUE, uni.sigma = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 0.9637276 0.9637276 0.9637276 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.2, 0.4, 0.8 ), qs, c( -3, 3 ) )

# normal test
qs <- c( qnorm( 0.2, 0, 1 ), qnorm( 0.4, 0, 1.01 ), qnorm( 0.8, 0, 1.05 ) )
a$set.waypoints(
data.frame(
p = c( 0.2, 0.4, 0.8 ),
q = qs ),
this.type1.type = 2, continuous = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.0259111 0.9939195 1.0569349 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.2, 0.4, 0.8 ), qs, c( -3, 3 ) )

# normal test
qs <- c( qnorm( 0.1, -0.2, 1 ), qnorm( 0.4, 0, 1 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, uni.sigma = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.0209410 1.1345404 0.8930057 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5 ), qs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )

# normal test (without uni.sigma; Compare to above)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.186791 1.186791 1.186791 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5 ), qs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )


# normal test
qs <- c( qnorm( 0.1, -0.2, 1 ), qnorm( 0.4, 0, 1 ), qnorm( 0.6, 0.1, 1 ), qnorm( 0.8, 0.4, 1 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.6, 0.8 ),
q = qs ),
this.type1.type = 3, uni.sigma = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.386057 1.193551 1.554909 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.6, 0.8 ), qs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )

# normal test (without uni.sigma; Compare to above)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.6, 0.8 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 2.446724 1.194284 3.247553 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.6, 0.8 ), qs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )

# Error case (to show that uni.sigma = TRUE option is very hard to convergent)
qs <- c( qnorm( 0.1, -0.2, 0.8 ), qnorm( 0.4, 0, 0.9 ), qnorm( 0.6, 0.1, 1 ), qnorm( 0.8, 0.4, 1.1 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.6, 0.8 ),
q = qs ),
this.type1.type = 3, uni.sigma = TRUE )

# normal test (without uni.sigma; Compare to above)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.6, 0.8 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 3.7430475 0.9905935 5.1999551 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.6, 0.8 ), qs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )


# normal test
qs <- c( -1.464921, -0.280095, 0.027170, 0.352307, 1.201652 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5, 0.6, 0.8 ),
q = qs ),
this.type1.type = 4, uni.sigma = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.315927 1.178712 1.440128 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5, 0.6, 0.8 ), qs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )

# normal test (without uni.sigma; Compare to above)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5, 0.6, 0.8 ),
q = qs ),
this.type1.type = 4 )
c( a$sd(), a$lsd(), a$usd() ) # 1.325977 1.164378 1.469917 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5, 0.6, 0.8 ), qs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )

# Error case (to show that uni.sigma = TRUE option is very hard to convergent)
qs <- round( qs, 3 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5, 0.6, 0.8 ),
q = qs ),
this.type1.type = 4, uni.sigma = TRUE, control = list( maxit = 2000 ) )

# normal test (without uni.sigma; Compare to above)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5, 0.6, 0.8 ),
q = qs ),
this.type1.type = 4 )
c( a$sd(), a$lsd(), a$usd() ) # 1.326608 1.164520 1.470943 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.5, 0.6, 0.8 ), qs, c( -3, 3 ) )
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )


##	4 waypoints for type1.type = 2
# normal test
qs <- c( qnorm( 0.1, 0, 1.1 ), qnorm( 0.25, 0, 1 ), qnorm( 0.75, 0, 1.05 ), qnorm( 0.9, 0, 1.06 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.75, 0.9 ),
q = qs ),
this.type1.type = 2, continuous = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.131012 1.192637 1.065829 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.75, 0.9 ), qs, c( -3, 3 ) )

# normal test (control option)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.75, 0.9 ),
q = qs ),
this.type1.type = 2, continuous = TRUE, control = control )
c( a$sd(), a$lsd(), a$usd() )
a$p( qs ) # 0.1000118 0.2500168 0.7499932 0.8999589 (about)

# normal test
qs <- c( -1.7, -0.2, 0.3, 1.9 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.75, 0.9 ),
q = qs ),
this.type1.type = 2, continuous = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.523708 1.516014 1.531364 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.75, 0.9 ), qs, c( -3, 3 ) )

plot( seq( 0.4, 0.6, 0.001 ), a$q( seq( 0.4, 0.6, 0.001 ) ), type = "l" )
plot( seq( 0.01, 0.06, 0.001 ), a$q( seq( 0.01, 0.06, 0.001 ) ), type = "l" )

# normal test for v2.3.0 and up
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 2, continuous = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.553601 1.343706 1.738334 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.5, 0.75, 0.9 ), qs, c( -3, 3 ) )

## 5 or 6 waypoints for type1.type = 3
# Error case
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = c( qnorm( 0.1, 0, 1.12 ), qnorm( 0.25, 0, 1.11 ), qnorm( 0.5, 0, 1 ), qnorm( 0.75, 0, 1.05 ), qnorm( 0.9, 0, 1.06 ) ) ),
this.type1.type = 3, symmetric = TRUE )

# normal test
qs <- c( qnorm( 0.1, 0, 1.12 ), qnorm( 0.25, 0, 1.11 ), qnorm( 0.5, 0, 1 ), qnorm( 0.75, 0, 1.05 ), qnorm( 0.9, 0, 1.06 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.092617 1.111492 1.073411 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.75, 0.9 ), qs, c( -3, 3 ) )

# normal test (control option)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3, control = control )
c( a$sd(), a$lsd(), a$usd() )
a$p( qs ) # 0.1000345 0.2500480 0.5000082 0.7500175 0.8999795 (about)

# normal test
qs <- c( -1.4, -0.7, 0, 0.75, 1.42 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.115318 1.141139 1.088885 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.75, 0.9 ), qs, c( -3, 3 ) )

# normal test
qs <- c( -0.4, 0.3, 1, 1.75, 2.42 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.115318 1.141139 1.088885 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.75, 0.9 ), qs, c( -2, 4 ) )

# normal test
qs <- c( -1.412, -0.262, 0.272, 0.756, 1.427 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.106111 1.129753 1.081952 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.4, 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )

# Error case
qs <- c( -1.4, -0.1, 0.1, 0.75, 1.42 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3 )

# normal test
qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
			qnorm( 0.4, 0, 0.98 ), qnorm( 0.6, 0, 1.03 ),
			qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 0.9920315 0.8930488 1.0819967 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )

# normal test (control option)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3, control = control )
c( a$sd(), a$lsd(), a$usd() )
a$p( qs ) # 0.0999907 0.2499966 0.4000068 0.5999995 0.7499936 0.8999972 (about)

# normal test
qs <- c( qnorm( 0.1, 0, 0.8 ), qnorm( 0.25, 0, 0.85 ),
			qnorm( 0.4, 0, 0.9 ), qnorm( 0.6, 0, 1.2 ),
			qnorm( 0.75, 0, 1.21 ), qnorm( 0.9, 0, 1.23 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3 )
c( a$sd(), a$lsd(), a$usd() ) # 1.0304095 0.8159903 1.2073307 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )

## type1.type == 4

# Error case
a$set.waypoints(
data.frame(
p = c( 0.5 ),
q = c( 0 ) ),
this.type1.type = 4 )

# Error case
qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
			qnorm( 0.4, 0, 0.98 ), qnorm( 0.6, 0, 1.03 ),
			qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.3, 0.4, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.6, 0.7, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )

# normal test for v2.3.0 and up
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )
c( a$sd(), a$lsd(), a$usd() ) # 1.0776972 0.7169528 1.3449317 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )

# Error case
qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
			qnorm( 0.5, 0, 0.98 ), qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )

# Error case
qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
			qnorm( 0.5, 0, 0.98 ), qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.5, 0.9 ),
q = qs ),
this.type1.type = 4 )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.6, 0.9 ),
q = qs ),
this.type1.type = 4 )

# Error case
qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ), qnorm( 0.4, 0, 0.98 ),
			qnorm( 0.5, 0, 0.98 ),
			qnorm( 0.6, 0, 1.03 ), qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.55, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.45, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.45, 0.55, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )

# Error case
qs <- c( qnorm( 0.25, 0, 0.9 ), qnorm( 0.5, 0, 0.95 ), qnorm( 0.75, 0, 1.1 ) )
a$set.waypoints(
data.frame(
p = c( 0.25, 0.5, 0.75 ),
q = qs ),
this.type1.type = 4 )

# Error case
qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
			qnorm( 0.6, 0, 1.03 ), qnorm( 0.75, 0, 1.1 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )

# normal test
qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
			qnorm( 0.5, 0, 1 ),
			qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )
c( a$sd(), a$lsd(), a$usd() ) # 1.0131453 0.8928412 1.1206077 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.75, 0.9 ), qs, c( -3, 3 ) )
a$is.conn.p( c( 0, 0.5, 1 ) ) # FALSE TRUE FALSE, 1 1 2, NaN 2 NaN
a$is.ind.p( c( 0, 0.5, 1 ) ) # TRUE FALSE TRUE, 1 NaN 2
a$is.conn.q( c( -Inf, 0, Inf ) ) # FALSE TRUE FALSE, 1 1 2, NaN 2 NaN
a$is.ind.q( c( -Inf, 0, Inf ) ) # TRUE FALSE TRUE, 1 NaN 2

# normal test (control option)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4, control = control )
c( a$sd(), a$lsd(), a$usd() )
a$p( qs ) # 0.1000013 0.2499950 0.5000000 0.7500129 0.8999999 (about)

# normal test
qs <- c( qnorm( 0.1, 0, 2 ), qnorm( 0.25, 0, 1.5 ),
			qnorm( 0.5, 0, 1 ),
			qnorm( 0.75, 0, 0.9 ), qnorm( 0.9, 0, 0.8 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )
c( a$sd(), a$lsd(), a$usd() ) # 1.5698853 2.0417000 0.8720897 (about)
a$p( qs )
a$is.uni.mean() # TRUE
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.75, 0.9 ), qs, c( -3, 3 ) )

# normal test
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4, diff.mean = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.5683332 2.0368452 0.8778382 (about)
a$p( qs )
a$is.uni.mean() # FALSE
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.75, 0.9 ), qs, c( -3, 3 ) )

# normal test
qs <- c( qnorm( 0.1, 0, 2 ), qnorm( 0.25, 0, 1.5 ),
			qnorm( 0.5, 0, 1 ),
			qnorm( 0.75, 0, 0.9 ), qnorm( 0.9, 0, 0.792 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )
c( a$sd(), a$lsd(), a$usd() ) # 1.569847 2.040980 0.873638 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.75, 0.9 ), qs, c( -3, 3 ) )

# normal test
qs <- c( qnorm( 0.1, 0, 0.792 ), qnorm( 0.25, 0, 0.9 ),
			qnorm( 0.5, 0, 1 ),
			qnorm( 0.75, 0, 1.5 ), qnorm( 0.9, 0, 2 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )
c( a$sd(), a$lsd(), a$usd() ) # 1.569847 0.873638 2.040980 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.75, 0.9 ), qs, c( -3, 3 ) )

# normal test for v2.2.0 and up
qs <- c( qnorm( 0.1, 0, 2 ), qnorm( 0.25, 0, 1.5 ),
			qnorm( 0.5, 0, 1 ),
			qnorm( 0.75, 0, 0.95 ), qnorm( 0.9, 0, 0.792 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )
c( a$sd(), a$lsd(), a$usd() ) # 1.5870049 2.0821556 0.8377336 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.75, 0.9 ), qs, c( -3, 3 ) )

# normal test for v2.2.0 and up
qs <- c( qnorm( 0.1, 0, 0.792 ), qnorm( 0.25, 0, 0.95 ),
			qnorm( 0.5, 0, 1 ),
			qnorm( 0.75, 0, 1.5 ), qnorm( 0.9, 0, 2 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )
c( a$sd(), a$lsd(), a$usd() ) # 1.5833584 0.8151501 2.0855641 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.5, 0.75, 0.9 ), qs, c( -3, 3 ) )

# normal test
qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ), qnorm( 0.4, 0, 0.97 ),
			qnorm( 0.6, 0, 1.1 ), qnorm( 0.75, 0, 1.12 ), qnorm( 0.9, 0, 1.2 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )
c( a$sd(), a$lsd(), a$usd() ) # 1.0608425 0.8970766 1.2025088 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )

# Error case
qs <- c( qnorm( 0.1, 0, 0.6 ), qnorm( 0.25, 0, 0.95 ), qnorm( 0.4, 0, 0.97 ),
			qnorm( 0.6, 0, 1.1 ), qnorm( 0.75, 0, 1.12 ), qnorm( 0.9, 0, 1.3 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )

# Error case
qs <- c( -1, -0.7, -0.3, 0, 0.32, 0.9, 1.5 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )

# normal test (Baltan)
qs <- c( -1, -0.8, -0.3, 0, 0.32, 1, 1.2 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4, control = list( maxit = 1000 ) )
c( a$sd(), a$lsd(), a$usd() ) # 0.8413850 0.7632772 0.9128337 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )

# Error case (This test should be performed after the previous test immediately)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4, control = list( allowSingular = FALSE ) )

# normal test (sub1 of Baltan)
qs <- c( -1, -0.8, -0.3 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )
c( a$sd(), a$lsd(), a$usd() )
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.4 ), qs, c( -3, 3 ) )

# normal test (sub2 of Baltan)
qs <- c( 0.32, 1, 1.2 )
a$set.waypoints(
data.frame(
p = c( 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )
c( a$sd(), a$lsd(), a$usd() )
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )

# Error case (Too straight points)
qs <- c( -1.2, -0.83, -0.31, 0, 0.32, 0.85, 1.2 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )

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
c( a$sd(), a$lsd(), a$usd() ) # 0.8413850 0.9128337 0.7632772 (about)
a$p( c( -1.2, -1, -0.32, 0, 0.3, 0.8, 1 ) )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), -qs[order( -qs )], c( -3, 3 ) )

# normal test (Baltan Rev.) (for success, it needs about 1000 iterations)
qs <- c( -1.2, -1, -0.32, 0, 0.3, 0.8, 1 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4, control = list( maxit = 1000 ) )
c( a$sd(), a$lsd(), a$usd() ) # 1.122514 1.120770 1.124254 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )


# normal test
# qs are given by qs <- c( qnorm( 0.3, -0.2, 1 ), 0, qnorm( 0.7, 0.2, 0.8 ) ); a$set.waypoints( data.frame( p = c( 0.3, 0.5, 0.7 ), q = qs ), this.type1.type = 2, continuous = TRUE ); a$q( c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ) )
qs <- c( -1.9535092, -0.9569967, -0.3331447, 0, 0.3085147, 0.7858078, 1.4341828 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )
c( a$sd(), a$lsd(), a$usd() ) # 1.334316 1.522966 1.114170 (about)
a$p( qs )
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )

# normal test (control option)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4, control = control )
c( a$sd(), a$lsd(), a$usd() )
a$p( qs ) # 0.1000971 0.2500470 0.4000137 0.4999966 0.5999756 0.7499354 0.8999094 (about)


# normal test
qs <- c( -1.4, -0.78, -0.3, 0, 0.32, 0.96, 2 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )
c( a$sd(), a$lsd(), a$usd() ) # 1.362232 1.090828 1.587906 (about)
a$p( qs ) # 0.10 0.25 0.40 0.50 0.60 0.75 0.90
plot.waypoints.and.p( a,
	c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ), qs, c( -3, 3 ) )

# normal test (compare to above)
# This test should be performed after the previous test immediately (to use the previous result).
qs.2 <- a$q( c( 0.25, 0.5, 0.75 ) )
a$set.waypoints(
data.frame(
p = c( 0.25, 0.5, 0.75 ),
q = qs.2 ),
this.type1.type = 2, continuous = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.334304 1.104462 1.529999 (about)
a$p( qs.2 )
a$p( qs ) # ‚ 0.10 0.25 0.40 0.50 0.60 0.75 0.90

# Error case
qs <- c( -1.4, -0.96, -0.61, -0.3, 0.32, 0.715, 1.24, 2 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.2, 0.3, 0.5, 0.6, 0.7, 0.8, 0.9 ),
q = qs ),
this.type1.type = 4, control = list( maxit = 2000 ) )

# Error case
qs <- c( -1.4, -0.96, -0.61, -0.3, 0.32, 0.715, 1.24, 2 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.2, 0.3, 0.35, 0.4, 0.7, 0.8, 0.9 ),
q = qs ),
this.type1.type = 4 )

# Error case
qs <- c( -1.4, -0.96, -0.61, -0.3, 0.32, 0.715, 1.24, 2 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.2, 0.3, 0.55, 0.6, 0.7, 0.8, 0.9 ),
q = qs ),
this.type1.type = 4 )

# normal test
qs <- c( -1.4, -0.96, -0.61, -0.3, 0.32, 0.715, 1.24, 2 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9 ),
q = qs ),
this.type1.type = 4 )
c( a$sd(), a$lsd(), a$usd() ) # 1.317654 1.062110 1.531127 (about)
a$p( qs ) # 0.1 0.2 0.3 0.4 0.6 0.7 0.8 0.9
plot.waypoints.and.p( a,
	c( 0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9 ), qs, c( -3, 3 ) )

# normal test
qs <- c( a$q( 0.1 ), a$q( 0.25 ), a$q( 0.4 ), a$q( 0.50 ), a$q( 0.6 ), a$q( 0.7 ), a$q( 0.8 ), a$q( 0.9 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ),
q = qs ),
this.type1.type = 4 )
c( a$sd(), a$lsd(), a$usd() ) # 1.317654 1.062110 1.531127 (about)

# normal test (compare to above)
# This test should be performed after the previous test immediately (to use the previous result).
qs.2 <- a$q( c( 0.25, 0.5, 0.75 ) )
a$set.waypoints(
data.frame(
p = c( 0.25, 0.5, 0.75 ),
q = qs.2 ),
this.type1.type = 2, continuous = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 1.332606 1.098767 1.531140
plot.waypoints.and.p( a,
	c( 0.25, 0.5, 0.75 ), qs.2, c( -3, 3 ) )


#### Appendix

# Plot of the burden ratio of the lower distribution for type1.type = 3, symmetric.
x<-seq( -5, 5, 0.01 ); plot( x, pnorm( x, 0, 1 ) - pnorm( x, 0, 1/sqrt(2) ) / sqrt(2), type="l" )


# Plots to show pnorm( x, 0, 1/sqrt(2) ) / pnorm( x, 0, 1 ) is bounded on [1, 1.107] where x > 0,
# and ( 1 - pnorm( x, 0, 1/sqrt(2) ) ) / ( 1 - pnorm( x, 0, 1 ) ) is also bounded on [1, 1.107] where x < 0.
x<-seq( 0, 4, 0.01 ); plot( x, pnorm( x, 0, 1/sqrt(2) ) / pnorm( x, 0, 1 ), type="l" )
x<-seq( -4, 0, 0.01 ); plot( x, ( 1 - pnorm( x, 0, 1/sqrt(2) ) ) / ( 1 - pnorm( x, 0, 1 ) ), type="l" )

max.pp <- cgd:::bisection( function(x) dnorm( x, 0, 1/sqrt(2) ) / pnorm( x, 0, 1 ) - pnorm( x, 0, 1/sqrt(2) ) * dnorm( x, 0, 1 ) / pnorm( x, 0, 1 )^2, c( 0, 1 ))
max.pp
pnorm( max.pp, 0, 1/sqrt(2) ) / pnorm( max.pp, 0, 1 )
( 1 - pnorm( -max.pp, 0, 1/sqrt(2) ) ) / ( 1 - pnorm( -max.pp, 0, 1 ) )


# Plot of x which maximize pnorm( x, 0, sigma * 1/sqrt(2) ) / pnorm( x, 0, sigma ) where sigma > 0 (it is linear).
sigma<-seq( 0.1, 2, 0.1 )
x<-vapply( sigma, function(s) { cgd:::bisection( function(y) dnorm( y, 0, s * 1/sqrt(2) ) / pnorm( y, 0, s ) - pnorm( y, 0, s * 1/sqrt(2) ) * dnorm( y, 0, s ) / pnorm( y, 0, s )^2, c( 0, s * 2 ) ) }, 0 )
plot( sigma, x, type="l" )

# Plot of the maximum values (it is a constant).
plot( sigma, pnorm( x, 0, sigma * 1/sqrt(2) ) / pnorm( x, 0, sigma ), type="l" )


## The following plots are of no use to this library, but may be interesting as mathematics.

# Plot of x.max which maximize pnorm( x, 0, sigma ) / pnorm( x, 0, 1 ) where 0 < sigma < 1.
sigma<-c( 0.001, seq( 0.01, 0.99, 0.01 ), 0.999 )
x.max<-vapply( sigma, function(s) { cgd:::bisection( function(y) dnorm( y, 0, s ) / pnorm( y, 0, 1 ) - pnorm( y, 0, s ) * dnorm( y, 0, 1 ) / pnorm( y, 0, 1 )^2, c( 0, 1 ) ) }, 0 )
plot( sigma, x.max, type="l" )

# Plot of the maximum values.
plot( sigma, pnorm( x.max, 0, sigma ) / pnorm( x.max, 0, 1 ), type="l" )
