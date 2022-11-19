library( "cgd" )
a<-CGD$new()
dev.new(); plot.new()


# There 5 commands should be run after each non-error test to confirm the result.
# You can change the numbers '3' and '1000' of below to any other numbers.
a
plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
plot( seq( -3, 3, 0.01 ), a$p( seq( -3, 3, 0.01 ) ), type = "l" )
plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" )
sample <- a$r( 1000 ); hist( sample )

# It's strongly recomended to run this plot to compare integration a$d() to a$p().
y <- numeric()
i <- 1
for ( x in seq( -8, 8, 0.02 ) )
{
	y[i] <- integrate( f <- function( t ) { a$d( t ) }, -Inf, x )$value - a$p( x )
	i <- i + 1
}
plot( seq( -8, 8, 0.02 ), y, type = "l" )

# normal test
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), this.type1.type = 1 )
a$is.ind.p( c( 0, 0.1, 0.3, 0.5, 0.6, 0.65, 0.7, 1 ) )
a$is.conn.p( c( 0, 0.1, 0.3, 0.5, 0.6, 0.65, 0.7, 1 ) )
a$is.ind.q( c( -Inf, -3, 0, 0.2533471, 0.2533472, 0.49, 0.5, 0.52440, 0.52441, 1, Inf ) )
a$is.conn.q( c( -Inf, -3, 0, 0.2533471, 0.2533472, 0.49, 0.5, 0.52440, 0.52441, 1, Inf ) )
a$p( c( 0.253347, 0.253348, 0.5, 0.52440, 0.52441 ) )
( a$p( 0.253347 ) == pnorm( 0.253347, 0, a$intervals[[1]]$sd ) )
( a$p( 0.253348 ) > pnorm( 0.253348, 0, a$intervals[[1]]$sd ) )
( a$p( 0.49 ) < pnorm( 0.49, 0, a$intervals[[2]]$sd ) )
( a$p( 0.5 ) == pnorm( 0.5, 0, a$intervals[[2]]$sd ) )
( a$p( 0.52440 ) == pnorm( 0.52440, 0, a$intervals[[2]]$sd ) )
a$sd() # 0.9773839 (about)

# This test must be run after each normal test
# then the result value must be small enough (than about 1e-6).
abs( a$sd() - sqrt( integrate( f <- function( x ) { ( x - a$mean )^2 * a$d( x ) }, -Inf, Inf )$value ) )

# normal test (type 1)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ), this.type1.type = 2 )
a$sd() # 0.9773785 (about)

# normal test (type 2)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.7 ) ), this.type1.type = 2 )
a$sd() # 1.176089

# normal test (type 2)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 1.2 ) ), this.type1.type = 2 )
a$sd() # 1.753938

# normal test (type 2)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( -5, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ) ), this.type1.type = 2 )
a$sd() # 2.596195

# normal test (type 3a)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( qnorm( c( 0.1, 0.3 ), 0, 1 ), 0, qnorm( c( 0.6, 0.7 ), 0, 1.1 ) ) ), this.type1.type = 2 )
a$sd() # 1.051084

# normal test (type 3b)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( qnorm( c( 0.1, 0.3 ), 0, 1.1 ), 0, qnorm( c( 0.6, 0.7 ), 0, 1 ) ) ), this.type1.type = 2 )
a$sd() # 1.050306

# normal test (type 1)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( -1.1, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ) ), this.type1.type = 2 )
a$sd() # 0.9417019 (about)

# normal test (type 2)
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
q = c( -1.4, qnorm( c( 0.3, 0.5, 0.6, 0.7 ), 0, 1 ) ) ), this.type1.type = 2 )
a$sd() # 1.038298

# normal test
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3,  0.5, 0.6, 0.7 ),
q = c( -0.5, -0.04, 0, 1, 2 ) ) )
a$intervals[[2]]$p.ind[1] - 0.3
a$is.ind.p( c( 0, 0.05, 0.1, 0.2, 0.3, a$intervals[[2]]$p.ind[1], 0.4, 0.5, 0.6, 0.65, 0.7, 0.8 ) )
a$is.conn.p( c( 0, 0.05, 0.1, 0.2, 0.3, a$intervals[[2]]$p.ind[1], 0.4, 0.5, 0.6, 0.65, 0.7, 0.8 ) )
a$is.ind.q( c( -1, -0.48, -0.04, 0, 0.5, 1, 1.5, 2 ) )
a$is.conn.q( c( -1, -0.48, -0.04, 0, 0.5, 1, 1.5, 2 ) )
a$sd() # 2.703973 (about)

plot( seq( -1, 0.05, 0.01 ), a$d( seq( -1, 0.05, 0.01 ) ), type = "l" )
plot( seq( 0.05, 10, 0.01 ), a$d( seq( 0.05, 10, 0.01 ) ), type = "l" )


# warning test
a$set.waypoints(
data.frame(
p = c( 0.5 ),
q = c( 1 ) ),
this.type1.type = 1 )

# normal test
a$set.waypoints(
data.frame(
p = c( 0.5, 0.7 ),
q = c( 0, qnorm( 0.7, 0, 1 ) ) ),
this.type1.type = 1 )
a$sd() # 1

# normal test
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ),
q = c( -9, -8.7, -5.4, -1, 0, 0.3, 0.5, 0.77 ) ),
this.type1.type = 1 )
a$sd() # 5.511733 (about)

# normal test
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ),
q = c( -9, -8.7, -5.4, -1, 0, 0.3, 0.5, 0.77 ) ),
this.type1.type = 2 )
a$sd() # 5.51078 (about)

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.6, 0, 0.75 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.6 ),
q = qs ),
this.type1.type = 1, continuous = TRUE )
a$sd() # 1.111522
a$p( qs )

# normal test
qs <- c( qnorm( 0.1, -0.5, 1.9 ), -0.5, qnorm( 0.7, -0.5, 1.8 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.7 ),
q = qs ),
this.type1.type = 1, symmetric = TRUE )
a$sd() # 1.986118
a$p( qs )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.6 ),
q = c( qnorm( 0.1, 0, 0.9 ), 0, qnorm( 0.6, 0, 1 ) ) ),
this.type1.type = 1, continuous = TRUE )

# for bug-fix
qs <- c( qnorm( 0.1, 0, 1.9 ), 0, qnorm( 0.7, 0, 0.4 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.7 ),
q = c( qnorm( 0.1, 0, 1.9 ), 0, qnorm( 0.7, 0, 0.4 ) ) ),
this.type1.type = 1, symmetric = TRUE )
a$sd() # 2.04997
a$p( qs )


# normal test
qs <- c( qnorm( 0.3, -0.2, 1 ), 0, qnorm( 0.7, 0.2, 0.8 ) )
a$set.waypoints(
data.frame(
p = c( 0.3, 0.5, 0.7 ),
q = qs ),
this.type1.type = 2, continuous = TRUE )
a$sd() # 1.332352 (about)
a$p( qs )

# normal test
qs <- c( qnorm( 0.3, 0.2, 0.7 ), 0.2, qnorm( 0.9, 0.2, 1.1 ) )
a$set.waypoints(
data.frame(
p = c( 0.3, 0.5, 0.9 ),
q = qs ),
this.type1.type = 2, continuous = TRUE )
a$sd() # 0.8988465 (about)
a$p( qs )

# normal test - sharp center
qs <- c( qnorm( 0.3, -0.2, 0.7 ), -0.2, qnorm( 0.9, -0.2, 1 ) )
a$set.waypoints(
data.frame(
p = c( 0.3, 0.5, 0.9 ),
q = qs ),
this.type1.type = 2, symmetric = TRUE )
a$sd() # 0.9863577 (about)
a$p( qs )

# normal test - dent center
qs <- c( qnorm( 0.6, 0, 1.2 ), qnorm( 0.9, 0, 1.05 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.6, 0.9, 0.5 ),
q = qs ),
this.type1.type = 2, symmetric = TRUE )
a$sd() # 1.044687 (about)
a$p( qs )

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
a$sd() # 1.76902 (about)
a$p( qs )

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
a$sd() # 1.090041
a$p( qs )

# normal test
qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.4, 0, 0.7 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5 ),
q = qs ),
this.type1.type = 3 )
a$sd() # 0.8258552
a$p( qs )

# normal test
qs <- c( 0, qnorm( 0.6, 0, 0.7 ), qnorm( 0.9, 0, 0.9 ) )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )
a$sd() # 0.8258552
a$p( qs )

# normal test
qs <- c( 0, qnorm( 0.6, 0, 0.9 ), qnorm( 0.9, 0, 1.2 ) )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )
a$sd() # 1.090041
a$p( qs )

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.7, 0, 0.9 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3, symmetric = TRUE )
a$sd() # 1.022428
a$p( qs )

# normal test
qs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.7, 0, 0.9 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3 )
a$sd() # 0.928998
a$p( qs )

# normal test for v1.3.5 and up
qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.9 ), qnorm( 0.4, 0, 0.87 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.4, 0.5 ),
q = qs ),
this.type1.type = 3 )
a$sd() # 0.9205978 (about)
a$p( qs )

# normal test (also)
qs <- c( 0.5, qnorm( 0.6, 0.5, 0.9 ), qnorm( 0.75, 0.52, 1 ), qnorm( 0.9, 0.57, 1.1 ) )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3 )
a$sd() # 0.9452409 (about)
a$p( qs )

# normal test
qs <- c( -1, -0.1, 0.2, 1.3 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )
a$sd() # 0.9437787 (about)
a$p( qs )

# normal test
qs <- c( qnorm( 0.3, 0, 0.2 ), 0, qnorm( 0.6, 0, 0.7 ) )
a$set.waypoints(
data.frame(
p = c( 0.3, 0.5, 0.6 ),
q = qs ),
this.type1.type = 3 )
a$sd() # 0.8688877
a$p( qs )

# normal test
qs <- c( -1, 0, 0.5 )
a$set.waypoints(
data.frame(
p = c( 0.499, 0.5, 0.6 ),
q = qs ), this.type1.type = 3 )
a$sd() # 511.7395
a$p( qs )

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
a$sd() # 1.326603
a$p( qs )

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
a$sd() # 1.226794
a$p( qs )

# normal test
qs <- c( 1, 1.2, 2.9 )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )
a$sd() # 1.226794
a$p( qs )

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

# normal test
qs <- c( -2, -0.5, 0, 0.7 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.8 ),
q = qs ),
this.type1.type = 3 )
a$sd() # 1.283321
a$p( qs )

# normal test
qs <- c( -2, -0.5, 0, 0.5 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3 )
a$sd() # 1.640214
a$p( qs )

# normal test
qs <- c( -4, -2.5, -2, -1.5 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3 )
a$sd() # 1.640214
a$p( qs )

# normal test
qs <- c( -1.5, -0.4, 0, 0.5 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3 )
a$sd() # 1.122981
a$p( qs )

# This test should be performed after the previous test immediately (to use the previous result).
sd.2 <- a$intervals[[2]]$sd
qs <- c( -1.5, -0.4, 0, qnorm( 0.7, 0, sd.2 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3 )
a$sd() # 1.469891
a$p( qs )

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
a$sd() # 1.69558
a$p( qs )

# This test should be performed after the previous test immediately (to use the previous result).
sd.2 <- a$intervals[[2]]$sd
qs <- c( qnorm( 0.1, 0, sd.2 ), 0, 0.1, 2.3 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )
a$sd() # 1.307507
a$p( qs )

# normal test
qs <- c( 2.3, -1.5, 0, 0.3 )
a$set.waypoints(
data.frame(
p = c( 0.9, 0.1, 0.5, 0.6 ),
q = qs ),
this.type1.type = 3 )
a$sd() # 1.572562
a$p( qs )

# Compare to above test
qs <- c( 0, 0.1, 2.3 )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )
a$sd() # 1.307507
a$p( qs )


# Error case
a$set.waypoints(
data.frame(
p = c( 0.4, 0.5, 0.6, 0.9 ),
q = c( -0.2, 0, 0.1, 2.3 ) ),
this.type1.type = 3 )

# Error case
a$set.waypoints(
data.frame(
p = c( 0.004, 0.5, 0.6, 0.9 ),
q = c( -0.2, 0, 0.1, 2.3 ) ),
this.type1.type = 3 )

# uni.sigma
# Error case
a$set.waypoints(
data.frame(
p = c( 0.25, 0.5, 0.75 ),
q = c( -2, 0, 2.1 ) ),
this.type1.type = 2, continuous = TRUE, uni.sigma = TRUE )

# normal test
qs <- c( -2, 0, 1.8 )
a$set.waypoints(
data.frame(
p = c( 0.25, 0.5, 0.7 ),
q = qs ),
this.type1.type = 2, continuous = TRUE, uni.sigma = TRUE )
a$sd() # 2.171117 (about)
a$p( qs )

plot( seq( -8, 8, 0.01 ), a$d( seq( -8, 8, 0.01 ) ), type = "l" )
plot( seq( -8, 8, 0.01 ), a$p( seq( -8, 8, 0.01 ) ), type = "l" )
plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" )
sample <- a$r( 1000 ); hist( sample )

# normal test
qs <- c( qnorm( 0.25, 0, 1 ), 0, qnorm( 0.7, 0, 1.02 ) )
a$set.waypoints(
data.frame(
p = c( 0.25, 0.5, 0.7 ),
q = qs ),
this.type1.type = 2, continuous = TRUE, uni.sigma = TRUE )
a$sd() # 0.9156116 (about)
a$p( qs )

# 4 waypoints for type1.type = 2
# normal test
qs <- c( qnorm( 0.1, 0, 1.1 ), qnorm( 0.25, 0, 1 ), qnorm( 0.75, 0, 1.05 ), qnorm( 0.9, 0, 1.06 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.75, 0.9 ),
q = qs ),
this.type1.type = 2, continuous = TRUE )
a$sd() # 1.131012 (about)
a$p( qs )

# normal test
qs <- c( -1.7, -0.2, 0.3, 1.9 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.75, 0.9 ),
q = qs ),
this.type1.type = 2, continuous = TRUE )
a$sd() # 1.523708 (about)
a$p( qs )

plot( seq( 0.4, 0.6, 0.001 ), a$q( seq( 0.4, 0.6, 0.001 ) ), type = "l" )
plot( seq( 0.01, 0.06, 0.001 ), a$q( seq( 0.01, 0.06, 0.001 ) ), type = "l" )

# 5 or 6 waypoints for type1.type = 3
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
a$sd() # 1.098104 (about)
a$p( qs )

# normal test
qs <- c( -1.4, -0.7, 0, 0.75, 1.42 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3 )
a$sd() # 1.106754 (about)
a$p( qs )

# normal test
qs <- c( -0.4, 0.3, 1, 1.75, 2.42 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3 )
a$sd() # 1.12696 (about)
a$p( qs )

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
a$sd() # 0.9920315 (about)
a$p( qs )

# normal test
qs <- c( qnorm( 0.1, 0, 0.8 ), qnorm( 0.25, 0, 0.85 ),
			qnorm( 0.4, 0, 0.9 ), qnorm( 0.6, 0, 1.2 ),
			qnorm( 0.75, 0, 1.21 ), qnorm( 0.9, 0, 1.23 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3 )
a$sd() # 1.030409 (about)
a$p( qs )

# plot the burden ratio of the lower distribution for type1.type = 3, symmetric
x<-seq( -5, 5, 0.01 ); plot( x, pnorm( x, 0 ,1 ) - pnorm( x, 0, 1/sqrt(2) ) / sqrt(2), type="l" )

