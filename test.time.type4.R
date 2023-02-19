#### This file is for processing time measurement for CGD$q where type1.type = 4.

a<-CGD$new()
control <- list( ftol = 1e-4 )
test.count <- 0
total.time <- 0

qs <- c( -1.464921, -0.280095, 0.027170, 0.352307, 1.201652 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5, 0.6, 0.8 ),
q = qs ),
this.type1.type = 4, uni.sigma = TRUE )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5, 0.6, 0.8 ),
q = qs ),
this.type1.type = 4, uni.sigma = TRUE )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5, 0.6, 0.8 ),
q = qs ),
this.type1.type = 4 )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- round( qs, 3 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5, 0.6, 0.8 ),
q = qs ),
this.type1.type = 4 )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
			qnorm( 0.4, 0, 0.98 ), qnorm( 0.6, 0, 1.03 ),
			qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
			qnorm( 0.5, 0, 1 ),
			qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4, control = control )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 2 ), qnorm( 0.25, 0, 1.5 ),
			qnorm( 0.5, 0, 1 ),
			qnorm( 0.75, 0, 0.9 ), qnorm( 0.9, 0, 0.8 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4, diff.mean = TRUE )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 2 ), qnorm( 0.25, 0, 1.5 ),
			qnorm( 0.5, 0, 1 ),
			qnorm( 0.75, 0, 0.9 ), qnorm( 0.9, 0, 0.792 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 0.792 ), qnorm( 0.25, 0, 0.9 ),
			qnorm( 0.5, 0, 1 ),
			qnorm( 0.75, 0, 1.5 ), qnorm( 0.9, 0, 2 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 2 ), qnorm( 0.25, 0, 1.5 ),
			qnorm( 0.5, 0, 1 ),
			qnorm( 0.75, 0, 0.95 ), qnorm( 0.9, 0, 0.792 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 0.792 ), qnorm( 0.25, 0, 0.95 ),
			qnorm( 0.5, 0, 1 ),
			qnorm( 0.75, 0, 1.5 ), qnorm( 0.9, 0, 2 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ), qnorm( 0.4, 0, 0.97 ),
			qnorm( 0.6, 0, 1.1 ), qnorm( 0.75, 0, 1.12 ), qnorm( 0.9, 0, 1.2 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -1, -0.8, -0.3, 0, 0.32, 1, 1.2 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4, control = list( maxit = 1000 ) )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

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

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -1.2, -1, -0.32, 0, 0.3, 0.8, 1 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4, control = list( maxit = 1000 ) )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -1.9535092, -0.9569967, -0.3331447, 0, 0.3085147, 0.7858078, 1.4341828 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4, control = control )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -1.4, -0.78, -0.3, 0, 0.32, 0.96, 2 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 4 )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -1.4, -0.96, -0.61, -0.3, 0.32, 0.715, 1.24, 2 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9 ),
q = qs ),
this.type1.type = 4 )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( a$q( 0.1 ), a$q( 0.25 ), a$q( 0.4 ), a$q( 0.50 ), a$q( 0.6 ), a$q( 0.7 ), a$q( 0.8 ), a$q( 0.9 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ),
q = qs ),
this.type1.type = 4 )

test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

print( paste( "test.count:", test.count ) )
print( paste( "total.time:", total.time ) )
print( paste( "mean time:", total.time / test.count ) )
