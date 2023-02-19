#### This file is for processing time measurement for CGD$q where type1.type = 3.

a<-CGD$new()
control <- list( ftol = 1e-4 )
test.count <- 0
total.time <- 0

qs <- c( qnorm( 0.1, 0, 1.2 ), qnorm( 0.4, 0, 0.9 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5 ),
q = qs ),
this.type1.type = 3, control = control )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.4, 0, 0.7 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( 0, qnorm( 0.6, 0, 0.7 ), qnorm( 0.9, 0, 0.9 ) )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( 0, qnorm( 0.6, 0, 0.9 ), qnorm( 0.9, 0, 1.2 ) )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.4, 0, 0.7 ), qnorm( 0.6, 0, 0.7 ), qnorm( 0.9, 0, 0.9 ) )
a$set.waypoints(
data.frame(
p = c( 0.4, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( 0, qnorm( 0.6, 0, 0.9 ), qnorm( 0.9, 0, 1.2 ) )
a$set.waypoints(
data.frame(
p = c( 0.4, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 1.5 ), qnorm( 0.3, 0, 1 ), qnorm( 0.6, 0, 0.8 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.6 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.7, 0, 0.9 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, control = control )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]


system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.9 ), qnorm( 0.4, 0, 0.85 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.4 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.4 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, control = control )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.9, 0, 1 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.9 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -0.479, -0.472, -0.215 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.7 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.32 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( 0.215, 0.217, 0.472 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.12, 0.3 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

a$set.waypoints(
data.frame(
p = c( 0.6, 0.62, 0.8 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.9 ), 0, qnorm( 0.6, 0, 0.85 )  )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.6 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.9 ), qnorm( 0.4, 0, 0.85 ), qnorm( 0.6, 0, 0.85 )  )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.4, 0.6 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.4, 0.6 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, control = control )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.6, 0, 1 ), qnorm( 0.7, 0, 1.01 ), qnorm( 0.8, 0, 1.1 ), qnorm( 0.9, 0, 1.12 )  )
a$set.waypoints(
data.frame(
p = c( 0.6, 0.7, 0.8, 0.9 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.85 ), qnorm( 0.6, 0, 0.85 )  )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.6 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.85 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.9 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.8 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( 0, qnorm( 0.7, 0, 0.8 ), qnorm( 0.9, 0, 1 ) )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.7, 0.9 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( 0, 0.7, 0.9 )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.7, 0.9 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( 0, 0.7, 0.9 )
a$clear()
a$set.waypoints(
data.frame(
p = c( 0.5, 0.7, 0.9 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )
c( a$sd(), a$lsd(), a$usd() ) # 0.8121997 0.8874782 0.7291907

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.8 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -0.7, -0.5, 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -0.7, -0.5, 0 )
a$clear()
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -1.7, -0.5, 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -1.7, -0.5, 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.4, 0, 0.8 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.6, 0, 0.85 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.6 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.7, 0, 0.9 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3, control = control )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 1 ), qnorm( 0.3, 0, 0.9 ), qnorm( 0.4, 0, 0.87 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.4, 0.5 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( 0.5, qnorm( 0.6, 0.5, 0.9 ), qnorm( 0.75, 0.52, 1 ), qnorm( 0.9, 0.57, 1.1 ) )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -1, -0.1, 0.2, 1.3 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.3, 0, 0.2 ), 0, qnorm( 0.6, 0, 0.7 ) )
a$set.waypoints(
data.frame(
p = c( 0.3, 0.5, 0.6 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -1, 0, 0.5 )
a$set.waypoints(
data.frame(
p = c( 0.499, 0.5, 0.6 ),
q = qs ), this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -2, -0.5, 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( 0, 0.2, 1.9 )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( 1, 1.2, 2.9 )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -2, -0.5, 0, 0.7 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.8 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -2, -0.5, 0, 0.5 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -4, -2.5, -2, -1.5 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.3, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -1.5, -0.4, 0, 0.5 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

sd.2 <- a$intervals[[2]]$sd
qs <- c( -1.5, -0.4, 0, qnorm( 0.7, 0, sd.2 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5, 0.7 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -2, 0, 0.1, 2.3 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3, diff.mean = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

sd.2 <- a$intervals[[2]]$sd
qs <- c( qnorm( 0.1, 0, sd.2 ), 0, 0.1, 2.3 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( 2.3, -1.5, 0, 0.3 )
a$set.waypoints(
data.frame(
p = c( 0.9, 0.1, 0.5, 0.6 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( 0, 0.3, 2.3 )
a$set.waypoints(
data.frame(
p = c( 0.5, 0.6, 0.9 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -2.3, -0.2, 0, 0.2 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5, 0.6 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, -0.2, 1 ), qnorm( 0.4, 0, 1 ), 0 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE, uni.sigma = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.5 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, -0.2, 1 ), qnorm( 0.4, 0, 1 ), qnorm( 0.6, 0.1, 1 ), qnorm( 0.8, 0.4, 1 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.6, 0.8 ),
q = qs ),
this.type1.type = 3, uni.sigma = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.6, 0.8 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.6, 0.8 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 1.12 ), qnorm( 0.25, 0, 1.11 ), qnorm( 0.5, 0, 1 ), qnorm( 0.75, 0, 1.05 ), qnorm( 0.9, 0, 1.06 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3, control = control )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -1.4, -0.7, 0, 0.75, 1.42 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -0.4, 0.3, 1, 1.75, 2.42 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.5, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -1.412, -0.262, 0.272, 0.756, 1.427 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.4, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 0.9 ), qnorm( 0.25, 0, 0.95 ),
			qnorm( 0.4, 0, 0.98 ), qnorm( 0.6, 0, 1.03 ),
			qnorm( 0.75, 0, 1.1 ), qnorm( 0.9, 0, 1.13 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3, control = control )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( qnorm( 0.1, 0, 0.8 ), qnorm( 0.25, 0, 0.85 ),
			qnorm( 0.4, 0, 0.9 ), qnorm( 0.6, 0, 1.2 ),
			qnorm( 0.75, 0, 1.21 ), qnorm( 0.9, 0, 1.23 ) )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4, 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3 )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( -1, -0.8, -0.3 )
a$set.waypoints(
data.frame(
p = c( 0.1, 0.25, 0.4 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

qs <- c( 0.32, 1, 1.2 )
a$set.waypoints(
data.frame(
p = c( 0.6, 0.75, 0.9 ),
q = qs ),
this.type1.type = 3, v.grad = TRUE )

system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )
test.count <- test.count + 1
total.time <- total.time + system.time( plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" ) )[1]

print( paste( "test.count:", test.count ) )
print( paste( "total.time:", total.time ) )
print( paste( "mean time:", total.time / test.count ) )
