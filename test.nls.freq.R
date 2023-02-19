#### This file is for testing CGD$nls.freq and CGD$nls.freq.all.

library( "cgd" )
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
a$type1.type # 1
diff.check( a, x, freq, total )

a <- nls.freq( x, freq, total, normal = TRUE )
a$type1.type # 2
diff.check( a, x, freq, total )

########
# This command-block should be run after each non-error test to confirm the result.
a
plot.freq.and.d( a, x, freq, total )
########

# Error case
a$nls.freq( x[1:length( x ) - 1], freq, total, normal = TRUE )
a$nls.freq( x, freq[1:length( freq ) - 1], total, normal = TRUE )
a$nls.freq( x, freq, floor( sum( freq ) ), normal = TRUE )
a$nls.freq( x[1:2], freq[1:2], floor( sum( freq[1:2] ) ), normal = TRUE )
a$nls.freq( -x, -freq, floor( sum( freq ) ), normal = TRUE )
a$nls.freq( x, freq, total, this.type1.type = 0 )
a$nls.freq( x, freq, total, this.type1.type = 5 )
a$nls.freq( x, freq, total, start = list( mean = -10, sqrt.sd = 10 ), normal = TRUE )

# normal test
a$nls.freq( x, freq, total, normal = TRUE, this.type1.type = 1, trace = TRUE )

# Error case
freq <- floor( dnorm( x, 0, 0.5 ) * 1000 )
total <- sum( freq ) * 1.1
a$nls.freq( x, freq, total, normal = TRUE )

# normal test
a$nls.freq( x, freq, total, normal = TRUE, set.by.start = TRUE )
diff.check( a, x, freq, total )

# normal test
freq <- dnorm( x, 0, 0.5 ) * 1000 + sin( x + 0.05 ) * 10
total <- sum( freq ) * 1.1
a$nls.freq( x, freq, total, normal = TRUE, this.type1.type = 3 )
a$type1.type == 1 # TRUE
diff.check( a, x, freq, total )

# normal test
a$set.waypoints(
	data.frame( p = c( 0.3, 0.5, 0.7 ), q = c( qnorm( 0.3, -0.2, 1 ), 0, qnorm( 0.7, 0.2, 0.8 ) ) ),
	this.type1.type = 2, continuous = TRUE )
a$nls.freq( x, freq, total, normal = TRUE )
a$type1.type == 2 # TRUE
diff.check( a, x, freq, total )

# normal test
a$set.waypoints(
	data.frame( p = c( 0.1, 0.4, 0.5 ), q = c( qnorm( 0.1, 0, 1.2 ), qnorm( 0.4, 0, 0.9 ), 0 ) ),
	this.type1.type = 3 )
a$nls.freq( x, freq, total, normal = TRUE )
a$type1.type == 1 # TRUE
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
a$nls.freq( x, freq, total, symmetric = TRUE )

# Warning case
a$nls.freq( x, freq, total, control = list( warnOnly = TRUE ), symmetric = TRUE )
a$type1.type == 2 # TRUE
a$is.symmetric() # TRUE
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, normal = TRUE )
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, this.type1.type = 1 )
a$type1.type == 1 # TRUE
a$is.symmetric() # TRUE
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, set.by.start = TRUE, symmetric = TRUE )
a$type1.type == 2 # TRUE
a$is.symmetric() # TRUE
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, v.grad = TRUE )
a$type1.type == 3 # TRUE
a$is.v.grad() # TRUE
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, uni.sigma = TRUE )
a$type1.type == 3 # TRUE
a$is.v.grad() # FALSE
a$is.uni.sigma() # TRUE
diff.check( a, x, freq, total )

#### nls.freq.all

# normal test
cgd.kind( 1:15 )
cgd.kind.index( 1:15 )

# normal test
cgds <- nls.freq.all( x, freq, total )
cgds
cgds$cgd
cgds$best
cgds$best.cor
cgds$cor
for ( i in 1:length( cgds$cgd ) ) print( paste( i, ":", cgds$cgd[[i]]$kind ) )

plot.freq.and.d( cgds$best, x, freq, total )

# normal test
cgd.kind( cgds$cgd )
cgd.kind.index( cgds$cgd )
cgd.kind( cgds$cgd[[1]] ) # == cgds$cgd[[1]]$kind
cgd.kind.index( cgds$cgd[[1]] ) # == cgds$cgd[[1]]$kind.index

## nls.start.template (a revenge of cgds$cgd[[13]])
diff.check( cgds$cgd[[10]], x, freq, total )$summary[5]
diff.check( cgds$cgd[[13]], x, freq, total )$summary[5] # smaller

cgds$cgd[[10]]$kind # "3-Mean-Differed Sigma-Equaled Vertical Gradational Distribution"
cgds$cgd[[13]]$kind # "Mean-Differed Sigma-Equaled Vertical-Horizontal Gradational Distribution"

cgds$cgd[[10]]$intervals.mean() # -0.6709839 -0.1977604  0.2928316 (about)
cgds$cgd[[10]]$intervals.sd() # 0.6400899 0.6400899 0.6400899 (about)

start.list <- nls.start.template( cgds$cgd[[13]]$kind )
start.list

start.list$mean.1.1 <- -0.6709839
start.list$mean.1.2 <- -0.1977604
start.list$mean.2.1 <- 0.2928316
start.list$mean.2.2 <- -0.1977604
start.list$sqrt.sd <- sqrt( 0.64 ) # sqrt( 0.64 ) == 0.8
start.list

a$nls.freq( x, freq, total, start = start.list, kind = cgds$cgd[[13]] )
a$kind == cgds$cgd[[13]]$kind # TRUE
diff.check( a, x, freq, total )$summary[5] # > diff.check( cgds$cgd[[10]], x, freq, total )$summary[5]
# a revenge is done.

# then retrying nls.freq.all
start.list <- init.start.list()
length( start.list ) # 15
start.list # All elements are NULL.

start.list[[13]] <- nls.start.template( cgds$cgd[[13]]$kind )

start.list[[13]]$mean.1.1 <- -0.6709839
start.list[[13]]$mean.1.2 <- -0.1977604
start.list[[13]]$mean.2.1 <- 0.2928316
start.list[[13]]$mean.2.2 <- -0.1977604
start.list[[13]]$sqrt.sd <- sqrt( 0.64 ) # sqrt( 0.64 ) == 0.8
start.list

cgds <- nls.freq.all( x, freq, total, start = start.list )
cgds
cgds$cgd
cgds$best
cgds$best.cor
cgds$cor
cgds$cor[13] # 0.9956901 (about)

#### dent center base
seed$set.waypoints(
data.frame( p = c( 0.6, 0.9, 0.5 ), q = c( qnorm( 0.6, 0, 1.2 ), qnorm( 0.9, 0, 1.05 ), 0 ) ),
this.type1.type = 2, symmetric = TRUE )
freq <- ( seed$p( x + 0.1 ) - seed$p( x - 0.1 ) ) * ( 1000 + sin( x * 10 + 0.5 ) * 100 )
total <- sum( freq )


## set.by.start
a$nls.freq( x, freq, total, set.by.start = TRUE, normal = TRUE )
diff.check( a, x, freq, total )

a$nls.freq( x, freq, total, set.by.start = TRUE, this.type1.type = 1 )
diff.check( a, x, freq, total )

a$nls.freq( x, freq, total, set.by.start = TRUE, symmetric = TRUE )
diff.check( a, x, freq, total )

a$nls.freq( x, freq, total, set.by.start = TRUE, this.type1.type = 1, uni.sigma = TRUE )
a$type1.type # 1
a$is.uni.sigma() # FALSE
diff.check( a, x, freq, total )

a$nls.freq( x, freq, total, set.by.start = TRUE, this.type1.type = 2 )
diff.check( a, x, freq, total )

a$nls.freq( x, freq, total, set.by.start = TRUE, uni.sigma = TRUE )
a$type1.type # 2
a$is.uni.sigma() # TRUE
diff.check( a, x, freq, total )

a$nls.freq( x, freq, total, set.by.start = TRUE, this.type1.type = 2, uni.mean = FALSE )
diff.check( a, x, freq, total )

a$nls.freq( x, freq, total, set.by.start = TRUE, v.grad = TRUE )
diff.check( a, x, freq, total )

a$nls.freq( x, freq, total, set.by.start = TRUE, v.grad = TRUE, uni.mean = FALSE )
diff.check( a, x, freq, total )

a$nls.freq( x, freq, total, set.by.start = TRUE, this.type1.type = 3, uni.sigma = TRUE )
diff.check( a, x, freq, total )

a$nls.freq( x, freq, total, set.by.start = TRUE, this.type1.type = 3 )
diff.check( a, x, freq, total )

a$nls.freq( x, freq, total, set.by.start = TRUE, this.type1.type = 3, uni.mean = FALSE )
diff.check( a, x, freq, total )

a$nls.freq( x, freq, total, set.by.start = TRUE, this.type1.type = 4, uni.sigma = TRUE )
diff.check( a, x, freq, total )

a$nls.freq( x, freq, total, set.by.start = TRUE, this.type1.type = 4 )
diff.check( a, x, freq, total )

a$nls.freq( x, freq, total, set.by.start = TRUE, this.type1.type = 4, uni.mean = FALSE )
diff.check( a, x, freq, total )

## nls test
# normal test
a$nls.freq( x, freq, total, this.type1.type = 2, uni.sigma = TRUE )
a$type1.type == 2 # TRUE
a$is.uni.sigma() # TRUE
a$is.uni.mean() # FALSE
a$intervals.mean() # 1.161789 -1.161223 (about)
a$intervals.sd() # 0.8834894 0.8834894 (about)
diff.check( a, x, freq, total )

# Warning case
a$nls.freq( x, freq, total, control = list( warnOnly = TRUE ), this.type1.type = 1 )
a$type1.type == 1 # TRUE
a$is.uni.mean() # TRUE
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, this.type1.type = 2 )
a$type1.type == 2 # TRUE
a$is.uni.sigma() # FALSE
a$is.uni.mean() # TRUE
all( a$intervals.mean() == a$intervals.mean()[1] ) # TRUE
a$intervals.sd() # 1.084905 1.087016 (about)
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, uni.mean = FALSE, this.type1.type = 2 )
a$type1.type == 2 # TRUE
a$is.uni.sigma() # FALSE
a$is.uni.mean() # FALSE
a$intervals.mean() # -0.2743978  0.2750668 (about)
a$intervals.sd() # 0.6652693 0.6667470 (about)
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, this.type1.type = 3 )
a$type1.type == 3 # TRUE
all( a$intervals.mean() == a$intervals.mean()[1] ) # TRUE
a$intervals.sd() # 0.8854747 1.1734463 0.8907395 (about)
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, uni.mean = FALSE, this.type1.type = 3 )
a$type1.type == 3 # TRUE
a$is.uni.mean() # FALSE
a$intervals.mean() # 0.147964498  0.007134283 -0.225932696 (about)
a$intervals.sd() # 0.9219731 1.2638195 0.9800101 (about)
diff.check( a, x, freq, total )

# Warning case
a$nls.freq( x, freq, total, control = list( warnOnly = TRUE ), v.grad = TRUE )
a$type1.type == 3 # TRUE
a$is.v.grad() # TRUE
all( a$intervals.mean() == a$intervals.mean()[1] ) # TRUE
a$intervals.sd() # 0.8881116 1.1734366 0.8881116 (about)
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, v.grad = TRUE, uni.mean = FALSE )
a$type1.type == 3 # TRUE
a$is.v.grad() # TRUE
all( a$intervals.mean() == a$intervals.mean()[1] ) # FALSE
a$intervals.sd() # 0.8881109 1.1734375 0.8881109 (about)
diff.check( a, x, freq, total )

# Warning case
a$nls.freq( x, freq, total, control = list( warnOnly = TRUE ), this.type1.type = 4 )
a$type1.type == 4 # TRUE
all( a$intervals.mean() == a$intervals.mean()[1] ) # TRUE
a$intervals.sd() # 0.8890329 1.1712108 0.8890329 0.8871634 1.1756816 0.8871634 (about)
diff.check( a, x, freq, total )

# normal test
a$nls.freq( x, freq, total, control = list( maxiter = 250 ), uni.mean = FALSE, this.type1.type = 4 )
a$type1.type == 4	# TRUE
a$intervals.mean()	# -0.5235105 -0.2758431 -0.5235105 -0.2828730  0.5360592 -0.2828730 (about)
a$intervals.mean()[1] == a$intervals.mean()[3]	# TRUE ([1] and [3] are the mean of the lower-outer normal distribution)
a$intervals.mean()[4] == a$intervals.mean()[6]	# TRUE ([4] and [6] are the mean of the upper-outer normal distribution)
a$intervals.mean()[1] != a$intervals.mean()[2]	# TRUE ([2] is the mean of the lower-inner normal distribution)
a$intervals.mean()[4] != a$intervals.mean()[5]	# TRUE ([5] is the mean of the upper-inner normal distribution)
a$intervals.sd()	# 0.6690816 0.7251914 0.6690816 0.4712487 1.0446081 0.4712487 (about)
a$intervals.sd()[1] == a$intervals.sd()[3]	# TRUE ([1] and [3] are the sd of the lower-outer normal distribution)
a$intervals.sd()[4] == a$intervals.sd()[6]	# TRUE ([4] and [6] are the sd of the upper-outer normal distribution)
a$intervals.sd()[1] != a$intervals.sd()[2]	# TRUE ([2] is the sd of the lower-inner normal distribution)
a$intervals.sd()[4] != a$intervals.sd()[5]	# TRUE ([5] is the sd of the upper-inner normal distribution)
diff.check( a, x, freq, total )

#### nls.freq.all
# normal test
cgds <- nls.freq.all( x, freq, total )
cgds
cgds$cgd
cgds$best
cgds$best.cor
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
nls.start.template( 1 )
nls.start.template( 15 )
nls.start.template( "Mean of Mean-Equaled Sigma-Differed 2 Normal Distributions" )

# Warning case
nls.start.template( 16 )

start.list <- init.start.list()
start.list[[ 1]] <- list( mean = 0,						sqrt.sd = sqrt( 1 ) )
start.list[[ 2]] <- list( mean = 0,						sqrt.sd.1 = sqrt( 2 ), sqrt.sd.2 = sqrt( 1 ) )
start.list[[ 3]] <- list( mean = 0,						sqrt.sd.1 = sqrt( 2 ), sqrt.sd.2 = sqrt( 1 ) )
start.list[[ 4]] <- list( mean.1 = -1, mean.2 = 1,		sqrt.sd = sqrt( 1 ) )
start.list[[ 5]] <- list( mean = 0,						sqrt.sd.1 = sqrt( 1 ), sqrt.sd.2 = sqrt( 1 ) )
start.list[[ 6]] <- list( mean.1 = -1, mean.2 = 1,		sqrt.sd.1 = sqrt( 1 ), sqrt.sd.2 = sqrt( 1 ) )
start.list[[ 7]] <- list( mean.1 = 0, mean.2 = 0,		sqrt.sd = sqrt( 1 ) )
start.list[[ 8]] <- list( mean = 0,						sqrt.sd.1 = sqrt( 2 ), sqrt.sd.2 = sqrt( 1 ) )
start.list[[ 9]] <- list( mean.1 = 0, mean.2 = 0,		sqrt.sd.1 = sqrt( 2 ), sqrt.sd.2 = sqrt( 1 ) )
start.list[[10]] <- list( mean.1 = 0, mean.2 = 0, mean.3 = 0,
														sqrt.sd = sqrt( 1 ) )
start.list[[11]] <- list( mean = 0,						sqrt.sd.1 = sqrt( 2 ), sqrt.sd.2 = sqrt( 1 ), sqrt.sd.3 = sqrt( 2 ) )
start.list[[12]] <- list( mean.1 = 0, mean.2 = 0, mean.3 = 0,
														sqrt.sd.1 = sqrt( 2 ), sqrt.sd.2 = sqrt( 1 ), sqrt.sd.3 = sqrt( 2 ) )
start.list[[13]] <- list( mean.1.1 = 0, mean.1.2 = 0, mean.2.1 = 0, mean.2.2 = 0,
														sqrt.sd = sqrt( 1 ) )
start.list[[14]] <- list( mean = 0,						sqrt.sd.1.1 = sqrt( 2 ), sqrt.sd.1.2 = sqrt( 1 ),
														sqrt.sd.2.1 = sqrt( 2 ), sqrt.sd.2.2 = sqrt( 1 ) )
start.list[[15]] <- list( mean.1.1 = 0, mean.1.2 = 0,	sqrt.sd.1.1 = sqrt( 2 ), sqrt.sd.1.2 = sqrt( 1 ),
						  mean.2.1 = 0, mean.2.2 = 0,	sqrt.sd.2.1 = sqrt( 2 ), sqrt.sd.2.2 = sqrt( 1 ) )

cgds <- nls.freq.all( x, freq, total, start = start.list )
cgds
cgds$cgd
cgds$best
cgds$best.cor
cgds$cor
plot.freq.and.d( cgds$best, x, freq, total )

# normal and error test
cgds <- nls.freq.all( x, freq, total, control = list( maxiter = 50, warnOnly = FALSE ) )
cgds
cgds$cgd
cgds$best
cgds$best.cor
cgds$cor
plot.freq.and.d( cgds$best, x, freq, total )

# normal test
cgds <- nls.freq.all( x, freq, total, method = "spearman", trace = TRUE )
cgds
cgds$cgd
cgds$best
cgds$best.cor
cgds$cor
plot.freq.and.d( cgds$best, x, freq, total )
