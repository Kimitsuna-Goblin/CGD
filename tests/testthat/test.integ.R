################################################################################################
## This file is to test integrals for both used in CGD.R and
## not used directly in CGD.R but are important intermediate calculations.
##
## Warning: If you run this file entirely with show.progress <- TRUE option,
##          it outputs over 1M lines of text (about 23MB).
################################################################################################

library( "cgd" )
library( "testthat" )

################################
# test routines
################################

means <- c( 0, 1, -1, 0.2, -0.2, 2, -2, 3, -3, 4, -4 )
sds <- c( 1, 0.4, 0.6, 1.2, 3, 3.55 )
xs <- list( c( 0, 1 ), c( -1, 0 ), c( -2, 3 ), c( 2, 3 ), c( 0.2, 0.6 ), c( 1, 1.6 ),
            c( -1.6, -1 ), c( -3, -2 ), c( -3.5, -3 ), c( -0.6, -0.2 ), c( -0.2, 0.6 ) )
xs.inf <- list( c( -Inf, 1 ),  c( -Inf, 0 ), c( -Inf, 1.2 ), c( -Inf, 3 ), c( -Inf, 0.2 ), c( -Inf, 0.6 ),
                c( -Inf, -2 ), c( -Inf, -1.2 ), c( -Inf, -0.2 ), c( -Inf, -0.6 ) )

## If you want to show the progresses during check.integ,
## set TRUE this option.
show.progress <- FALSE
#show.progress <- TRUE

# Alias of cat() to show progress.
cat.progress <- function( x ) cat()
if ( show.progress )
{
	cat.progress <- cat
}

################################################################################################
#' Check the value and error of integral-solved function with integrate function.
#'
#' Check the integral-solved function of 1-mean, 1-s.d. and 1-interval arguments.
#' This function uses above means, sds, xs and xs.inf for samples.
#' @param   integ           The integral-solved function. This function must return a numeric.
#' @param   via.integrate   The function using integrate.
#'                          This function should return a list like \link[stats]{integrate}.
#' @param   target.cond     This function is called before each of check processes and
#'                          gets mean, s.d., and x (interval) of a sample as arguments.
#'                          If this function returns TRUE, the sample is regardes as good,
#'                          and the check prosses will run with the sample.
#'                          Otherwise, if this function returns FALSE,
#'                          the sample is regarded as not suitable (ex. too strange),
#'                          the check prosses will skipped.
#' @param   d.range         The maximum abs-difference between integ and via.integrate values
#'                          to consider as check is succeded.
#' @param   permit.d        This function is called after each of check processes and gets
#'                          d (the abs-difference between integ and via.integrate values),
#'                          abs.error (returned by via.integrate),
#'                          info (a list of mean, sd and x of the sample) and
#'                          parent.info (a list of pass, mean and sd of check.integ.2)
#'                          of the check process as arguments.
#'                          If this function returns TRUE, the check is regarded as success
#'                          even if d > d.range.
#'                          If this function returns FALSE, the check result follows
#'                          the comparison of d and d-range.
#'                          The purpose of this function is to rescue the check evaluations from
#'                          unreasonable results caused by singularities and other problems
#'                          of the integrete function.
#' @param   inf.to.x        When you check with intervals of from -Inf to a numeric, set TRUE.
#' @param   inf.to.inf      When you check with intervals of from -Inf to Inf, set TRUE.
#' @param   x.to.inf        When you check with intervals of from a numeric to Inf, set TRUE.
#' @return  TRUE if all checks are succeeded.
################################################################################################
check.integ <- function( integ = function( mean, sd, x ) {},
                            via.integrate = function( mean, sd, x ) {},
                            target.cond = function( mean, sd, x ) { TRUE }, d.range = 1e-8,
                            permit.d = function( d, abs.error, info, parent.info ) { FALSE },
                            parent.info = NULL,
                            inf.to.x = FALSE, inf.to.inf = FALSE, x.to.inf = FALSE )
{
    cat( "integ: " )
    print( integ )

    if ( inf.to.inf )
    {
        target.xs <- list( c( -Inf, Inf ) )
    }
    else if ( inf.to.x )
    {
        target.xs <- xs.inf
    }
    else if ( x.to.inf )
    {
        target.xs <- lapply( xs.inf, function( xs ) c( xs[2], xs[1] ) )
    }
    else
    {
        target.xs <- xs
    }

    max.d <- 0
    max.abs.error <- 0
    for ( mean in means )
    {
        for ( sd in sds )
        {
            for ( x in target.xs )
            {
                if ( target.cond( mean, sd, x ) )
                {
                    result.integrate <- via.integrate( mean, sd, x )
                    d <- integ( mean, sd, x ) - result.integrate$value
                    abs.error <- result.integrate$abs.error
                    if ( abs( d ) > max( d.range, abs.error ) &&
                         !permit.d( d, abs.error, list( mean = mean, sd = sd, x = x ), parent.info ) )
                    {
                        cat( "Error!\n\n" )
                        print( paste( "mean:", mean ) )
                        print( paste( "sd:", sd ) )
                        print( paste( "x:", x[1], x[2] ) )
                        cat( "\n" )
                        print( paste( "integ:", integ( mean, sd, x ) ) )
                        print( paste( "via.integrate:", result.integrate$value ) )
                        print( paste( "abs.error: ", abs.error ) )
                        print( paste( "d: ", d ) )
                        cat( "\n" )
                        stop( "The results are too different." )
                    }
                    else if ( abs( d ) > max.d )
                    {
                        max.d <- abs( d )
                    }

                    if ( abs.error > max.abs.error )
                    {
                        max.abs.error <- abs.error
                    }

                    cat.progress( "." )
                }
            }
        }
        cat.progress( "\n" )
    }

    if ( max.d > d.range )
    {
        print( paste( "The max.d is over d.range as the abs.error is too large. max of abs.error:", max.abs.error ) )
    }

    cat( "OK.\n" )
    print( paste( "max.d:", max.d ) )
    return ( TRUE )
}

################################################################################################
#' Check the value and error of integral-solved function with integrate function.
#'
#' Check the integral-solved function of 2-mean, 2-s.d. and 1-interval arguments.
#' This function uses above means, sds, xs and xs.inf for samples.
#' @param   integ           The integral-solved function. This function must return a numeric.
#' @param   via.integrate   The function using integrate.
#'                          This function should return a list like \link[stats]{integrate}.
#' @param   target.cond     This function is called before each of check processes and
#'                          gets mean, s.d., and x (interval) of a sample as arguments.
#'                          If this function returns TRUE, the sample is regardes as good,
#'                          and the check prosses will run with the sample.
#'                          Otherwise, if this function returns FALSE,
#'                          the sample is regarded as not suitable (ex. too strange),
#'                          the check prosses will skipped.
#' @param   d.range         The maximum abs-difference between integ and via.integrate values
#'                          to consider as check is succeded.
#' @param   permit.d        This function is called after each of check processes and gets
#'                          d (the abs-difference between integ and via.integrate values),
#'                          abs.error (returned by via.integrate),
#'                          info (a list of mean, sd and x of the sample) and
#'                          parent.info (a list of pass, mean and sd of check.integ.2)
#'                          of the check process as arguments.
#'                          If this function returns TRUE, the check is regarded as success
#'                          even if d > d.range.
#'                          If this function returns FALSE, the check result follows
#'                          the comparison of d and d-range.
#'                          The purpose of this function is to rescue the check evaluations from
#'                          unreasonable results caused by singularities and other problems
#'                          of the integrete function.
#' @param   inf.to.x        When you check with intervals of from -Inf to a numeric, set TRUE.
#' @param   inf.to.inf      When you check with intervals of from -Inf to Inf, set TRUE.
#' @param   x.to.inf        When you check with intervals of from a numeric to Inf, set TRUE.
#' @return  TRUE if all checks are succeeded.
################################################################################################
check.integ.2 <- function( integ = function( mean.1, sd.1, mean.2, sd.2, x ) {},
                            via.integrate = function( mean.1, sd.1, mean.2, sd.2, x ) { TRUE },
                            target.cond = function( mean.1, sd.1, mean.2, sd.2, x ) { TRUE },
                            d.range = 1e-8,
                            permit.d = function( d, abs.error, info, parent.info ) { FALSE },
                            inf.to.x = FALSE, inf.to.inf = FALSE, x.to.inf = FALSE )
{
    print( "[check.integ.2] begin" )
    cat( "integ: " )
    print( integ )

    print( "[pass 1]" )
    for ( mean.1 in means )
    {
        for ( sd.1 in sds )
        {
            print( paste( "mean.1:", mean.1, " sd.1:", sd.1 ) )
            check.integ( function( mean, sd, x ) integ( mean.1, sd.1, mean, sd, x ),
                            function( mean, sd, x ) via.integrate( mean.1, sd.1, mean, sd, x ),
                            function( mean, sd, x ) target.cond( mean.1, sd.1, mean, sd, x ),
                            d.range, permit.d, list( pass = 1, mean = mean.1, sd = sd.1 ),
                            inf.to.x, inf.to.inf, x.to.inf )
        }
    }

    print( "[pass 2]" )
    for ( mean.2 in means )
    {
        for ( sd.2 in sds )
        {
            print( paste( "mean.2:", mean.2, " sd.2:", sd.2 ) )
            check.integ( function( mean, sd, x ) integ( mean, sd, mean.2, sd.2, x ),
                            function( mean, sd, x ) via.integrate( mean, sd, mean.2, sd.2, x ),
                            function( mean, sd, x ) target.cond( mean, sd, mean.2, sd.2, x ),
                            d.range, permit.d, list( pass = 2, mean = mean.2, sd = sd.2 ),
                            inf.to.x, inf.to.inf, x.to.inf )
        }
    }

    print( "[check.integ.2] OK" )
    return ( TRUE )
}

################################################################################################
#' Get max of abs.error of integrate function.
#'
#' Get max of abs.error of integrate function of 1-mean, 1-s.d. and 1-interval arguments.
#' This function uses above means, sds, xs and xs.inf for samples.
#' @param   integ.func      The function using integrate.
#'                          This function should return a list like \link[stats]{integrate}.
#' @param   target.cond     This function is called before each of check processes and
#'                          gets mean, s.d., and x (interval) of a sample as arguments.
#'                          If this function returns TRUE, the sample is regardes as good,
#'                          and the check prosses will run with the sample.
#'                          Otherwise, if this function returns FALSE,
#'                          the sample is regarded as not suitable (ex. too strange),
#'                          the check prosses will skipped.
#' @param   inf.to.x        When you check with intervals of from -Inf to a numeric, set TRUE.
#' @param   inf.to.inf      When you check with intervals of from -Inf to Inf, set TRUE.
#' @param   print.result    If you want to display the results of this function, set TRUE.
#' @return  A list of max.abs.error (value of max of abs.error),
#'          mean (mean of the sample which gives max.abs.error),
#'          sd (s.d. of the sample which gives max.abs.error) and
#'          x (interval of the sample which gives max.abs.error).
################################################################################################
check.max.absolute.error <- function( integ.func = function( mean, sd, x ) {},
                            target.cond = function( mean, sd, x ) { TRUE },
                            inf.to.x = FALSE, inf.to.inf = FALSE, print.result = TRUE )
{
    cat( "integ.func: " )
    print( integ.func )

    if ( inf.to.inf )
    {
        target.xs <- list( c( -Inf, Inf ) )
    }
    else if ( inf.to.x )
    {
        target.xs <- xs.inf
    }
    else
    {
        target.xs <- xs
    }

    max.abs.error <- abs.error <- 0
    max.mean <- max.sd <- numeric()
    max.x <- numeric( 2 )
    for ( mean in means )
    {
        for ( sd in sds )
        {
            for ( x in target.xs )
            {
                if ( target.cond( mean, sd, x ) )
                {
                    result <- integ.func( mean, sd, x )
                    if ( is.nan( result$value ) )
                    {
                        warning( "The result value of integ.func is NaN." )
                        max.abs.error <- Inf

                        max.mean <- mean
                        max.sd <- sd
                        max.x <- x

                        break
                    }
                    else if ( max.abs.error < result$abs.error )
                    {
                        max.abs.error <- result$abs.error

                        max.mean <- mean
                        max.sd <- sd
                        max.x <- x
                    }

                    cat.progress( "." )
                }
            }
        }
        cat.progress( "\n" )
    }

    cat( "done.\n" )
    if ( print.result )
    {
        print( paste( "max.abs.error:", max.abs.error ) )
        print( paste( "mean:", max.mean ) )
        print( paste( "sd:", max.sd ) )
        print( paste( "x:", max.x ) )
    }

    return ( list( "max.abs.error" = max.abs.error,
                    "mean" = max.mean,
                    "sd" = max.sd,
                    "x" = max.x ) )
}

################################################################################################
#' Get max of abs.error of integrate function.
#'
#' Get max of abs.error of integrate function of 2-mean, 2-s.d. and 1-interval arguments
#' and display the result. This function uses above means, sds, xs and xs.inf for samples.
#' @param   integ.func      The function using integrate.
#'                          This function should return a list like \link[stats]{integrate}.
#' @param   target.cond     This function is called before each of check processes and
#'                          gets mean, s.d., and x (interval) of a sample as arguments.
#'                          If this function returns TRUE, the sample is regardes as good,
#'                          and the check prosses will run with the sample.
#'                          Otherwise, if this function returns FALSE,
#'                          the sample is regarded as not suitable (ex. too strange),
#'                          the check prosses will skipped.
#' @param   inf.to.x        When you check with intervals of from -Inf to a numeric, set TRUE.
#' @param   inf.to.inf      When you check with intervals of from -Inf to Inf, set TRUE.
#' @return  A list of max.abs.error (value of max of abs.error),
#'          mean.1 (mean.1 of the sample which gives max.abs.error),
#'          sd.1 (sd.1 of the sample which gives max.abs.error),
#'          mean.2 (mean.2 of the sample which gives max.abs.error),
#'          sd.2 (sd.2 of the sample which gives max.abs.error) and
#'          x (interval of the sample which gives max.abs.error).
################################################################################################
check.max.absolute.error.2 <- function( integ.func = function( mean.1, sd.1, mean.2, sd.2, x ) { TRUE },
                                        target.cond = function( mean.1, sd.1, mean.2, sd.2, x ) { TRUE },
                                        inf.to.x = FALSE, inf.to.inf = FALSE )
{
    print( "[check.max.absolute.error.2] begin" )
    cat( "integ: " )
    print( integ.func )

    max.abs.error <- 0
    max.mean.1 <- max.sd.1 <- max.mean.2 <- max.sd.2 <- numeric()
    max.x <- numeric( 2 )

    print( "[pass 1]" )
    for ( mean.1 in means )
    {
        for ( sd.1 in sds )
        {
            print( paste( "mean.1:", mean.1, " sd.1:", sd.1 ) )
            result <- check.max.absolute.error(
                            function( mean, sd, x ) integ.func( mean.1, sd.1, mean, sd, x ),
                            function( mean, sd, x ) target.cond( mean.1, sd.1, mean, sd, x ), inf.to.x, inf.to.inf,
                            print.result = FALSE )
            if ( max.abs.error < result$max.abs.error )
            {
                max.abs.error <- result$max.abs.error

                max.mean.1 <- mean.1
                max.sd.1 <- sd.1
                max.mean.2 <- result$mean
                max.sd.2 <- result$sd
                max.x <- result$x
            }

            if ( is.infinite( max.abs.error ) )
            {
                break
            }
        }
    }

    print( "[pass 2]" )
    for ( mean.2 in means )
    {
        for ( sd.2 in sds )
        {
            print( paste( "mean.2:", mean.2, " sd.2:", sd.2 ) )
            result <- check.max.absolute.error(
                            function( mean, sd, x ) integ.func( mean, sd, mean.2, sd.2, x ),
                            function( mean, sd, x ) target.cond( mean, sd, mean.2, sd.2, x ), inf.to.x, inf.to.inf,
                            print.result = FALSE )
            if ( max.abs.error < result$max.abs.error )
            {
                max.abs.error <- result$max.abs.error

                max.mean.1 <- result$mean
                max.sd.1 <- result$sd
                max.mean.2 <- mean.2
                max.sd.2 <- sd.2
                max.x <- result$x
            }

            if ( is.infinite( max.abs.error ) )
            {
                break
            }
        }
    }

    print( "[check.max.absolute.error.2] done." )
    print( paste( "max.abs.error:", max.abs.error ) )
    print( paste( "mean.1:", max.mean.1 ) )
    print( paste( "sd.1:", max.sd.1 ) )
    print( paste( "mean.2:", max.mean.2 ) )
    print( paste( "sd.2:", max.sd.2 ) )
    print( paste( "x:", max.x ) )

    return ( list( "max.abs.error" = max.abs.error,
                    "mean.1" = max.mean.1,
                    "sd.1" = max.sd.1,
                    "mean.2" = max.mean.2,
                    "sd.2" = max.sd.2,
                    "x" = max.x ) )
}

################################################################################################
#' Check the values of 2 (semi-)integral-solved functions.
#'
#' Check the (semi-)integral-solved functions of 1-mean, 1-s.d. and 1-interval arguments.
#' This function uses above means, sds, xs and xs.inf for samples.
#' @param   f1              A (semi-)integral-solved function.
#' @param   f2              Another (semi-)integral-solved function.
#'                          Both f1 and f2 functions should return a numric.
#' @param   target.cond     This function is called before each of check processes and
#'                          gets mean, s.d., and x (interval) of a sample as arguments.
#'                          If this function returns TRUE, the sample is regardes as good,
#'                          and the check prosses will run with the sample.
#'                          Otherwise, if this function returns FALSE,
#'                          the sample is regarded as not suitable (ex. too strange),
#'                          the check prosses will skipped.
#' @param   d.range         The maximum abs-difference between integ and via.integrate values
#'                          to consider as check is succeded.
#' @param   inf.to.x        When you check with intervals of from -Inf to a numeric, set TRUE.
#' @param   inf.to.inf      When you check with intervals of from -Inf to Inf, set TRUE.
#' @return  TRUE if all checks are succeeded.
################################################################################################
check.funcs <- function( f1 = function( mean, sd, x ) {}, f2 = function( mean, sd, x ) {},
                            target.cond = function( mean, sd, x ) { TRUE },
                            d.range = 1e-8, inf.to.x = FALSE, inf.to.inf = FALSE )
{
    cat( "f1: " )
    print( f1 )

    cat( "f2: " )
    print( f2 )

    if ( inf.to.inf )
    {
        target.xs <- list( c( -Inf, Inf ) )
    }
    else if ( inf.to.x )
    {
        target.xs <- xs.inf
    }
    else
    {
        target.xs <- xs
    }

    max.d <- 0
    for ( mean in means )
    {
        for ( sd in sds )
        {
            for ( x in target.xs )
            {
                if ( target.cond( mean, sd, x ) )
                {
                    d <- f1( mean, sd, x ) - f2( mean, sd, x )
                    if ( abs( d ) > d.range )
                    {
                        cat( "Error!\n\n" )
                        print( paste( "mean:", mean ) )
                        print( paste( "sd:", sd ) )
                        print( paste( "x:", x[1], x[2] ) )
                        cat( "\n" )
                        print( paste( "f1 result:", f1( mean, sd, x ) ) )
                        print( paste( "f2 result:", f2( mean, sd, x ) ) )
                        print( paste( "d: ", d ) )
                        cat( "\n" )
                        stop( "The results are too different." )
                    }
                    else if ( abs( d ) > max.d )
                    {
                        max.d <- abs( d )
                    }

                    cat.progress( "." )
                }
            }
        }
        cat.progress( "\n" )
    }

    cat( "OK.\n" )
    print( paste( "max.d:", max.d ) )
    return ( TRUE )
}

################################################################################################
#' Check the values of 2 (semi-)integral-solved functions.
#'
#' Check the (semi-)integral-solved functions of 2-mean, 2-s.d. and 1-interval arguments.
#' This function uses above means, sds, xs and xs.inf for samples.
#' @param   f1              An (semi-)integral-solved function.
#' @param   f2              Another (semi-)integral-solved function.
#'                          Both f1 and f2 functions should return a numric.
#' @param   target.cond     This function is called before each of check processes and
#'                          gets mean, s.d., and x (interval) of a sample as arguments.
#'                          If this function returns TRUE, the sample is regardes as good,
#'                          and the check prosses will run with the sample.
#'                          Otherwise, if this function returns FALSE,
#'                          the sample is regarded as not suitable (ex. too strange),
#'                          the check prosses will skipped.
#' @param   d.range         The maximum abs-difference between integ and via.integrate values
#'                          to consider as check is succeded.
#' @param   inf.to.x        When you check with intervals of from -Inf to a numeric, set TRUE.
#' @param   inf.to.inf      When you check with intervals of from -Inf to Inf, set TRUE.
#' @return  TRUE if all checks are succeeded.
################################################################################################
check.funcs.2 <- function( f1 = function( mean.1, sd.1, mean.2, sd.2, x ) {},
                            f2 = function( mean.1, sd.1, mean.2, sd.2, x ) {},
                            target.cond = function( mean.1, sd.1, mean.2, sd.2, x ) { TRUE },
                            d.range = 1e-8, inf.to.x = FALSE, inf.to.inf = FALSE )
{
    print( "[check.funcs.2] begin" )
    cat( "f1: " )
    print( f1 )
    cat( "f2: " )
    print( f2 )

    print( "[pass 1]" )
    for ( mean.1 in means )
    {
        for ( sd.1 in sds )
        {
            print( paste( "mean.1:", mean.1, " sd.1:", sd.1 ) )
            check.funcs( function( mean, sd, x ) f1( mean.1, sd.1, mean, sd, x ),
                            function( mean, sd, x ) f2( mean.1, sd.1, mean, sd, x ),
                            function( mean, sd, x ) target.cond( mean.1, sd.1, mean, sd, x ), d.range, inf.to.x )
        }
    }

    print( "[pass 2]" )
    for ( mean.2 in means )
    {
        for ( sd.2 in sds )
        {
            print( paste( "mean.2:", mean.2, " sd.2:", sd.2 ) )
            check.funcs( function( mean, sd, x ) f1( mean, sd, mean.2, sd.2, x ),
                            function( mean, sd, x ) f2( mean, sd, mean.2, sd.2, x ),
                            function( mean, sd, x ) target.cond( mean, sd, mean.2, sd.2, x ), d.range, inf.to.x )
        }
    }

    print( "[check.funcs.2] OK" )
    return ( TRUE )
}

################################
# intermediate calculations
################################

################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} x f(x) dx} where \eqn{f} is the PDF of N(mean, sd).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.x2.d.inf <- function( mean, sd, x )
{
    ( mean^2 + sd^2 ) * pnorm( x, mean, sd ) - sd^2 * ( x + mean ) * dnorm( x, mean, sd )
}

integ.x2.d.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { x^2 * dnorm( x, mean, sd ) }, x[1], x[2] )
}

expect_equal( check.integ( function( mean, sd, x ) { integ.x2.d.inf( mean, sd, x[2] ) },
			  integ.x2.d.via.integrate, inf.to.x = TRUE ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} x^2 f^*(x) dx} where f^* is the PDF of N(mean, sd/sqrt(2)).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.x2.dstar <- function( mean, sd, x )
{
    ( mean^2 + sd^2 / 2 ) * ( pnorm( x[2], mean, sd * sqrt( 2 ) / 2 ) - pnorm( x[1], mean, sd * sqrt( 2 ) / 2 ) ) -
    sd^2 / 2 * ( ( x[2] + mean ) * dnorm( x[2], mean, sd * sqrt( 2 ) / 2 ) -
                 ( x[1] + mean ) * dnorm( x[1], mean, sd * sqrt( 2 ) / 2 ) )
}

integ.x2.dstar.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { x^2 * dnorm( x, mean, sd * sqrt( 2 ) / 2 ) }, x[1], x[2] )
}

expect_equal( check.integ( integ.x2.dstar, integ.x2.dstar.via.integrate ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} f(x)^2 dx} where \eqn{f} is the PDF of N(mean, sd).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.d2 <- function( mean, sd, x )
{
    ( pnorm( x[2], mean, sd * sqrt( 2 ) / 2 ) -
      pnorm( x[1], mean, sd * sqrt( 2 ) / 2 ) ) / sqrt( pi ) / sd / 2
}

integ.d2.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { dnorm( x, mean, sd )^2 }, x[1], x[2] )
}

expect_equal( check.integ( integ.d2, integ.d2.via.integrate ), TRUE )
expect_equal( check.integ( integ.d2, integ.d2.via.integrate, inf.to.x = TRUE ), TRUE )
expect_equal( check.integ( integ.d2, integ.d2.via.integrate, inf.to.inf = TRUE ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} x f(x)^2 dx} where \eqn{f} is the PDF of N(mean, sd).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.x.d2 <- function( mean, sd, x )
{
    ( mean *
        ( pnorm( x[2], mean, sd * sqrt( 2 ) / 2 ) -
          pnorm( x[1], mean, sd * sqrt( 2 ) / 2 ) ) / sqrt( pi ) / sd -
        sd^2 * ( dnorm( x[2], mean, sd )^2 - dnorm( x[1], mean, sd )^2 ) ) / 2
}

integ.x.d2.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { x * dnorm( x, mean, sd )^2 }, x[1], x[2] )
}

expect_equal( check.integ( integ.x.d2, integ.x.d2.via.integrate ), TRUE )
expect_equal( check.integ( integ.x.d2, integ.x.d2.via.integrate, inf.to.x = TRUE, d.range = 1e-6 ), TRUE )
expect_equal( check.integ( integ.x.d2, integ.x.d2.via.integrate, inf.to.inf = TRUE ), TRUE )



################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} x^n f(x) dx} where \eqn{f} is the PDF of N(mean, sd).
#' @param   n       The power number for x.
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @param   d       The value of the dnorm (give for speed-up).
#' @param   p       The value of the pnorm (give for speed-up).
#' @return  The result value of calculation.
################################################################################################
integ.xn.d <- function( n, mean, sd, x = numeric( 2 ),
                        d = dnorm( x, mean, sd ), p = pnorm( x, mean, sd ) )
{
    if ( n == 1 )
    {
        mean * ( p[2] - p[1] ) - sd^2 * ( d[2] - d[1] )
    }
    else if ( n == 2 )
    {
        if ( x[1] == -Inf )
        {
            ( mean^2 + sd^2 ) * p[2] -
            sd^2 * ( x[2] + mean ) * dnorm( x[2], mean, sd )
        }
        else
        {
            ( mean^2 + sd^2 ) * ( p[2] - p[1] ) -
            sd^2 * ( ( x[2] + mean ) * d[2] - ( x[1] + mean ) * d[1] )
        }
    }
}

integ.xn.d.via.integrate <- function( n, mean, sd, x )
{
    integrate( function( x ) { x^n * dnorm( x, mean, sd ) }, x[1], x[2] )
}

expect_equal( check.integ( function( mean, sd, x ) { integ.xn.d( 1, mean, sd, x ) },
                function( mean, sd, x ) { integ.xn.d.via.integrate( 1, mean, sd, x ) } ), TRUE )

expect_equal( check.integ( function( mean, sd, x ) integ.xn.d( 2, mean, sd, x ),
                function( mean, sd, x ) integ.xn.d.via.integrate( 2, mean, sd, x ) ), TRUE )

expect_equal( check.integ( function( mean, sd, x ) integ.xn.d( 2, mean, sd, x ),
                function( mean, sd, x ) integ.xn.d.via.integrate( 2, mean, sd, x ), inf.to.x = TRUE ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} x^n \Phi(x) dx} where \eqn{\Phi} is the CDF of N(mean, sd).
#' @param   n       The power number for x.
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.xn.p <- function( n, mean, sd, x )
{
    p <- pnorm( x, mean, sd )
    d <- dnorm( x, mean, sd )

    if ( x[1] == -Inf )
    {
        # ŒvŽZ‚Å”­ŽU‚µ‚È‚¢‚æ‚¤‚É -Inf ‚Ì‘ã‚í‚è‚É“K“–‚È”‚ð“ü‚ê‚Ä‚¨‚­
        #  (ŒvŽZ‚Å‚Í 0 ‚ðŠ|‚¯‚é‚Ì‚ÅA‚Ç‚ñ‚È—LŒÀ‚Ì’l‚Å‚à—Ç‚¢)
        x[1] <- 0
    }

    if ( n == 0 )
    {
        ( x[2] - mean ) * p[2] - ( x[1] - mean ) * p[1] +
        ( d[2] - d[1] ) * sd^2
    }
    else if ( n == 1 )
    {
        ( ( x[2]^2 - mean^2 - sd^2 ) * p[2] -
          ( x[1]^2 - mean^2 - sd^2 ) * p[1] +
          ( ( x[2] + mean ) * d[2] - ( x[1] + mean ) * d[1] ) * sd^2 ) / 2
    }
    else if ( n == 2 )
    {
        ( ( x[2]^3 - mean^3 - 3 * mean * sd^2 ) * p[2] -
          ( x[1]^3 - mean^3 - 3 * mean * sd^2 ) * p[1] +
          ( ( ( x[2] + mean )^2 - x[2] * mean + 2 * sd^2 ) * d[2] -
            ( ( x[1] + mean )^2 - x[1] * mean + 2 * sd^2 ) * d[1] ) * sd^2
        ) / 3
    }
}

integ.xn.p.via.integrate <- function( n, mean, sd, x )
{
    integrate( function( x ) { x^n * pnorm( x, mean, sd ) }, x[1], x[2] )
}

expect_equal( check.integ( function( mean, sd, x ) { integ.xn.p( 0, mean, sd, x ) },
                function( mean, sd, x ) { integ.xn.p.via.integrate( 0, mean, sd, x ) } ), TRUE )

expect_equal( check.integ( function( mean, sd, x ) { integ.xn.p( 1, mean, sd, x ) },
                function( mean, sd, x ) { integ.xn.p.via.integrate( 1, mean, sd, x ) } ), TRUE )

expect_equal( check.integ( function( mean, sd, x ) integ.xn.p( 2, mean, sd, x ),
                function( mean, sd, x ) integ.xn.p.via.integrate( 2, mean, sd, x ) ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} 2 x^n f(x) \Phi(x) dx}
#' where \eqn{f} is the PDF of N(mean, sd) and \eqn{\Phi} is the CDF of N(mean, sd).
#' @param   n       The power number for x.
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @param   d       The value of the dnorm (give for speed-up).
#' @param   p       The value of the pnorm (give for speed-up).
#' @return  The result value of calculation.
################################################################################################
integ.2xn.d.p <- function( n, mean, sd, x,
                        d = dnorm( x, mean, sd ), p = pnorm( x, mean, sd ) )
{
    pstar <- pnorm( x, mean, sd * sqrt( 2 ) / 2 )

    if ( n == 1 )
    {
        mean * ( p[2]^2 - p[1]^2 ) - 2 * sd^2 * ( d[2] * p[2] - d[1] * p[1] ) +
        sd * ( pstar[2] - pstar[1] ) / sqrt( pi )
    }
    else if ( n == 2 )
    {
        sd.2 <- sd^2

        ( mean^2 + sd.2 ) * ( p[2]^2 - p[1]^2 ) +
        2 * mean * sd * ( pstar[2] - pstar[1] ) / sqrt( pi ) -
        sd.2 * ( 2 * ( x[2] + mean ) * d[2] * p[2] -
                 2 * ( x[1] + mean ) * d[1] * p[1] +
                 sd.2 * ( d[2]^2 - d[1]^2 ) )
    }
}

integ.2xn.d.p.via.integrate <- function( n, mean, sd, x )
{
    integrate( function( x ) { 2 * x^n * dnorm( x, mean, sd ) * pnorm( x, mean, sd ) }, x[1], x[2] )
}

expect_equal( check.integ( function( mean, sd, x ) { integ.2xn.d.p( 1, mean, sd, x ) },
                function( mean, sd, x ) { integ.2xn.d.p.via.integrate( 1, mean, sd, x ) } ), TRUE )

expect_equal( check.integ( function( mean, sd, x ) integ.2xn.d.p( 2, mean, sd, x ),
                function( mean, sd, x ) integ.2xn.d.p.via.integrate( 2, mean, sd, x ) ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} \Phi^*(x) dx}
#' where \eqn{\Phi^*} is the CDF of N(mean, sd/sqrt(2)).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.pstar <- function( mean, sd, x )
{
    ( x[2] - mean ) * pnorm( x[2], mean, sd * sqrt( 2 ) / 2 ) -
    ( x[1] - mean ) * pnorm( x[1], mean, sd * sqrt( 2 ) / 2 ) +
    sd^2 * ( dnorm( x[2], mean, sd * sqrt( 2 ) / 2 ) - dnorm( x[1], mean, sd * sqrt( 2 ) / 2 ) ) / 2
}

integ.pstar.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { pnorm( x, mean, sd * sqrt( 2 ) / 2 ) }, x[1], x[2] )
}

expect_equal( check.integ( integ.pstar, integ.pstar.via.integrate ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} \Phi(x)^2 dx} where \eqn{\Phi} is the CDF of N(mean, sd).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.p2 <- function( mean, sd, x )
{
    ( x[2] - mean ) * pnorm( x[2], mean, sd )^2 - ( x[1] - mean ) * pnorm( x[1], mean, sd )^2 +
    2 * sd^2 * ( dnorm( x[2], mean, sd ) * pnorm( x[2], mean, sd ) -
                 dnorm( x[1], mean, sd ) * pnorm( x[1], mean, sd ) ) -
    sd * ( pnorm( x[2], mean, sd * sqrt( 2 ) / 2 ) - pnorm( x[1], mean, sd * sqrt( 2 ) / 2 ) ) / sqrt( pi )
}


integ.p2.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { pnorm( x, mean, sd )^2 }, x[1], x[2] )
}

expect_equal( check.integ( integ.p2, integ.p2.via.integrate ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} x^2 f(x)^2 dx} where \eqn{f} is the PDF of N(mean, sd).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.x2.d2 <- function( mean, sd, x )
{
    if ( x[1] == -Inf )
    {
        ( ( 2 * mean^2 + sd^2 ) * pnorm( x[2], mean, sd * sqrt( 2 ) / 2 ) / ( 2 * sqrt( pi )* sd ) -
            sd^2 * ( x[2] + mean ) * dnorm( x[2], mean, sd )^2 ) / 2
    }
    else
    {
        ( ( 2 * mean^2 + sd^2 ) * ( pnorm( x[2], mean, sd * sqrt( 2 ) / 2 ) -
                                    pnorm( x[1], mean, sd * sqrt( 2 ) / 2 ) ) / ( 2 * sqrt( pi )* sd ) -
            sd^2 * ( ( x[2] + mean ) * dnorm( x[2], mean, sd )^2 -
                     ( x[1] + mean ) * dnorm( x[1], mean, sd )^2 ) ) / 2
    }
}

integ.x2.d2.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { x^2 * dnorm( x, mean, sd )^2 }, x[1], x[2] )
}

expect_equal( check.integ( integ.x2.d2, integ.x2.d2.via.integrate ), TRUE )
expect_equal( check.integ( integ.x2.d2, integ.x2.d2.via.integrate, inf.to.x = TRUE, d.range = 1e-6 ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} f(x) (\int_{-\infty}^{x} t^2 f(t) dt) dx}
#' where \eqn{f} is the PDF of N(mean, sd).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.d.integ.x2.d <- function( mean, sd, x )
{
    ( mean^2 + sd^2 ) * ( pnorm( x[2], mean, sd )^2 -
                          pnorm( x[1], mean, sd )^2 ) / 2 +
    sd^4 * ( dnorm( x[2], mean, sd )^2 - dnorm( x[1], mean, sd )^2 ) / 2 -
    mean * sd * ( pnorm( x[2], mean, sd * sqrt( 2 ) / 2 ) -
                  pnorm( x[1], mean, sd * sqrt( 2 ) / 2 ) ) / sqrt( pi )
}

integ.d.integ.x2.d.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { ( ( mean^2 + sd^2 ) * pnorm( x, mean, sd ) -
                                sd^2 * ( x + mean ) * dnorm( x, mean, sd ) ) * dnorm( x, mean, sd ) },
                x[1], x[2] )
}

expect_equal( check.integ( integ.d.integ.x2.d, integ.d.integ.x2.d.via.integrate ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} 2 x^2 f(x) \Phi(x) dx}.
#' where \eqn{f} is the PDF of N(mean, sd) and \eqn{\Phi} is the CDF of N(mean, sd).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.2x2.d.p <- function( mean, sd, x )
{
    ( mean^2 + sd^2 ) * ( pnorm( x[2], mean, sd )^2 - pnorm( x[1], mean, sd )^2 ) +
    2 * mean * sd * ( pnorm( x[2], mean, sd * sqrt( 2 ) / 2 ) -
                      pnorm( x[1], mean, sd * sqrt( 2 ) / 2 ) ) / sqrt( pi ) -
    sd^2 * ( sd^2 * dnorm( x[2], mean, sd )^2 +
                2 * ( x[2] + mean ) * dnorm( x[2], mean, sd ) * pnorm( x[2], mean, sd ) -
                2 * ( x[1] + mean ) * dnorm( x[1], mean, sd ) * pnorm( x[1], mean, sd ) -
                sd^2 * dnorm( x[1], mean, sd )^2 )
}

integ.2x2.d.p.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { 2 * x^2 * dnorm( x, mean, sd ) * pnorm( x, mean, sd ) }, x[1], x[2] )
}

expect_equal( check.integ( function( mean, sd, x ) integ.2x2.d.p( mean, sd, x ),
                function( mean, sd, x ) integ.2x2.d.p.via.integrate( mean, sd, x ) ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{-\infty}^{x_2} f(x)^2 \Phi(x) dx}.
#' where \eqn{f} is the PDF of N(mean, sd) and \eqn{\Phi} is the CDF of N(mean, sd).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[-Inf, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.d2.p.inf.to.x <- function( mean, sd, x )
{
    d <- dnorm( x, mean, sd )
    p <- pnorm( x, mean, sd )
    p.star <- pnorm( x, mean, sd * sqrt( 2 ) / 2 )

    ( d[2] * p[2]^2 - d[1] * p[1]^2 +
    integrate( function( x ) { ( x - mean ) * dnorm( x, mean, sd ) * pnorm( x, mean, sd )^2 },
                x[1], x[2] )$value / sd^2 ) / 2
#   d[2] * p[2]^2 - d[1] * p[1]^2 -
#   integrate( function( x ) { pnorm( x, mean, sd ) * ( dnorm( x, mean, sd )^2 -
#                                                       pnorm( x, mean, sd ) * ( x - mean ) / sd^2 * dnorm( x, mean, sd ) ) },
#               x[1], x[2] )$value
}

integ.d2.p.inf.to.x.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )
}

expect_equal( check.integ( function( mean, sd, x ) integ.d2.p.inf.to.x( mean, sd, x ),
                function( mean, sd, x ) integ.d2.p.inf.to.x.via.integrate( mean, sd, x ),
                inf.to.x = TRUE, d.range = 6e-4 ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} f(x)^2 \Phi(x) dx}.
#' where \eqn{f} is the PDF of N(mean, sd) and \eqn{\Phi} is the CDF of N(mean, sd).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.d2.p <- function( mean, sd, x )
{
    ( dnorm( x[2], mean, sd ) * pnorm( x[2], mean, sd )^2 -
      dnorm( x[1], mean, sd ) * pnorm( x[1], mean, sd )^2 ) / 2 -
    mean * ( pnorm( x[2], mean, sd )^3 - pnorm( x[1], mean, sd )^3 ) / ( 6 * sd^2 ) +
    integrate( function( x ) { x * dnorm( x, mean, sd ) * pnorm( x, mean, sd )^2 }, x[1], x[2] )$value / ( 2 * sd^2 )
#   ( x[2] * dnorm( x[2], mean, sd )^2 * pnorm( x[2], mean, sd ) -
#     x[1] * dnorm( x[1], mean, sd )^2 * pnorm( x[1], mean, sd ) ) +
#   2 * integrate( function( x ) { x^2 * dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value / sd^2 -
#   2 * mean * integrate( function( x ) { x * dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value / sd^2 -
#   integrate( function( x ) { x * dnorm( x, mean, sd )^3 }, x[1], x[2] )$value
}

integ.d2.p.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )
}

expect_equal( check.integ( integ.d2.p, integ.d2.p.via.integrate ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} f(x) (\int_{-\infty}^{x} t f(t) \Phi(t) dt) dx}
#' where \eqn{f} is the PDF of N(mean, sd) and \eqn{\Phi} is the CDF of N(mean, sd).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.d.integ.x.d.p <- function( mean, sd, x )
{
    mean * ( pnorm( x[2], mean, sd )^3 - pnorm( x[1], mean, sd )^3 ) / 6 +
    sd * ( pnorm( x[2], mean, sd ) * pnorm( x[2], mean, sd * sqrt( 2 ) / 2 ) -
            pnorm( x[1], mean, sd ) * pnorm( x[1], mean, sd * sqrt( 2 ) / 2 ) ) / ( 2 * sqrt( pi ) ) -
    2 * sd^2 * integrate( function( x ) { dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value
}

integ.d.integ.x.d.p.via.integrate <- function( mean, sd, x )
{
    integrate( function( x )
                {
                    vapply( x, function( x )
                                {
                                    dnorm( x, mean, sd ) *
                                    integrate( function( t ) { t * dnorm( t, mean, sd ) * pnorm( t, mean, sd ) },
                                                -Inf, x )$value
                                }, 0 )
                }, x[1], x[2] )
}

expect_equal( check.integ( integ.d.integ.x.d.p, integ.d.integ.x.d.p.via.integrate ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} x f(x)^2 \Phi(x) dx}
#' where \eqn{f} is the PDF of N(mean, sd) and \eqn{\Phi} is the CDF of N(mean, sd).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.x.d2.p <- function( mean, sd, x )
{
    d <- dnorm( x, mean, sd )
    p <- pnorm( x, mean, sd )
    p3star <- pnorm( x, mean, sd * sqrt( 3 ) / 3 )

    -sd^2 * ( d[2]^2 * p[2] - d[1]^2 * p[1] ) / 2 +
    ( p3star[2] - p3star[1] ) / ( 4 * sqrt( 3 ) * pi ) +
    mean * integrate( function( x ) { dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value
}

integ.x.d2.p.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { x * dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )
}

expect_equal( check.integ( integ.x.d2.p, integ.x.d2.p.via.integrate ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} x f(x) \Phi(x)^2 dx}
#' where \eqn{f} is the PDF of N(mean, sd) and \eqn{\Phi} is the CDF of N(mean, sd).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.x.d.p2 <- function( mean, sd, x )
{
    d <- dnorm( x, mean, sd )
    p <- pnorm( x, mean, sd )

    mean * ( p[2]^3 - p[1]^3 ) / 3 -
    sd^2 * ( d[2] * p[2]^2 - d[1] * p[1]^2 ) +
    sd * ( p[2] * pnorm( x[2], mean, sd * sqrt( 2 ) / 2 ) -
           p[1] * pnorm( x[1], mean, sd * sqrt( 2 ) / 2 ) ) / sqrt( pi ) / 2 +
    sd^2 * integrate( function( x ) { dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value -
    sd * integrate( function( x ) { dnorm( x, mean, sd ) * pnorm( x, mean, sd  * sqrt( 2 ) / 2 ) }, x[1], x[2] )$value /
    sqrt( pi ) / 2
}

integ.x.d.p2.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { x * dnorm( x, mean, sd ) * pnorm( x, mean, sd )^2 }, x[1], x[2] )
}

expect_equal( check.integ( integ.x.d.p2, integ.x.d.p2.via.integrate ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} f(x)^2 \Phi^*(x) dx}
#' where \eqn{f} is the PDF of N(mean, sd) and \eqn{\Phi^*} is the CDF of N(mean, sd/sqrt(2)).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.d.pstar <- function( mean, sd, x )
{
    ( pnorm( x[2], mean, sd ) * pnorm( x[2], mean, sd * sqrt( 2 ) / 2 ) -
      pnorm( x[1], mean, sd ) * pnorm( x[1], mean, sd * sqrt( 2 ) / 2 ) ) -
    2 * sqrt( pi ) * sd * integrate( function( x ) { dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value
#   ( pnorm( x[2], mean, sd ) * pnorm( x[2], mean, sd * sqrt( 2 ) / 2 ) -
#     pnorm( x[1], mean, sd ) * pnorm( x[1], mean, sd * sqrt( 2 ) / 2 ) ) -
#   2 * sqrt( pi ) * sd * integrate( function( x ) { dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value
}

integ.d.pstar.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { dnorm( x, mean, sd ) * pnorm( x, mean, sd * sqrt( 2 ) / 2 ) }, x[1], x[2] )
}

expect_equal( check.integ( integ.d.pstar, integ.d.pstar.via.integrate ), TRUE )


################################################################################################
#' Calculates \eqn{-\dfrac{\mu}{2 \sqrt{\pi \sigma}} \int_{x_1}^{x_2} x f(x) \Phi^*(x) dx}
#' where \eqn{f} is the PDF of \eqn{N(\mu, \sigma)} and
#' \eqn{\Phi^*} is the CDF of \eqn{N(\mu, \sigma / \sqrt{2})}.
#' @param   mean    The mean of \eqn{N(\mu, \sigma)}.
#' @param   sd      The s.d. of \eqn{N(\mu, \sigma)}.
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.mmper2sqrtpisd.d.pstar <- function( mean, sd, x )
{
    - mean * ( pnorm( x[2], mean, sd ) * pnorm( x[2], mean, sd * sqrt( 2 ) / 2 ) -
               pnorm( x[1], mean, sd ) * pnorm( x[1], mean, sd * sqrt( 2 ) / 2 ) ) / ( 2 * sqrt( pi ) * sd ) +
    mean * integrate( function( x ) { dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value
}

integ.mmper2sqrtpisd.d.pstar.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) {  -mean / ( 2 * sqrt( pi ) * sd ) *
                                dnorm( x, mean, sd ) * pnorm( x, mean, sd * sqrt( 2 ) / 2 ) }, x[1], x[2] )
}

expect_equal( check.integ( integ.mmper2sqrtpisd.d.pstar, integ.mmper2sqrtpisd.d.pstar.via.integrate ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} x f(x) \Phi^*(x) dx}
#' where \eqn{f} is the PDF of N(mean, sd) and \eqn{\Phi^*} is the CDF of N(mean, sd/sqrt(2)).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.x.d.pstar <- function( mean, sd, x )
{
    d <- dnorm( x, mean, sd )
    p <- pnorm( x, mean, sd )
    pstar <- pnorm( x, mean, sd * sqrt( 2 ) / 2 )
    p3star <- pnorm( x, mean, sd * sqrt( 3 ) / 3 )

    ( mean * p[2] - sd^2 * d[2] ) * pstar[2] -
    ( mean * p[1] - sd^2 * d[1] ) * pstar[1] + sd * ( p3star[2] - p3star[1] ) / sqrt( 3 * pi ) -
    2 * sqrt( pi ) * sd * mean *
    integrate( function( x ) { dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value
}

integ.x.d.pstar.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { x * dnorm( x, mean, sd ) * pnorm( x, mean, sd * sqrt( 2 ) / 2 ) }, x[1], x[2] )
}

expect_equal( check.integ( integ.x.d.pstar, integ.x.d.pstar.via.integrate ), TRUE )


################################################################################################
#' Test function for mean.conn.bound function (type1.type = 2)
#'
#' Calculates the mean on a specified closed interval
#' in a continuous distribution of type1.type = 2.
#' Different values for mean.1 and mean.2 are allowed.
#' @param   mean.1      The mean value of the normal distribution #1.
#' @param   sd.1        The standard deviation of the normal distribution #1.
#' @param   mean.2      The mean value of the normal distribution #2.
#' @param   sd.2        The standard deviation of the normal distribution #2.
#' @param   x           The range of the closed interval to calculate the mean value.
#'                      A vector of 2 numerics.
#' @return  The mean value on the closed interval
#'          of the continuous distribution model of type1.type = 2.
################################################################################################
mean.conn.bound.type1.type.2 <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    d.1 <- dnorm( x, mean.1, sd.1 )
    d.2 <- dnorm( x, mean.2, sd.2 )
    p.1 <- pnorm( x, mean.1, sd.1 )
    p.2 <- pnorm( x, mean.2, sd.2 )
    p.sum <- p.1 + p.2
    pstar.1 <- pnorm( x, mean.1, sd.1 * sqrt( 2 ) / 2 )
    pstar.2 <- pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 )

    mean <- ( ( p.2[2] - p.1[1] ) * ( mean.1 * ( p.1[2] - p.1[1] ) +
                                      mean.2 * ( p.2[2] - p.2[1] ) ) +
              sd.1^2 * ( d.1[2] * ( p.1[2] - p.2[2] ) +
                         d.1[1] * ( p.1[2] + p.2[2] - 2 * p.1[1] ) ) +
              sd.2^2 * ( d.2[2] * ( p.1[1] + p.2[1] - 2 * p.2[2] ) +
                         d.2[1] * ( p.2[1] - p.1[1] ) ) -
              ( sd.1 * ( pstar.1[2] - pstar.1[1] ) -
                sd.2 * ( pstar.2[2] - pstar.2[1] ) ) / sqrt( pi )
            ) / ( p.sum[2] - p.sum[1] )

    return ( mean )
}

# Semi-integral-solved expression of mean.conn.bound where type1.type = 2
mean.conn.bound.type1.type.2.a <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    p.1 <- pnorm( x, mean.1, sd.1 )
    p.2 <- pnorm( x, mean.2, sd.2 )
    d.1 <- dnorm( x, mean.1, sd.1 )
    d.2 <- dnorm( x, mean.2, sd.2 )

    p.sum <- p.1 + p.2

    mean <- ( p.sum[2] * integ.xn.d( 1, mean.1, sd.1, x, d.1, p.1 ) -
                integ.2xn.d.p( 1, mean.1, sd.1, x, d.1, p.1 ) +
                integ.2xn.d.p( 1, mean.2, sd.2, x, d.2, p.2 ) -
                p.sum[1] * integ.xn.d( 1, mean.2, sd.2, x, d.2, p.2 ) ) /
            ( p.sum[2] - p.sum[1] )

    return ( mean )
}

check.funcs.2(  mean.conn.bound.type1.type.2,
                mean.conn.bound.type1.type.2.a,
                target.cond = function( mean.1, sd.1, mean.2, sd.2, x )
                {
                    pnorm( x[2], mean.1, sd.1 ) < 1 && pnorm( x[2], mean.2, sd.2 ) < 1 &&
                    pnorm( x[2], mean.1, sd.1 / sqrt( 2 ) ) < 1 && pnorm( x[2], mean.2, sd.2 / sqrt( 2 ) ) < 1
                } )

# Integral-solved-but-not-rearranged expression of mean.conn.bound where type1.type = 2
mean.conn.bound.type1.type.2.b <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    p.1 <- pnorm( x, mean.1, sd.1 )
    p.2 <- pnorm( x, mean.2, sd.2 )
    d.1 <- dnorm( x, mean.1, sd.1 )
    d.2 <- dnorm( x, mean.2, sd.2 )

    p.sum <- p.1 + p.2

    mean <- (   mean.1 * p.sum[2] * ( p.1[2] - p.1[1] ) -
                mean.2 * p.sum[1] * ( p.2[2] - p.2[1] ) -
                sd.1^2 * p.sum[2] * ( d.1[2] - d.1[1] ) +
                sd.2^2 * p.sum[1] * ( d.2[2] - d.2[1] ) -
                mean.1 * ( p.1[2]^2 - p.1[1]^2 ) +
                mean.2 * ( p.2[2]^2 - p.2[1]^2 ) +
                2 * sd.1^2 * ( d.1[2] * p.1[2] - d.1[1] * p.1[1] ) -
                2 * sd.2^2 * ( d.2[2] * p.2[2] - d.2[1] * p.2[1] ) -
                ( sd.1 *
                    ( pnorm( x[2], mean.1, sd.1 * sqrt( 2 ) / 2 ) -
                      pnorm( x[1], mean.1, sd.1 * sqrt( 2 ) / 2 ) ) -
                sd.2 *
                    ( pnorm( x[2], mean.2, sd.2 * sqrt( 2 ) / 2 ) -
                      pnorm( x[1], mean.2, sd.2 * sqrt( 2 ) / 2 ) )
                ) / sqrt( pi ) ) / ( p.sum[2] - p.sum[1] )

    return ( mean )
}

check.funcs.2(  mean.conn.bound.type1.type.2,
                mean.conn.bound.type1.type.2.b,
                target.cond = function( mean.1, sd.1, mean.2, sd.2, x )
                {
                    pnorm( x[2], mean.1, sd.1 ) < 1 && pnorm( x[2], mean.2, sd.2 ) < 1 &&
                    pnorm( x[2], mean.1, sd.1 / sqrt( 2 ) ) < 1 && pnorm( x[2], mean.2, sd.2 / sqrt( 2 ) ) < 1
                } )

################################################################################################
#' Test function for mean.cont function (type1.type = 3)
#'
#' Calculates \eqn{\int_{-\infty}^{\infty} x g_k(x) dx}, \eqn{(k = 1, 2, 3)}
#' where \eqn{g_k} is the PDF of intervals[[k]] of a CGD model of type1.type = 3.
#' @param   k           The number of the normal distribution member of a CGD model.
#' @param   mean.k      The mean value of intervals[[k]].
#' @param   sd.k        The standard deviation of intervals[[k]].
#' @return  The mean value of \eqn{\int_{-\infty}^{\infty} x g_k(x) dx}.
################################################################################################
mean.cont.t3.sub <- function( k, mean.k, sd.k )
{
    if ( k == 1 )
    {
        mean <- mean.k * ( 2 - sqrt( 2 ) ) / 4 - sd.k / sqrt( 2 * pi ) / 2
    }
    else if ( k == 2 )
    {
        mean <- mean.k * sqrt( 2 ) / 2
    }
    else if ( k == 3 )
    {
        mean <- mean.k * ( 2 - sqrt( 2 ) ) / 4 + sd.k / sqrt( 2 * pi ) / 2
    }

    return ( mean )
}

mean.cont.t3.sub.via.integrate <- function( k, mean.k, sd.k )
{
    if ( k == 1 )
    {
        integrate( function( x )
                    {
                        x * ( 1 - dnorm( x, mean.k, sd.k ) / dnorm( mean.k, mean.k, sd.k ) ) *
                            dnorm( x, mean.k, sd.k )
                    }, -Inf, mean.k )
    }
    else if ( k == 2 )
    {
        integrate( function( x )
                    {
                        x * dnorm( x, mean.k, sd.k )^2 / dnorm( mean.k, mean.k, sd.k )
                    }, -Inf, Inf )
    }
    else if ( k == 3 )
    {
        integrate( function( x )
                    {
                        x * ( 1 - dnorm( x, mean.k, sd.k ) / dnorm( mean.k, mean.k, sd.k ) ) *
                            dnorm( x, mean.k, sd.k )
                    }, mean.k, Inf )
    }
}

expect_equal( all( vapply( 1:3,
            function( k )
            check.integ( function( mean, sd, x )
                         { mean.cont.t3.sub( k, mean, sd ) },
                         function( mean, sd, x )
                         { mean.cont.t3.sub.via.integrate( k, mean, sd ) }, d.range = 1e-6 ),
            TRUE ) ), TRUE )


################################################################################################
#' Test function for v.cont function (type1.type = 3)
#'
#' Calculates \eqn{\int_{-\infty}^{\infty} (x - \mu)^2 g_k(x) dx}, \eqn{(k = 1, 2, 3)}
#' where \mu is the mean value of a CGD model of type1.type = 3 and
#' \eqn{g_k} is the PDF of intervals[[k]] of the model.
#' @param   k           The number of the normal distribution member of a CGD model.
#' @param   mean        The mean value of the CGD model.
#' @param   mean.k      The mean value of intervals[[k]].
#' @param   sd.k        The standard deviation of intervals[[k]].
#' @return  The mean value of \eqn{\int_{-\infty}^{\infty} x g_k(x) dx}.
################################################################################################
v.cont.t3.sub <- function( k, mean, mean.k, sd.k )
{
    if ( k == 1 )
    {
        ( mean.k - mean )^2 * ( 2 - sqrt( 2 ) ) / 4 -
        ( mean.k - mean ) * sd.k * sqrt( 2 ) / sqrt( pi ) / 2 +
        sd.k^2 * ( 4 - sqrt( 2 ) ) / 8
    }
    else if ( k == 2 )
    {
        ( ( mean.k - mean )^2 * 2 + sd.k^2 ) * sqrt( 2 ) / 4
    }
    else if ( k == 3 )
    {
        ( mean.k - mean )^2 * ( 2 - sqrt( 2 ) ) / 4 +
        ( mean.k - mean ) * sd.k * sqrt( 2 ) / sqrt( pi ) / 2 +
        sd.k^2 * ( 4 - sqrt( 2 ) ) / 8
    }
}

v.cont.t3.sub.via.integrate <- function( k, mean, mean.k, sd.k, x )
{
    if ( k == 1 )
    {
        if ( x[1] < mean.k )
        {
            integrate( function( x )
                        {
                            ( x - mean )^2 *
                            ( 1 - dnorm( x, mean.k, sd.k ) / dnorm( mean.k, mean.k, sd.k ) ) *
                            dnorm( x, mean.k, sd.k )
                        }, max( -Inf, x[1] ), min( mean.k, x[2] ) )
        }
        else
        {
            list( value = 0, abs.error = 0 )
        }
    }
    else if ( k == 2 )
    {
        integrate( function( x )
                    {
                        ( x - mean )^2 *
                        dnorm( x, mean.k, sd.k )^2 / dnorm( mean.k, mean.k, sd.k )
                    }, max( -Inf, x[1] ), min( Inf, x[2] ) )
    }
    else if ( k == 3 )
    {
        if ( mean.k < x[2] )
        {
            integrate( function( x )
                        {
                            ( x - mean )^2 *
                            ( 1 - dnorm( x, mean.k, sd.k ) / dnorm( mean.k, mean.k, sd.k ) ) *
                            dnorm( x, mean.k, sd.k )
                        }, max( mean.k, x[1] ), min( Inf, x[2] ) )
        }
        else
        {
            list( value = 0, abs.error = 0 )
        }
    }
}

expect_equal( all( vapply( 1:3,
        # Here using x[1] instead of mean for efficiency.
        function( k )
        {
            print( paste( "k:", k ) )
            check.integ( function( mean, sd, x )
                         { v.cont.t3.sub( k, x[1], mean, sd ) },
                         function( mean, sd, x )
                         { v.cont.t3.sub.via.integrate( k, x[1], mean, sd, c( -Inf, Inf ) ) },
                         d.range = 1e-6 )
        }, TRUE ) ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{-x_1}^{x_2} (x - \mu)^2 f(x) dx}
#' where \mu is the mean value of a CGD model of type1.type = 3 and
#' \eqn{f} is the PDF of N(mean.k, sd.k).
#' @param   mean        The mean value of a CGD model.
#' @param   mean.k      The mean value of intervals[[k]].
#' @param   sd.k        The standard deviation of intervals[[k]].
#' @param   x           The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
v.bound.t3.sub.d <- function( mean, mean.k, sd.k, x )
{
    d <- dnorm( x, mean.k, sd.k )
    p <- pnorm( x, mean.k, sd.k )

    ( mean.k^2 + sd.k^2 ) * ( p[2] - p[1] ) -
    sd.k^2 * ( ( x[2] + mean.k ) * d[2] - ( x[1] + mean.k ) * d[1] ) -
    2 * mean * ( mean.k * ( p[2]- p[1] ) - sd.k^2 * ( d[2] - d[1] ) ) +
    mean^2 * ( p[2] - p[1] )
}

v.bound.t3.sub.d.via.integrate <- function( mean, mean.k, sd.k, x )
{
    integrate( function( x ) { ( x - mean )^2 * dnorm( x, mean.k, sd.k ) }, x[1], x[2] )
}

expect_equal( all( vapply( means, function( mean )
                    check.integ( function( mean.k, sd.k, x ) { v.bound.t3.sub.d( mean, mean.k, sd.k, x ) },
                                 function( mean.k, sd.k, x ) { v.bound.t3.sub.d.via.integrate( mean, mean.k, sd.k, x ) } ),
                    TRUE ) ), TRUE )


################################################################################################
#' Calculates \eqn{\dfrac{1}{\sqrt{2}} \int_{-x_1}^{x_2} (x - \mu)^2 f^*(x) dx}
#' where \mu is the mean value of a CGD model of type1.type = 3 and
#' \eqn{f^*} is the PDF of N(mean.k, sd.k/sqrt(2)).
#' @param   mean        The mean value of a CGD model.
#' @param   mean.k      The mean value of intervals[[k]].
#' @param   sd.k        The standard deviation of intervals[[k]].
#' @param   x           The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
v.bound.t3.sub.dstar.psqrt2 <- function( mean, mean.k, sd.k, x )
{
    dstar <- dnorm( x, mean.k, sd.k * sqrt( 2 ) / 2 )
    pstar <- pnorm( x, mean.k, sd.k * sqrt( 2 ) / 2 )

    ( ( mean.k^2 + sd.k^2 / 2 ) * ( pstar[2] - pstar[1] ) -
      sd.k^2 * ( ( x[2] + mean.k ) * dstar[2] - ( x[1] + mean.k ) * dstar[1] ) / 2 -
      mean * ( 2 * mean.k * ( pstar[2]- pstar[1] ) - sd.k^2 * ( dstar[2] - dstar[1] ) ) +
      mean^2 * ( pstar[2] - pstar[1] ) ) * sqrt( 2 ) / 2
}

v.bound.t3.sub.dstar.psqrt2.via.integrate <- function( mean, mean.k, sd.k, x )
{
    integrate( function( x ) { ( x - mean )^2 * dnorm( x, mean.k, sd.k * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2 }, x[1], x[2] )
}

expect_equal( all( vapply( means,
            function( mean )
            check.integ( function( mean.k, sd.k, x )
                         { v.bound.t3.sub.dstar.psqrt2( mean, mean.k, sd.k, x ) },
                         function( mean.k, sd.k, x )
                         { v.bound.t3.sub.dstar.psqrt2.via.integrate( mean, mean.k, sd.k, x ) } ),
            TRUE ) ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{-\infty}^{\mu} (x - \mu)^2 g_k(x) dx}
#' where \mu is the mean value of a CGD model of type1.type = 3 and
#' \eqn{g_k} is the PDF of intervals[[k]] of the model.
#' @param   k           The number of the normal distribution member of a CGD model.
#' @param   mean        The mean value of the CGD model.
#' @param   mean.k      The mean value of intervals[[k]].
#' @param   sd.k        The standard deviation of intervals[[k]].
#' @return  The result value of calculation.
################################################################################################
lv.t3.sub <- function( k, mean, mean.k, sd.k )
{
    if ( k == 1 )
    {
        cgd:::v.sub( 3, mean, mean.k, sd.k, min( mean, mean.k ), k = k )
    }
    else if ( k == 2 )
    {
        cgd:::v.sub( 3, mean, mean.k, sd.k, mean, k = k )
    }
    else if ( k == 3 )
    {
        if ( mean.k < mean )
        {
            cgd:::v.sub( 3, mean, mean.k, sd.k, mean, k = k ) -
            ( mean.k - mean )^2 * ( 2 - sqrt( 2 ) ) / 4 +
            ( mean.k - mean ) * sd.k * sqrt( 2 ) / sqrt( pi ) / 2 -
            sd.k^2 * ( 4 - sqrt( 2 ) ) / 8
        }
        else
        {
            0
        }
    }
}

lv.t3.sub.via.integrate <- function( k, mean, mean.k, sd.k )
{
    if ( k == 3 && mean.k > mean )
    {
        cat.progress( "0" )
        list( value = 0, abs.error = 0 )
    }
    else
    {
        v.cont.t3.sub.via.integrate( k, mean, mean.k, sd.k, c( -Inf, mean ) )
    }
}

expect_equal( all( vapply( 1:3,
        # Here using x[1] instead of mean for efficiency.
        function( k )
        {
            print( paste( "k:", k ) )
            check.integ( function( mean, sd, x )
                         { lv.t3.sub( k, x[1], mean, sd ) },
                         function( mean, sd, x )
                         { lv.t3.sub.via.integrate( k, x[1], mean, sd ) },
                         d.range = 1e-6,
                         permit.d = function( d, abs.error, info, parent.info )
                                    { abs( d ) < 1 && abs( d ) < abs.error * 4 } )
        }, TRUE ) ), TRUE )

expect_equal( check.integ( function( mean, sd, x )
             { lv.t3.sub( 1, x[1], mean, sd ) + lv.t3.sub( 2, x[1], mean + x[2], sd ) },
             function( mean, sd, x )
             {
                lv.1 <- lv.t3.sub.via.integrate( 1, x[1], mean, sd )
                lv.2 <- lv.t3.sub.via.integrate( 2, x[1], mean + x[2], sd )

                list( value = lv.1$value + lv.2$value, abs.error = lv.1$abs.error + lv.2$abs.error )
             },
             d.range = 1e-6,
             permit.d = function( d, abs.error, info, parent.info )
                        { abs( d ) < 1 && abs( d ) < abs.error * 4 } ), TRUE )

expect_equal( check.integ( function( mean, sd, x )
             {
                lv.t3.sub( 1, x[1], mean, sd ) +
                lv.t3.sub( 2, x[1], mean + x[2] / 4, sd + 0.3 ) +
                lv.t3.sub( 3, x[1], mean + x[2], sd + 0.1 )
             },
             function( mean, sd, x )
             {
                lv.1 <- lv.t3.sub.via.integrate( 1, x[1], mean, sd )
                lv.2 <- lv.t3.sub.via.integrate( 2, x[1], mean + x[2] / 4, sd + 0.3 )
                lv.3 <- lv.t3.sub.via.integrate( 3, x[1], mean + x[2], sd + 0.1 )

                list( value = lv.1$value + lv.2$value + lv.3$value,
                      abs.error = lv.1$abs.error + lv.2$abs.error + lv.3$abs.error )
             },
             d.range = 1e-6,
             permit.d = function( d, abs.error, info, parent.info )
                        { abs( d ) < 1 && abs( d ) < abs.error * 4 } ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{\mu}^{\infty} (x - \mu)^2 g_k(x) dx}
#' where \mu is the mean value of a CGD model of type1.type = 3 and
#' \eqn{g_k} is the PDF of intervals[[k]] of the model.
#' @param   k           The number of the normal distribution member of a CGD model.
#' @param   mean        The mean value of the CGD model.
#' @param   mean.k      The mean value of intervals[[k]].
#' @param   sd.k        The standard deviation of intervals[[k]].
#' @return  The result value of calculation.
################################################################################################
uv.t3.sub <- function( k, mean, mean.k, sd.k )
{
    if ( k == 1 )
    {
        if ( mean < mean.k )
        {
            ( mean.k - mean )^2 * ( 2 - sqrt( 2 ) ) / 4 -
            ( mean.k - mean ) * sd.k * sqrt( 2 ) / sqrt( pi ) / 2 +
            sd.k^2 * ( 4 - sqrt( 2 ) ) / 8 -
            cgd:::v.sub( 3, mean, mean.k, sd.k, mean, k = k )
        }
        else
        {
            0
        }
    }
    else if ( k == 2 )
    {
        ( mean.k - mean )^2 * sqrt( 2 ) / 2 +
        sd.k^2 * sqrt( 2 ) / 4 -
        cgd:::v.sub( 3, mean, mean.k, sd.k, mean, k = k )
    }
    else if ( k == 3 )
    {
        ( mean.k - mean )^2 * ( 2 - sqrt( 2 ) ) / 2 +
        sd.k^2 * ( 4 - sqrt( 2 ) ) / 4 -
        cgd:::v.sub( 3, mean, mean.k, sd.k, max( mean, mean.k ), k = k )
    }
}

uv.t3.sub.via.integrate <- function( k, mean, mean.k, sd.k )
{
    if ( k == 1 && mean > mean.k )
    {
        cat.progress( "0" )
        list( value = 0, abs.error = 0 )
    }
    else
    {
        v.cont.t3.sub.via.integrate( k, mean, mean.k, sd.k, c( mean, Inf ) )
    }
}

expect_equal( all( vapply( 1:3,
        # Here using x[1] instead of mean for efficiency.
        function( k )
        {
            print( paste( "k:", k ) )
            check.integ( function( mean, sd, x )
                         { uv.t3.sub( k, x[1], mean, sd ) },
                         function( mean, sd, x )
                         { uv.t3.sub.via.integrate( k, x[1], mean, sd ) },
                         d.range = 1e-6 )
        }, TRUE ) ), TRUE )

expect_equal( check.integ( function( mean, sd, x )
             { uv.t3.sub( 2, x[1], mean, sd ) + uv.t3.sub( 3, x[1], mean + x[2], sd + 0.3 ) },
             function( mean, sd, x )
             {
                uv.2 <- uv.t3.sub.via.integrate( 2, x[1], mean, sd )
                uv.3 <- uv.t3.sub.via.integrate( 3, x[1], mean + x[2], sd + 0.3 )

                list( value = uv.2$value + uv.3$value, abs.error = uv.2$abs.error + uv.3$abs.error )
             },
             d.range = 1e-6,
             permit.d = function( d, abs.error, info, parent.info )
                        { abs( d ) < 1 && abs( d ) < abs.error * 4 } ), TRUE )

expect_equal( check.integ( function( mean, sd, x )
             {
                uv.t3.sub( 1, x[1], mean, sd ) +
                uv.t3.sub( 2, x[1], mean + x[2] / 2, sd + 0.1 ) +
                uv.t3.sub( 3, x[1], mean + x[2], sd + 0.3 )
             },
             function( mean, sd, x )
             {
                uv.1 <- uv.t3.sub.via.integrate( 1, x[1], mean, sd )
                uv.2 <- uv.t3.sub.via.integrate( 2, x[1], mean + x[2] / 2, sd + 0.1 )
                uv.3 <- uv.t3.sub.via.integrate( 3, x[1], mean + x[2], sd + 0.3 )

                list( value = uv.1$value + uv.2$value + uv.3$value,
                      abs.error = uv.1$abs.error + uv.2$abs.error + uv.3$abs.error )
             },
             d.range = 1e-6,
             permit.d = function( d, abs.error, info, parent.info )
                        { abs( d ) < 1 && abs( d ) < abs.error * 4 } ), TRUE )


## Integral of R1, R2 for t4.mean begin

# R1 for t4.mean
integ.m.x.d.pstar.psqrt2 <- function( mean, sd, x )
{
    d <- dnorm( x, mean, sd )
    p <- pnorm( x, mean, sd )
    pstar <- pnorm( x, mean, sd * sqrt( 2 ) / 2 )
    p3star <- pnorm( x, mean, sd * sqrt( 3 ) / 3 )

    -mean * ( p[2] * pstar[2] - p[1] * pstar[1] ) / sqrt( 2 ) +
    sd^2 * ( d[2] * pstar[2] - d[1] * pstar[1] ) / sqrt( 2 ) -
    sd * ( p3star[2] - p3star[1] ) / sqrt( 6 * pi ) +
    sqrt( 2 * pi ) * sd * mean * integrate( function( x ) { dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value
}

integ.m.x.d.pstar.psqrt2.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { -x * dnorm( x, mean, sd ) * pnorm( x, mean, sd * sqrt( 2 ) / 2 ) / sqrt( 2 ) }, x[1], x[2] )
}

expect_equal( check.integ( integ.m.x.d.pstar.psqrt2, integ.m.x.d.pstar.psqrt2.via.integrate ), TRUE )


# R2 for t4.mean
integ.m.sqrt2pisd2.x.d2.p <- function( mean, sd, x )
{
    d <- dnorm( x, mean, sd )
    p <- pnorm( x, mean, sd )
    pstar <- pnorm( x, mean, sd * sqrt( 2 ) / 2 )
    p3star <- pnorm( x, mean, sd * sqrt( 3 ) / 3 )

    sqrt( pi / 2 ) * sd^3 * ( d[2]^2 * p[2] - d[1]^2 * p[1] ) -
    sd * ( p3star[2] - p3star[1] ) / ( 2 * sqrt( 6 * pi ) ) -
    sqrt( 2 * pi ) * sd * mean * integrate( function( x ) { dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value
}

integ.m.sqrt2pisd2.x.d2.p.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { -sqrt( 2 * pi ) * sd * x * dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )
}

expect_equal( check.integ( integ.m.sqrt2pisd2.x.d2.p, integ.m.sqrt2pisd2.x.d2.p.via.integrate ), TRUE )


# R1 + R2 for t4.mean
integ.m.x.d.pstar.psqrt2.m.sqrt2pisd2.x.d2.p <- function( mean, sd, x )
{
    d <- dnorm( x, mean, sd )
    p <- pnorm( x, mean, sd )
    pstar <- pnorm( x, mean, sd * sqrt( 2 ) / 2 )
    p3star <- pnorm( x, mean, sd * sqrt( 3 ) / 3 )

    ( sqrt( pi ) * sd^3 * ( d[2]^2 * p[2] - d[1]^2 * p[1] ) -
    mean * ( p[2] * pstar[2] - p[1] * pstar[1] ) +
    sd^2 * ( d[2] * pstar[2] - d[1] * pstar[1] ) -
    sqrt( 3 ) * sd * ( p3star[2] - p3star[1] ) / ( 2 * sqrt( pi ) ) ) / sqrt( 2 )
}

integ.m.x.d.pstar.psqrt2.m.sqrt2pisd2.x.d2.p.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { -x * dnorm( x, mean, sd ) * pnorm( x, mean, sd * sqrt( 2 ) / 2 ) / sqrt( 2 ) -
                                sqrt( 2 * pi ) * sd * x * dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )
}

expect_equal( check.integ( integ.m.x.d.pstar.psqrt2.m.sqrt2pisd2.x.d2.p,
						   integ.m.x.d.pstar.psqrt2.m.sqrt2pisd2.x.d2.p.via.integrate ), TRUE )

## R1, R2 for t4.mean end


## Integral of B1, B2 for t4.mean begin

# B1 for t4.mean
integ.x.d_1.pstar_2.psqrt2 <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    d.1 <- dnorm( x, mean.1, sd.1 )
    p.1 <- pnorm( x, mean.1, sd.1 )
    pstar.2 <- pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 )

    ( ( mean.1 * p.1[2] - sd.1^2 * d.1[2] ) * pstar.2[2] -
      ( mean.1 * p.1[1] - sd.1^2 * d.1[1] ) * pstar.2[1] ) / sqrt( 2 ) -
    sqrt( 2 * pi ) * sd.2 * mean.1 *
    integrate( function( x ) { pnorm( x, mean.1, sd.1 ) * dnorm( x, mean.2, sd.2 )^2 }, x[1], x[2] )$value +
    sqrt( 2 * pi ) * sd.2 * sd.1^2 *
    integrate( function( x ) { dnorm( x, mean.1, sd.1 ) * dnorm( x, mean.2, sd.2 )^2 }, x[1], x[2] )$value
}

integ.x.d_1.pstar_2.psqrt2.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integrate( function( x ) { x * dnorm( x, mean.1, sd.1 ) * pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) / sqrt( 2 ) },
                x[1], x[2] )
}

expect_equal( check.integ.2( integ.x.d_1.pstar_2.psqrt2,
							 integ.x.d_1.pstar_2.psqrt2.via.integrate ), TRUE )


# B2 for t4.mean
integ.sqrt2pisd_22.x.d_22.pstar_1 <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    d.2 <- dnorm( x, mean.2, sd.2 )
    p.1 <- pnorm( x, mean.1, sd.1 )

    -sqrt( pi / 2 ) * sd.2^3 * ( p.1[2] * d.2[2]^2 - p.1[1] * d.2[1]^2 ) +
    sqrt( 2 * pi ) * sd.2 * mean.2 *
    integrate( function( x ) { pnorm( x, mean.1, sd.1 ) * dnorm( x, mean.2, sd.2 )^2 }, x[1], x[2] )$value +
    sqrt( pi / 2 ) * sd.2^3 *
    integrate( function( x ) { dnorm( x, mean.1, sd.1 ) * dnorm( x, mean.2, sd.2 )^2 }, x[1], x[2] )$value
}

integ.sqrt2pisd_22.x.d_22.pstar_1.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integrate( function( x ) { sqrt( 2 * pi ) * sd.2 * x * dnorm( x, mean.2, sd.2 )^2 * pnorm( x, mean.1, sd.1 ) },
                x[1], x[2] )
}

expect_equal( check.integ.2( integ.sqrt2pisd_22.x.d_22.pstar_1,
							 integ.sqrt2pisd_22.x.d_22.pstar_1.via.integrate, d.range = 3e-8 ), TRUE )


# B1 + B2 for t4.mean
integ.B1.plus.B2.mean <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    d.1 <- dnorm( x, mean.1, sd.1 )
    d.2 <- dnorm( x, mean.2, sd.2 )
    p.1 <- pnorm( x, mean.1, sd.1 )
    pstar.2 <- pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 )

    ( ( mean.1 * p.1[2] - sd.1^2 * d.1[2] ) * pstar.2[2] -
      ( mean.1 * p.1[1] - sd.1^2 * d.1[1] ) * pstar.2[1] ) / sqrt( 2 ) -
    sqrt( pi / 2 ) * sd.2^3 * ( p.1[2] * d.2[2]^2 - p.1[1] * d.2[1]^2 ) -
    sqrt( 2 * pi ) * sd.2 * ( mean.1 - mean.2 ) *
    integrate( function( x ) { pnorm( x, mean.1, sd.1 ) * dnorm( x, mean.2, sd.2 )^2 }, x[1], x[2] )$value +
    ( 2 * sd.1^2 + sd.2^2 ) / ( 4 * pi * sd.1 * sd.2 ) *
    integrate( function( x )
                {
                    exp( -( ( 2 * sd.1^2 + sd.2^2 ) * x^2 -
                            2 * ( 2 * sd.1^2 * mean.2 + sd.2^2 * mean.1 ) * x +
                            2 * sd.1^2 * mean.2^2 + sd.2^2 * mean.1^2 ) / ( 2 * sd.1^2 * sd.2^2 ) )
                }, x[1], x[2] )$value
}

integ.B1.plus.B2.mean.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integrate( function( x ) { x * dnorm( x, mean.1, sd.1 ) * pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) / sqrt( 2 ) +
                               sqrt( 2 * pi ) * sd.2 * x * dnorm( x, mean.2, sd.2 )^2 * pnorm( x, mean.1, sd.1 ) },
                x[1], x[2] )
}

expect_equal( check.integ.2( integ.B1.plus.B2.mean, integ.B1.plus.B2.mean.via.integrate ), TRUE )


integ.B1.plus.B2.mean.inf <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    mean.1 / sqrt( 2 ) +
    sqrt( ( 2 * sd.1^2 + sd.2^2 ) / ( 8 * pi ) ) * exp( -( mean.1 - mean.2 )^2 / ( 2 * sd.1^2 + sd.2^2 ) ) -
    ( mean.1 - mean.2 ) / sqrt( 2 ) *
    integrate( function( x ) { pnorm( x, mean.1, sd.1 ) * dnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) }, x[1], x[2] )$value
}

integ.B1.plus.B2.mean.inf.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integ.B1.plus.B2.mean.via.integrate( mean.1, sd.1, mean.2, sd.2, x )
}

expect_equal( check.integ.2( integ.B1.plus.B2.mean.inf,
							 integ.B1.plus.B2.mean.inf.via.integrate, inf.to.inf = TRUE, d.range = 7e-5 ), TRUE )

## B1, B2 for t4.mean end


## Integral of G1, G2 for t4.mean begin

# G1 for t4.mean
integ.msqrtpi.sd_1.x.d_12.pstar_2 <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    d.1 <- dnorm( x, mean.1, sd.1 )
    pstar.2 <- pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 )

    sqrt( pi ) * sd.1^3 * ( d.1[2]^2 * pstar.2[2] - d.1[1]^2 * pstar.2[1] ) / 2 -
    mean.1 * integrate( function( x )
                        { dnorm( x, mean.1, sd.1 * sqrt( 2 ) / 2 ) * pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) },
                        x[1], x[2] )$value / 2 -
    sd.1 / ( 4 * pi * sd.2 ) *
    integrate( function( x )
                {
                    exp( -( ( sd.1^2 + sd.2^2 ) * x^2 -
                            2 * ( sd.2^2 * mean.1 + sd.1^2 * mean.2 ) * x +
                            sd.2^2 * mean.1^2 + sd.1^2 * mean.2^2 ) / ( sd.1^2 * sd.2^2 ) )
                }, x[1], x[2] )$value
}

integ.msqrtpi.sd_1.x.d_12.pstar_2.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integrate( function( x )
                { -sqrt( pi ) * sd.1 * x * dnorm( x, mean.1, sd.1 )^2 * pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) },
                x[1], x[2] )
}

expect_equal( check.integ.2( integ.msqrtpi.sd_1.x.d_12.pstar_2,
							 integ.msqrtpi.sd_1.x.d_12.pstar_2.via.integrate, d.range = 3e-7 ), TRUE )


integ.msqrtpi.sd_1.x.d_12.pstar_2.inf <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    -sd.1^2 / ( 4 * sqrt( pi * ( sd.1^2 + sd.2^2 ) ) ) * exp( -( mean.1 - mean.2 )^2 / ( sd.1^2 + sd.2^2 ) ) -
    mean.1 * integrate( function( x )
                        { dnorm( x, mean.1, sd.1 * sqrt( 2 ) / 2 ) * pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) },
                        x[1], x[2] )$value / 2
}

integ.msqrtpi.sd_1.x.d_12.pstar_2.inf.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integ.msqrtpi.sd_1.x.d_12.pstar_2.via.integrate( mean.1, sd.1, mean.2, sd.2, x )
}

expect_equal( check.integ.2( integ.msqrtpi.sd_1.x.d_12.pstar_2.inf,
							 integ.msqrtpi.sd_1.x.d_12.pstar_2.inf.via.integrate,
                inf.to.inf = TRUE, d.range = 5e-4 ), TRUE )


# G2 for t4.mean
integ.msqrtpi.sd_2.x.d_22.pstar_1 <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    d.2 <- dnorm( x, mean.2, sd.2 )
    pstar.1 <- pnorm( x, mean.1, sd.1 * sqrt( 2 ) / 2 )
    pstar.2 <- pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 )

    -mean.2 * ( pstar.1[2] * pstar.2[2] - pstar.1[1] * pstar.2[1] ) / 2 +
    sqrt( pi ) * sd.2^3 * ( pstar.1[2] * d.2[2]^2 - pstar.1[1] * d.2[1]^2 ) / 2 +
    mean.2 * integrate( function( x )
                        { dnorm( x, mean.1, sd.1 * sqrt( 2 ) / 2 ) * pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) },
                        x[1], x[2] )$value / 2 -
    sd.2^2 *
    integrate( function( x )
                {
                    dnorm( x, mean.1, sd.1 * sqrt( 2 ) / 2 ) * dnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 )
                }, x[1], x[2] )$value / 4
}

integ.msqrtpi.sd_2.x.d_22.pstar_1.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integrate( function( x )
                { -sqrt( pi ) * sd.2 * x * dnorm( x, mean.2, sd.2 )^2 * pnorm( x, mean.1, sd.1 * sqrt( 2 ) / 2 ) },
                x[1], x[2] )
}

expect_equal( check.integ.2( integ.msqrtpi.sd_2.x.d_22.pstar_1,
							 integ.msqrtpi.sd_2.x.d_22.pstar_1.via.integrate, d.range = 3e-7 ), TRUE )


integ.msqrtpi.sd_2.x.d_22.pstar_1.inf <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    -mean.2 / 2 -
    sd.2^2 / ( 4 * sqrt( pi * ( sd.1^2 + sd.2^2 ) ) ) * exp( -( mean.1 - mean.2 )^2 / ( sd.1^2 + sd.2^2 ) ) +
    mean.2 * integrate( function( x )
                        { dnorm( x, mean.1, sd.1 * sqrt( 2 ) / 2 ) * pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) },
                        x[1], x[2] )$value / 2
}

integ.msqrtpi.sd_2.x.d_22.pstar_1.inf.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integ.msqrtpi.sd_2.x.d_22.pstar_1.via.integrate( mean.1, sd.1, mean.2, sd.2, x )
}

expect_equal( check.integ.2( integ.msqrtpi.sd_2.x.d_22.pstar_1.inf,
							 integ.msqrtpi.sd_2.x.d_22.pstar_1.inf.via.integrate,
                inf.to.inf = TRUE, d.range = 1e-3 ), TRUE )


# G1 + G2 for t4.mean
integ.G1.plus.G2.mean.inf <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    -mean.2 / 2 -
    sqrt( ( sd.1^2 + sd.2^2 ) / pi ) / 4 * exp( -( mean.1 - mean.2 )^2 / ( sd.1^2 + sd.2^2 ) ) -
    ( mean.1 - mean.2 ) *
    integrate( function( x )
                { dnorm( x, mean.1, sd.1 * sqrt( 2 ) / 2 ) * pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) },
                x[1], x[2] )$value / 2
}

integ.G1.plus.G2.mean.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integrate( function( x )
                {
                    -sqrt( pi ) * x * ( sd.1 * dnorm( x, mean.1, sd.1 )^2 * pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) +
                                        sd.2 * dnorm( x, mean.2, sd.2 )^2 * pnorm( x, mean.1, sd.1 * sqrt( 2 ) / 2 ) )
                }, x[1], x[2] )
}

integ.G1.plus.G2.mean.inf.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integ.G1.plus.G2.mean.via.integrate( mean.1, sd.1, mean.2, sd.2, x )
}

expect_equal( check.integ.2( integ.G1.plus.G2.mean.inf,
							 integ.G1.plus.G2.mean.inf.via.integrate, inf.to.inf = TRUE, d.range = 5e-4 ), TRUE )

## G1, G2 for t4.mean end


################################################################################################
#' Calculates \eqn{\dfrac{1}{\sqrt{2}} \int_{x_1}^{x_2} x f(x)^2 \Phi^*(x) dx}
#' where \eqn{f} is the PDF of N(mean, sd) and \eqn{\Phi^*} is the CDF of N(mean, sd/sqrt(2)).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.x.d2.pstar.psqrt2 <- function( mean, sd, x )
{
#   dstar <- dnorm( x, mean, sd * sqrt( 2 ) / 2 )
#   pstar <- pnorm( x, mean, sd * sqrt( 2 ) / 2 )
#
#   ( ( ( mean / 2 * pstar[2]^2 - sd^2 * dstar[2] * pstar[2] / 2 ) -
#       ( mean / 2 * pstar[1]^2 - sd^2 * dstar[1] * pstar[1] / 2 ) ) +
#     integrate( function( x ) { sd^2 / 2 * dnorm( x, mean, sd * sqrt( 2 ) / 2 )^2 }, x[1], x[2] )$value ) /
#   ( 2 * sqrt( 2 * pi ) * sd )
    dstar <- dnorm( x, mean, sd * sqrt( 2 ) / 2 )
    pstar <- pnorm( x, mean, sd * sqrt( 2 ) / 2 )
    p4star <- pnorm( x, mean, sd / 2 )

    ( ( mean * ( pstar[2]^2 - pstar[1]^2 ) / sd -
        sd * ( dstar[2] * pstar[2] - dstar[1] * pstar[1] ) ) / sqrt( 2 * pi ) +
      ( p4star[2] - p4star[1] ) / pi / 2 ) / 4
}

integ.x.d2.pstar.psqrt2.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { x * dnorm( x, mean, sd )^2 * pnorm( x, mean, sd * sqrt( 2 ) / 2 ) / sqrt( 2 ) }, x[1], x[2] )
}

expect_equal( check.integ( integ.x.d2.pstar.psqrt2, integ.x.d2.pstar.psqrt2.via.integrate ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} x \Psi_1(x) g_2(x) dx}
#' where \eqn{\Psi_1} is the CDF of intervals[[1]] of a CGD model of type1.type = 4
#' and \eqn{g_2} is the PDF of intervals[[2]] of the model.
#' Remark both intervals[[1]] and intervals[[2]] are CGD models of type1.type = 3.
#' @param   mean.1  The mean of intervals[[1]].
#' @param   sd.1    The s.d. of intervals[[1]].
#' @param   mean.2  The mean of intervals[[2]].
#' @param   sd.2    The s.d. of intervals[[2]].
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.t4.x.psi.g.inf <- function( mean.1, sd.1, mean.2, sd.2, x )
{
#   mean.1 / 2 + sd.1 / ( 2 * sqrt( pi ) ) -
#   mean.1 / sqrt( 2 ) - sqrt( 3 ) * sd.1 / ( 2 * sqrt( 2 * pi ) ) +
#   integ.B1.plus.B2.inf( mean.1, sd.1, mean.2, sd.2, x ) +
#   integ.G1.plus.G2.inf( mean.1, sd.1, mean.2, sd.2, x ) +
#   mean.1 / 4 + sd.1 * sqrt( 2 ) / sqrt( pi ) / 8 +
#   mean.2 / 4 + sd.2 * sqrt( 2 ) / sqrt( pi ) / 8
    d.mean <- mean.1 - mean.2

    ( ( ( 4 + sqrt( 2 ) - 2 * sqrt( 6 ) ) * sd.1 + sqrt( 2 ) * sd.2 ) / 8 +
      sqrt( ( 2 * sd.1^2 + sd.2^2 ) / 8 ) * exp( -d.mean^2 / ( 2 * sd.1^2 + sd.2^2 ) ) -
      sqrt( sd.1^2 + sd.2^2 ) * exp( -d.mean^2 / ( sd.1^2 + sd.2^2 ) ) / 4 ) / sqrt( pi ) +
    ( mean.1 + mean.2 ) / 4 -
    d.mean / 2 * ( sqrt( 2 ) * pnorm( -d.mean / sqrt( sd.1^2 + sd.2^2 / 2 ), 0, 1 ) -
                   pnorm( -sqrt( 2 ) * d.mean / sqrt( sd.1^2 + sd.2^2 ), 0, 1 ) )
}

integ.t4.x.psi.g.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integrate( function( x )
                {
                    x * ( dnorm( x, mean.1, sd.1 ) / sqrt( 2 ) -
                          sqrt( pi ) * ( sd.1 * dnorm( x, mean.1, sd.1 )^2 -
                                         sd.2 * dnorm( x, mean.2, sd.2 )^2 ) ) *
                    ( sqrt( 2 ) * pnorm( x, mean.1, sd.1 ) -
                      pnorm( x, mean.1, sd.1 * sqrt( 2 ) / 2 ) +
                      pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) )
                }, x[1], x[2] )
}

integ.t4.x.psi.g.inf.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integ.t4.x.psi.g.via.integrate( mean.1, sd.1, mean.2, sd.2, x )
}

expect_equal( check.integ.2( integ.t4.x.psi.g.inf,
							 integ.t4.x.psi.g.inf.via.integrate, inf.to.inf = TRUE, d.range = 6.5e-4 ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} f(x)^3 dx} where \eqn{f} is the PDF of N(mean, sd).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.d3 <- function( mean, sd, x )
{
    ( pnorm( x[2], mean, sd * sqrt( 3 ) / 3 ) - pnorm( x[1], mean, sd * sqrt( 3 ) / 3 ) ) / ( 2 * sqrt( 3 ) * pi * sd^2 )
}

integ.d3.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { dnorm( x, mean, sd )^3 }, x[1], x[2] )
}

expect_equal( check.integ( integ.d3, integ.d3.via.integrate ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} \Phi^{3*}(x) dx}
#' where \eqn{\Phi^{3*}} is the CDF of N(mean, sd/sqrt(3)).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.p3star <- function( mean, sd, x )
{
#   ( x[2] - mean ) * pnorm( x[2], mean, sd * sqrt( 3 ) / 3 ) -
#   ( x[1] - mean ) * pnorm( x[1], mean, sd * sqrt( 3 ) / 3 ) +
#   sd^4 * ( dnorm( x[2], mean, sd )^3 - dnorm( x[1], mean, sd )^3 ) * pi * 2 * sqrt( 3 ) / 3

    sd.3star <- sd * sqrt( 3 ) / 3

    ( x[2] - mean ) * pnorm( x[2], mean, sd.3star ) -
    ( x[1] - mean ) * pnorm( x[1], mean, sd.3star ) +
    sd^2* ( dnorm( x[2], mean, sd.3star ) - dnorm( x[1], mean, sd.3star ) ) / 3
}

integ.p3star.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { pnorm( x, mean, sd * sqrt( 3 ) / 3 ) }, x[1], x[2] )
}

expect_equal( check.integ( integ.p3star, integ.p3star.via.integrate ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} x f(x)^3 dx} where \eqn{f} is the PDF of N(mean, sd).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.x.d3 <- function( mean, sd, x )
{
    sd.3star <- sd * sqrt( 3 ) / 3

    mean * ( pnorm( x[2], mean, sd.3star ) - pnorm( x[1], mean, sd.3star ) ) / ( sd^2 * pi * sqrt( 3 ) * 2 ) -
    sd^2 * ( dnorm( x[2], mean, sd )^3 - dnorm( x[1], mean, sd )^3 ) / 3
#   ( x[2] * pnorm( x[2], mean, sd * sqrt( 3 ) / 3 ) -
#     x[1] * pnorm( x[1], mean, sd * sqrt( 3 ) / 3 ) -
#     integrate( function( x ) { pnorm( x, mean, sd * sqrt( 3 ) / 3 ) }, x[1], x[2] )$value ) / ( 2 * sqrt( 3 ) * pi * sd^2 )
}

integ.x.d3.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { x * dnorm( x, mean, sd )^3 }, x[1], x[2] )
}

expect_equal( check.integ( integ.x.d3, integ.x.d3.via.integrate ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} x^2 f(x)^2 \Phi(x) dx}
#' where \eqn{f} is the PDF of N(mean, sd) and \eqn{\Phi} is the CDF of N(mean, sd).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.x2.d2.p <- function( mean, sd, x )
{
    -sd^2 / 2 * ( x[2] * dnorm( x[2], mean, sd )^2 * pnorm( x[2], mean, sd ) -
                  x[1] * dnorm( x[1], mean, sd )^2 * pnorm( x[1], mean, sd ) ) +
    mean * ( pnorm( x[2], mean, sd * sqrt( 3 ) / 3 ) - pnorm( x[1], mean, sd * sqrt( 3 ) / 3 ) ) / ( 4 * sqrt( 3 ) * pi ) -
    sd^4 * ( dnorm( x[2], mean, sd )^3 - dnorm( x[1], mean, sd )^3 ) / 6 +
    sd^2 / 2 * integrate( function( x ) { dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value +
    mean * integrate( function( x ) { x * dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value
#   -sd^2 / 2 * ( x[2] * dnorm( x[2], mean, sd )^2 * pnorm( x[2], mean, sd ) -
#                 x[1] * dnorm( x[1], mean, sd )^2 * pnorm( x[1], mean, sd ) ) +
#   sd^2 / 2 * integ.x.d3( mean, sd, x ) +
#   sd^2 / 2 * integrate( function( x ) { dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value +
#   mean * integrate( function( x ) { x * dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value
##  -sd^2 * ( x[2] * dnorm( x[2], mean, sd )^2 * pnorm( x[2], mean, sd ) -
##            x[1] * dnorm( x[1], mean, sd )^2 * pnorm( x[1], mean, sd ) ) +
##  sd^2 * integrate( function( x ) { dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value -
##  integrate( function( x ) { x^2 * dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value +
##  2 * mean * integrate( function( x ) { x * dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value +
##  sd^2 * integ.x.d3( mean, sd, x )
### -sd^2 * ( x[2] * dnorm( x[2], mean, sd )^2 * pnorm( x[2], mean, sd ) -
###           x[1] * dnorm( x[1], mean, sd )^2 * pnorm( x[1], mean, sd ) ) +
### sd^2 * integrate( function( x ) { dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value -
### integrate( function( x ) { ( x^2 - mean * x ) * dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value +
### sd^2 * integ.x.d3( mean, sd, x ) +
### mean * integrate( function( x ) { x * dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )$value
}

integ.x2.d2.p.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { x^2 * dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )
}

expect_equal( check.integ( integ.x2.d2.p, integ.x2.d2.p.via.integrate ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} x^2 f(x)^2 \Phi^*(x) dx}
#' where \eqn{f} is the PDF of N(mean, sd) and \eqn{\Phi^*} is the CDF of N(mean, sd/sqrt(2)).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.x2.d2.pstar <- function( mean, sd, x )
{
    dstar <- dnorm( x, mean, sd * sqrt( 2 ) / 2 )
    pstar <- pnorm( x, mean, sd * sqrt( 2 ) / 2 )

    ( ( 2 * mean^2 + sd^2 ) * ( pstar[2]^2 - pstar[1]^2 ) / 4 +
    mean * sd * ( pnorm( x[2], mean, sd / 2 ) -
                  pnorm( x[1], mean, sd / 2 ) ) / sqrt( 2 * pi ) -
    sd^2 * ( ( x[2] + mean ) * dstar[2] * pstar[2] -
             ( x[1] + mean ) * dstar[1] * pstar[1] ) / 2 -
    sd^4 * ( dstar[2]^2 - dstar[1]^2 ) / 8 ) / ( 2 * sqrt( pi ) * sd )
}

integ.x2.d2.pstar.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { x^2 * dnorm( x, mean, sd )^2 * pnorm( x, mean, sd * sqrt( 2 ) / 2 ) }, x[1], x[2] )
}

expect_equal( check.integ( integ.x2.d2.pstar, integ.x2.d2.pstar.via.integrate ), TRUE )


################################################################################################
#' Calculates \eqn{\dfrac{1}{\sqrt{2}} \int_{x_1}^{x_2} x^2 f(x) \Phi^*(x) dx}
#' where \eqn{f} is the PDF of N(mean, sd) and \eqn{\Phi^*} is the CDF of N(mean, sd/sqrt(2)).
#' @param   mean    The mean of N(mean, sd).
#' @param   sd      The s.d. of N(mean, sd).
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.x2.d.pstar <- function( mean, sd, x )
{
    sd.star <- sd * sqrt( 2 ) / 2
    sd.3star <- sd * sqrt( 3 ) / 3
    d <- dnorm( x, mean, sd )
    p <- pnorm( x, mean, sd )
    pstar <- pnorm( x, mean, sd.star )
    d3star <- dnorm( x, mean, sd.3star )
    p3star <- pnorm( x, mean, sd.3star )

    ( mean^2 + sd^2 ) * ( p[2] * pstar[2] - p[1] * pstar[1] ) -
    sd^2 * ( ( x[2] + mean ) * d[2] * pstar[2] - ( x[1] + mean ) * d[1] * pstar[1] ) +
    sd * ( 2 * mean * ( p3star[2] - p3star[1] ) -
           sd^2 * ( d3star[2] - d3star[1] ) / 3 ) / sqrt( 3 * pi ) -
    ( mean^2 + sd^2 ) *
    integrate( function( x ) { dnorm( x, mean, sd.star ) * pnorm( x, mean, sd ) }, x[1], x[2] )$value
}

integ.x2.d.pstar.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { x^2 * dnorm( x, mean, sd ) * pnorm( x, mean, sd * sqrt( 2 ) / 2 ) }, x[1], x[2] )
}

expect_equal( check.integ( integ.x2.d.pstar, integ.x2.d.pstar.via.integrate ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} x^2 g_i(x) dx}
#' where \eqn{g_i} is the CDF of intervals[[i]] (i = 1 or 2) of a CGD model of type1.type = 4.
#' Remark intervals[[i]] is a CGD models of type1.type = 3.
#' @param   mean.1  The mean of intervals[[i]][[1]].
#' @param   sd.1    The s.d. of intervals[[i]][[1]].
#' @param   mean.2  The mean of intervals[[i]][[2]].
#' @param   sd.2    The s.d. of intervals[[i]][[2]].
#' @param   x       The interval of \eqn{[x_1, x_2]}.
#' @return  The result value of calculation.
################################################################################################
integ.t4.x2.g <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integ.xn.d( 2, mean.1, sd.1, x ) -
    integ.xn.d( 2, mean.1, sd.1 * sqrt( 2 ) / 2, x ) / sqrt( 2 ) +
    integ.xn.d( 2, mean.2, sd.2 * sqrt( 2 ) / 2, x ) / sqrt( 2 )
}

################################################################################################
#' Calculates \eqn{\int_{-\infty}^{\infty} x^2 g_i(x) dx}
#' where \eqn{g_i} is the CDF of intervals[[i]] (i = 1 or 2) of a CGD model of type1.type = 4.
#' Remark intervals[[i]] is a CGD models of type1.type = 3.
#' @param   mean.1  The mean of intervals[[i]][[1]].
#' @param   sd.1    The s.d. of intervals[[i]][[1]].
#' @param   mean.2  The mean of intervals[[i]][[2]].
#' @param   sd.2    The s.d. of intervals[[i]][[2]].
#' @param   x       The interval of \eqn{[\infty, \infty]}.
#'                  This argument is not used. It is prepared just for compatibility.
#' @return  The result value of calculation.
################################################################################################
integ.t4.x2.g.inf <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    ( ( 2 - sqrt( 2 ) ) * mean.1^2 + ( 4 - sqrt( 2 ) ) * sd.1^2 / 2 +
    sqrt( 2 ) * ( mean.2^2 + sd.2^2 / 2 ) ) / 2
}

integ.t4.x2.g.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integrate( function( x ) { x^2 * ( dnorm( x, mean.1, sd.1 ) -
                                       sqrt( 2 * pi ) * sd.1 *  dnorm( x, mean.1, sd.1 )^2 +
                                       sqrt( 2 * pi ) * sd.2 *  dnorm( x, mean.2, sd.2 )^2 ) }, x[1], x[2] )
}

integ.t4.x2.g.inf.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integ.t4.x2.g.via.integrate( mean.1, sd.1, mean.2, sd.2, x )
}

expect_equal( check.integ.2( integ.t4.x2.g, integ.t4.x2.g.via.integrate ), TRUE )

expect_equal( check.integ.2( integ.t4.x2.g.inf,
							 integ.t4.x2.g.inf.via.integrate, inf.to.inf = TRUE ), TRUE )


################################################################################################
#' Calculates \eqn{\sqrt{\pi \sigma^2}} \int_{x_1}^{x_2} x^2 f(x)^2 \Phi^*(x) dx}
#' where \eqn{f} is the PDF of N(mean, \eqn{\sigma}) and
#' \eqn{\Phi^*} is the CDF of N(mean, \eqn{\sigma / \sqrt{2}}).
#' @param   mean    The mean of N(mean, \eqn{\sigma}).
#' @param   sd      The s.d. of N(mean, \eqn{\sigma}).
#' @param   x       The interval of \eqn{[x_1, x_2]}}.
#' @return  The result value of calculation.
################################################################################################
integ.sqrtpisd2.x2.d2.pstar <- function( mean, sd, x )
{
    dstar <- dnorm( x, mean, sd * sqrt( 2 ) / 2 )
    pstar <- pnorm( x, mean, sd * sqrt( 2 ) / 2 )
    p4star <- pnorm( x, mean, sd / 2 )

    ( ( 2 * mean^2 + sd^2 ) * ( pstar[2]^2 - pstar[1]^2 ) / 4 +
      mean * sd * ( p4star[2] - p4star[1] ) / sqrt( 2 * pi ) -
      sd^2 * ( ( x[2] + mean ) * dstar[2] * pstar[2] - ( x[1] + mean ) * dstar[1] * pstar[1] ) / 2 -
      sd^4 * ( dstar[2]^2 - dstar[1]^2 ) / 8 ) / 2
}

integ.sqrtpisd2.x2.d2.pstar.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { sqrt( pi ) * sd * x^2 * dnorm( x, mean, sd )^2 * pnorm( x, mean, sd * sqrt( 2 ) / 2 ) },
                x[1], x[2] )
}

expect_equal( check.integ( integ.sqrtpisd2.x2.d2.pstar, integ.sqrtpisd2.x2.d2.pstar.via.integrate ), TRUE )


################################################################################################
#' Calculates \eqn{\sqrt{\pi \sigma^2}} \int_{-\infty}^{\infty} x^2 f(x)^2 \Phi^*(x) dx}
#' where \eqn{f} is the PDF of N(mean, \eqn{\sigma}) and
#' \eqn{\Phi^*} is the CDF of N(mean, \eqn{\sigma / \sqrt{2}}).
#' @param   mean    The mean of N(mean, \eqn{\sigma}).
#' @param   sd      The s.d. of N(mean, \eqn{\sigma}).
#' @param   x       The interval of \eqn{[x_1, x_2]}}.
#' @return  The result value of calculation.
################################################################################################
integ.sqrtpisd2.x2.d2.pstar.inf <- function( mean, sd, x )
{
    ( ( 2 * mean^2 + sd^2 ) / 4 + mean * sd / sqrt( 2 * pi ) ) / 2
}

integ.sqrtpisd2.x2.d2.pstar.inf.via.integrate <- function( mean, sd, x )
{
    integ.sqrtpisd2.x2.d2.pstar.via.integrate( mean, sd, x )
}

expect_equal( check.integ( integ.sqrtpisd2.x2.d2.pstar.inf,
						   integ.sqrtpisd2.x2.d2.pstar.inf.via.integrate, inf.to.inf = TRUE ), TRUE )


## Integral of R1, R2 for t4.sd begin

# R1 for t4.sd
integ.R1.sd <- function( mean, sd, x )
{
    sd.star <- sd * sqrt( 2 ) / 2
    sd.3star <- sd * sqrt( 3 ) / 3
    d <- dnorm( x, mean, sd )
    p <- pnorm( x, mean, sd )
    pstar <- pnorm( x, mean, sd.star )
    d3star <- dnorm( x, mean, sd.3star )
    p3star <- pnorm( x, mean, sd.3star )

    -( ( mean^2 + sd^2 ) * ( p[2] * pstar[2] - p[1] * pstar[1] ) -
       sd^2 * ( ( x[2] + mean ) * d[2] * pstar[2] - ( x[1] + mean ) * d[1] * pstar[1] ) +
       sd * ( 2 * mean * ( p3star[2] - p3star[1] ) -
              sd^2 * ( d3star[2] - d3star[1] ) / 3 ) / sqrt( 3 * pi ) ) / sqrt( 2 ) +
    ( mean^2 + sd^2 ) / sqrt( 2 ) *
    integrate( function( x ) { dnorm( x, mean, sd.star ) * pnorm( x, mean, sd ) }, x[1], x[2] )$value
}

integ.R1.sd.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { -x^2 * dnorm( x, mean, sd ) * pnorm( x, mean, sd * sqrt( 2 ) / 2 ) / sqrt( 2 ) }, x[1], x[2] )
}

expect_equal( check.integ( integ.R1.sd, integ.R1.sd.via.integrate ), TRUE )


# R2 for t4.sd
integ.R2.sd <- function( mean, sd, x )
{
    sd.star <- sd * sqrt( 2 ) / 2
    sd.3star <- sd * sqrt( 3 ) / 3
    d <- dnorm( x, mean, sd )
    p <- pnorm( x, mean, sd )
    dstar <- dnorm( x, mean, sd.star )
    pstar <- pnorm( x, mean, sd.star )
    d3star <- dnorm( x, mean, sd.3star )
    p3star <- pnorm( x, mean, sd.3star )

    -( -sd^2 / 2 * ( ( x[2] + mean ) * dstar[2] * p[2] - ( x[1] + mean ) * dstar[1] * p[1] ) +
       sd / sqrt( 3 * pi ) / 2 * ( 2 * mean * ( p3star[2] - p3star[1] ) - sd^2 / 3 * ( d3star[2] - d3star[1] ) ) +
       ( mean^2 + sd^2 / 2 ) *
       integrate( function( x ) { dnorm( x, mean, sd.star ) * pnorm( x, mean, sd ) }, x[1], x[2] )$value ) / sqrt( 2 )
}

integ.R2.sd.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { -sqrt( 2 * pi ) * sd * x^2 * dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )
}

expect_equal( check.integ( integ.R2.sd, integ.R2.sd.via.integrate ), TRUE )


# R1 + R2 for t4.sd
integ.R1.plus.R2.sd <- function( mean, sd, x )
{
    sd.star <- sd * sqrt( 2 ) / 2
    sd.3star <- sd * sqrt( 3 ) / 3
    d <- dnorm( x, mean, sd )
    p <- pnorm( x, mean, sd )
    dstar <- dnorm( x, mean, sd.star )
    pstar <- pnorm( x, mean, sd.star )
    d3star <- dnorm( x, mean, sd.3star )
    p3star <- pnorm( x, mean, sd.3star )

    -( ( mean^2 + sd^2 ) * ( p[2] * pstar[2] - p[1] * pstar[1] ) -
       sd^2 * ( ( x[2] + mean ) * ( d[2] * pstar[2] + ( dstar[2] * p[2] ) / 2 ) -
                ( x[1] + mean ) * ( d[1] * pstar[1] + ( dstar[1] * p[1] ) / 2 ) ) +
       3 * sd / 2 * ( 2 * mean * ( p3star[2] - p3star[1] ) -
                      sd^2 * ( d3star[2] - d3star[1] ) / 3 ) / sqrt( 3 * pi ) -
       sd^2 / 2 * integrate( function( x ) { dnorm( x, mean, sd.star ) * pnorm( x, mean, sd ) }, x[1], x[2] )$value
    ) / sqrt( 2 )
}

integ.R1.plus.R2.sd.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { -x^2 * dnorm( x, mean, sd ) * pnorm( x, mean, sd * sqrt( 2 ) / 2 ) / sqrt( 2 ) -
                               sqrt( 2 * pi ) * sd * x^2 * dnorm( x, mean, sd )^2 * pnorm( x, mean, sd ) }, x[1], x[2] )
}

expect_equal( check.integ( integ.R1.plus.R2.sd, integ.R1.plus.R2.sd.via.integrate ), TRUE )

integ.R1.plus.R2.sd.inf <- function( mean, sd, x )
{
    -( mean^2 + sqrt( 3 / pi ) * mean * sd + 3 * sd^2 / 4 ) * sqrt( 2 ) / 2
}

integ.R1.plus.R2.sd.inf.via.integrate <- function( mean, sd, x )
{
    integ.R1.plus.R2.sd.via.integrate( mean, sd, x )
}

expect_equal( check.integ( integ.R1.plus.R2.sd.inf,
						   integ.R1.plus.R2.sd.inf.via.integrate, inf.to.inf = TRUE ), TRUE )

# R1, R2 for sd end


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} x f_1(x)^2 \Phi^*_2(x) dx}
#' where \eqn{f_1} is the PDF of N(mean.1, sd.1) and
#' \eqn{\Phi^*_2} is the CDF of N(mean.2, sd.2).
#' @param   mean.1  The mean of N(mean.1, sd.1).
#' @param   sd.1    The s.d. of N(mean.1, sd.1).
#' @param   mean.2  The mean of N(mean.2, sd.2).
#' @param   sd.2    The s.d. of N(mean.2, sd.2).
#' @param   x       The interval of \eqn{[x_1, x_2]}}.
#' @return  The result value of calculation.
################################################################################################
integ.x.d.pstar.2 <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    sd.star.2 <- sd.2 * sqrt( 2 ) / 2
    d.1 <- dnorm( x, mean.1, sd.1 )
    p.1 <- pnorm( x, mean.1, sd.1 )
    pstar.2 <- pnorm( x, mean.2, sd.star.2 )

    ( mean.1 * p.1[2] - sd.1^2 * d.1[2] ) * pstar.2[2] -
    ( mean.1 * p.1[1] - sd.1^2 * d.1[1] ) * pstar.2[1] -
    mean.1 *
    integrate( function( x ) { pnorm( x, mean.1, sd.1 ) * dnorm( x, mean.2, sd.star.2 ) }, x[1], x[2] )$value +
    sd.1^2 *
    integrate( function( x ) { dnorm( x, mean.1, sd.1 ) * dnorm( x, mean.2, sd.star.2 ) }, x[1], x[2] )$value
}

integ.x.d.pstar.2.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integrate( function( x ) { x * dnorm( x, mean.1, sd.1 ) * pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) }, x[1], x[2] )
}

expect_equal( check.integ.2( integ.x.d.pstar.2, integ.x.d.pstar.2.via.integrate ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} x^2 f_1(x) \Phi^*_2(x) dx}
#' where \eqn{f_1} is the PDF of N(mean.1, sd.1) and
#' \eqn{\Phi^*_2} is the CDF of N(mean.2, sd.2).
#' @param   mean.1  The mean of N(mean.1, sd.1).
#' @param   sd.1    The s.d. of N(mean.1, sd.1).
#' @param   mean.2  The mean of N(mean.2, sd.2).
#' @param   sd.2    The s.d. of N(mean.2, sd.2).
#' @param   x       The interval of \eqn{[x_1, x_2]}}.
#' @return  The result value of calculation.
################################################################################################
integ.x2.d.pstar.2 <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    sd.star.2 <- sd.2 * sqrt( 2 ) / 2
    d.1 <- dnorm( x, mean.1, sd.1 )
    p.1 <- pnorm( x, mean.1, sd.1 )
    pstar.2 <- pnorm( x, mean.2, sd.star.2 )

    ( ( mean.1^2 + sd.1^2 ) * p.1[2] - sd.1^2 * ( x[2] + mean.1 ) * d.1[2] ) * pstar.2[2] -
    ( ( mean.1^2 + sd.1^2 ) * p.1[1] - sd.1^2 * ( x[1] + mean.1 ) * d.1[1] ) * pstar.2[1] -
    ( mean.1^2 + sd.1^2 ) *
    integrate( function( x ) { pnorm( x, mean.1, sd.1 ) * dnorm( x, mean.2, sd.star.2 ) }, x[1], x[2] )$value +
    sd.1^2 *
    integrate( function( x ) { x * dnorm( x, mean.1, sd.1 ) * dnorm( x, mean.2, sd.star.2 ) }, x[1], x[2] )$value +
    mean.1 * sd.1^2 *
    integrate( function( x ) { dnorm( x, mean.1, sd.1 ) * dnorm( x, mean.2, sd.star.2 ) }, x[1], x[2] )$value
}

integ.x2.d.pstar.2.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integrate( function( x ) { x^2 * dnorm( x, mean.1, sd.1 ) * pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) }, x[1], x[2] )
}

expect_equal( check.integ.2( integ.x2.d.pstar.2, integ.x2.d.pstar.2.via.integrate ), TRUE )


################################################################################################
#' Calculates \eqn{\int_{x_1}^{x_2} x f_1(x) \f^*_2(x) dx}
#' where \eqn{f_1} is the PDF of N(mean.1, sd.1) and
#' \eqn{\f^*_2} is the PDF of N(mean.2, sd.2).
#' @param   mean.1  The mean of N(mean.1, sd.1).
#' @param   sd.1    The s.d. of N(mean.1, sd.1).
#' @param   mean.2  The mean of N(mean.2, sd.2).
#' @param   sd.2    The s.d. of N(mean.2, sd.2).
#' @param   x       The interval of \eqn{[x_1, x_2]}}.
#' @return  The result value of calculation.
################################################################################################
integ.x.d.dstar.2 <- function( mean.1, sd.1, mean.2, sd.2, x )
{
#   sd.star.2 <- sd.2 * sqrt( 2 ) / 2
#   d.1 <- dnorm( x, mean.1, sd.1 )
#   p.1 <- pnorm( x, mean.1, sd.1 )
#   dstar.2 <- dnorm( x, mean.2, sd.star.2 )
#   pstar.2 <- pnorm( x, mean.2, sd.star.2 )
#
#   d.1[2] * ( mean.2 * pstar.2[2] - sd.2^2 / 2 * dstar.2[2] ) -
#   d.1[1] * ( mean.2 * pstar.2[1] - sd.2^2 / 2 * dstar.2[1] ) +
#   mean.2 / sd.1^2 *
#   integrate( function( x ) { x * dnorm( x, mean.1, sd.1 ) * pnorm( x, mean.2, sd.star.2 ) }, x[1], x[2] )$value -
#   mean.1 * mean.2 / sd.1^2 *
#   integrate( function( x ) { dnorm( x, mean.1, sd.1 ) * pnorm( x, mean.2, sd.star.2 ) }, x[1], x[2] )$value -
#   sd.2^2 / ( 2 * sd.1^2 ) *
#   integrate( function( x ) { x * dnorm( x, mean.1, sd.1 ) * dnorm( x, mean.2, sd.star.2 ) }, x[1], x[2] )$value +
#   mean.1 * sd.2^2 / ( 2 * sd.1^2 ) *
#   integrate( function( x ) { dnorm( x, mean.1, sd.1 ) * dnorm( x, mean.2, sd.star.2 ) }, x[1], x[2] )$value
    sd.star.2 <- sd.2 * sqrt( 2 ) / 2
    d.1 <- dnorm( x, mean.1, sd.1 )
    dstar.2 <- dnorm( x, mean.2, sd.star.2 )

    ( ( 2 * mean.2 * sd.1^2 + mean.1 * sd.2^2 ) *
      integrate( function( x ) { dnorm( x, mean.1, sd.1 ) * dnorm( x, mean.2, sd.star.2 ) }, x[1], x[2] )$value -
      sd.1^2 * sd.2^2 * ( d.1[2] * dstar.2[2] - d.1[1] * dstar.2[1] ) ) / ( 2 * sd.1^2 + sd.2^2 )
}

integ.x.d.dstar.2.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integrate( function( x ) { x * dnorm( x, mean.1, sd.1 ) * dnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) }, x[1], x[2] )
}

## The d.range is need for the previous (comment-outed) integ.x.d.dstar.2 definition.
#expect_equal( check.integ.2( integ.x.d.dstar.2, integ.x.d.dstar.2.via.integrate, d.range = 1e-6 ), TRUE )
expect_equal( check.integ.2( integ.x.d.dstar.2, integ.x.d.dstar.2.via.integrate ), TRUE )


## Integral of B1, B2 for t4.sd begin

# B1 for t4.sd
integ.B1.sd <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    sd.star.2 <- sd.2 * sqrt( 2 ) / 2
    d.1 <- dnorm( x, mean.1, sd.1 )
    p.1 <- pnorm( x, mean.1, sd.1 )
    dstar.2 <- dnorm( x, mean.2, sd.star.2 )
    pstar.2 <- pnorm( x, mean.2, sd.star.2 )

    ( ( mean.1^2 + sd.1^2 ) * ( p.1[2] * pstar.2[2] - p.1[1] * pstar.2[1] ) -
       sd.1^2 * ( ( x[2] + mean.1 ) * d.1[2] * pstar.2[2] - ( x[1] + mean.1 ) * d.1[1] * pstar.2[1] ) -
      ( mean.1^2 + sd.1^2 ) *
      integrate( function( x ) { dnorm( x, mean.2, sd.star.2 ) * pnorm( x, mean.1, sd.1 ) },
                 x[1], x[2] )$value +
      sd.1^2 *
      integrate( function( x ) { ( x + mean.1 ) * dnorm( x, mean.1, sd.1 ) * dnorm( x, mean.2, sd.star.2 ) },
                 x[1], x[2] )$value
    ) / sqrt( 2 )
}

integ.B1.sd.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integrate( function( x ) { x^2 * dnorm( x, mean.1, sd.1 ) * pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) / sqrt( 2 ) },
               x[1], x[2] )
}

expect_equal( check.integ.2( integ.B1.sd, integ.B1.sd.via.integrate ), TRUE )


# B2 for t4.sd
integ.B2.sd <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    sd.star.2 <- sd.2 * sqrt( 2 ) / 2
    d.1 <- dnorm( x, mean.1, sd.1 )
    p.1 <- pnorm( x, mean.1, sd.1 )
    dstar.2 <- dnorm( x, mean.2, sd.star.2 )
    pstar.2 <- pnorm( x, mean.2, sd.star.2 )

#   ( ( mean.2^2 + sd.2^2 / 2 ) * ( p.1[2] * pstar.2[2] - p.1[1] * pstar.2[1] ) -
#     sd.2^2 / 2 * ( ( x[2] + mean.2 ) * p.1[2] * dstar.2[2] - ( x[1] + mean.2 ) * p.1[1] * dstar.2[1] ) -
#     integrate( function( x ) { ( ( mean.2^2 + sd.2^2 / 2 ) * pnorm( x, mean.2, sd.star.2 ) -
#                                  sd.2^2 / 2 * ( x + mean.2 ) * dnorm( x, mean.2, sd.star.2 ) ) *
#                                dnorm( x, mean.1, sd.1 ) }, x[1], x[2] )$value ) / sqrt( 2 )
    ( ( mean.2^2 + sd.2^2 / 2 ) * ( p.1[2] * pstar.2[2] - p.1[1] * pstar.2[1] ) -
       sd.2^2 / 2 * ( ( x[2] + mean.2 ) * p.1[2] * dstar.2[2] - ( x[1] + mean.2 ) * p.1[1] * dstar.2[1] ) -
      ( mean.2^2 + sd.2^2 / 2 ) *
      integrate( function( x ) { dnorm( x, mean.1, sd.1 ) * pnorm( x, mean.2, sd.star.2 ) },
                 x[1], x[2] )$value +
      sd.2^2 / 2 *
      integrate( function( x ) { ( x + mean.2 ) * dnorm( x, mean.1, sd.1 ) * dnorm( x, mean.2, sd.star.2 ) },
                 x[1], x[2] )$value
    ) / sqrt( 2 )
}

integ.B2.sd.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integrate( function( x ) { sqrt( 2 * pi ) * sd.2 * x^2 * dnorm( x, mean.2, sd.2 )^2 * pnorm( x, mean.1, sd.1 ) },
               x[1], x[2] )
}

expect_equal( check.integ.2( integ.B2.sd, integ.B2.sd.via.integrate, d.range = 7e-7 ), TRUE )


# B1 + B2 for t4.sd
integ.B1.plus.B2.sd <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    sd.star.2 <- sd.2 * sqrt( 2 ) / 2
    d.1 <- dnorm( x, mean.1, sd.1 )
    p.1 <- pnorm( x, mean.1, sd.1 )
    dstar.2 <- dnorm( x, mean.2, sd.star.2 )
    pstar.2 <- pnorm( x, mean.2, sd.star.2 )

#   ( ( mean.2^2 + sd.2^2 / 2 ) * ( p.1[2] * pstar.2[2] - p.1[1] * pstar.2[1] ) -
#      sd.1^2 * ( ( x[2] + mean.1 ) * d.1[2] * pstar.2[2] - ( x[1] + mean.1 ) * d.1[1] * pstar.2[1] ) -
#      sd.2^2 * ( ( x[2] + mean.2 ) * p.1[2] * dstar.2[2] - ( x[1] + mean.2 ) * p.1[1] * dstar.2[1] ) / 2 +
#     ( mean.1^2 - mean.2^2 + sd.1^2 - sd.2^2 / 2 ) *
#     integrate( function( x ) { dnorm( x, mean.1, sd.1 ) * pnorm( x, mean.2, sd.star.2 ) },
#                 x[1], x[2] )$value +
#     ( sd.1^2 + sd.2^2 / 2 ) *
#     integrate( function( x ) { x * dnorm( x, mean.1, sd.1 ) * dnorm( x, mean.2, sd.star.2 ) },
#                x[1], x[2] )$value +
#     ( mean.1 * sd.1^2 + mean.2 * sd.2^2 / 2 ) *
#     integrate( function( x ) { dnorm( x, mean.1, sd.1 ) * dnorm( x, mean.2, sd.star.2 ) },
#                x[1], x[2] )$value
#   ) / sqrt( 2 )
    sqrt( 2 ) * ( ( 2 * mean.2^2 + sd.2^2 ) * ( p.1[2] * pstar.2[2] - p.1[1] * pstar.2[1] ) -
                  2 * sd.1^2 * ( ( x[2] + mean.1 ) * d.1[2] * pstar.2[2] - ( x[1] + mean.1 ) * d.1[1] * pstar.2[1] ) -
                  sd.2^2 * ( ( x[2] + mean.2 ) * p.1[2] * dstar.2[2] - ( x[1] + mean.2 ) * p.1[1] * dstar.2[1] ) -
                  sd.1^2 * sd.2^2 * ( d.1[2] * dstar.2[2] - d.1[1] * dstar.2[1] ) +
                  ( 2 * ( mean.1^2 - mean.2^2 + sd.1^2 ) - sd.2^2 ) *
                  integrate( function( x ) { dnorm( x, mean.1, sd.1 ) * pnorm( x, mean.2, sd.star.2 ) },
                             x[1], x[2] )$value +
                  ( mean.1 + mean.2 ) * ( 2 * sd.1^2 + sd.2^2 ) *
                  integrate( function( x ) { dnorm( x, mean.1, sd.1 ) * dnorm( x, mean.2, sd.star.2 ) },
                             x[1], x[2] )$value
                ) / 4
}

integ.B1.plus.B2.sd.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integrate( function( x ) { x^2 * dnorm( x, mean.1, sd.1 ) * pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) / sqrt( 2 ) +
                               sqrt( 2 * pi ) * sd.2 * x^2 * dnorm( x, mean.2, sd.2 )^2 * pnorm( x, mean.1, sd.1 ) },
               x[1], x[2] )
}

expect_equal( check.integ.2( integ.B1.plus.B2.sd, integ.B1.plus.B2.sd.via.integrate, d.range = 5e-7 ), TRUE )


integ.B1.plus.B2.sd.inf <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    sqrt( 2 ) * ( 2 * mean.2^2 + sd.2^2 +
                  ( 2 * ( mean.1^2 - mean.2^2 + sd.1^2 ) - sd.2^2 ) *
                  pnorm( ( mean.1 - mean.2 ) / sqrt( sd.1^2 + sd.2^2 / 2 ), 0, 1 ) +
                  ( mean.1 + mean.2 ) * sqrt( ( 2 * sd.1^2 + sd.2^2 ) / pi ) *
                  exp( -( mean.1 - mean.2 )^2 / ( 2 * sd.1^2 + sd.2^2 ) )
                ) / 4
}

integ.B1.plus.B2.sd.inf.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integ.B1.plus.B2.sd.via.integrate( mean.1, sd.1, mean.2, sd.2, x )
}

expect_equal( check.integ.2( integ.B1.plus.B2.sd.inf,
							 integ.B1.plus.B2.sd.inf.via.integrate, inf.to.inf = TRUE, d.range = 1e-6 ), TRUE )

# B1, B2 for sd end


integ.x.dstar.dstar.2 <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    sd.star.1 <- sd.1 * sqrt( 2 ) / 2
    sd.star.2 <- sd.2 * sqrt( 2 ) / 2
    dstar.1 <- dnorm( x, mean.1, sd.star.1 )
    dstar.2 <- dnorm( x, mean.2, sd.star.2 )

    ( ( mean.2 * sd.1^2 + mean.1 * sd.2^2 ) *
      integrate( function( x ) { dnorm( x, mean.1, sd.star.1 ) * dnorm( x, mean.2, sd.star.2 ) }, x[1], x[2] )$value -
      sd.1^2 * sd.2^2 * ( dstar.1[2] * dstar.2[2] - dstar.1[1] * dstar.2[1] ) / 2 ) / ( sd.1^2 + sd.2^2 )
}

integ.x.dstar.dstar.2.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integrate( function( x ) { x * dnorm( x, mean.1, sd.1 * sqrt( 2 ) / 2 ) * dnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) },
                x[1], x[2] )
}

expect_equal( check.integ.2( integ.x.dstar.dstar.2, integ.x.dstar.dstar.2.via.integrate ), TRUE )


## Integral of G1, G2 for t4.sd begin

# G1 for t4.sd
integ.G1.sd <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    sd.star.1 <- sd.1 * sqrt( 2 ) / 2
    sd.star.2 <- sd.2 * sqrt( 2 ) / 2
    dstar.1 <- dnorm( x, mean.1, sd.star.1 )
    pstar.1 <- pnorm( x, mean.1, sd.star.1 )
    dstar.2 <- dnorm( x, mean.2, sd.star.2 )
    pstar.2 <- pnorm( x, mean.2, sd.star.2 )

    ( ( sd.1^2 * ( ( x[2] + mean.1 ) * dstar.1[2] * pstar.2[2] - ( x[1] + mean.1 ) * dstar.1[1] * pstar.2[1] ) +
        sd.1^4 * sd.2^2 / ( sd.1^2 + sd.2^2 ) / 2 * ( dstar.1[2] * dstar.2[2] - dstar.1[1] * dstar.2[1] ) ) -
      ( 2 * mean.1^2 + sd.1^2 ) *
      integrate( function( x ) { dnorm( x, mean.1, sd.star.1 ) * pnorm( x, mean.2, sd.star.2 ) },
                 x[1], x[2] )$value -
      sd.1^2 * ( ( mean.1 + mean.2 ) * sd.1^2 + 2 * mean.1 * sd.2^2 ) / ( sd.1^2 + sd.2^2 ) *
      integrate( function( x ) { dnorm( x, mean.1, sd.star.1 ) * dnorm( x, mean.2, sd.star.2 ) },
                 x[1], x[2] )$value

    ) / 4
}

integ.G1.sd.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integrate( function( x ) { -sqrt( pi ) * sd.1 *
                               x^2 * dnorm( x, mean.1, sd.1 )^2 * pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) },
               x[1], x[2] )
}

expect_equal( check.integ.2( integ.G1.sd, integ.G1.sd.via.integrate ), TRUE )


# G2 for t4.sd
integ.G2.sd <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    sd.star.1 <- sd.1 * sqrt( 2 ) / 2
    sd.star.2 <- sd.2 * sqrt( 2 ) / 2
    dstar.1 <- dnorm( x, mean.1, sd.star.1 )
    pstar.1 <- pnorm( x, mean.1, sd.star.1 )
    dstar.2 <- dnorm( x, mean.2, sd.star.2 )
    pstar.2 <- pnorm( x, mean.2, sd.star.2 )

    ( -( ( 2 * mean.2^2 + sd.2^2 ) * ( pstar.1[2] * pstar.2[2] - pstar.1[1] * pstar.2[1] ) -
         sd.2^2 * ( ( x[2] + mean.2 ) * dstar.2[2] * pstar.1[2] - ( x[1] + mean.2 ) * dstar.2[1] * pstar.1[1] ) -
         sd.1^2 * sd.2^4 / ( sd.1^2 + sd.2^2 ) / 2 * ( dstar.1[2] * dstar.2[2] - dstar.1[1] * dstar.2[1] ) ) +
        ( 2 * mean.2^2 + sd.2^2 ) *
        integrate( function( x ) { dnorm( x, mean.1, sd.star.1 ) * pnorm( x, mean.2, sd.star.2 ) },
                   x[1], x[2] )$value -
        sd.2^2 * ( 2 * mean.2 * sd.1^2 + ( mean.1 + mean.2 ) * sd.2^2 ) / ( sd.1^2 + sd.2^2 ) *
        integrate( function( x ) { dnorm( x, mean.1, sd.star.1 ) * dnorm( x, mean.2, sd.star.2 ) },
                   x[1], x[2] )$value

    ) / 4
}

integ.G2.sd.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integrate( function( x ) { -sqrt( pi ) * sd.2 *
                               x^2 * dnorm( x, mean.2, sd.2 )^2 * pnorm( x, mean.1, sd.1 * sqrt( 2 ) / 2 ) },
               x[1], x[2] )
}

expect_equal( check.integ.2( integ.G2.sd, integ.G2.sd.via.integrate ), TRUE )


# G1 + G2 for t4.sd
integ.G1.plus.G2.sd <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    sd.star.1 <- sd.1 * sqrt( 2 ) / 2
    sd.star.2 <- sd.2 * sqrt( 2 ) / 2
    dstar.1 <- dnorm( x, mean.1, sd.star.1 )
    pstar.1 <- pnorm( x, mean.1, sd.star.1 )
    dstar.2 <- dnorm( x, mean.2, sd.star.2 )
    pstar.2 <- pnorm( x, mean.2, sd.star.2 )

    ( -( 2 * mean.2^2 + sd.2^2 ) * ( pstar.1[2] * pstar.2[2] - pstar.1[1] * pstar.2[1] ) +
      sd.1^2 * ( ( x[2] + mean.1 ) * dstar.1[2] * pstar.2[2] - ( x[1] + mean.1 ) * dstar.1[1] * pstar.2[1] ) +
      sd.2^2 * ( ( x[2] + mean.2 ) * dstar.2[2] * pstar.1[2] - ( x[1] + mean.2 ) * dstar.2[1] * pstar.1[1] ) +
      sd.1^2 * sd.2^2 * ( dstar.1[2] * dstar.2[2] - dstar.1[1] * dstar.2[1] ) / 2 -
      ( 2 * ( mean.1^2 - mean.2^2 ) + sd.1^2 - sd.2^2 ) *
      integrate( function( x ) { dnorm( x, mean.1, sd.star.1 ) * pnorm( x, mean.2, sd.star.2 ) }, x[1], x[2] )$value -
      ( mean.1 + mean.2 ) * ( sd.1^2 + sd.2^2 ) *
      integrate( function( x ) { dnorm( x, mean.1, sd.star.1 ) * dnorm( x, mean.2, sd.star.2 ) }, x[1], x[2] )$value
    ) / 4
}

integ.G1.plus.G2.sd.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integrate( function( x ) { -sqrt( pi ) * sd.1 *
                               x^2 * dnorm( x, mean.1, sd.1 )^2 * pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) -
                               sqrt( pi ) * sd.2 *
                               x^2 * dnorm( x, mean.2, sd.2 )^2 * pnorm( x, mean.1, sd.1 * sqrt( 2 ) / 2 ) },
               x[1], x[2] )
}

expect_equal( check.integ.2( integ.G1.plus.G2.sd, integ.G1.plus.G2.sd.via.integrate ), TRUE )


integ.G1.plus.G2.sd.inf <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    ( -( 2 * mean.2^2 + sd.2^2 ) -
      ( 2 * ( mean.1^2 - mean.2^2 ) + sd.1^2 - sd.2^2 ) *
      pnorm( sqrt( 2 ) * ( mean.1 - mean.2 ) / sqrt( sd.1^2 + sd.2^2 ), 0, 1 ) -
      ( mean.1 + mean.2 ) * sqrt( sd.1^2 + sd.2^2 ) *
      exp( -( mean.1 - mean.2 )^2 / ( sd.1^2 + sd.2^2 ) ) / sqrt( pi )
    ) / 4
}

integ.G1.plus.G2.sd.inf.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integ.G1.plus.G2.sd.via.integrate( mean.1, sd.1, mean.2, sd.2, x )
}

expect_equal( check.integ.2( integ.G1.plus.G2.sd.inf,
							 integ.G1.plus.G2.sd.inf.via.integrate, inf.to.inf = TRUE ), TRUE )

# G1, G2 for sd end


integ.t4.x2.psi.g.inf <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    d.mean <- mean.1 - mean.2

#   ( mean.1^2 + sd.1^2 ) / 2 + mean.1 * sd.1 / sqrt( pi ) +
#   integ.sqrtpisd2.x2.d2.pstar.inf( mean.1, sd.1, x ) +
#   integ.sqrtpisd2.x2.d2.pstar.inf( mean.2, sd.2, x ) +
#   integ.R1.plus.R2.sd.inf( mean.1, sd.1, x ) +
#   integ.B1.plus.B2.sd.inf( mean.1, sd.1, mean.2, sd.2, x ) +
#   integ.G1.plus.G2.sd.inf( mean.1, sd.1, mean.2, sd.2, x )
    ( ( 6 - 4 * sqrt( 2 ) ) * mean.1^2 + ( 5 - 3 * sqrt( 2 ) ) * sd.1^2 +
      ( 4 * sqrt( 2 ) - 2 ) * mean.2^2 + ( 2 * sqrt( 2 ) - 1 ) * sd.2^2 ) / 8 +
    ( ( 4 + sqrt( 2 ) - 2 * sqrt( 6 ) ) * mean.1 * sd.1 +
      sqrt( 2 ) * mean.2 * sd.2 ) / sqrt( pi ) / 4 +
    ( sqrt( 2 ) * ( 2 * ( mean.1^2 - mean.2^2 + sd.1^2 ) - sd.2^2 ) *
      pnorm( d.mean / sqrt( sd.1^2 + sd.2^2 / 2 ), 0, 1 ) -
      ( 2 * ( mean.1^2 - mean.2^2 ) + sd.1^2 - sd.2^2 ) *
      pnorm( sqrt( 2 ) * d.mean / sqrt( sd.1^2 + sd.2^2 ), 0, 1 ) ) / 4 +
    ( mean.1 + mean.2 ) *
    ( sqrt( 4 * sd.1^2 + 2 * sd.2^2 ) *
      exp( -d.mean^2 / ( 2 * sd.1^2 + sd.2^2 ) ) -
      sqrt( sd.1^2 + sd.2^2 ) *
      exp( -d.mean^2 / ( sd.1^2 + sd.2^2 ) ) ) / sqrt( pi ) / 4
}

integ.t4.x2.psi.g.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integrate( function( x )
                {
                    x^2 * ( dnorm( x, mean.1, sd.1 ) / sqrt( 2 ) -
                            sqrt( pi ) * ( sd.1 * dnorm( x, mean.1, sd.1 )^2 -
                                           sd.2 * dnorm( x, mean.2, sd.2 )^2 ) ) *
                    ( sqrt( 2 ) * pnorm( x, mean.1, sd.1 ) -
                      pnorm( x, mean.1, sd.1 * sqrt( 2 ) / 2 ) +
                      pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) )
                }, x[1], x[2] )
}

integ.t4.x2.psi.g.inf.via.integrate <- function( mean.1, sd.1, mean.2, sd.2, x )
{
    integ.t4.x2.psi.g.via.integrate( mean.1, sd.1, mean.2, sd.2, x )
}

expect_equal( check.integ.2( integ.t4.x2.psi.g.inf,
							 integ.t4.x2.psi.g.inf.via.integrate, inf.to.inf = TRUE, d.range = 2e-3 ), TRUE )


################################
# functions in the library
################################

mean.nd.bound.via.integrate <- function( mean, sd, x )
{
    integrate( function( x ) { x * dnorm( x, mean, sd ) }, x[1], x[2] )
}

expect_equal( check.integ( cgd:::mean.nd.bound, mean.nd.bound.via.integrate ), TRUE )


mean.conn.bound.via.integrate <- function( type1.type, median, sd.1, sd.2, x )
{
    mean.1 <- mean.2 <- median

    if ( type1.type == 1 )
    {
        upper <- x[2]
        lower <- x[1]

        f <- function( x )
        {
            x * (   ( upper - x ) * dnorm( x, mean.1, sd.1 ) +
                    ( x - lower ) * dnorm( x, mean.2, sd.2 ) +
                    pnorm( x, mean.2, sd.2 ) - pnorm( x, mean.1, sd.1 )
                ) / ( upper - lower )
        }
    }
    else if ( type1.type == 2 )
    {
        p.1 <- pnorm( x, mean.1, sd.1 )
        p.2 <- pnorm( x, mean.2, sd.2 )
        p.sum <- p.1 + p.2

        f <- function( x )
        {
            x * (   ( p.sum[2] - 2 * pnorm( x, mean.1, sd.1 ) ) * dnorm( x, mean.1, sd.1 ) +
                    ( 2 * pnorm( x, mean.2, sd.2 ) - p.sum[1] ) * dnorm( x, mean.2, sd.2 )
                ) / ( p.sum[2] - p.sum[1] )
        }
    }

    integrate( f, x[1], x[2] )
}

expect_equal( check.integ.2( function( mean.1, sd.1, mean.2, sd.2, x )
                { cgd:::mean.conn.bound( 1, mean.1, sd.1, sd.2, x ) },
                function( mean.1, sd.1, mean.2, sd.2, x )
                { mean.conn.bound.via.integrate( 1, mean.1, sd.1, sd.2, x ) } ), TRUE )

expect_equal( check.integ.2( function( mean.1, sd.1, mean.2, sd.2, x )
                { cgd:::mean.conn.bound( 2, mean.1, sd.1, sd.2, x ) },
                function( mean.1, sd.1, mean.2, sd.2, x )
                { mean.conn.bound.via.integrate( 2, mean.1, sd.1, sd.2, x ) },
                target.cond = function( mean.1, sd.1, mean.2, sd.2, x )
                {
                    pnorm( x[2], mean.1, sd.1 ) < 1 && pnorm( x[2], mean.2, sd.2 ) < 1 &&
                    pnorm( x[2], mean.1, sd.1 / sqrt( 2 ) ) < 1 && pnorm( x[2], mean.2, sd.2 / sqrt( 2 ) ) < 1
                } ), TRUE )


mean.cont.via.integrate <- function( type1.type, mean.1, sd.1, mean.2, sd.2,
                                    mean.3 = mean.1, sd.3 = sd.1, mean.4 = mean.4, sd.4 = sd.4 )
{
    if ( type1.type == 2 )
    {
        f <- function( x )
        {
            x * ( ( 1 - pnorm( x, mean.1, sd.1 ) ) * dnorm( x, mean.1, sd.1 ) +
                  pnorm( x, mean.2, sd.2 ) * dnorm( x, mean.2, sd.2 ) )
        }

        result <- integrate( f, -Inf, Inf )
    }
    else if ( type1.type == 3 )
    {
        if ( mean.1 == mean.3 && sd.1 == sd.3 )
        {
            f <- function( x )
            {
                x * ( ( 1 - dnorm( x, mean.1, sd.1 ) / dnorm( mean.1, mean.1, sd.1 ) ) * dnorm( x, mean.1, sd.1 ) +
                      dnorm( x, mean.2, sd.2 )^2 / dnorm( mean.2, mean.2, sd.2 ) )
            }

            result <- integrate( f, -Inf, Inf )
        }
        else
        {
            result = list( value = numeric(), abs.error = numeric() )

            int.sub <- integrate( function( x )
                                    {
                                        x * ( 1 - dnorm( x, mean.1, sd.1 ) / dnorm( mean.1, mean.1, sd.1 ) ) *
                                            dnorm( x, mean.1, sd.1 )
                                    }, -Inf, mean.1 )

            result$value <- int.sub$value
            result$abs.error <- int.sub$abs.error

            int.sub <- integrate( function( x )
                                    {
                                        x * dnorm( x, mean.2, sd.2 )^2 / dnorm( mean.2, mean.2, sd.2 )
                                    }, -Inf, Inf )

            result$value <- result$value + int.sub$value
            result$abs.error <- result$abs.error + int.sub$abs.error

            int.sub <- integrate( function( x )
                                    {
                                        x * ( 1 - dnorm( x, mean.3, sd.3 ) / dnorm( mean.3, mean.3, sd.3 ) ) *
                                            dnorm( x, mean.3, sd.3 )
                                    }, mean.3, Inf )

            result$value <- result$value + int.sub$value
            result$abs.error <- result$abs.error + int.sub$abs.error
        }
    }
    else if ( type1.type == 4 )
    {
        f <- function( x )
        {
            x * ( ( 1 - ( pnorm( x, mean.1, sd.1 ) -
                          pnorm( x, mean.1, sd.1 * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2 +
                          pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2 ) ) *
                  ( ( 1 - dnorm( x, mean.1, sd.1 ) / dnorm( mean.1, mean.1, sd.1 ) ) * dnorm( x, mean.1, sd.1 ) +
                    dnorm( x, mean.2, sd.2 )^2 / dnorm( mean.2, mean.2, sd.2 ) ) +
                  ( pnorm( x, mean.3, sd.3 ) -
                    pnorm( x, mean.3, sd.3 * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2 +
                    pnorm( x, mean.4, sd.4 * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2 ) *
                  ( ( 1 - dnorm( x, mean.3, sd.3 ) / dnorm( mean.3, mean.3, sd.3 ) ) * dnorm( x, mean.3, sd.3 ) +
                    dnorm( x, mean.4, sd.4 )^2 / dnorm( mean.4, mean.4, sd.4 ) ) )
        }

        result <- integrate( f, -Inf, Inf )
    }

    return( result )
}

expect_equal( check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                { cgd:::mean.cont( 2, c( mean.1, mean.2 ), c( sd.1, sd.2 ) ) },
                function( mean.1, sd.1, mean.2, sd.2, x )
                { mean.cont.via.integrate( 2, mean.1, sd.1, mean.2, sd.2 ) },
                inf.to.inf = TRUE, d.range = 1.65e-3 ), TRUE )

expect_equal( check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                { cgd:::mean.cont( 3, c( mean.1, mean.2, mean.1 ), c( sd.1, sd.2, sd.1 ) ) },
                function( mean.1, sd.1, mean.2, sd.2, x )
                { mean.cont.via.integrate( 3, mean.1, sd.1, mean.2, sd.2 ) },
                inf.to.inf = TRUE ), TRUE )

## If some error have occurred in check.integ.2 for type1.type = 3, use this slower version.
#for ( mean.3 in means[abs( means ) < 3] )
#{
#   for ( sd.3 in sds[sds > 0.5] )
#   {
#       print( paste( "mean.3:", mean.3 ) )
#       print( paste( "sd.3:", sd.3 ) )
#       e <- try( check.integ.2(
#                       function( mean.1, sd.1, mean.2, sd.2, x )
#                       { mean.cont( 3, c( mean.1, mean.2, mean.3 ), c( sd.1, sd.2, sd.3 ) ) },
#                       function( mean.1, sd.1, mean.2, sd.2, x )
#                       { mean.cont.via.integrate( 3, mean.1, sd.1, mean.2, sd.2, mean.3, sd.3 ) },
#                       inf.to.inf = TRUE,
#                       target.cond = function( mean.1, sd.1, mean.2, sd.2, x )
#                                       {
#                                           ( sd.1 > 0.5 && sd.3 > 0.5 &&
#                                             abs( mean.1 ) < 3 && abs( mean.3 ) < 3 ) ||
#                                           ( sd.1 > 1 && sd.3 > 1 &&
#                                             abs( mean.1 - mean.3 ) < 2.799 )
#                                       },
#                       permit.d = function( d, abs.error, info, parent.info )
#                                       { abs( d ) < 1 && abs( d ) < abs.error * 256 } ) )
#       if ( inherits( e, "try-error" ) )
#       {
#           print( paste( "mean.3:", mean.3 ) )
#           print( paste( "sd.3:", sd.3 ) )
#           stop()
#       }
#   }
#}
## If no error, you can use this faster version instead of above "for loop" version.
expect_equal( all( vapply( means,
            function( mean.3 )
            {
                vapply( sds[sds > 0.5],
                        function( sd.3 )
                        {
                            print( paste( "mean.3:", mean.3 ) )
                            print( paste( "sd.3:", sd.3 ) )
                            check.integ.2(
                                function( mean.1, sd.1, mean.2, sd.2, x )
                                { cgd:::mean.cont( 3, c( mean.1, mean.2, mean.3 ), c( sd.1, sd.2, sd.3 ) ) },
                                function( mean.1, sd.1, mean.2, sd.2, x )
                                { mean.cont.via.integrate( 3, mean.1, sd.1, mean.2, sd.2, mean.3, sd.3 ) },
                                inf.to.inf = TRUE,
                                target.cond = function( mean.1, sd.1, mean.2, sd.2, x )
                                                {
                                                    ( sd.1 > 0.5 &&
                                                      abs( mean.1 ) < 3 && abs( mean.3 ) < 3 ) ||
                                                    ( sd.1 > 1 && sd.3 > 1 &&
                                                      abs( mean.1 - mean.3 ) < 2.799 )
                                                },
                                permit.d = function( d, abs.error, info, parent.info )
                                                { abs( d ) < 1 && abs( d ) < abs.error * 256 } )
                        }, TRUE )
            }, rep( TRUE, length( sds[sds > 0.5] ) ) ) ), TRUE )

## permit.d of this test function is to avoid singulality points
## where the value of the integrate suddenly changes than before or after.
expect_equal( all( vapply( c( 0, 1.5, -0.75 ),
        function( mean.4 )
        vapply( c( 1, 0.8, 1.21 ),
                function( sd.4 )
                vapply( c( 0, 1.5, -0.75 ),
                        function( mean.1 )
                        vapply( c( 1, 0.8, 1.21 ),
                                function( sd.1 )
                                {
                                    print( paste( "mean.1:", mean.1 ) )
                                    print( paste( "sd.1:", sd.1 ) )
                                    print( paste( "mean.4:", mean.4 ) )
                                    print( paste( "sd.4:", sd.4 ) )
                                    check.integ.2(
                                        function( mean.2, sd.2, mean.3, sd.3, x )
                                        { cgd:::mean.cont( 4, c( mean.1, mean.2, mean.3, mean.4 ),
                                                              c( sd.1, sd.2, sd.3, sd.4 ) ) },
                                        function( mean.2, sd.2, mean.3, sd.3, x )
                                        { mean.cont.via.integrate( 4, mean.1, sd.1, mean.2, sd.2,
                                                                      mean.3, sd.3, mean.4, sd.4 ) },
                                        inf.to.inf = TRUE,
                                        permit.d = function( d, abs.error, info, parent.info )
                                                    {
                                                        if ( abs( d ) < 1 && abs( d ) < abs.error * 64 )
                                                        {
                                                            TRUE
                                                        }
                                                        else
                                                        {
                                                            # Avoid singular point of integrate function.
                                                            if ( parent.info$pass == 1 )
                                                            {
                                                                mean.2  <- parent.info$mean - 0.05
                                                                sd.2    <- parent.info$sd
                                                                mean.3  <- info$mean - 0.05
                                                                sd.3    <- info$sd
                                                            }
                                                            else
                                                            {
                                                                mean.2  <- info$mean - 0.05
                                                                sd.2    <- info$sd
                                                                mean.3  <- parent.info$mean - 0.05
                                                                sd.3    <- parent.info$sd
                                                            }

                                                            abs( cgd:::mean.cont( 4,
                                                                        c( mean.1, mean.2, mean.3, mean.4 ),
                                                                        c( sd.1, sd.2, sd.3, sd.4 ) ) -
                                                                 mean.cont.via.integrate( 4,
                                                                        mean.1, sd.1, mean.2, sd.2,
                                                                        mean.3, sd.3, mean.4, sd.4 )$value ) < abs.error * 64
                                                        }
                                                    } )
                                }, TRUE ), rep( TRUE, 3 ) ), rep( TRUE, 9 ) ), rep( TRUE, 27 ) ) ), TRUE )


v.conn.bound.via.integrate <- function( type1.type, mean, median, sd.1, sd.2, q, x = q )
{
    mean.1 <- mean.2 <- median

    if ( type1.type == 1 )
    {
        if ( ( q[2] - q[1] ) == 0 )
        {
            f <- function( x ) { x - x }

            cat( "\n" )
            print( "f is 0" )
            print( paste( "q[1]:", q[1] ) )
            print( paste( "q[2]:", q[2] ) )
        }
        else
        {
            f <- function( x )
            {
                ( x - mean )^2 *
                ( ( q[2] - x ) * dnorm( x, mean.1, sd.1 ) +
                  ( x - q[1] ) * dnorm( x, mean.2, sd.2 ) +
                  pnorm( x, mean.2, sd.2 ) - pnorm( x, mean.1, sd.1 )
                ) / ( q[2] - q[1] )
            }
        }
    }
    else if ( type1.type == 2 )
    {
        p.1 <- pnorm( q, mean.1, sd.1 )
        p.2 <- pnorm( q, mean.2, sd.2 )
        p.sum <- p.1 + p.2

        if ( ( p.sum[2] - p.sum[1] ) == 0 )
        {
            f <- function( x ) { x - x }

            cat( "\n" )
            print( "f is 0" )
            print( paste( "mean.1:", mean.1 ) )
            print( paste( "sd.1:", sd.1 ) )
            print( paste( "mean.2:", mean.2 ) )
            print( paste( "sd.2:", sd.2 ) )
            print( paste( "q[1]:", q[1] ) )
            print( paste( "q[2]:", q[2] ) )
        }
        else
        {
            f <- function( x )
            {
                ( x - mean )^2 *
                ( ( p.sum[2] - 2 * pnorm( x, mean.1, sd.1 ) ) * dnorm( x, mean.1, sd.1 ) +
                  ( 2 * pnorm( x, mean.2, sd.2 ) - p.sum[1] ) * dnorm( x, mean.2, sd.2 )
                ) / ( p.sum[2] - p.sum[1] )
            }
        }
    }

    integrate( f, x[1], x[2] )
}

expect_equal( all( vapply( c( 0, 0.75, -0.3, 2.1, -3.4 ),
        function( mean )
        all( c(
            check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                            { cgd:::v.conn.bound( 1, mean, mean.1, sd.1, sd.2, x ) },
                            function( mean.1, sd.1, mean.2, sd.2, x )
                            { v.conn.bound.via.integrate( 1, mean, mean.1, sd.1, sd.2, x ) } ),
            check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                            {
                                q <- c( x[1], x[2] + 5 )
                                cgd:::v.conn.bound( 1, mean, mean.1, sd.1, sd.2, q, x )
                            },
                            function( mean.1, sd.1, mean.2, sd.2, x )
                            {
                                q <- c( x[1], x[2] + 5 )
                                v.conn.bound.via.integrate( 1, mean, mean.1, sd.1, sd.2, q, x )
                            } ),
            check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                            {
                                q <- c( x[1] - 3, x[2] )
                                cgd:::v.conn.bound( 1, mean, mean.1, sd.1, sd.2, q, x )
                            },
                            function( mean.1, sd.1, mean.2, sd.2, x )
                            {
                                q <- c( x[1] - 3, x[2] )
                                v.conn.bound.via.integrate( 1, mean, mean.1, sd.1, sd.2, q, x )
                            } ),
            check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                            {
                                q <- c( x[1] - 6.3, x[2] + 7.2 )
                                cgd:::v.conn.bound( 1, mean, mean.1, sd.1, sd.2, q, x )
                            },
                            function( mean.1, sd.1, mean.2, sd.2, x )
                            {
                                q <- c( x[1] - 6.3, x[2] + 7.2 )
                                v.conn.bound.via.integrate( 1, mean, mean.1, sd.1, sd.2, q, x )
                            } )
        ) ), TRUE ) ), TRUE )

expect_equal( all( vapply( c( 0, 0.75, -0.3, 2.1, -3.4 ),
        function( mean )
        all( c(
            check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                            { cgd:::v.conn.bound( 2, mean, mean.1, sd.1, sd.2, x ) },
                            function( mean.1, sd.1, mean.2, sd.2, x )
                            { v.conn.bound.via.integrate( 2, mean, mean.1, sd.1, sd.2, x ) } ),
            check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                            {
                                q <- c( x[1], x[2] + 5 )
                                cgd:::v.conn.bound( 2, mean, mean.1, sd.1, sd.2, q, x )
                            },
                            function( mean.1, sd.1, mean.2, sd.2, x )
                            {
                                q <- c( x[1], x[2] + 5 )
                                v.conn.bound.via.integrate( 2, mean, mean.1, sd.1, sd.2, q, x )
                            } ),
            check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                            {
                                q <- c( x[1] - 3, x[2] )
                                cgd:::v.conn.bound( 2, mean, mean.1, sd.1, sd.2, q, x )
                            },
                            function( mean.1, sd.1, mean.2, sd.2, x )
                            {
                                q <- c( x[1] - 3, x[2] )
                                v.conn.bound.via.integrate( 2, mean, mean.1, sd.1, sd.2, q, x )
                            } ),
            check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                            {
                                q <- c( x[1] - 6.3, x[2] + 7.2 )
                                cgd:::v.conn.bound( 2, mean, mean.1, sd.1, sd.2, q, x )
                            },
                            function( mean.1, sd.1, mean.2, sd.2, x )
                            {
                                q <- c( x[1] - 6.3, x[2] + 7.2 )
                                v.conn.bound.via.integrate( 2, mean, mean.1, sd.1, sd.2, q, x )
                            } )
        ) ), TRUE ) ), TRUE )


v.cont.via.integrate <- function( type1.type, mean.1, sd.1, mean.2, sd.2,
                                    mean.3 = mean.1, sd.3 = sd.1, mean.4 = mean.2, sd.4 = sd.2,
                                    symmetric = FALSE, get.lv = FALSE, get.uv = FALSE )
{
	if ( type1.type == 2 && symmetric )
	{
		if ( mean.1 != mean.2 )
		{
			stop( "Error: mean.1 and mean.2 must be equal where type1.type == 2 and symmetric = TRUE." )
		}

		mean <- mean.1
	}
	else
	{
	    mean <- cgd:::mean.cont( type1.type, c( mean.1, mean.2, mean.3, mean.4 ), c( sd.1, sd.2, sd.3, sd.4 ) )
	}

    if ( type1.type == 2 )
    {
    	if ( symmetric )
    	{
	        f <- function( x )
	        {
	            ( x - mean )^2 *
	            ( ( 1 - 2 * pnorm( x, mean, sd.1 ) ) * dnorm( x, mean, sd.1 ) +
	              2 * pnorm( x, mean, sd.2 ) * dnorm( x, mean, sd.2 ) )
	        }

	        if ( get.lv || get.uv )
	        {
	            result <- integrate( f, -Inf, mean )
	        }
	        else
	        {
	            result <- integrate( function( x ) { f( x ) * 2 }, -Inf, mean )
	        }
    	}
    	else
    	{
	        f <- function( x )
	        {
	            ( x - mean )^2 *
	            ( ( 1 - pnorm( x, mean.1, sd.1 ) ) * dnorm( x, mean.1, sd.1 ) +
	              pnorm( x, mean.2, sd.2 ) * dnorm( x, mean.2, sd.2 ) )
	        }

	        if ( get.lv )
	        {
	            result <- integrate( f, -Inf, mean )
	        }
	        else if ( get.uv )
	        {
	            result <- integrate( f, mean, Inf )
	        }
	        else
	        {
	            result <- integrate( f, -Inf, Inf )
	        }
	   	}
    }
    else if ( type1.type == 3 )
    {
        if ( mean.1 == mean.3 && sd.1 == sd.3 )
        {
            f <- function( x )
            {
                ( x - mean )^2 *
                ( ( 1 - dnorm( x, mean.1, sd.1 ) / dnorm( mean.1, mean.1, sd.1 ) ) * dnorm( x, mean.1, sd.1 ) +
                  dnorm( x, mean.2, sd.2 )^2 / dnorm( mean.2, mean.2, sd.2 ) )
            }

            if ( get.lv )
            {
                result <- integrate( f, -Inf, mean )
            }
            else if ( get.uv )
            {
                result <- integrate( f, mean, Inf )
            }
            else
            {
                result <- integrate( f, -Inf, Inf )
            }
        }
        else
        {
            result = list( value = numeric(), abs.error = numeric() )

            f <- function( x )
                    {
                        ( x - mean )^2 *
                        dnorm( x, mean.2, sd.2 )^2 / dnorm( mean.2, mean.2, sd.2 )
                    }

            if ( get.lv )
            {
                int.sub <- integrate( f, -Inf, mean )
            }
            else if ( get.uv )
            {
                int.sub <- integrate( f, mean, Inf )
            }
            else
            {
                int.sub <- integrate( f, -Inf, Inf )
            }

            result$value <- int.sub$value
            result$abs.error <- int.sub$abs.error

            f <- function( x )
                    {
                        ( x - mean )^2 *
                        ( 1 - dnorm( x, mean.1, sd.1 ) / dnorm( mean.1, mean.1, sd.1 ) ) *
                        dnorm( x, mean.1, sd.1 )
                    }

            if ( get.lv )
            {
                int.sub <- integrate( f, -Inf, min( mean, mean.1 ) )
            }
            else if ( get.uv )
            {
                if ( mean < mean.1 )
                {
                    int.sub <- integrate( f, mean, mean.1 )
                }
                else
                {
                    int.sub <- list( value = 0, abs.error = 0 )
                }
            }
            else
            {
                int.sub <- integrate( f, -Inf, mean.1 )
            }

            result$value <- result$value + int.sub$value
            result$abs.error <- result$abs.error + int.sub$abs.error

            f <- function( x )
                    {
                        ( x - mean )^2 *
                        ( 1 - dnorm( x, mean.3, sd.3 ) / dnorm( mean.3, mean.3, sd.3 ) ) *
                        dnorm( x, mean.3, sd.3 )
                    }

            if ( get.lv )
            {
                if ( mean.3 < mean )
                {
                    int.sub <- integrate( f, mean.3, mean )
                }
                else
                {
                    int.sub <- list( value = 0, abs.error = 0 )
                }
            }
            else if ( get.uv )
            {
                int.sub <- integrate( f, max( mean, mean.3 ), Inf )
            }
            else
            {
                int.sub <- integrate( f, mean.3, Inf )
            }

            result$value <- result$value + int.sub$value
            result$abs.error <- result$abs.error + int.sub$abs.error
        }
    }
    #else if ( type1.type == 4 )
    #{
    ##	For type1.type == 4, use cdg:::v.cont.t4.via.integrate instead of this fuction.
    #}

    return( result )
}


expect_equal( check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont( 2, c( mean.1, mean.2 ), c( sd.1, sd.2 ) ),
                function( mean.1, sd.1, mean.2, sd.2, x )
                v.cont.via.integrate( 2, mean.1, sd.1, mean.2, sd.2 ), inf.to.inf = TRUE ), TRUE )

expect_equal( check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont( 2, c( mean.1, mean.2 ), c( sd.1, sd.2 ), get.lv = TRUE ),
                function( mean.1, sd.1, mean.2, sd.2, x )
                v.cont.via.integrate( 2, mean.1, sd.1, mean.2, sd.2, get.lv = TRUE ), inf.to.x = TRUE ), TRUE )

expect_equal( check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont( 2, c( mean.1, mean.2 ), c( sd.1, sd.2 ), get.uv = TRUE ),
                function( mean.1, sd.1, mean.2, sd.2, x )
                v.cont.via.integrate( 2, mean.1, sd.1, mean.2, sd.2, get.uv = TRUE ), inf.to.x = TRUE ), TRUE )


expect_equal( check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont( 2, c( mean.1, mean.1 ), c( sd.1, sd.2 ), symmetric = TRUE ),
                function( mean.1, sd.1, mean.2, sd.2, x )
                v.cont.via.integrate( 2, mean.1, sd.1, mean.1, sd.2, symmetric = TRUE ), inf.to.inf = TRUE ), TRUE )

expect_equal( check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont( 2, c( mean.1, mean.1 ), c( sd.1, sd.2 ), symmetric = TRUE, get.lv = TRUE ),
                function( mean.1, sd.1, mean.2, sd.2, x )
                v.cont.via.integrate( 2, mean.1, sd.1, mean.1, sd.2, symmetric = TRUE, get.lv = TRUE ), inf.to.x = TRUE ), TRUE )

expect_equal( check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont( 2, c( mean.1, mean.1 ), c( sd.1, sd.2 ), symmetric = TRUE, get.uv = TRUE ),
                function( mean.1, sd.1, mean.2, sd.2, x )
                v.cont.via.integrate( 2, mean.1, sd.1, mean.1, sd.2, symmetric = TRUE, get.uv = TRUE ), inf.to.x = TRUE ), TRUE )


expect_equal( check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont( 3, c( mean.1, mean.2, mean.1 ), c( sd.1, sd.2, sd.1 ) ),
                function( mean.1, sd.1, mean.2, sd.2, x )
                v.cont.via.integrate( 3, mean.1, sd.1, mean.2, sd.2 ), inf.to.inf = TRUE ), TRUE )

expect_equal( check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont( 3, c( mean.1, mean.2, mean.1 ), c( sd.1, sd.2, sd.1 ), get.lv = TRUE ),
                function( mean.1, sd.1, mean.2, sd.2, x )
                v.cont.via.integrate( 3, mean.1, sd.1, mean.2, sd.2, get.lv = TRUE ), inf.to.x = TRUE ), TRUE )

expect_equal( check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont( 3, c( mean.1, mean.2, mean.1 ), c( sd.1, sd.2, sd.1 ), get.uv = TRUE ),
                function( mean.1, sd.1, mean.2, sd.2, x )
                v.cont.via.integrate( 3, mean.1, sd.1, mean.2, sd.2, get.uv = TRUE ), inf.to.x = TRUE ), TRUE )


expect_equal( check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont( 3, c( mean.1, mean.1, mean.1 ), c( sd.1, sd.2, sd.1 ), symmetric = TRUE ),
                function( mean.1, sd.1, mean.2, sd.2, x )
                v.cont.via.integrate( 3, mean.1, sd.1, mean.1, sd.2, symmetric = TRUE ), inf.to.inf = TRUE ), TRUE )

expect_equal( check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont( 3, c( mean.1, mean.1, mean.1 ), c( sd.1, sd.2, sd.1 ), symmetric = TRUE, get.lv = TRUE ),
                function( mean.1, sd.1, mean.2, sd.2, x )
                v.cont.via.integrate( 3, mean.1, sd.1, mean.1, sd.2, symmetric = TRUE, get.lv = TRUE ), inf.to.x = TRUE ), TRUE )

expect_equal( check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont( 3, c( mean.1, mean.1, mean.1 ), c( sd.1, sd.2, sd.1 ), symmetric = TRUE, get.uv = TRUE ),
                function( mean.1, sd.1, mean.2, sd.2, x )
                v.cont.via.integrate( 3, mean.1, sd.1, mean.1, sd.2, symmetric = TRUE, get.uv = TRUE ), inf.to.x = TRUE ), TRUE )

expect_equal( all( vapply( means,
            function( mean.3 )
            {
                vapply( sds[sds > 0.5],
                        function( sd.3 )
                        {
                            print( paste( "mean.3:", mean.3 ) )
                            print( paste( "sd.3:", sd.3 ) )
                            check.integ.2(
                                function( mean.1, sd.1, mean.2, sd.2, x )
                                { cgd:::v.cont( 3, c( mean.1, mean.2, mean.3 ), c( sd.1, sd.2, sd.3 ) ) },
                                function( mean.1, sd.1, mean.2, sd.2, x )
                                { v.cont.via.integrate( 3, mean.1, sd.1, mean.2, sd.2, mean.3, sd.3 ) },
                                inf.to.inf = TRUE,
                                target.cond = function( mean.1, sd.1, mean.2, sd.2, x )
                                                {
                                                    ( sd.1 > 0.5 &&
                                                      abs( mean.1 ) < 3 && abs( mean.3 ) < 3 ) ||
                                                    ( sd.1 > 1 && sd.3 > 1 &&
                                                      abs( mean.1 - mean.3 ) < 2.799 )
                                                },
                                permit.d = function( d, abs.error, info, parent.info )
                                                { abs( d ) < 1 && abs( d ) < abs.error * 256 } )
                        }, TRUE )
            }, rep( TRUE, length( sds[sds > 0.5] ) ) ) ), TRUE )

expect_equal( all( vapply( means,
            function( mean.3 )
            {
                vapply( sds[sds > 0.5],
                        function( sd.3 )
                        {
                            print( paste( "mean.3:", mean.3 ) )
                            print( paste( "sd.3:", sd.3 ) )
                            check.integ.2(
                                function( mean.1, sd.1, mean.2, sd.2, x )
                                { cgd:::v.cont( 3, c( mean.1, mean.2, mean.3 ), c( sd.1, sd.2, sd.3 ), get.lv = TRUE ) },
                                function( mean.1, sd.1, mean.2, sd.2, x )
                                { v.cont.via.integrate( 3, mean.1, sd.1, mean.2, sd.2, mean.3, sd.3, get.lv = TRUE ) },
                                inf.to.x = TRUE,
                                target.cond = function( mean.1, sd.1, mean.2, sd.2, x )
                                                {
                                                    ( sd.1 > 0.5 &&
                                                      abs( mean.1 ) < 3 && abs( mean.3 ) < 3 ) ||
                                                    ( sd.1 > 1 && sd.3 > 1 &&
                                                      abs( mean.1 - mean.3 ) < 2.799 )
                                                },
                                permit.d = function( d, abs.error, info, parent.info )
                                                { abs( d ) < 1 && abs( d ) < abs.error * 256 } )
                        }, TRUE )
            }, rep( TRUE, length( sds[sds > 0.5] ) ) ) ), TRUE )

expect_equal( all( vapply( means,
            function( mean.3 )
            {
                vapply( sds[sds > 0.5],
                        function( sd.3 )
                        {
                            print( paste( "mean.3:", mean.3 ) )
                            print( paste( "sd.3:", sd.3 ) )
                            check.integ.2(
                                function( mean.1, sd.1, mean.2, sd.2, x )
                                { cgd:::v.cont( 3, c( mean.1, mean.2, mean.3 ), c( sd.1, sd.2, sd.3 ), get.uv = TRUE ) },
                                function( mean.1, sd.1, mean.2, sd.2, x )
                                { v.cont.via.integrate( 3, mean.1, sd.1, mean.2, sd.2, mean.3, sd.3, get.uv = TRUE ) },
                                x.to.inf = TRUE,
                                target.cond = function( mean.1, sd.1, mean.2, sd.2, x )
                                                {
                                                    ( sd.1 > 0.5 &&
                                                      abs( mean.1 ) < 3 && abs( mean.3 ) < 3 ) ||
                                                    ( sd.1 > 1 && sd.3 > 1 &&
                                                      abs( mean.1 - mean.3 ) < 2.799 )
                                                },
                                permit.d = function( d, abs.error, info, parent.info )
                                                { abs( d ) < 1 && abs( d ) < abs.error * 256 } )
                        }, TRUE )
            }, rep( TRUE, length( sds[sds > 0.5] ) ) ) ), TRUE )

########
# Functions for manually check
# to search out copy errors betweeen this file and the CGD.R source around *.t3.sub.
lv.t3.test <- function( means, sds )
{
    mean <- cgd:::mean.cont( 3, means, sds )

    lv.t3.sub( 1, mean, means[1], sds[1] ) +
    lv.t3.sub( 2, mean, means[2], sds[2] ) +
    lv.t3.sub( 3, mean, means[3], sds[3] )
}

uv.t3.test <- function( means, sds )
{
    mean <- cgd:::mean.cont( 3, means, sds )

    uv.t3.sub( 1, mean, means[1], sds[1] ) +
    uv.t3.sub( 2, mean, means[2], sds[2] ) +
    uv.t3.sub( 3, mean, means[3], sds[3] )
}
########

v.sub.t4.via.integrate <- function( mean, mean.1, sd.1, mean.2, sd.2 )
{
    integrate( function( x )
                {
                    x^2 *
                    ( pnorm( x, mean.1, sd.1 ) -
                      pnorm( x, mean.1, sd.1 * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2 +
                      pnorm( x, mean.2, sd.2 * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2 ) *
                    ( ( 1 - dnorm( x, mean.1, sd.1 ) / dnorm( mean.1, mean.1, sd.1 ) ) *
                      dnorm( x, mean.1, sd.1 ) +
                      dnorm( x, mean.2, sd.2 )^2 / dnorm( mean.2, mean.2, sd.2 ) )
                }, -Inf, Inf )
}

expect_equal( all( vapply( means,
            function( mean )
            {
                print( paste( "whole mean:", mean ) )
                check.integ.2(
                    function( mean.1, sd.1, mean.2, sd.2, x )
                    { cgd:::v.sub.t4( mean, c( mean.1, mean.2 ), c( sd.1, sd.2 ) ) },
                    function( mean.1, sd.1, mean.2, sd.2, x )
                    { v.sub.t4.via.integrate( mean, mean.1, sd.1, mean.2, sd.2 ) },
                    inf.to.inf = TRUE,
                    permit.d = function( d, abs.error, info, parent.info )
                                {
                                    ( abs( d ) < 1 && abs( d ) < abs.error * 64 ) ||
                                    abs( cgd:::v.sub.t4( mean, c( mean.1, mean.2 - 0.05 ), c( sd.1, sd.2 ) ) -
                                         v.sub.t4.via.integrate( mean, mean.1, sd.1,
                                                                 mean.2 - 0.05, sd.2 )$value ) < abs.error * 64
                                } )
            }, TRUE ) ), TRUE )


expect_equal( all( vapply( c( 0, 1.5, -0.75 ),
    function( mean.4 )
    vapply( c( 1, 0.8, 1.21 ),
            function( sd.4 )
            vapply( c( 0, 1.5, -0.75 ),
                    function( mean.1 )
                    vapply( c( 1, 0.8, 1.21 ),
                            function( sd.1 )
                            {
                                print( paste( "mean.1:", mean.1 ) )
                                print( paste( "sd.1:", sd.1 ) )
                                print( paste( "mean.4:", mean.4 ) )
                                print( paste( "sd.4:", sd.4 ) )

                                check.integ.2(
                                    function( mean.2, sd.2, mean.3, sd.3, x )
                                    { cgd:::v.cont( 4, c( mean.1, mean.2, mean.3, mean.4 ), c( sd.1, sd.2, sd.3, sd.4 ) ) },
                                    function( mean.2, sd.2, mean.3, sd.3, x )
                                    { cgd:::v.cont.t4.via.integrate( c( mean.1, mean.2, mean.3, mean.4 ),
                                                                     c( sd.1, sd.2, sd.3, sd.4 ) ) },
                                    inf.to.inf = TRUE,
                                    permit.d = function( d, abs.error, info, parent.info )
                                                    {
                                                        if ( abs( d ) < 1 && abs( d ) < abs.error * 64 )
                                                        {
                                                            TRUE
                                                        }
                                                        else
                                                        {
                                                            # Avoid singular point of integrate function.
                                                            if ( parent.info$pass == 1 )
                                                            {
                                                                mean.2  <- parent.info$mean - 0.05
                                                                sd.2    <- parent.info$sd
                                                                mean.3  <- info$mean - 0.05
                                                                sd.3    <- info$sd
                                                            }
                                                            else
                                                            {
                                                                mean.2  <- info$mean - 0.05
                                                                sd.2    <- info$sd
                                                                mean.3  <- parent.info$mean - 0.05
                                                                sd.3    <- parent.info$sd
                                                            }

                                                            abs( cgd:::v.cont( 4,
                                                                        c( mean.1, mean.2, mean.3, mean.4 ),
                                                                        c( sd.1, sd.2, sd.3, sd.4 ) ) -
                                                                 cgd:::v.cont.t4.via.integrate(
                                                                        c( mean.1, mean.2, mean.3, mean.4 ),
                                                                        c( sd.1, sd.2, sd.3, sd.4 ) )$value ) < abs.error * 64
                                                        }
                                                    } )
                            }, TRUE ), rep( TRUE, 3 ) ), rep( TRUE, 9 ) ), rep( TRUE, 27 ) ) ), TRUE )

## These get.uv/get.lv options are not supported yet for type1.type = 4.
expect_error( cgd:::v.cont( 4, c( 0, 0, 0, 0 ), c( 1, 1, 1, 1 ), get.uv = TRUE ), "not supported yet" )

expect_error( cgd:::v.cont( 4, c( 0, 0, 0, 0 ), c( 1, 1, 1, 1 ), get.lv = TRUE ), "not supported yet" )
########

expect_equal( check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont( 2, c( mean.1, mean.2 ), c( sd.1, sd.2 ) ),
                function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont.t4.via.integrate( c( mean.1, mean.1, mean.2, mean.2 ),
                                               c( sd.1, sd.1, sd.2, sd.2 ) ), inf.to.inf = TRUE ), TRUE )

expect_equal( check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont( 2, c( mean.1, mean.2 ), c( sd.1, sd.2 ), get.lv = TRUE ),
                function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont.t4.via.integrate( c( mean.1, mean.1, mean.2, mean.2 ),
                                               c( sd.1, sd.1, sd.2, sd.2 ), get.lv = TRUE ), inf.to.inf = TRUE ), TRUE )

expect_equal( check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont( 2, c( mean.1, mean.2 ), c( sd.1, sd.2 ), get.uv = TRUE ),
                function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont.t4.via.integrate( c( mean.1, mean.1, mean.2, mean.2 ),
                                               c( sd.1, sd.1, sd.2, sd.2 ), get.uv = TRUE ), inf.to.inf = TRUE ), TRUE )

expect_equal( check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont( 3, c( mean.1, mean.2, mean.1 ), c( sd.1, sd.2,sd.1 ) ),
                function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont.t4.via.integrate( c( mean.1, mean.2, mean.1, mean.2 ),
                                               c( sd.1, sd.2, sd.1, sd.2 ) ), inf.to.inf = TRUE ), TRUE )

expect_equal( check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont( 3, c( mean.1, mean.2, mean.1 ), c( sd.1, sd.2,sd.1 ), get.lv = TRUE ),
                function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont.t4.via.integrate( c( mean.1, mean.2, mean.1, mean.2 ),
                                               c( sd.1, sd.2, sd.1, sd.2 ), get.lv = TRUE ), inf.to.inf = TRUE ), TRUE )

expect_equal( check.integ.2(  function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont( 3, c( mean.1, mean.2, mean.1 ), c( sd.1, sd.2,sd.1 ), get.uv = TRUE ),
                function( mean.1, sd.1, mean.2, sd.2, x )
                cgd:::v.cont.t4.via.integrate( c( mean.1, mean.2, mean.1, mean.2 ),
                                               c( sd.1, sd.2, sd.1, sd.2 ), get.uv = TRUE ), inf.to.inf = TRUE ), TRUE )
