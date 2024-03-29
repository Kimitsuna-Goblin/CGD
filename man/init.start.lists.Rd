% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CGD.R
\name{init.start.lists}
\alias{init.start.lists}
\title{nls.freq.all の start.lists 用リスト初期化}
\usage{
init.start.lists()
}
\value{
length( cgd:::kinds ) - 1 個の NULL が入ったリスト
}
\description{
要素数が length( cgd:::kinds ) - 1 と等しい NULL のリストを得る。
\link[cgd]{nls.freq.all} の初期値 start にいくつかの初期値を自分で指定したいときは、
この関数の戻り値のリストを得て、
そのリストの中の初期値を指定したい所に start に与えるリストを与えればよい。
}
\details{
リストの何番目の要素がどの分布の初期値に当たるかは cgd:::kinds の順番を参照すること。
なお、 \link[cgd]{nls.start.template} で、各リストのテンプレートが得られる。
}
\examples{
 ## preparing
 x <- seq( -2, 2, 0.2 )
 freq <- c( 15.164, 22.923,  25.134,  27.631,  37.239, 40.464,
            47.126, 79.469, 109.966, 118.241, 111.333, 78.674,
            46.921, 41.026,  36.975,  27.403,  25.493, 22.838,
            14.992, 11.468,   9.174 )
 total <- sum( freq )

 ## getting the initial lists
 start.lists <- init.start.lists()

 ## setting the start parameters for cgd:::kinds[13]
 start.lists[[13]] <- nls.start.template( 13 )
 start.lists[[13]]   ## checking the parameters for cgd:::kinds[13]

 start.lists[[13]]$mean.1.1 <- -0.671
 start.lists[[13]]$mean.1.2 <- -0.198
 start.lists[[13]]$mean.2.1 <- 0.293
 start.lists[[13]]$mean.2.2 <- -0.198
 start.lists[[13]]$sqrt.sd <- sqrt( 0.640 )  ## the sqrt of the standard deviation.

 ## try nls.freq.all
 ##   remark that some warnings will occur but please don't mind.
 result <- nls.freq.all( x, freq, total, start.lists )
 result$cor
}
\seealso{
\link[cgd]{nls.freq.all}, \link[cgd]{nls.start.template}
}
