% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CGD.R
\name{sqnorm}
\alias{sqnorm}
\title{正規分布の平均値からの相対位置算出}
\usage{
sqnorm(p)
}
\arguments{
\item{p}{確率。}
}
\value{
平均値からの相対位置(\eqn{\sigma} 単位)
}
\description{
正規分布の累積分布関数において、与えられた確率に当たる点が、平均値から何σ離れているかを得る。
}
\examples{
 sqnorm( 0.5 ) # 0
 sqnorm( pnorm( -2, 0, 1 ) ) # -2
 sqnorm( seq( 0, 1, 0.1 ) ) # increces from -Inf to Inf
}
