% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CGD.R
\name{adjust.mean.sd}
\alias{adjust.mean.sd}
\alias{\S4method{adjust.mean.sd}{CGD}}
\title{平均値・標準偏差設定}
\usage{
\S4method{adjust.mean.sd}{CGD}()
}
\value{
なし
}
\description{
連結ガウス分布クラス (\link[cgd]{CGD}) の平均値と標準偏差のフィールドを
正しく設定する。
この関数を呼び出す前に intervals フィールドと type1.type フィールドを
設定し、かつ adjust.kind.index メソッドを呼び出しておく必要がある。
通常、このメソッドは、クラスの利用者が直接呼び出す必要はない。
}
