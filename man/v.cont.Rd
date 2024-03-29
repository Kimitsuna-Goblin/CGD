% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CGD.R
\name{v.cont}
\alias{v.cont}
\title{[内部関数] 分散算出 (連続分布)}
\usage{
v.cont(
  type1.type,
  means,
  sds,
  mean = mean.cont(type1.type, means, sds),
  symmetric = FALSE,
  get.lv = FALSE,
  get.uv = FALSE
)
}
\arguments{
\item{type1.type}{type1.typeの値。}

\item{means}{intervals の構成要素の正規分布の平均値のベクトル。}

\item{sds}{intervals の構成要素の正規分布の標準偏差のベクトル。}

\item{mean}{分布全体の平均値
(デフォルト: mean.cont( type1.type, means, sds ))。}

\item{symmetric}{TRUE の場合、中央値を中心として左右対称な分布として扱う。
type1.type = 2 または 3 の場合に有効 (デフォルト: FALSE)。}

\item{get.lv}{TRUE の場合、下半分散を算出する (デフォルト: FALSE)。}

\item{get.uv}{TRUE の場合、上半分散を算出する (デフォルト: FALSE)。
get.lv と get.uv を両方同時に TRUE にすべきではないが、
もし両方とも TRUE の場合は、 get.lv が優先される。
両方とも FALSE の場合は、分布全体の分散を算出する。}
}
\value{
連続な連結ガウス分布の分散 (あるいは半分散)。
}
\description{
連続な連結ガウス分布の分散を求める。
\link[stats]{integrate} は使用しない。
type1.type = 4 の場合、 get.lv と get.uv のオプションは無効。
その理由は \eqn{\int_{-\infty}^{\mu} f_{i,1}(x) \Phi_{i,2}(x) dx} を
数値積分を使わないで計算する方法が分からないため。
なお、 type1.type = 4 の場合、 means と sds の要素数は 4 でも 6 でも同様に計算可能。
}
