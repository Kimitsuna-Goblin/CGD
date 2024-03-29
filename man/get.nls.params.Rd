% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CGD.R
\name{get.nls.params}
\alias{get.nls.params}
\title{[内部関数] nls 用パラメータ取得}
\usage{
get.nls.params(
  x,
  freq,
  total,
  normal,
  symmetric,
  v.grad,
  type1.type,
  uni.sigma,
  uni.mean
)
}
\arguments{
\item{x}{X座標のベクトル。
最低 3 個以上の要素を含み、昇順に並んでいて、値の重複はない。}

\item{freq}{度数分布のベクトル。}

\item{total}{度数の合計。}

\item{normal}{正規分布で近似するフラグ。}

\item{symmetric}{type1.type = 2, symmetric = TRUE で近似するフラグ。}

\item{v.grad}{type1.type = 3, v.grad = TRUE で近似するフラグ。}

\item{type1.type}{フィールドの type1.type の値。}

\item{uni.sigma}{構成要素の正規分布の標準偏差をすべて等しくするかどうかのフラグ。}

\item{uni.mean}{構成要素の正規分布の平均値をすべて等しくするかどうかのフラグ。}
}
\value{
nls に渡す formula と start のリスト
}
\description{
設定値をもとに、 nls に渡す主要な引数を取得する
}
