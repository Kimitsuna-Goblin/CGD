##############################################################################
# 連結ガウス分布 (Connected Gaussian Distribution) クラス (nleqslv 使用)
# @file			CGD.need-nleqslv.R
# @version		1.1.9904
# @author		Kimitsuna-Goblin
# @copyright	Copyright (C) 2022 Ura Kimitsuna
# @license		Released under the MIT license.
#				see https://opensource.org/licenses/MIT/
##############################################################################

library( nleqslv )

# ▼ 他のライブラリ (common.R) からのコピー (ここから) ▼

# common.R
# version 0.1.0
# Copyright (C) 2022 Ura Kimitsuna
# Released under the MIT license.

###############################################################################
# 与えられた確率における、平均値からのσ単位の相対位置を得る
# @param	p  		確率
# @param	...		qnorm の引数
# @return	平均値からの相対位置(σ単位)
###############################################################################
sqnorm <- function( p, lower.tail = TRUE, log.p = FALSE )
{
	return ( qnorm( p, 0, 1, lower.tail, log.p ) )
}

###############################################################################
# 与えられた平均値を取り、1点における確率を満たす正規分布の標準偏差を得る
# @param	mean	平均値
# @param	q		X座標 (クォンタイル)
# @param	p		その点における確率
# @return	標準偏差
###############################################################################
sd.mqp.norm <- function( mean, q, p )
{
	return ( ( q - mean ) / sqnorm( p ) )
}

# ▲ 他のライブラリ (common.R) からのコピー (ここまで) ▲

###############################################################################
#' 連結ガウス分布区間クラス
#'
#' 連結ガウス分布で使用する区間を表すクラス
#' @export
#' @field	sd				標準偏差
#' @field	q.ind 			独立区間におけるX座標 (クォンタイル)
#' @field	q.conn.prev		前の区間との接続区間の確率内に収まる、この標準偏差のX座標 (クォンタイル)
#' @field	q.conn.next		次の区間との接続区間の確率内に収まる、この標準偏差のX座標 (クォンタイル)
#' @field	p.ind			独立区間における確率
#' @field	p.conn.prev		前の区間との接続区間の確率
#' @field	p.conn.next		次の区間との接続区間の確率
###############################################################################
CGDInterval <- setRefClass(

	# クラス名
	Class = "CGDInterval",

	# プロパティ
	fields = list(
		sd = "numeric",				# 標準偏差
		q.ind = "vector",			# 独立区間におけるX座標 (クォンタイル)
		q.conn.prev = "vector",		# 前の区間との接続区間の確率内に収まる、この標準偏差のX座標 (クォンタイル)
		q.conn.next = "vector",		# 次の区間との接続区間の確率内に収まる、この標準偏差のX座標 (クォンタイル)
		p.ind = "vector",			# 独立区間における確率
		p.conn.prev = "vector",		# 前の区間との接続区間の確率
		p.conn.next = "vector"		# 次の区間との接続区間の確率
	)
)

# メソッド
###############################################################################
#' 負担区間取得
#'
#' 負担区間 (前の区間との接続区間～次の区間との接続区間) のX座標 (クォンタイル) を取得する
#' @name CGDInterval_q
#' @return	負担区間のX座標 (クォンタイル)
###############################################################################
NULL
CGDInterval$methods(
	q.manage = function()
	{
		return ( c( q.conn.prev[1], q.conn.next[2] ) )
	}
)

##############################################################################
#' 連結ガウス分布 (Connected Gaussian Distribution) クラス
#'
#' 平均値の等しい正規分布をX軸方向に連結したモデルを表すクラス
#'
#' 連結ガウス分布は、任意のクォンタイルをゼロ誤差で満たす分布を構成できる。
#' そのため、何かクォンタイルが与えられたときに、
#' それを高精度に再現する分布が欲しいときなどに使うことができる。
#' ただし、確率密度関数は一般に不連続であり、歪んだ形になる。
#'
#' 連結ガウス分布を使って滑らかなグラフを得るには、
#' ランダムサンプルを取得して、そのヒストグラムを描画するとよい。
#'
#' また、クォンタイルが3点 (50％の点が1点と、それ以外の確率の点が2点) の場合は、
#' クォンタイルの位置が過度にいびつでなければ、確率密度関数を連続にできる。
#' ただし、その場合でも、確率密度関数は歪んだり、中央が尖ったりする。
#' @export
#' @field	mean			平均値
#' @field	intervals		連結区間 (CGDInterval クラスのリスト)
#' @field	type1.type		接続区間が type 1 の場合の計算方法
#' @field	m.sd			計算済みの標準偏差 (クラス外からは直接参照しないこと)
###############################################################################
CGD <- setRefClass(

	# クラス名
	Class = "CGD",

	# プロパティ
	fields = list(
		mean = "numeric",			# 平均値
		intervals = "list",			# 連結区間 (CGDInterval クラスのリスト)
		type1.type = "numeric",		# 接続区間が type 1 の場合の計算方法
		m.sd = "numeric"			# 計算済みの標準偏差 (クラス外からは直接参照しないこと)
	)
)

# メソッド
###############################################################################
#' コンストラクタ
#'
#' コンストラクタの引数は予め連結区間の構成が分かっている場合に指定する。
#' 詳細については、 set.waypoints() の説明を参照。
#' @name	CGD_initialize
#' @param	mean			平均値 (デフォルト: NULL)
#' @param	intervals		連結区間 (CGDInterval クラスのリスト) (デフォルト: NULL)
#' @param	type1.type		type 1 の場合の計算方法 (1、2、3 のいずれかを指定する) (デフォルト: 1)
#'
###############################################################################
NULL
CGD$methods(
	initialize = function( mean = NULL, intervals = NULL, type1.type = 1 )
	{
		if ( is.null( mean ) )
		{
			mean <<- 0
		}
		else
		{
			mean <<- mean
		}

		if ( is.null( intervals ) )
		{
			intervals <<- c( CGDInterval$new( sd = 1,
												q.ind = c( -Inf, Inf ),
												q.conn.prev = c( -Inf, -Inf ), q.conn.next = c( Inf, Inf ),
												p.ind = c( 0, 1 ),
												p.conn.prev = c( 0, 0 ), p.conn.next = c( 1, 1 ) ) )
		}
		else
		{
			intervals <<- intervals
		}

		if ( type1.type == 1 || type1.type == 2 || type1.type == 3 )
		{
			type1.type <<- type1.type
		}
		else
		{
			warning( paste( "Warning: type1.type" , type1.type, "is undefined." ) )
			type1.type <<- 1
		}

		m.sd <<- -Inf
	}
)

###############################################################################
#' 経路設定
#'
#' 累積分布関数の経路 (クォンタイル) を設定する
#' @name	CGD_set.waypoints
#' @param	waypoints		経路の data.frame( q = 経路のX座標 (クォンタイル), p = その点における確率 )
#'							X座標 (クォンタイル) は昇順にソートしておくこと。
#'							平均値は p = 0.5 の点のX座標として与えること (p = 0.5 の点は必須)
#' @param	continuous		独立区間を [0, 0] と [1, 1] の2点にして、
#'							確率密度関数が全区間 (-∞, ∞) で連続になるように分布構成を試みる (デフォルト: FALSE)
#' @param	symmetric		独立区間を [0, 0], [0.5, 0.5], [1, 1] の3点にして、
#'							1番目と3番目の確率分布を同一にすることにより、
#'							確率密度関数が全区間 (-∞, ∞) で連続で、かつ左右対称になるように試みる  (デフォルト: FALSE)
#'							 (continuous または symmetric を TRUE にする場合、
#'							  前提として、経路は確率 0.5 の点が1点と、確率 0, 0.5, 1 以外の点が 2点 の、
#'							  合計 3点 から成っていること。
#'							  例: waypoints = data.frame( p = c( 0.2, 0.5, 0.7 ), q = c( -1.6, 0, 0.4 ) ) )
#' @param	type1.type		type 1 の場合の計算方法 (1、2、3 のいずれかを指定する) (デフォルト: 1)
#'
#'				type1.type の値によって、接続区間 (β_i, α_{i+1}) が type 1 の場合、
#'				接続区間の累積分布関数 Ψ_i(x) を以下のように計算する。
#'
#'				1: Ψ_i(x) = ( α_{i+1} - x ) / ( α_{i+1} - β_i ) * Φ_i(x) +
#'							 ( x - β_i ) / ( α_{i+1} - β_i ) * Φ_{i+1}(x)
#'
#'				2: Ψ_i(x) = ( Φ~_i(α_{i+1}) - Φ~_i(x) ) / ( Φ~_i(α_{i+1}) - Φ~_i(β_i) ) * Φ_i(x) +
#'							 ( Φ~_i(x) - Φ~_i(β_i) ) / ( Φ~_i(α_{i+1}) - Φ~_i(β_i) ) * Φ_{i+1}(x)
#'
#'				3: Ψ_i(x) = ∫_{-∞}^x { ( 1 - f_i(t) / f_i(μ) ) f_i(t) + f_{i+1}(t)^2 / f_{i+1}(μ) } dt
#'
#'				ただし、Φ_i, Φ_{i+1} は接続区間の前後の独立区間における累積分布関数。
#'				Φ~_i(x) = ( Φ_i(x) + Φ_{i+1}(x) ) / 2。
#'
#'				f_i, f_{i+1} は接続区間の前後の独立区間における確率密度関数。μ は平均値。
#'
#'				type1.type = 1 では、
#'				set.waypoints() の引数で continuous または symmetric を TRUE にした場合、
#'				独立区間は [0, 0], [1, 1] の2点になり、
#'				接続区間の累積分布関数は Ψ(x) = ( Φ_i(x) + Φ_{i+1}(x) ) / 2 となる
#'				  (この場合、独立区間を [0, 0], [0.5, 0.5], [1, 1] の3点と見做しても結果は同じ)。
#'
#'				type1.type = 2 では、
#'				set.waypoints() の引数で continuous を TRUE にした場合、
#'				独立区間は [0, 0], [1, 1] の2点になり、
#'				接続区間の累積分布関数は上の 2 の式になる。
#'				set.waypoints() の引数で symmetric を TRUE にした場合、
#'				独立区間は [0, 0], [0.5, 0.5], [1, 1] の3点になり、
#'				接続区間 (0, 0.5) の累積分布関数 Ψ_1(x) は上の 2 の式で、
#'				接続区間 (0.5, 1) の累積分布関数 Ψ_2(x) は Ψ_2(x) = 1 - Ψ_1(2μ - x) となる。
#'
#'				type1.type = 3 では、
#'				独立区間は [0, 0], [0.5, 0.5], [1, 1], [0, 0.5], [0.5, 1], [0, 1] の6種類しか取り得ない。
#'				set.waypoints() の引数の continuous は無効になる。
#'				set.waypoints() の引数で symmetric を TRUE にした場合、
#'				独立区間は [0, 0], [0.5, 0.5], [1, 1] の3点になり、
#'				2つの接続区間 (0, 0.5), (0.5, 1) の累積分布関数はどちらも同じ、上の 3 の式になる。
#'
#'				type1.type = 2 と type1.type = 3 でsymmetric を TRUE にした場合、
#'				[0, 0] と [1, 1] の2点が1つめの正規分布 N_1 の独立区間、
#'				[0.5, 0.5] が2つめの正規分布 N_2 の独立区間となる。
#'
#' @return	nleqslv() を内部で実行した場合はその結果、それ以外は NULL
###############################################################################
NULL
CGD$methods(
	set.waypoints = function( waypoints, continuous = FALSE, symmetric = FALSE, type1.type = 1 )
	{
		result <- NULL

		# メンバ変数を初期化
		intervals <<- list()
		m.sd <<- -Inf

		if ( type1.type == 1 || type1.type == 2 || type1.type == 3 )
		{
			type1.type <<- type1.type
		}
		else
		{
			stop( paste( "Error: type1.type" , type1.type, "is undefined." ) )
		}

		wp <- data.frame( q = numeric(), p = numeric() )	# 平均値を除き、昇順に並べた経路

		# 平均値および、平均値を除いた経路の data.frame を取得
		j <- 1
		is.set.mean <- FALSE
		wp.order <- order( waypoints$p )
		for ( i in 1:nrow( waypoints ) )
		{
			oi <- wp.order[i]
			if ( waypoints$p[oi] == 0.5 )
			{
				# 平均値を取得
				mean <<- waypoints[oi,]$q
				is.set.mean <- TRUE
			}
			else if ( waypoints$p[oi] < 0 || waypoints$p[oi] > 1 )
			{
				# 確率が負または1を超えている
				warning( paste( "Warning: probability" , waypoints$p[oi] , "is out of range [0, 1]." ) )
			}
			else if ( waypoints$q[oi] == -Inf || waypoints$q[oi] == Inf )
			{
				# X座標が±∞の点は無視 (ただし、確率が 0 または 1 でない場合は警告)
				if ( ( waypoints$q[oi] == -Inf &&  waypoints$p[oi] != 0 ) ||
						( waypoints$q[oi] == Inf && waypoints$p[oi] != 1 ) )
				{
					warning( "Warning: q is infinite (-Inf or Inf) but probability is not 0 or 1." )
				}
			}
			else
			{
				# 経路を取得
				wp[j,]$q <- waypoints[oi,]$q
				wp[j,]$p <- waypoints[oi,]$p
				if ( j > 1 )
				{
					# X座標が確率に対して昇順に並んでいなければエラー
					if ( wp[j - 1,]$q >= wp[j,]$q )
					{
						stop( "Error: order of q is not along with that of p" )
					}
				}

				j <- j + 1
			}
		}
		if ( !is.set.mean )
		{
			stop( "Error: q for mean (p = 0.5) is not given." )
		}
		if ( nrow( wp ) == 0 )
		{
			warning( "Warning: no waypoints other than (p = 0, 0.5, 1) are given." )
		}

		if ( nrow( wp ) == 2 )
		{
			# 引数と type1.type と経路上の点の数の条件が満たされる場合、確率密度関数がなるべく連続になるように試みる
			if ( type1.type == 1 && ( continuous || symmetric ) )
			{
				sds <- c( 0.9, 1.1 )
				e <- try( result <- nleqslv( sds, f <- function( x )
														{
															c(	( pnorm( wp$q[1], mean, x[1] ) +
																	pnorm( wp$q[1], mean, x[2] ) ) / 2 - wp$p[1],
																( pnorm( wp$q[2], mean, x[1] ) +
																	pnorm( wp$q[2], mean, x[2] ) ) / 2 - wp$p[2] )
														} ), silent = TRUE )
				if ( class( e ) == "try-error" )
				{
					stop( "Error: failed to make up a continuous probability density function." )
				}
				else if ( result$termcd != 1 )
				{
					message( paste( "nleqslv is failed. message:", result$message ) )
					stop( "Error: failed to make up a continuous probability density function." )
				}
				else
				{
					intervals <<- list( CGDInterval$new(
											sd = result$x[1],
											q.ind = c( -Inf, -Inf ), q.conn.prev = c( -Inf, -Inf ), q.conn.next= c( 0, Inf ),
											p.ind = c( 0, 0 ), p.conn.prev = c( 0, 0 ), p.conn.next = c( 0, 1 ) ),
										CGDInterval$new(
											sd = result$x[2],
											q.ind = c( Inf, Inf ), q.conn.prev = c( 0, Inf ), q.conn.next= c( Inf, Inf ),
											p.ind = c( 1, 1 ), p.conn.prev = c( 0, 1 ), p.conn.next = c( 1, 1 ) ) )
				}
			}
			if ( type1.type == 2 && continuous )
			{
				sds <- c( 1, 1 )
				e <- try( result <- nleqslv( sds, f <- function( x )
														{
															p1.1 <- pnorm( wp$q[1], mean, x[1] )
															p1.2 <- pnorm( wp$q[1], mean, x[2] )
															p2.1 <- pnorm( wp$q[2], mean, x[1] )
															p2.2 <- pnorm( wp$q[2], mean, x[2] )

															ave1 <- ( p1.1 + p1.2 ) / 2
															ave2 <- ( p2.1 + p2.2 ) / 2

															c( ( 1 - ave1 ) * p1.1 + ave1 * p1.2 - wp$p[1],
																( 1 - ave2 ) * p2.1 + ave2 * p2.2 - wp$p[2] )
														} ), silent = TRUE )
				if ( class( e ) == "try-error" )
				{
					stop( "Error: failed to make up a continuous probability density function." )
				}
				else if ( result$termcd != 1 )
				{
					message( paste( "nleqslv is failed. message:", result$message ) )
					stop( "Error: failed to make up a continuous probability density function." )
				}
				else
				{
					intervals <<- list( CGDInterval$new(
											sd = result$x[1],
											q.ind = c( -Inf, -Inf ), q.conn.prev = c( -Inf, -Inf ), q.conn.next= c( 0, Inf ),
											p.ind = c( 0, 0 ), p.conn.prev = c( 0, 0 ), p.conn.next = c( 0, 1 ) ),
										CGDInterval$new(
											sd = result$x[2],
											q.ind = c( Inf, Inf ), q.conn.prev = c( 0, Inf ), q.conn.next= c( Inf, Inf ),
											p.ind = c( 1, 1 ), p.conn.prev = c( 0, 1 ), p.conn.next = c( 1, 1 ) ) )
				}
			}
			else if ( type1.type == 2 && symmetric )
			{
				# symmetric の場合、高速化のために、経路の確率をすべて 0.5 以下にそろえて計算する
				wp$p <- 0.5 - abs( 0.5 - wp$p )
				wp$q <- mean - abs( mean - wp$q )

				sds <- c( 1, 1 )
				e <- try( result <- nleqslv( sds, f <- function( x )
														{
															p1.1 <- pnorm( wp$q[1], mean, x[1] )
															p1.2 <- pnorm( wp$q[1], mean, x[2] )
															p2.1 <- pnorm( wp$q[2], mean, x[1] )
															p2.2 <- pnorm( wp$q[2], mean, x[2] )
															c(	( 1 - p1.1 ) * p1.1 + p1.2 * p1.2 - wp$p[1],
																( 1 - p2.1 ) * p2.1 + p2.2 * p2.2 - wp$p[2] )
														} ), silent = TRUE )
				if ( class( e ) == "try-error" )
				{
					stop( "Error: failed to make up a symmetric probability density function." )
				}
				else if ( result$termcd != 1 )
				{
					message( paste( "nleqslv is failed. message:", result$message ) )
					stop( "Error: failed to make up a symmetric probability density function." )
				}
				else
				{
					intervals <<- list( CGDInterval$new(
											sd = result$x[1],
											q.ind = c( -Inf, -Inf ),
											q.conn.prev = c( -Inf, -Inf ), q.conn.next= c( 0, mean ),
											p.ind = c( 0, 0 ),
											p.conn.prev = c( 0, 0 ), p.conn.next = c( 0, 0.5 ) ),
										CGDInterval$new(
											sd = result$x[2],
											q.ind = c( mean, mean ),
											q.conn.prev = c( 0, mean ), q.conn.next= c( mean, Inf ),
											p.ind = c( 0.5, 0.5 ),
											p.conn.prev = c( 0, 0.5 ), p.conn.next = c( 0.5, 1 ) ),
										CGDInterval$new(
											sd = result$x[1],
											q.ind = c( Inf, Inf ),
											q.conn.prev = c( mean, Inf ), q.conn.next= c( Inf, Inf ),
											p.ind = c( 1, 1 ),
											p.conn.prev = c( 0.5, 1 ), p.conn.next = c( 1, 1 ) ) )
				}
			}
			else if ( type1.type == 3 && symmetric )
			{
				sds <- c( 1, 1 )
				e <- try( result <- nleqslv( sds, f <- function( x )
														{
															p1.1 <- pnorm( wp$q[1], mean, x[1] )
															p1.a1 <- pnorm( wp$q[1], mean, x[1] / sqrt( 2 ) )
															p1.a2 <- pnorm( wp$q[1], mean, x[2] / sqrt( 2 ) )
															p2.1 <- pnorm( wp$q[2], mean, x[1] )
															p2.a1 <- pnorm( wp$q[2], mean, x[1] / sqrt( 2 ) )
															p2.a2 <- pnorm( wp$q[2], mean, x[2] / sqrt( 2 ) )

															c( p1.1 - p1.a1 / sqrt( 2 ) + p1.a2 / sqrt( 2 ) - wp$p[1],
																p2.1 - p2.a1 / sqrt( 2 ) + p2.a2 / sqrt( 2 ) - wp$p[2] )
														} ), silent = TRUE )
				if ( class( e ) == "try-error" )
				{
					stop( "Error: failed to make up a symmetric probability density function." )
				}
				else if ( result$termcd != 1 )
				{
					message( paste( "nleqslv is failed. message:", result$message ) )
					stop( "Error: failed to make up a symmetric probability density function." )
				}
				else
				{
					intervals <<- list( CGDInterval$new(
											sd = result$x[1],
											q.ind = c( -Inf, -Inf ),
											q.conn.prev = c( -Inf, -Inf ), q.conn.next= c( 0, mean ),
											p.ind = c( 0, 0 ),
											p.conn.prev = c( 0, 0 ), p.conn.next = c( 0, 0.5 ) ),
										CGDInterval$new(
											sd = result$x[2],
											q.ind = c( mean, mean ),
											q.conn.prev = c( 0, mean ), q.conn.next= c( mean, Inf ),
											p.ind = c( 0.5, 0.5 ),
											p.conn.prev = c( 0, 0.5 ), p.conn.next = c( 0.5, 1 ) ),
										CGDInterval$new(
											sd = result$x[1],
											q.ind = c( Inf, Inf ),
											q.conn.prev = c( mean, Inf ), q.conn.next= c( Inf, Inf ),
											p.ind = c( 1, 1 ),
											p.conn.prev = c( 0.5, 1 ), p.conn.next = c( 1, 1 ) ) )
				}
			}
		}

		if ( type1.type == 3 && !symmetric )
		{
			if ( nrow( wp ) < 2 || nrow( wp ) > 4 )
			{
				# type1.type == 3 の場合、経路3点または4点以外は場合はエラーとする
				stop( "Error: number of waypoints must be 3 or 4 if type1.type is 3." )
			}

			wp.lower <- data.frame( q = numeric(), p = numeric() )	# p < 0.5 の経路
			wp.upper <- data.frame( q = numeric(), p = numeric() )	# p > 0.5 の経路
			j.lower <- 1
			j.upper <- 1
			for ( i in 1:nrow( wp ) )
			{
				if ( wp$p[i] < 0.5 )
				{
					wp.lower[j.lower,]$q <- wp[i,]$q
					wp.lower[j.lower,]$p <- wp[i,]$p
					j.lower <- j.lower + 1
				}
				else
				{
					wp.upper[j.upper,]$q <- wp[i,]$q
					wp.upper[j.upper,]$p <- wp[i,]$p
					j.upper <- j.upper + 1
				}
			}

			if ( j.lower > 3 || j.upper > 3 )
			{
				# 一方の経路に3点以上ある場合はエラーとする
				stop( "Error: number of waypoints which p is lower/upper than 0.5 must be less than 3 if type1.type is 3." )
			}

			# 場合分けして累積分布関数を構成
			if ( j.lower == 2 && j.upper == 2 )
			{
				# ( #lower, #upper ) = ( 1, 1 )
				sds <- c( 1, 1 )
				e <- try( result <- nleqslv( sds, f <- function( x )
														{
															x.ave <- ( x[1] + x[2] ) / 2
															p1.1 <- pnorm( wp$q[1], mean, x[1] )
															p1.a1 <- pnorm( wp$q[1], mean, x[1] / sqrt( 2 ) )
															p1.a2 <- pnorm( wp$q[1], mean, x.ave / sqrt( 2 ) )
															p2.1 <- pnorm( wp$q[2], mean, x[2] / 2 )
															p2.a1 <- pnorm( wp$q[2], mean, x[2] / sqrt( 2 ) )
															p2.a2 <- pnorm( wp$q[2], mean, x.ave / sqrt( 2 ) )

															c( p1.1 - p1.a1 / sqrt( 2 ) + p1.a2 / sqrt( 2 ) - wp$p[1],
																p2.1 - p2.a1 / sqrt( 2 ) + p2.a2 / sqrt( 2 ) - wp$p[2] )
														} ), silent = TRUE )
				if ( class( e ) == "try-error" )
				{
					stop( "Error: failed to make up a probability density function." )
				}
				else if ( result$termcd != 1 )
				{
					message( paste( "nleqslv is failed. message:", result$message ) )
					stop( "Error: failed to make up a probability density function." )
				}
				else
				{
					sds[1] <- result$x[1]
					sds[2] <- ( result$x[1] + result$x[2] ) / 2
					sds[3] <- result$x[2]
				}
			}
			else if ( j.lower == 3 && j.upper == 1 )
			{
				# ( #lower, #upper ) = ( 2, 0 )
				sds <- c( 1, 1 )
				e <- try( result <- nleqslv( sds, f <- function( x )
														{
															x.ave <- ( x[1] + x[2] ) / 2
															p1.1 <- pnorm( wp$q[1], mean, x[1] )
															p1.a1 <- pnorm( wp$q[1], mean, x[1] / sqrt( 2 ) )
															p1.a2 <- pnorm( wp$q[1], mean, x.ave / sqrt( 2 ) )
															p2.1 <- pnorm( wp$q[2], mean, x[2] / 2 )
															p2.a1 <- pnorm( wp$q[2], mean, x[2] / sqrt( 2 ) )
															p2.a2 <- pnorm( wp$q[2], mean, x.ave / sqrt( 2 ) )

															c( p1.1 - p1.a1 / sqrt( 2 ) + p1.a2 / sqrt( 2 ) - wp$p[1],
																p2.1 - p2.a1 / sqrt( 2 ) + p2.a2 / sqrt( 2 ) - wp$p[2] )
														} ), silent = TRUE )
				if ( class( e ) == "try-error" )
				{
					stop( "Error: failed to make up a probability density function." )
				}
				else if ( result$termcd != 1 )
				{
					message( paste( "nleqslv is failed. message:", result$message ) )
					stop( "Error: failed to make up a probability density function." )
				}
				else
				{
					sds[1] <- result$x[1]
					sds[2] <- result$x[2]
					sds[3] <- result$x[2]
				}
			}
			else if ( j.lower == 1 && j.upper == 3 )
			{
				# ( #lower, #upper ) = ( 0, 2 )
				sds <- c( 1, 1 )
				e <- try( result <- nleqslv( sds, f <- function( x )
														{
															x.ave <- ( x[1] + x[2] ) / 2
															p1.1 <- pnorm( wp$q[1], mean, x[1] )
															p1.a1 <- pnorm( wp$q[1], mean, x[1] / sqrt( 2 ) )
															p1.a2 <- pnorm( wp$q[1], mean, x.ave / sqrt( 2 ) )
															p2.1 <- pnorm( wp$q[2], mean, x[2] / 2 )
															p2.a1 <- pnorm( wp$q[2], mean, x[2] / sqrt( 2 ) )
															p2.a2 <- pnorm( wp$q[2], mean, x.ave / sqrt( 2 ) )

															c( p1.1 - p1.a1 / sqrt( 2 ) + p1.a2 / sqrt( 2 ) - wp$p[1],
																p2.1 - p2.a1 / sqrt( 2 ) + p2.a2 / sqrt( 2 ) - wp$p[2] )
														} ), silent = TRUE )
				if ( class( e ) == "try-error" )
				{
					stop( "Error: failed to make up a probability density function." )
				}
				else if ( result$termcd != 1 )
				{
					message( paste( "nleqslv is failed. message:", result$message ) )
					stop( "Error: failed to make up a probability density function." )
				}
				else
				{
					sds[1] <- result$x[1]
					sds[2] <- result$x[1]
					sds[3] <- result$x[2]
				}
			}
			else
			{
				# ( #lower, #upper ) = ( 2, 1 ) or ( 1, 2 )
				sds <- c( 1, 1, 1 )
				e <- try( result <- nleqslv( sds, f <- function( x )
														{
															x.ave <- ( x[1] + x[2] ) / 2
															p1.1 <- pnorm( wp$q[1], mean, x[1] )
															p1.a1 <- pnorm( wp$q[1], mean, x[1] / sqrt( 2 ) )
															p1.a2 <- pnorm( wp$q[1], mean, x[2] / sqrt( 2 ) )
															if ( wp$p[2] < 0.5 )
															{
																p2.1 <- pnorm( wp$q[2], mean, x[1] / 2 )
																p2.a1 <- pnorm( wp$q[2], mean, x[1] / sqrt( 2 ) )
																p2.a2 <- pnorm( wp$q[2], mean, x[2] / sqrt( 2 ) )
															}
															else
															{
																p2.1 <- pnorm( wp$q[2], mean, x[3] / 2 )
																p2.a1 <- pnorm( wp$q[2], mean, x[3] / sqrt( 2 ) )
																p2.a2 <- pnorm( wp$q[2], mean, x[2] / sqrt( 2 ) )
															}
															p3.1 <- pnorm( wp$q[2], mean, x[3] / 2 )
															p3.a1 <- pnorm( wp$q[2], mean, x[3] / sqrt( 2 ) )
															p3.a2 <- pnorm( wp$q[2], mean, x[2] / sqrt( 2 ) )

															c( p1.1 - p1.a1 / sqrt( 2 ) + p1.a2 / sqrt( 2 ) - wp$p[1],
																p2.1 - p2.a1 / sqrt( 2 ) + p2.a2 / sqrt( 2 ) - wp$p[2],
																p3.1 - p3.a1 / sqrt( 2 ) + p3.a2 / sqrt( 2 ) - wp$p[3] )
														} ), silent = TRUE )
				if ( class( e ) == "try-error" )
				{
					stop( "Error: failed to make up a probability density function." )
				}
				else if ( result$termcd != 1 )
				{
					message( paste( "nleqslv is failed. message:", result$message ) )
					stop( "Error: failed to make up a probability density function." )
				}
				else
				{
					sds <- result$x
				}
			}

			intervals <<- list( CGDInterval$new(
									sd = sds[1],
									q.ind = c( -Inf, -Inf ),
									q.conn.prev = c( -Inf, -Inf ), q.conn.next= c( 0, mean ),
									p.ind = c( 0, 0 ),
									p.conn.prev = c( 0, 0 ), p.conn.next = c( 0, 0.5 ) ),
								CGDInterval$new(
									sd = sds[2],
									q.ind = c( mean, mean ),
									q.conn.prev = c( 0, mean ), q.conn.next= c( mean, Inf ),
									p.ind = c( 0.5, 0.5 ),
									p.conn.prev = c( 0, 0.5 ), p.conn.next = c( 0.5, 1 ) ),
								CGDInterval$new(
									sd = sds[3],
									q.ind = c( Inf, Inf ),
									q.conn.prev = c( mean, Inf ), q.conn.next= c( Inf, Inf ),
									p.ind = c( 1, 1 ),
									p.conn.prev = c( 0.5, 1 ), p.conn.next = c( 1, 1 ) ) )
		}

		if ( length( intervals ) > 0 )
		{
			return ( result )
		}

		# 経路上の点を通る標準偏差を取得
		sds <- sd.mqp.norm( rep( mean, nrow( wp ) ), wp$q, wp$p )

		# 連結区間を設定
		i <- 1
		while ( i <= nrow( wp ) )
		{
			if ( i == 1 )
			{
				# デフォルトの連結区間パラメータを設定
				p.conn.prev <- c( 0, 0 )
				q.conn.prev <- c( -Inf, -Inf )

				p.ind <- c( 0, 1 )
				q.ind <- c( -Inf, Inf )

				p.conn.next <- c( 1, 1 )
				q.conn.next <- c( Inf, Inf )
			}
			else
			{
				# 前の区間との接続区間を取得
				p.conn.prev <- intervals[[length( intervals )]]$p.conn.next
				q.conn.prev <- c( qnorm( intervals[[length( intervals )]]$p.conn.next[1], mean, sds[i] ), wp[( i ),]$q )

				# 独立区間の始点は前の区間との接続区間の終点
				p.ind <- c( p.conn.prev[2], 1 )
				q.ind <- c( q.conn.prev[2], Inf )
			}

			# 標準偏差を変えずに次の経路上の点を通れるか探索
			if ( i < nrow( wp ) )
			{
				j <- i + 1
				extended.to <- 0	# 延長先の区間
				while ( j <= length( sds ) )
				{
					if ( sds[i] == sds[j] )
					{
						# 標準偏差が次の経路上の点を通る分布の標準偏差と等しい場合
						# 独立区間を次の経路上の点まで延長
						q.ind[2] <- wp[j,]$q
						p.ind[2] <- pnorm( wp[j,]$q, mean, sds[i] )
						extended.to <- j

						j <- j + 1
					}
					else
					{
						# 通過できなかったので探索終了
						break
					}
				}

				if ( extended.to > 0 )
				{
					i <- extended.to
				}
			}

			# 次の区間との接続区間を取得
			if ( i == nrow( wp ) )
			{
				# 経路の最後の点まで来たので、次の区間は無し
				p.conn.next <- c( 1, 1 )
				q.conn.next <- c( Inf, Inf )

				# 経路の最後の点まで通過できた場合、独立区間は無限大まで
				p.ind[2] <- 1
				q.ind[2] <- Inf
			}
			else
			{
				p.conn.next <- c( pnorm( wp[i,]$q, mean, sds[i] ), pnorm( wp[( i + 1 ),]$q, mean, sds[i + 1] ) )
				q.conn.next <- c( wp[i,]$q, qnorm( p.conn.next[2], mean, sds[i] ) )

				# 独立区間の終点は次の区間との接続区間の始点
				p.ind[2] <- p.conn.next[1]
				q.ind[2] <- q.conn.next[1]
			}

			# 連結区間クラスのインスタンス生成
			intervals <<- c( intervals, CGDInterval$new(
											sd = sds[i],
											q.ind = q.ind, q.conn.prev = q.conn.prev, q.conn.next= q.conn.next,
											p.ind = p.ind, p.conn.prev = p.conn.prev, p.conn.next = p.conn.next ) )
			i <- i + 1
		}

		return ( result )
	}
)

###############################################################################
#' continuous 判定
#'
#' continuous かどうかを調べる
#' @name	CGD_is.continuous
#' @return	continuous = TRUE で構成されていれば TRUE、そうでなければ FALSE
###############################################################################
NULL
CGD$methods(
	is.continuous = function()
	{
 		return ( intervals[[1]]$p.conn.next[1] == 0 && intervals[[1]]$p.conn.next[2] == 1 )
	}
)

###############################################################################
#' symmetric 判定
#'
#' symmetric かどうかを調べる
#' @name	CGD_is.symmetric
#' @return	symmetric = TRUE で構成されていれば TRUE、そうでなければ FALSE
###############################################################################
NULL
CGD$methods(
	is.symmetric = function()
	{
		# type1.type == 2 の場合の判定式は、
		# length( intervals ) <= 2 のときエラーが発生しないように、少し複雑な判定式になっている
		#  ( intervals[[3]]$p.conn.prev[1] と書くと、エラーになることがある)
		return ( ( type1.type == 1 && intervals[[1]]$p.conn.next[1] == 0 && intervals[[1]]$p.conn.next[2] == 1 )
		 			|| ( type1.type == 2 && length( intervals ) == 3 &&
							intervals[[1]]$p.conn.next[1] == 0 &&
							intervals[[1]]$p.conn.next[2] == 0.5 &&
							intervals[[length( intervals )]]$p.conn.prev[1] == 0.5 &&
							intervals[[length( intervals )]]$p.conn.prev[2] == 1 &&
							intervals[[1]]$sd == intervals[[length( intervals )]]$sd )
					|| ( type1.type == 3 && intervals[[1]]$sd == intervals[[length( intervals )]]$sd ) )
	}
)

###############################################################################
#' 確率密度取得
#'
#' X座標 (クォンタイル) を指定して、確率密度を取得する
#' @name	CGD_d
#' @param	x		X座標 (クォンタイル) のベクトル
#' @return	確率密度
###############################################################################
NULL
CGD$methods(
	d = function( x )
	{
		results <- numeric()

		if ( is.symmetric() )
		{
			x <- mean - abs( mean - x )
		}

		for ( i in 1:length( x ) )
		{
			if ( type1.type == 3 )
			{
				# type1.type == 3 の場合の計算は symmetric であってもなくても同じ
				if ( x <= mean )
				{
					results[i] <- ( 1 - dnorm( x[i], mean, intervals[[1]]$sd ) / dnorm( mean, mean, intervals[[1]]$sd ) ) *
										dnorm( x[i], mean, intervals[[1]]$sd ) +
									dnorm( x[i], mean, intervals[[2]]$sd )^2 / dnorm( mean, mean, intervals[[2]]$sd )
				}
				else
				{
					results[i] <- ( 1 - dnorm( x[i], mean, intervals[[3]]$sd ) / dnorm( mean, mean, intervals[[3]]$sd ) ) *
										dnorm( x[i], mean, intervals[[3]]$sd ) +
									dnorm( x[i], mean, intervals[[2]]$sd )^2 / dnorm( mean, mean, intervals[[2]]$sd )
				}
			}
			else if ( is.continuous() )
			{
				# continuous の場合
				if ( type1.type == 1 )
				{
 					results[i] <- ( dnorm( x[i], mean, intervals[[1]]$sd ) + dnorm( x[i], mean, intervals[[2]]$sd ) ) / 2
				}
				else # if ( type1.type == 2 )
				{
					results[i] <- ( 1 - pnorm( x[i], mean, intervals[[1]]$sd ) ) * dnorm( x[i], mean, intervals[[1]]$sd ) +
									pnorm( x[i], mean, intervals[[2]]$sd ) * dnorm( x[i], mean, intervals[[2]]$sd )
				}
			}
			else if ( is.symmetric() )
			{
				# symmetric の場合
				# type1.type == 2 ( type1.type == 1 は is.continuous() == TRUE になる)
				results[i] <- ( 1 - 2 * pnorm( x[i], mean, intervals[[1]]$sd ) ) *
										dnorm( x[i], mean, intervals[[1]]$sd ) +
									2 * pnorm( x[i], mean, intervals[[2]]$sd ) *
										dnorm( x[i], mean, intervals[[2]]$sd )
			}
			else
			{
				# 与えられたX座標 (クォンタイル) を負担する分布を探索
				if ( x[i] == Inf )				{
					j <- length( intervals )
				}
				else
				{
					is.no.block = FALSE
					for ( j in ( 1:length( intervals ) ) )
					{
						if ( x[i] >= intervals[[j]]$q.manage()[1] && x[i] < intervals[[j]]$q.manage()[2] )
						{
							break
						}
						else if ( j > 1 )
						{
							if ( x[i] > intervals[[j - 1]]$q.manage()[2] && x[i] < intervals[[j]]$q.manage()[1] )
							{
								# 負担分布なし ⇒ d( x ) = 0
								results[i] <- 0
								is.no.block = TRUE
								break
							}
						}
					}
					if ( is.no.block )
					{
						next
					}
				}

				if ( x[i] >= intervals[[j]]$q.ind[1] && x[i] <= intervals[[j]]$q.ind[2] )
				{
					# 独立区間内 ⇒ 確率密度をそのまま出力
					results[i] <- dnorm( x[i], mean, intervals[[j]]$sd )
				}
				else
				{
					# 接続区間内 ⇒ 次の区間の標準偏差による確率密度と線形に割合を決めて負担を分割
					d.1 <- NULL				# 確率分布1 (接続区間前を負担)
					d.2 <- NULL				# 確率分布2 (接続区間後を負担)
					x.conn <- numeric()		# 接続区間のX座標 (クォンタイル)
					if ( x[i] >= intervals[[j]]$q.conn.next[1] && x[i] < intervals[[j]]$q.conn.next[2] )
					{
						d.1 <- intervals[[j]]
						d.2 <- intervals[[j + 1]]
					}
					else
					{
						d.1 <- intervals[[j - 1]]
						d.2 <- intervals[[j]]
					}
					x.conn <- c( d.1$q.conn.next[1], d.2$q.conn.prev[2] )

					if ( x.conn[1] < mean )		# 平均値より絶対小でなければならない。「以下」は不可
					{
						if ( x.conn[2] <= mean )
						{
							# 平均値をまたがない
							if ( d.2$sd >= d.1$sd )
							{
								# type 1
								if ( type1.type == 1 )
								{
									results[i] <- (	dnorm( x[i], mean, d.1$sd ) * ( x.conn[2] - x[i] ) +
													dnorm( x[i], mean, d.2$sd ) * ( x[i] - x.conn[1] ) +
													pnorm( x[i], mean, d.2$sd ) - pnorm( x[i], mean, d.1$sd ) ) /
													( x.conn[2] - x.conn[1] )
								}
								else # if ( type1.type == 2 )
								{
									p1.conn1 <- pnorm( x.conn[1], mean, d.1$sd )
									p2.conn1 <- pnorm( x.conn[1], mean, d.2$sd )
									psum.conn1 <- p1.conn1 + p2.conn1
									p1.conn2 <- pnorm( x.conn[2], mean, d.1$sd )
									p2.conn2 <- pnorm( x.conn[2], mean, d.2$sd )
									psum.conn2 <- p1.conn2 + p2.conn2
									results[i] <- ( ( psum.conn2 - 2 * pnorm( x[i], mean, d.1$sd ) ) *
														dnorm( x[i], mean, d.1$sd ) +
													( 2 * pnorm( x[i], mean, d.2$sd ) - psum.conn1 ) *
														dnorm( x[i], mean, d.2$sd ) ) /
													( psum.conn2 - psum.conn1 )
								}
							}
							else
							{
								# type 2
								if ( x[i] < d.2$q.conn.prev[1] )
								{
									# P2(x) < inf
									if ( x[i] < d.1$q.conn.next[2] )
									{
										# P1(x) < sup
										results[i] <- dnorm( x[i], mean, d.1$sd ) / 2
									}
									else
									{
										# P1(x) >= sup
										results[i] <- 0
									}
								}
								else
								{
									# P2(x) >= inf
									if ( x[i] < d.1$q.conn.next[2] )
									{
										# P1(x) < sup
										results[i] <- ( dnorm( x[i], mean, d.1$sd ) + dnorm( x[i], mean, d.2$sd ) ) / 2
									}
									else
									{
										# P1(x) >= sup
										results[i] <- dnorm( x[i], mean, d.2$sd ) / 2
									}
								}
							}
						}
						else
						{
							# 平均値をまたぐ
							if ( d.2$sd >= d.1$sd )
							{
								# type 3a
								if ( x[i] <= mean )
								{
									results[i] <- dnorm( x[i], mean, d.1$sd )
								}
								else
								{
									if ( x[i] < d.1$q.conn.next[2] )
									{
										# P1(x) < sup
										results[i] <- ( dnorm( x[i], mean, d.1$sd ) + dnorm( x[i], mean, d.2$sd ) ) / 2
									}
									else
									{
										# P1(x) >= sup
										results[i] <- dnorm( x[i], mean, d.2$sd ) / 2
									}
								}
							}
							else
							{
								# type 3b
								if ( x[i] <= mean )
								{
									if ( x[i] < d.2$q.conn.prev[1] )
									{
										# P1(x) < inf
										results[i] <- dnorm( x[i], mean, d.1$sd ) / 2
									}
									else
									{
										# P1(x) >= inf
										results[i] <- ( dnorm( x[i], mean, d.1$sd ) + dnorm( x[i], mean, d.2$sd ) ) / 2
									}
								}
								else
								{
									results[i] <- dnorm( x[i], mean, d.2$sd )
								}
							}
						}
					}
					else	# ( x.conn[1] >= mean )	# 平均値より絶対大でなくてもよい。「以上」でよい
					{
						if ( d.2$sd >= d.1$sd )
						{
							# type 2
							if ( x[i] < d.2$q.conn.prev[1] )
							{
								# P2(x) < inf
								if ( x[i] < d.1$q.conn.next[2] )
								{
									# P1(x) < sup
									results[i] <- dnorm( x[i], mean, d.1$sd ) / 2
								}
								else
								{
									# P1(x) >= sup
									results[i] <- 0
								}
							}
							else
							{
								# P2(x) >= inf
								if ( x[i] < d.1$q.conn.next[2] )
								{
									# P1(x) < sup
									results[i] <- ( dnorm( x[i], mean, d.1$sd ) + dnorm( x[i], mean, d.2$sd ) ) / 2
								}
								else
								{
									# P1(x) >= sup
									results[i] <- dnorm( x[i], mean, d.2$sd ) / 2
								}
							}
						}
						else
						{
							# type 1
							if ( type1.type == 1 )
							{
								results[i] <- (	dnorm( x[i], mean, d.1$sd ) * ( x.conn[2] - x[i] ) +
												dnorm( x[i], mean, d.2$sd ) * ( x[i] - x.conn[1] ) +
												pnorm( x[i], mean, d.2$sd ) - pnorm( x[i], mean, d.1$sd ) ) /
												( x.conn[2] - x.conn[1] )
							}
							else # if ( type1.type == 2 )
							{
								p1.conn1 <- pnorm( x.conn[1], mean, d.1$sd )
								p2.conn1 <- pnorm( x.conn[1], mean, d.2$sd )
								psum.conn1 <- p1.conn1 + p2.conn1
								p1.conn2 <- pnorm( x.conn[2], mean, d.1$sd )
								p2.conn2 <- pnorm( x.conn[2], mean, d.2$sd )
								psum.conn2 <- p1.conn2 + p2.conn2
								results[i] <- ( ( psum.conn2 - 2 * pnorm( x[i], mean, d.1$sd ) ) *
													dnorm( x[i], mean, d.1$sd ) +
												( 2 * pnorm( x[i], mean, d.2$sd ) - psum.conn1 ) *
													dnorm( x[i], mean, d.2$sd ) ) /
												( psum.conn2 - psum.conn1 )
							}
						}
					}
				}
			}
		}

		return ( results )
	}
)

###############################################################################
#' 確率取得
#'
#' X座標 (クォンタイル) を指定して、確率を取得する
#' @name	CGD_p
#' @param	q						X座標 (クォンタイル) のベクトル
#' @return	確率
###############################################################################
NULL
CGD$methods(
	p = function( q )
	{
		results <- numeric()

		for ( i in 1:length( q ) )
		{
			if ( type1.type == 3 )
			{
				# type1.type == 3 の場合の計算は symmetric であってもなくても同じ
				if ( x <= mean )
				{
					results[i] <- pnorm( q[i], mean, intervals[[1]]$sd ) -
									pnorm( q[i], mean, intervals[[1]]$sd / sqrt( 2 ) ) / sqrt( 2 ) +
									pnorm( q[i], mean, intervals[[2]]$sd / sqrt( 2 ) ) / sqrt( 2 )
				}
				else
				{
					results[i] <- pnorm( q[i], mean, intervals[[3]]$sd ) -
									pnorm( q[i], mean, intervals[[3]]$sd / sqrt( 2 ) ) / sqrt( 2 ) +
									pnorm( q[i], mean, intervals[[2]]$sd / sqrt( 2 ) ) / sqrt( 2 )
				}
			}
			else if ( is.continuous() )
			{
				# continuous の場合
				if ( type1.type == 1 )
				{
					results[i] <- ( pnorm( q[i], mean, intervals[[1]]$sd ) + pnorm( q[i], mean, intervals[[2]]$sd ) ) / 2
				}
				else # if ( type1.type == 2 )
				{
					p1 <- pnorm( q[i], mean, intervals[[1]]$sd )
					p2 <- pnorm( q[i], mean, intervals[[2]]$sd )
					results[i] <- p1 - p1 * p1 / 2 + p2 * p2 / 2
				}
			}
			else if ( is.symmetric() )
			{
				# symmetric の場合
				# type1.type == 2
				if ( q[i] <= mean )
				{
					p1 <- pnorm( q[i], mean, intervals[[1]]$sd )
					p2 <- pnorm( q[i], mean, intervals[[2]]$sd )
					results[i] <- ( 1 - p1 ) * p1 + p2 * p2
				}
				else
				{
					p1 <- pnorm( 2 * mean - q[i], mean, intervals[[1]]$sd )
					p2 <- pnorm( 2 * mean - q[i], mean, intervals[[2]]$sd )
					results[i] <- 1 - ( 1 - p1 ) * p1 - p2 * p2
				}
			}
			else
			{
				# 与えられたX座標 (クォンタイル) を負担する分布を探索
				if ( q[i] == Inf )
				{
					j <- length( intervals )
				}
				else
				{
					is.no.block = FALSE
					for ( j in ( 1:length( intervals ) ) )
					{
						if ( q[i] >= intervals[[j]]$q.manage()[1] && q[i] < intervals[[j]]$q.manage()[2] )
						{
							break
						}
						else if ( j > 1 )
						{
							if ( q[i] > intervals[[j - 1]]$q.manage()[2] && q[i] < intervals[[j]]$q.manage()[1] )
							{
								# 負担分布なし ⇒ p = 直前の分布が負担する確率の上限 = 接続区間の確率の上限と下限の平均値
								results[i] <- ( intervals[[j - 1]]$p.conn.next[1] + intervals[[j - 1]]$p.conn.next[2] ) / 2
								is.no.block = TRUE
								break
							}
						}
					}
					if ( is.no.block )
					{
						next
					}
				}

				if ( q[i] >= intervals[[j]]$q.ind[1] && q[i] <= intervals[[j]]$q.ind[2] )
				{
					# 独立区間内 ⇒ 確率をそのまま出力
					results[i] <- pnorm( q[i], mean, intervals[[j]]$sd )
				}
				else
				{
					# 接続区間内 ⇒ 次の区間の標準偏差による確率密度と線形に割合を決めて負担を分割
					d.1 <- NULL				# 確率分布1 (接続区間前を負担)
					d.2 <- NULL				# 確率分布2 (接続区間後を負担)
					x.conn <- numeric()		# 接続区間のX座標 (クォンタイル)
					if ( q[i] >= intervals[[j]]$q.conn.next[1] && q[i] < intervals[[j]]$q.conn.next[2] )
					{
						d.1 <- intervals[[j]]
						d.2 <- intervals[[j + 1]]
					}
					else
					{
						d.1 <- intervals[[j - 1]]
						d.2 <- intervals[[j]]
					}
					x.conn <- c( d.1$q.conn.next[1], d.2$q.conn.prev[2] )

					if ( x.conn[1] < mean )		# 平均値より絶対小でなければならない。「以下」は不可
					{
						if ( d.1$q.conn.next[2] <= mean )
						{
							# 平均値をまたがない
							if ( d.2$sd >= d.1$sd )
							{
								# type 1
								if ( type1.type == 1 )
								{
									results[i] <- (	pnorm( q[i], mean, d.1$sd ) * ( x.conn[2] - q[i] ) +
													pnorm( q[i], mean, d.2$sd ) * ( q[i] - x.conn[1] ) ) /
													( x.conn[2] - x.conn[1] )
								}
								else # if ( type1.type == 2 )
								{
									p1.conn1 <- pnorm( x.conn[1], mean, d.1$sd )
									p2.conn1 <- pnorm( x.conn[1], mean, d.2$sd )
									psum.conn1 <- p1.conn1 + p2.conn1
									p1.conn2 <- pnorm( x.conn[2], mean, d.1$sd )
									p2.conn2 <- pnorm( x.conn[2], mean, d.2$sd )
									psum.conn2 <- p1.conn2 + p2.conn2
									p1.q <- pnorm( q[i], mean, d.1$sd )
									p2.q <- pnorm( q[i], mean, d.2$sd )
									results[i] <- ( ( psum.conn2 - p1.q ) * p1.q + ( p2.q - psum.conn1 ) * p2.q ) /
													( psum.conn2 - psum.conn1 )
								}
							}
							else
							{
								# type 2
								if ( q[i] < d.2$q.conn.prev[1] )
								{
									# P2(x) < inf
									if ( q[i] < d.1$q.conn.next[2] )
									{
										# P1(x) < sup
										results[i] <- ( d.1$p.conn.next[1] + pnorm( q[i], mean, d.1$sd ) ) / 2
									}
									else
									{
										# P1(x) >= sup
										results[i] <- ( d.1$p.conn.next[1] + d.1$p.conn.next[2] ) / 2
									}
								}
								else
								{
									# P2(x) >= inf
									if ( q[i] < d.1$q.conn.next[2] )
									{
										# P1(x) < sup
										results[i] <- ( pnorm( q[i], mean, d.1$sd ) + pnorm( q[i], mean, d.2$sd ) ) / 2
									}
									else
									{
										# P1(x) >= sup
										results[i] <- ( d.1$p.conn.next[2] + pnorm( q[i], mean, d.2$sd ) ) / 2
									}
								}
							}
						}
						else
						{
							# 平均値をまたぐ
							if ( d.2$sd >= d.1$sd )
							{
								# type 3a
								if ( q[i] <= mean )
								{
									results[i] <- pnorm( q[i], mean, d.1$sd )
								}
								else
								{
									if ( q[i] < d.1$q.conn.next[2] )
									{
										# P1(x) < sup
										results[i] <- ( pnorm( q[i], mean, d.1$sd ) + pnorm( q[i], mean, d.2$sd ) ) / 2
									}
									else
									{
										# P1(x) >= sup
										results[i] <- ( d.1$p.conn.next[2] + pnorm( q[i], mean, d.2$sd ) ) / 2
									}
								}
							}
							else
							{
								# type 3b
								if ( q[i] <= mean )
								{
									if ( q[i] < d.2$q.conn.prev[1] )
									{
										# P1(x) < inf
										results[i] <- ( pnorm( q[i], mean, d.1$sd ) + d.2$p.conn.prev[1] ) / 2
									}
									else
									{
										# P1(x) >= inf
										results[i] <- ( pnorm( q[i], mean, d.1$sd ) + pnorm( q[i], mean, d.2$sd ) ) / 2
									}
								}
								else
								{
									results[i] <- pnorm( q[i], mean, d.2$sd )
								}
							}
						}
					}
					else	# ( x.conn[1] >= mean )	# 平均値より絶対大でなくてもよい。「以上」でよい
					{
						if ( d.2$sd >= d.1$sd )
						{
							# type 2
							if ( q[i] < d.2$q.conn.prev[1] )
							{
								# P2(x) < inf
								if ( q[i] < d.1$q.conn.next[2] )
								{
									# P1(x) < sup
									results[i] <- ( d.1$p.conn.next[1] + pnorm( q[i], mean, d.1$sd ) ) / 2
								}
								else
								{
									# P1(x) >= sup
									results[i] <- ( d.1$p.conn.next[1] + d.1$p.conn.next[2] ) / 2
								}
							}
							else
							{
								# P2(x) >= inf
								if ( q[i] < d.1$q.conn.next[2] )
								{
									# P1(x) < sup
									results[i] <- ( pnorm( q[i], mean, d.1$sd ) + pnorm( q[i], mean, d.2$sd ) ) / 2
								}
								else
								{
									# P1(x) >= sup
									results[i] <- ( d.1$p.conn.next[2] + pnorm( q[i], mean, d.2$sd ) ) / 2
								}
							}
						}
						else
						{
							# type 1
							if ( type1.type == 1 )
							{
								results[i] <- (	pnorm( q[i], mean, d.1$sd ) * ( x.conn[2] - q[i] ) +
												pnorm( q[i], mean, d.2$sd ) * ( q[i] - x.conn[1] ) ) /
												( x.conn[2] - x.conn[1] )
							}
							else # if ( type1.type == 2 )
							{
								p1.conn1 <- pnorm( x.conn[1], mean, d.1$sd )
								p2.conn1 <- pnorm( x.conn[1], mean, d.2$sd )
								psum.conn1 <- p1.conn1 + p2.conn1
								p1.conn2 <- pnorm( x.conn[2], mean, d.1$sd )
								p2.conn2 <- pnorm( x.conn[2], mean, d.2$sd )
								psum.conn2 <- p1.conn2 + p2.conn2
								p1.q <- pnorm( q[i], mean, d.1$sd )
								p2.q <- pnorm( q[i], mean, d.2$sd )
								results[i] <- ( ( psum.conn2 - p1.q ) * p1.q + ( p2.q - psum.conn1 ) * p2.q ) /
												( psum.conn2 - psum.conn1 )
							}
						}
					}
				}
			}
		}

		return ( results )
	}
)

###############################################################################
#' X座標 (クォンタイル) 取得
#'
#' 確率を指定して、X座標 (クォンタイル) を取得する
#' @name	CGD_q
#' @param	prob					確率のベクトル
#' @return	X座標 (クォンタイル)
###############################################################################
NULL
CGD$methods(
	q = function( prob )
	{
		results <- numeric()

		for ( i in ( 1:length( prob ) ) )
		{
			if ( prob[i] < 0 || prob[i] > 1 )
			{
				# 確率が負または1を超えている
				warning( paste( "Warning: probability" , prob[1] , "is out of range [0, 1]." ) )
				results[i] <- NaN
				next
			}

			if ( type1.type == 3 )
			{
				if ( prob[i] == 0 )
				{
					results[i] <- -Inf
				}
				else if ( prob[i] == 1 )
				{
					results[i] <- Inf
				}
				else if ( prob[i] == 0.5 )
				{
					results[i] <- mean
				}
				else if ( prob[1] < 0.5 )
				{
					results[i] <- uniroot( function( x ) { p( x ) - prob[i] },
											c( qnorm( prob[i], mean, intervals[[1]]$sd + intervals[[2]]$sd ), mean ) )$root
				}
				else
				{
					results[i] <- uniroot( function( x ) { p( x ) - prob[i] },
											c( mean, qnorm( prob[i], mean, intervals[[3]]$sd + intervals[[2]]$sd ) ) )$root
				}
			}
			else if ( is.continuous() || is.symmetric() )
			{
				# continuous or symmetric ⇒ 累積分布関数は至る所で微分可能なので、場合分けしなくても収束する
				if ( prob[i] == 0 )
				{
					results[i] <- -Inf
				}
				else if ( prob[i] == 1 )
				{
					results[i] <- Inf
				}
				else if ( prob[i] == 0.5 )
				{
					results[i] <- mean
				}
				else if ( prob[1] < 0.5 )
				{
					min.q <- min( qnorm( prob[i], mean, intervals[[1]]$sd ), qnorm( prob[i], mean, intervals[[2]]$sd ) )
					results[i] <- uniroot( function( x ) { p( x ) - prob[i] },
											c( min.q - ( mean - min.q ), mean ) )$root
				}
				else
				{
					max.q <- max( qnorm( prob[i], mean, intervals[[1]]$sd ), qnorm( prob[i], mean, intervals[[2]]$sd ) )
					results[i] <- uniroot( function( x ) { p( x ) - prob[i] },
											c( mean, max.q + ( max.q - mean ) ) )$root
				}
			}
			else
			{
				# 与えられた確率に相当する点を負担する分布を探索
				if ( prob[i] == 1 )
				{
					j <- length( intervals )
				}
				else
				{
					for ( j in ( 1:length( intervals ) ) )
					{
						if ( prob[i] >= intervals[[j]]$p.ind[1] && prob[i] < intervals[[j]]$p.conn.next[2] )
						{
							break
						}
					}
				}

				if ( prob[i] <= intervals[[j]]$p.ind[2] )
				{
					# 独立区間内 ⇒ 確率をそのまま出力
					results[i] <- qnorm( prob[i], mean, intervals[[j]]$sd )
				}
				else
				{
					# 接続区間内 ⇒ 次の区間の標準偏差による確率密度と線形に割合を決めて負担を分割
					if ( intervals[[j]]$p.conn.next[1] < 0.5 && intervals[[j]]$p.conn.next[2] > 0.5 )
					{
						# type 3 ⇒ prob[i] が平均値なら 0.5。また、それより大か小かで値域を絞れる
						if ( prob[i] == 0.5 )
						{
							results[i] <- mean
						}
						else if ( prob[i] < 0.5 )
						{
							results[i] <- uniroot( function( x ) { p( x ) - prob[i] },
													c( intervals[[j]]$q.conn.next[1], mean ) )$root
						}
						else
						{
							results[i] <- uniroot( function( x ) { p( x ) - prob[i] },
													c( mean, intervals[[j + 1]]$q.conn.prev[2] ) )$root
						}
					}
					else
					{
						if ( intervals[[j]]$sd < intervals[[j + 1]]$sd )
						{
							# type 1 ⇒ 接続区間内は至る所で微分可能なので、場合分けしなくても収束する
							results[i] <- uniroot( function( x ) { p( x ) - prob[i] },
													c( intervals[[j]]$q.conn.next[1],
														intervals[[j + 1]]$q.conn.prev[2] ) )$root
						}
						else
						{
							# type 2 ⇒ 収束しにくいので、場合分けが必要
							dm <- ( intervals[[j]]$p.conn.next[1] + intervals[[j]]$p.conn.next[2] ) / 2 - prob[i]
							if ( dm == 0 )
							{
								# prob[i] が接続区間の確率の平均値 ⇒ 負担分布なしの可能性あり
								if ( intervals[[j]]$q.conn.next[2] < intervals[[j + 1]]$q.conn.prev[1] )
								{
									# 負担分布なし
									# ⇒ 該当座標は一点に定まらず、区間
									#		[intervals[[j]]$q.manage()[2], intervals[[j + 1]]$q.manage()[1]] になる
									# 区間の平均値を出力しておく
									results[i] <- ( intervals[[j]]$q.manage()[2] + intervals[[j + 1]]$q.manage()[1] ) / 2
								}
								else
								{
									# 負担分布あり ⇒ X座標が接続区間の中点とは限らないので、その周りの範囲で方程式を解く
									results[i] <- uniroot( function( x ) { p( x ) - prob[i] },
															c( intervals[[j + 1]]$q.conn.prev[1],
																intervals[[j]]$q.conn.next[2] ) )$root
								}
							}
							else
							{
								results[i] <- uniroot( function( x ) { p( x ) - prob[i] },
														c( intervals[[j]]$q.conn.next[1],
															intervals[[j + 1]]$q.conn.prev[2] ) )$root
							}
						}
					}
				}
			}
		}

		return ( results )
	}
)

###############################################################################
#' ランダムサンプル取得
#'
#' ランダムサンプルを取得する
#' @name	CGD_r
#' @param	n		サンプル数
#' @return	ランダムサンプルのベクトル
###############################################################################
NULL
CGD$methods(
	r = function( n )
	{
		return ( q( runif( n, 0, 1 ) ) )
	}
)

###############################################################################
#' 標準偏差取得
#'
#' 標準偏差を取得する
#' @name	CGD_sd
#' @return	標準偏差
###############################################################################
NULL
CGD$methods(
	sd = function()
	{
		if ( m.sd >= 0 )
		{
			# 計算済みの標準偏差があればそれを返す
			return ( m.sd )
		}

		# 分散を計算
		v <- 0
		for ( i in 1:length( intervals ) )
		{
			v <- v + integrate( f <-
								function( x )
								{
									( x - mean )^2 * dnorm( x, mean, intervals[[i]]$sd )
								}, intervals[[i]]$q.ind[1], intervals[[i]]$q.ind[2] )$value
			if ( i < length( intervals ) )
			{
				v <- v + integrate( f <-
									function( x )
									{
										( x - mean )^2 * d( x )
									}, intervals[[i]]$q.conn.next[1], intervals[[i]]$q.conn.next[2] )$value
			}
		}

		# 標準偏差を出力
		m.sd <<- sqrt( v )

		return ( m.sd )
	}
)
