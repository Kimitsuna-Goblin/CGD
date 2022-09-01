##############################################################################
# 連結ガウス分布 (Connected Gaussian Distribution) クラス
#  (nleqslv ライブラリが必要です)
# @file			CGD.need-nleqslv.R
# @version		1.1.90
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
# 連結ガウス分布区間クラス
# @desc 	連結ガウス分布で使用する区間を表すクラス
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
	),

	# メソッド
	methods = list(
###############################################################################
# 負担区間のX座標 (クォンタイル) を取得する
# @return	負担区間のX座標 (クォンタイル)
###############################################################################
		q.manage = function()
		{
			return ( c( q.conn.prev[1], q.conn.next[2] ) )
		}
	)
)

###############################################################################
# 連結ガウス分布 (Connected Gaussian Distribution) クラス
# @desc 	平均値の等しい正規分布を横方向に連結したモデルを表すクラス
###############################################################################
CGD <- setRefClass(

	# クラス名
	Class = "CGD",

	# プロパティ
	fields = list(
		mean = "numeric",			# 平均値
		intervals = "list",			# 連結区間
		type1.type = "numeric",		# type 1 の計算方法
		m.sd = "numeric"			# 計算済みの標準偏差
	),

	# メソッド
	methods = list(
###############################################################################
# コンストラクタ
# @param	mean = NULL			平均値
# @param	intervals = NULL	連結区間
# @param	type1.type = 1		type 1 の計算方法 (1 または 2 を指定すること)
###############################################################################
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

			if ( type1.type == 1 || type1.type == 2 )
			{
				type1.type <<- type1.type
			}
			else
			{
				warning( paste( "Warning: type1.type" , type1.type, "is undefined." ) )
				type1.type <<- 1
			}

			m.sd <<- -Inf
		},

###############################################################################
# 累積分布関数の経路 (クォンタイル) を設定する
# @param	waypoints		経路の data.frame( q = 経路のX座標 (クォンタイル), p = その点における確率 )
#							X座標 (クォンタイル) は昇順にソートしておくこと。平均値は p = 0.5 の行に設定すること
# @param	continuous = FALSE
#							確率密度関数が全区間 (-∞, ∞) で連続になるように分布構成を試みる
# @param	symmetric = FALSE
#							確率密度関数が全区間 (-∞, ∞) で連続で、かつ左右対称になるように試みる
#							 (continuous, symmetric を TRUE にする場合、
#							  前提として、type1.type = 2 であり、経路上に確率 0, 1 以外の点が 3個 であること)
# @return	nleqslv() を内部で実行した場合はその結果、それ以外は NULL
###############################################################################
		set.waypoints = function( waypoints, continuous = FALSE, symmetric = FALSE )
		{
			# メンバ変数を初期化
			intervals <<- list()
			m.sd <<- -Inf

			result <- NULL

			if ( type1.type < 2 && ( continuous || symmetric ) )
			{
				warning( paste( "Warning: continuous or symmetric is not supported for type1.type =", type1.type ) )
				continuous = FALSE
				symmetric = FALSE
			}

			wp <- data.frame( q = numeric(), p = numeric() )	# 平均値を除いた経路

			# 平均値および、平均値を除いた経路の data.frame を取得
			j <- 1
			is.set.mean <- FALSE
			for ( i in 1:nrow( waypoints ) )
			{
				if ( waypoints$p[i] == 0.5 )
				{
					# 平均値を取得
					mean <<- waypoints[i,]$q
					is.set.mean <- TRUE
				}
				else if ( waypoints$p[i] < 0 || waypoints$p[i] > 1 )
				{
					# 確率が負または1を超えている
					warning( paste( "Warning: probability" , waypoints$p[i] , "is out of range [0, 1]." ) )
				}
				else if ( waypoints$q[i] == -Inf || waypoints$q[i] == Inf )
				{
					# X座標が±∞の点は無視 (ただし、確率が 0 または 1 でない場合は警告)
					if ( ( waypoints$q[i] == -Inf &&  waypoints$p[i] != 0 ) ||
							( waypoints$q[i] == Inf && waypoints$p[i] != 1 ) )
					{
						warning( "Warning: q is infinite (-Inf or Inf) but probability is not 0 or 1." )
					}
				}
				else
				{
					# 経路を取得
					wp[j,]$q <- waypoints[i,]$q
					wp[j,]$p <- waypoints[i,]$p
					j <- j + 1
				}
			}
			if ( !is.set.mean )
			{
				warning( "Warning: q for mean (p = 0.5) is not given." )
			}
			if ( nrow( wp ) == 0 )
			{
				warning( "Warning: no waypoints other than (p = 0, 0.5, 1) are given." )
			}

			# 引数と type1.type と経路上の点の数の条件が満たされる場合、確率密度関数が全区間 (-∞, ∞) で連続になるように試みる
			if ( nrow( wp ) == 2 )
			{
				if ( continuous )
				{
					sds <- c( 1, 1 )
					e <- try( result <- nleqslv( sds, f <- function( x )
															{
																c( ( 1 - pnorm( wp$q[1], mean, x[1] ) ) *
																		pnorm( wp$q[1], mean, x[1] ) +
																		pnorm( wp$q[1], mean, x[2] )^2 - wp$p[1],
																	( 1 - pnorm( wp$q[2], mean, x[1] ) ) *
																		pnorm( wp$q[2], mean, x[1] ) +
																		pnorm( wp$q[2], mean, x[2] )^2 - wp$p[2] )
															} ), silent = TRUE )
					if ( class( e ) == "try-error" )
					{
						warning( "Warning: failed to make up a continuous probability density function." )
					}
					else if ( result$termcd != 1 )
					{
						warning( "Warning: failed to make up a continuous probability density function." )
						message( paste( "nleqslv is failed. message:", result$message ) )
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
				else if ( symmetric )
				{
					sds <- c( 1, 1 )
					e <- try( result <- nleqslv( sds, f <- function( x )
															{
																p1.1 <- pnorm( wp$q[1], mean, x[1] )
																p1.2 <- pnorm( wp$q[1], mean, x[2] )
																p2.1 <- pnorm( wp$q[2], mean, x[1] )
																p2.2 <- pnorm( wp$q[2], mean, x[2] )
																c(	p1.2 + ( p1.1 - p1.2 ) * abs( 1 - p1.1 - p1.2 ) - wp$p[1],
																	p2.2 + ( p2.1 - p2.2 ) * abs( 1 - p2.1 - p2.2 ) - wp$p[2] )
															} ), silent = TRUE )
					if ( class( e ) == "try-error" )
					{
						warning( "Warning: failed to make up a symmetric probability density function." )
					}
					else if ( result$termcd != 1 )
					{
						warning( "Warning: failed to make up a symmetric probability density function." )
						message( paste( "nleqslv is failed. message:", result$message ) )
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
		},

###############################################################################
# continuous かどうかを調べる
# @return	continuous = TRUE で構成されていれば TRUE、そうでなければ FALSE
###############################################################################
		is.continuous = function()
		{
			return ( type1.type == 2 &&
						intervals[[1]]$p.conn.next[1] == 0 &&
						intervals[[1]]$p.conn.next[2] == 1 )
		},

###############################################################################
# symmetric かどうかを調べる
# @return	symmetric = TRUE で構成されていれば TRUE、そうでなければ FALSE
###############################################################################
		is.symmetric = function()
		{
			return ( type1.type == 2 && length( intervals ) == 3 &&
						intervals[[1]]$p.conn.next[1] == 0 &&
						intervals[[1]]$p.conn.next[2] == 0.5 &&
						intervals[[length( intervals )]]$p.conn.prev[1] == 0.5 &&
						intervals[[length( intervals )]]$p.conn.prev[2] == 1 &&
						intervals[[1]]$sd == intervals[[length( intervals )]]$sd )
		},

###############################################################################
# X座標 (クォンタイル) を指定して、確率密度を取得する
# @param	x		X座標 (クォンタイル) のベクトル
# @return	確率密度
###############################################################################
		d = function( x )
		{
			results <- numeric()

			for ( i in 1:length( x ) )
			{
				if ( is.continuous() )
				{
					# continuous の場合
					# type 1, type1.type == 2
					results[i] <- ( 1 - 2 * pnorm( x[i], mean, intervals[[1]]$sd ) ) * dnorm( x[i], mean, intervals[[1]]$sd ) +
									2 * pnorm( x[i], mean, intervals[[2]]$sd ) * dnorm( x[i], mean, intervals[[2]]$sd )
				}
				else if ( is.symmetric() )
				{
					# symmetric の場合
					# type 1, type1.type == 2
					results[i] <- 2 * abs( pnorm( x[i], mean, intervals[[1]]$sd ) - 0.5 ) *
											dnorm( x[i], mean, intervals[[1]]$sd ) +
									( 1 - 2 * abs( pnorm( x[i], mean, intervals[[2]]$sd ) - 0.5 ) ) *
													dnorm( x[i], mean, intervals[[2]]$sd )
				}
				else
				{
					# 与えられたX座標 (クォンタイル) を負担する分布を探索
					if ( x[i] == Inf )
					{
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
										p1.conn2 <- pnorm( x.conn[2], mean, d.1$sd )
										p2.conn1 <- pnorm( x.conn[1], mean, d.2$sd )
										results[i] <- ( p1.conn2 - 2 * pnorm( x[i], mean, d.1$sd ) ) /
														( p1.conn2 - d.1$p.conn.next[1] ) * dnorm( x[i], mean, d.1$sd ) +
														( 2 * pnorm( x[i], mean, d.2$sd ) - p2.conn1 ) /
														( d.1$p.conn.next[2] - p2.conn1 ) * dnorm( x[i], mean, d.2$sd )
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
									p1.conn2 <- pnorm( x.conn[2], mean, d.1$sd )
									p2.conn1 <- pnorm( x.conn[1], mean, d.2$sd )
									results[i] <- ( p1.conn2 - 2 * pnorm( x[i], mean, d.1$sd ) ) /
													( p1.conn2 - d.1$p.conn.next[1] ) * dnorm( x[i], mean, d.1$sd ) +
													( 2 * pnorm( x[i], mean, d.2$sd ) - p2.conn1 ) /
													( d.1$p.conn.next[2] - p2.conn1 ) * dnorm( x[i], mean, d.2$sd )
								}
							}
						}
					}
				}
			}

			return ( results )
		},

###############################################################################
# X座標 (クォンタイル) を指定して、確率を取得する
# @param	q						X座標 (クォンタイル) のベクトル
# @param	approximate = FALSE		高速に概算するかどうかのフラグ (TRUE: 線形近似、FALSE: 数値積分)
# @return	確率
###############################################################################
		p = function( q, approximate = FALSE )
		{
			results <- numeric()

			for ( i in 1:length( q ) )
			{
				if ( is.continuous() )
				{
					# continuous の場合
					# type 1, type1.type == 2
					p1 <- pnorm( q[i], mean, intervals[[1]]$sd )
					p2 <- pnorm( q[i], mean, intervals[[2]]$sd )
					results[i] <- ( 1 - p1 ) * p1 + p2 * p2
				}
				else if ( is.symmetric() )
				{
					# symmetric の場合
					# type 1, type1.type == 2
					#	Φ(x) = Φ_2(x) + (Φ_1(x) - Φ_2(x)) * abs( 1 - Φ_1(x) - Φ_2(x) )
					p1 <- pnorm( q[i], mean, intervals[[1]]$sd )
					p2 <- pnorm( q[i], mean, intervals[[2]]$sd )
					results[i] <- p2 + ( p1 - p2 ) * abs( 1 - p1 - p2 )
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
										p1.conn2 <- pnorm( x.conn[2], mean, d.1$sd )
										p2.conn1 <- pnorm( x.conn[1], mean, d.2$sd )
										results[i] <- ( p1.conn2 - pnorm( q[i], mean, d.1$sd ) ) /
														( p1.conn2 - d.1$p.conn.next[1] ) * pnorm( q[i], mean, d.1$sd ) +
														( pnorm( q[i], mean, d.2$sd ) - p2.conn1 ) /
														( d.1$p.conn.next[2] - p2.conn1 ) * pnorm( q[i], mean, d.2$sd )
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
									p1.conn2 <- pnorm( x.conn[2], mean, d.1$sd )
									p2.conn1 <- pnorm( x.conn[1], mean, d.2$sd )
									results[i] <- ( p1.conn2 - pnorm( q[i], mean, d.1$sd ) ) /
													( p1.conn2 - d.1$p.conn.next[1] ) * pnorm( q[i], mean, d.1$sd ) +
													( pnorm( q[i], mean, d.2$sd ) - p2.conn1 ) /
													( d.1$p.conn.next[2] - p2.conn1 ) * pnorm( q[i], mean, d.2$sd )
								}
							}
						}
					}
				}
			}

			return ( results )
		},

###############################################################################
# 確率を指定して、X座標 (クォンタイル) を取得する
# @param	prob					確率のベクトル
# @param	approximate = FALSE		高速に概算するかどうかのフラグ (TRUE: 線形近似、FALSE: 数値積分)
# @return	X座標 (クォンタイル)
###############################################################################
		q = function( prob, approximate = FALSE )
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

				if ( is.continuous() || is.symmetric() )
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
						if ( approximate )
						{
							results[i] <- qnorm( prob[i], mean, intervals[[j]]$sd ) *
												( intervals[[j + 1]]$p.conn.prev[2] - prob[i] ) /
												( intervals[[j + 1]]$p.conn.prev[2] - intervals[[j]]$p.conn.next[1] ) +
											qnorm( prob[i], mean, intervals[[j + 1]]$sd ) *
												( prob[i] - intervals[[j]]$p.conn.next[1] ) /
												( intervals[[j + 1]]$p.conn.prev[2] - intervals[[j]]$p.conn.next[1] )
						}
						else
						{
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
			}

			return ( results )
		},

###############################################################################
# ランダムサンプルを取得する
# @param	n		サンプル数
# @return	ランダムサンプルのベクトル
###############################################################################
		r = function( n )
		{
			return ( q( runif( n, 0, 1 ) ) )
		},

###############################################################################
# 標準偏差を取得する
# @return	標準偏差
###############################################################################
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
)
