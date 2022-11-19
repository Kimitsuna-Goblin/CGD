##############################################################################
# 連結ガウス分布 (Connected Gaussian Distribution) クラス
# @file			CGD.R
# @version		1.3.7
# @author		Kimitsuna-Goblin
# @copyright	Copyright (C) 2022 Ura Kimitsuna
# @license		Released under the MIT license.
#				see https://opensource.org/licenses/MIT/
##############################################################################

#library( nleqslv )

###############################################################################
#  定数

# ルート2π
sqrt.2pi <- sqrt( 2 * pi )

# type1.type=3・確率密度用関数ハンドル
f.t3.d <- list( function( x, m, s ) { ( 1 - dnorm( x, m, s ) * sqrt.2pi * s ) * dnorm( x, m, s ) },
				function( x, m, s ) { dnorm( x, m, s )^2 * sqrt.2pi * s },
				0 )
# type1.type=3・累積分布用関数ハンドル
f.t3.p <- list( function( x, m, s ) { pnorm( x, m, s ) - pnorm( x, m, s * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2 },
				function( x, m, s ) { pnorm( x, m, s * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2 },
				( ( 2 - sqrt( 2 ) ) / 4 ) )

###############################################################################
#  クラス・関数

###############################################################################
#' 正規分布において、与えられた確率が、平均値から何σ離れているかを得る
#' @export
#' @param	p			確率
#' @return	平均値からの相対位置(σ単位)
###############################################################################
sqnorm <- function( p )
{
	return ( qnorm( p, 0, 1 ) )
}

###############################################################################
#' 与えられた平均値を取り、1点における確率を満たす正規分布の標準偏差を得る
#' @export
#' @param	mean		平均値
#' @param	q			X座標 (クォンタイル)
#' @param	p			その点における確率
#' @return	標準偏差
###############################################################################
sd.mqp.norm <- function( mean, q, p )
{
	return ( ( q - mean ) / sqnorm( p ) )
}

###############################################################################
#' 正規分布の分散の閉区間範囲内の値を返す
#' @export
#' @param	x.bound		閉区間を指定する、要素2個のベクトル (x.bound[1] < x.bound[2] であること)
#' @param	mean		正規分布の平均
#' @param	sd			正規分布の標準偏差
#' @param	f.d			正規分布の確率密度を計算する関数 (デフォルト: dnorm( x, mean, sd ) )
#' @param	f.p			正規分布の確率を計算する関数 (デフォルト: pnorm( x, mean, sd ) )
#' @return	与えられた正規分布の分散を閉区間範囲内のみに限って計算した値
###############################################################################
variance.sub <- function( x.bound, mean, sd,
							f.d = function( x, mean, sd ) dnorm( x, mean, sd ),
							f.p = function( x, mean, sd ) pnorm( x, mean, sd ) )
{
 	f <- ifelse( is.infinite( x.bound ), 0, ( x.bound - mean ) * f.d( x.bound, mean, sd ) )

	return ( sd^2 * ( f.p( x.bound[2], mean, sd ) - f.p( x.bound[1], mean, sd ) - f[2] + f[1] ) )
}

###############################################################################
# 二分法により方程式を解く
# @param	f			解を探索する関数
# @param	interval	解を探索する範囲のベクトル
# @param	tol			許容する誤差 (デフォルト: .Machine$double.eps * 16)
# @return	方程式 f = 0 の解
###############################################################################
bisection <- function( f, interval, tol = .Machine$double.eps * 16 )
{
	ans.1 <- f( interval[1] )
	ans.2 <- f( interval[2] )
	if ( abs( ans.1 ) < tol || abs( ans.2 ) < tol )
	{
		return ( ifelse( abs( ans.1 ) < abs( ans.2 ), interval[1], interval[2] ) )
	}

	if ( ans.1 * ans.2 > 0 )
	{
		stop( "Error: f() values at end points not of opposite sign." )
	}

	if ( ans.1 > 0 )
	{
		a <- interval[1]
		interval[1] <- interval[2]
		interval[2] <- a
	}

	return ( bisection.sub( f, interval, tol ) )
}

bisection.sub <- function( f, interval, tol )
{
	mid <- ( interval[1] + interval[2] ) / 2
	ans <- f( mid )

	if ( abs( ans ) < tol )
	{
		return ( mid )
	}
	else if ( ans > 0 )
	{
		bisection.sub( f, c( interval[1], mid ), tol )
	}
	else
	{
		bisection.sub( f, c( mid, interval[2] ), tol )
	}
}

###############################################################################
#' 連結ガウス分布区間クラス
#'
#' 連結ガウス分布クラス (CGD) で使用する区間を表すクラス
#' @export		CGDInterval
#' @exportClass	CGDInterval
#' @field	mean			平均値
#' @field	sd				標準偏差
#' @field	q.ind			独立区間に対するX座標 (クォンタイル)
#' @field	q.conn.prev		前の区間との接続区間の確率内に収まる、この標準偏差のX座標 (クォンタイル)
#' @field	q.conn.next		次の区間との接続区間の確率内に収まる、この標準偏差のX座標 (クォンタイル)
#' @field	p.ind			独立区間
#' @field	p.conn.prev		前の区間との接続区間
#' @field	p.conn.next		次の区間との接続区間
#' @seealso	[CGD-class]
###############################################################################
CGDInterval <- setRefClass(

	# クラス名
	Class = "CGDInterval",

	# フィールド
	fields = list(
		mean = "numeric",			# 平均値
		sd = "numeric",				# 標準偏差
		q.ind = "vector",			# 独立区間に対するX座標 (クォンタイル)
		q.conn.prev = "vector",		# 前の区間との接続区間の確率内に収まる、この標準偏差のX座標 (クォンタイル)
		q.conn.next = "vector",		# 次の区間との接続区間の確率内に収まる、この標準偏差のX座標 (クォンタイル)
		p.ind = "vector",			# 独立区間
		p.conn.prev = "vector",		# 前の区間との接続区間
		p.conn.next = "vector"		# 次の区間との接続区間
	)
)

# メソッド
###############################################################################
#' 負担区間取得
#'
#' 負担区間 (前の区間との接続区間～次の区間との接続区間) に対するX座標 (クォンタイル) を取得する
#' @name CGDInterval_q.manage
#' @usage	a <- CGD$new()
#'			a$intervals[[1]]$q.manage()
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
#' クォンタイルが6点以下の場合は、
#' type1.type の設定を適切に与えることにより、
#' クォンタイルの位置が過度にいびつでなければ、
#' 確率密度関数が連続で滑らかな分布を構成することも可能である。
#'
#' 確率密度関数が不連続な連結ガウス分布を使って滑らかなグラフを得るには、
#' ランダムサンプルを取得して、そのヒストグラムを描画するとよい。
#' @export		CGD
#' @exportClass	CGD
#' @field	mean			平均値
#' @field	intervals		連結区間 (CGDInterval クラスのリスト)
#' @field	type1.type		接続区間が type 1 の場合の計算方法 (詳細は [CGD$set.waypoints] を参照)
#' @field	m.sd			計算済みの標準偏差 (クラス外からは直接参照しないこと)
#' @seealso	[CGDInterval-class]
#' @seealso	[CGD_set.waypoints()]
#' @seealso	\href{https://github.com/Kimitsuna-Goblin/CGD}{README.md} (GitHub)
###############################################################################
CGD <- setRefClass(

	# クラス名
	Class = "CGD",

	# フィールド
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
#' 詳細については、 [CGD$set.waypoints] を参照。
#' @name	CGD_initialize
#' @usage	a <- CGD$new(mean = NULL, intervals = NULL, type1.type = 1)
#' @param	mean			平均値 (デフォルト: NULL)
#' @param	intervals		連結区間 (CGDInterval クラスのリスト) (デフォルト: NULL)
#' @param	type1.type		type 1 の場合の計算方法 (1、2、3 のいずれかを指定する) (デフォルト: 1)
#' @seealso	[CGDInterval-class]
#' @seealso	[CGD_set.waypoints()]
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
			intervals <<- c( CGDInterval$new(	mean = ifelse( is.null( mean ), 0, mean ),
												sd = 1,
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
#' @usage	a <- CGD$new()
#'			a$set.waypoints(waypoints, continuous = FALSE, symmetric = FALSE, uni.sigma = FALSE, this.type1.type = NULL)
#' @param	waypoints			経路の data.frame( q = 経路のX座標 (クォンタイル), p = その点における確率 )
#'								X座標 (クォンタイル) は昇順にソートしておくこと。
#'								平均値は p = 0.5 の点のX座標として与えること
#'								 (type1.type = 2 で continuous = TRUE、または、type1.type = 3 の場合を除き、p = 0.5 の点は必須)
#' @param	continuous			独立区間を [0, 0] と [1, 1] の2点にして、
#'								確率密度関数が全区間 (-∞, ∞) で連続になるように分布構成を試みる。
#'								type1.type = 1, 2 で有効 (type1.type >= 3 では常に連続) (デフォルト: FALSE)
#' @param	symmetric			独立区間を [0, 0], [0.5, 0.5], [1, 1] の3点にして、
#'								1番目と3番目の確率分布を同一にすることにより、
#'								確率密度関数が全区間 (-∞, ∞) で連続で、かつ左右対称になるように試みる。
#'								type1.type = 1, 2, 3 で有効 (デフォルト: FALSE)
#' @param	uni.sigma			type1.type = 2、continuous = TRUE で、かつ、経路の構成点が3点の場合に、
#'								2つの確率密度関数 f_1(x) と f_2(x) の標準偏差を同じ値に揃えるかどうかのフラグ。
#'								このフラグが TRUE のとき、通常、 f_1(x) と f_2(x) の平均値は等しくならない (デフォルト: FALSE)
#' @param	this.type1.type		フィールドの type1.type に設定する値 (1、2、3 のいずれかを指定すること)。
#'								NULL の場合は type1.type の値を変更しない (デフォルト: NULL)
#'
#'				type1.type = 1 は最も単純な連結である。 continuous = TRUE にすると、2つの正規分布の平均が構成可能である。
#'
#'				type1.type = 2 は continuous = TRUE にすると、2つの確率密度関数の横方向グラデーションが構成可能である。
#'
#'				type1.type = 3 は symmetric = TRUE にすると、2つの確率密度関数の縦方向グラデーションが構成可能である。
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
#'				3: Ψ(x) = ∫_{-∞}^{min( x, μ_1 )} ( 1 - f_1(t) / f_1(μ_1) ) f_1(t) dt
#'							+ ∫_{-∞}^x f_2(t)^2 / f_2(μ) dt
#'							+ ∫_{min( x, μ_3 )}^x ( 1 - f_3(t) / f_3(μ_3) ) f_3(t) dt
#'						 = min( Φ_1(x) - Φ^*_1(x) / √2, ( 2 - √2 ) / 4 )
#'							+ Φ^*_2(x) / √2
#'							+ max( 0, Φ_3(x) - Φ^*_3(x) / √2 - ( 2 - √2 ) / 4 )
#'
#'				ただし、Φ_i, Φ_{i+1} は当該接続区間の前後の独立区間を負担する正規分布の累積分布関数。
#'				Φ~_i(x) = ( Φ_i(x) + Φ_{i+1}(x) ) / 2。
#'				f_i, f_{i+1} は当該接続区間の前後の独立区間を負担する正規分布の確率密度関数。μ は平均値。
#'				Φ^*_i は正規分布 N(μ_i, (σ_i / √2)^2) の累積分布関数。
#'
#'				type1.type = 1 では、
#'				set.waypoints() の引数で continuous または symmetric を TRUE にした場合 (2つの正規分布の平均)、
#'				独立区間は [0, 0], [1, 1] の2点になり、
#'				接続区間の累積分布関数は Ψ(x) = ( Φ_i(x) + Φ_{i+1}(x) ) / 2 となる。
#'
#'				type1.type = 2 では、
#'				set.waypoints() の引数で continuous を TRUE にした場合 (横方向グラデーション)、
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
#'				set.waypoints() の引数で symmetric を TRUE にした場合 (縦方向グラデーション)、
#'				独立区間は [0, 0], [0.5, 0.5], [1, 1] の3点になり、
#'				接続区間 (0, 0.5), (0.5, 1) の累積分布関数はどちらも同じ、上の 3 の式になる。
#'
#'				continuous = TRUE or symmetric = TRUE or type1.type = 3 の場合、経路の構成点は一定の個数でなければならない。
#'				具体的には、構成点の個数は以下のようにする必要がある (個数に (*) が付いているものは、確率 0.5 の点が必須)。
#'
#'				・continuous = TRUE :	type1.type = 1 ⇒ 3点(*)、
#'										type1.type = 2 ⇒ 3点(*) or 4点
#'
#'				・symmetric = TRUE :	type1.type = 1 or 2 ⇒ 3点(*)
#'
#'				・type1.type = 3 :		symmetric = FALSE ⇒ 3点(*) or 4点 or 5点(*) or 6点、
#'										symmetric = TRUE ⇒ 3点(*)
#'
#'				個数に (*) が付いていないものは、構成点の半数が確率 0.5 未満、半数が 0.5 超であることが望ましい。
#'				ただし、type1.type = 3 で 4点経路の場合は、構成点がどちらか一方に偏っていてもよい。
#' @return	nleqslv() を内部で実行した場合はその結果。それ以外は NULL
#' @importFrom	nleqslv		nleqslv
#' @seealso	\href{https://github.com/Kimitsuna-Goblin/CGD}{README.md} (GitHub)
###############################################################################
NULL
CGD$methods(
	set.waypoints = function( waypoints, continuous = FALSE, symmetric = FALSE, uni.sigma = FALSE, this.type1.type = NULL )
	{
		result <- NULL

		# メンバ変数を初期化
		intervals <<- list()
		m.sd <<- -Inf

		if ( !is.null( this.type1.type ) )
		{
			if ( this.type1.type == 1 || this.type1.type == 2 || this.type1.type == 3 )
			{
				type1.type <<- this.type1.type
			}
			else
			{
				stop( paste( "Error: type1.type" , this.type1.type, "is undefined." ) )
			}
		}

		wp <- data.frame( q = numeric(), p = numeric() )	# 平均値を除き、昇順に並べた経路

		# 平均値および、平均値を除いた経路の data.frame を取得
		j <- 1
		is.set.mean <- FALSE
		wp.order <- order( waypoints$p )
		for ( i in 1:nrow( waypoints ) )
		{
			oi <- wp.order[i]
			if ( i > 1 )
			{
				# X座標が確率に対して昇順に並んでいなければエラー
				if ( waypoints[wp.order[i - 1],]$q >= waypoints[oi,]$q )
				{
					stop( "Error: order of q is not along with that of p." )
				}
			}

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

				j <- j + 1
			}
		}
		if ( !is.set.mean )
		{
			if ( ( type1.type == 2 && continuous && nrow( wp ) == 4 ) ||
					( type1.type == 3 && !symmetric && ( nrow( wp ) == 4 || nrow( wp ) == 6 ) ) )
			{
				if ( wp$p[nrow( wp ) / 2] > 0.5 || wp$p[nrow( wp ) / 2 + 1] < 0.5 )
				{
					stop( "Error: half of p for waypoints must be less than 0.5 and the others must be greater than 0.5." )
				}
			}
			else
			{
				stop( "Error: q for mean (p = 0.5) is not given." )
			}
		}
		if ( nrow( wp ) == 0 )
		{
			warning( "Warning: no waypoints other than (p = 0, 0.5, 1) are given." )
		}

		if ( continuous && !( ( type1.type == 1 && nrow( wp ) == 2 ) ||
								( type1.type == 2 && ( ( nrow( wp ) == 2 && is.set.mean ) ||
														( nrow( wp ) == 4 && !is.set.mean ) ) ) ) )
		{
			stop( "Error: illegal number of waypoints or illegal type1.type for continuous = TRUE." )
		}

		if ( symmetric && nrow( wp ) != 2 )
		{
			stop( "Error: illegal number of waypoints or illegal type1.type for symmetric = TRUE." )
		}

		if ( type1.type == 3 && !( ( nrow( wp ) > 1 && nrow( wp ) < 5 && is.set.mean ) ||
									( ( nrow( wp ) == 4 || nrow( wp ) == 6 ) && !is.set.mean ) ) )
		{
			stop( "Error: illegal waypoints for type1.type == 3." )
		}

		####################################
		# 確率密度関数が連続な分布を構成

		#  nleqslv では、反復計算中に標準偏差が負値になって警告が出るのを防ぐために、標準偏差を2乗して計算する
		#	(2乗した方が、収束も1乗より少し速くなるようだ)

		# 引数と type1.type と経路上の点の数の条件が満たされる場合、確率密度関数がなるべく連続になるように試みる
		if ( type1.type == 1 && ( continuous || symmetric ) && nrow( wp ) == 2 )
		{
			sds <- c( 0.9, 1.1 )
			e <- try( result <- nleqslv( sds, f <- function( x )
													{
														c(	( pnorm( wp$q[1], mean, x[1]^2 ) +
																pnorm( wp$q[1], mean, x[2]^2 ) ) / 2 - wp$p[1],
															( pnorm( wp$q[2], mean, x[1]^2 ) +
																pnorm( wp$q[2], mean, x[2]^2 ) ) / 2 - wp$p[2] )
													} ), silent = TRUE )
			if ( class( e ) == "try-error" )
			{
				stop( "Error: failed to make up a continuous probability density function." )
			}
			else if ( result$termcd == 1 )
			{
				intervals <<- list( CGDInterval$new(
										mean = mean,
										sd = result$x[1]^2,
										q.ind = c( -Inf, -Inf ), q.conn.prev = c( -Inf, -Inf ), q.conn.next = c( -Inf, Inf ),
										p.ind = c( 0, 0 ), p.conn.prev = c( 0, 0 ), p.conn.next = c( 0, 1 ) ),
									CGDInterval$new(
										mean = mean,
										sd = result$x[2]^2,
										q.ind = c( Inf, Inf ), q.conn.prev = c( -Inf, Inf ), q.conn.next = c( Inf, Inf ),
										p.ind = c( 1, 1 ), p.conn.prev = c( 0, 1 ), p.conn.next = c( 1, 1 ) ) )
			}
			else
			{
				message( paste( "nleqslv is failed. message:", result$message ) )
				stop( "Error: failed to make up a continuous probability density function." )
			}
		}
		else if ( type1.type == 2 && continuous )
		{
			if ( nrow( wp ) == 2 )
			{
				if ( uni.sigma )
				{
					# 標準偏差を同一にして、平均を変えて連結

					# 仮の標準偏差を計算してから nleqslv を実行する
					sd.pseudo <- ( sd.mqp.norm( mean, wp$q[1], wp$p[1] ) + sd.mqp.norm( mean, wp$q[2], wp$p[2] ) ) * 15 / 32

					# nleqslv の計算
					e <- try( result <- nleqslv( c( 0, sqrt( sd.pseudo ) ),
													f <- function( x )
															{
																p1 <- pnorm( wp$q, mean - x[1], x[2]^2 )
																p2 <- pnorm( wp$q, mean + x[1], x[2]^2 )

																return ( p1 - ( p1 * p1 - p2 * p2 ) / 2 - wp$p )
															} ), silent = TRUE )
					if ( class( e ) == "try-error" )
					{
						stop( "Error: failed to make up a continuous probability density function." )
					}
					else if ( result$termcd == 1 )
					{
						intervals <<- list( CGDInterval$new(
												mean = mean - result$x[1],
												sd = result$x[2]^2,
												q.ind = c( -Inf, -Inf ),
												q.conn.prev = c( -Inf, -Inf ), q.conn.next = c( -Inf, Inf ),
												p.ind = c( 0, 0 ),
												p.conn.prev = c( 0, 0 ), p.conn.next = c( 0, 1 ) ),
											CGDInterval$new(
												mean = mean + result$x[1],
												sd = result$x[2]^2,
												q.ind = c( Inf, Inf ),
												q.conn.prev = c( -Inf, Inf ), q.conn.next = c( Inf, Inf ),
												p.ind = c( 1, 1 ),
												p.conn.prev = c( 0, 1 ), p.conn.next = c( 1, 1 ) ) )
					}
					else
					{
						message( paste( "nleqslv is failed. message:", result$message ) )
						stop( "Error: failed to make up a continuous probability density function." )
					}
				}
				else
				{
					# 平均値を同一にして、標準偏差を変えて連結
					sds <- c( 1, 1 )
					e <- try( result <- nleqslv( sds, f <- function( x )
															{
																p1 <- pnorm( wp$q, mean, x[1]^2 )
																p2 <- pnorm( wp$q, mean, x[2]^2 )

																return ( p1 - ( p1 * p1 - p2 * p2 ) / 2 - wp$p )
															} ), silent = TRUE )
					if ( class( e ) == "try-error" )
					{
						stop( "Error: failed to make up a continuous probability density function." )
					}
					else if ( result$termcd == 1 )
					{
						intervals <<- list( CGDInterval$new(
												mean = mean,
												sd = result$x[1]^2,
												q.ind = c( -Inf, -Inf ),
												q.conn.prev = c( -Inf, -Inf ), q.conn.next = c( -Inf, Inf ),
												p.ind = c( 0, 0 ),
												p.conn.prev = c( 0, 0 ), p.conn.next = c( 0, 1 ) ),
											CGDInterval$new(
												mean = mean,
												sd = result$x[2]^2,
												q.ind = c( Inf, Inf ),
												q.conn.prev = c( -Inf, Inf ), q.conn.next = c( Inf, Inf ),
												p.ind = c( 1, 1 ),
												p.conn.prev = c( 0, 1 ), p.conn.next = c( 1, 1 ) ) )
					}
					else
					{
						message( paste( "nleqslv is failed. message:", result$message ) )
						stop( "Error: failed to make up a continuous probability density function." )
					}
				}
			}
			else if ( nrow( wp ) == 4 )
			{
				# 平均値の指定なし・標準偏差を変えて連結

				# 仮の平均値と標準偏差を計算してから nleqslv を実行する
				mean.pseudo <- c(	( sqnorm( wp$p[2] ) * wp$q[1] - sqnorm( wp$p[1] ) * wp$q[2] ) /
										( sqnorm( wp$p[2] ) - sqnorm( wp$p[1] ) ),
									( sqnorm( wp$p[4] ) * wp$q[3] - sqnorm( wp$p[3] ) * wp$q[4] ) /
										( sqnorm( wp$p[4] ) - sqnorm( wp$p[3] ) ) )
				sd.pseudo <- c( ( wp$q[2] - wp$q[1] ) / ( sqnorm( wp$p[2] ) - sqnorm( wp$p[1] ) ),
								( wp$q[4] - wp$q[3] ) / ( sqnorm( wp$p[4] ) - sqnorm( wp$p[3] ) ) )

				# nleqslv の計算
  				e <- try( result <- nleqslv( c( mean.pseudo, sqrt( sd.pseudo ) ),
												f <- function( x )
														{
															p1 <- pnorm( wp$q, x[1], x[3]^2 )
															p2 <- pnorm( wp$q, x[2], x[4]^2 )

															return ( p1 - ( p1 * p1 - p2 * p2 ) / 2 - wp$p )
														} ), silent = TRUE )
				if ( class( e ) == "try-error" )
				{
					stop( "Error: failed to make up a continuous probability density function." )
				}
				else if ( result$termcd == 1 )
				{
					result.sd <- c( result$x[3]^2, result$x[4]^2 )

					intervals <<- list( CGDInterval$new(
											mean = result$x[1],
											sd = result.sd[1],
											q.ind = c( -Inf, -Inf ),
											q.conn.prev = c( -Inf, -Inf ), q.conn.next = c( -Inf, Inf ),
											p.ind = c( 0, 0 ),
											p.conn.prev = c( 0, 0 ), p.conn.next = c( 0, 1 ) ),
										CGDInterval$new(
											mean = result$x[2],
											sd = result.sd[2],
											q.ind = c( Inf, Inf ),
											q.conn.prev = c( -Inf, Inf ), q.conn.next = c( Inf, Inf ),
											p.ind = c( 1, 1 ),
											p.conn.prev = c( 0, 1 ), p.conn.next = c( 1, 1 ) ) )

					mean <<- bisection( function( x ) { p( x ) - 0.5 }, c( result$x[1], result$x[2] ) )
				}
				else
				{
					message( paste( "nleqslv is failed. message:", result$message ) )
					stop( "Error: failed to make up a continuous probability density function." )
				}
			}
		}
		else if ( type1.type == 2 && symmetric && nrow( wp ) == 2 )
		{
			# symmetric の場合、高速化のために、経路の確率をすべて 0.5 以下にそろえて計算する
			wp$p <- 0.5 - abs( 0.5 - wp$p )
			wp$q <- mean - abs( mean - wp$q )

			sds <- c( 1, 1 )
			e <- try( result <- nleqslv( sds, f <- function( x )
													{
														p1 <- pnorm( wp$q, mean, x[1]^2 )
														p2 <- pnorm( wp$q, mean, x[2]^2 )

														return( ( 1 - p1 ) * p1 + p2 * p2 - wp$p )
													} ), silent = TRUE )
			if ( class( e ) == "try-error" )
			{
				stop( "Error: failed to make up a symmetric probability density function." )
			}
			else if ( result$termcd == 1 )
			{
				intervals <<- list( CGDInterval$new(
										mean = mean,
										sd = result$x[1]^2,
										q.ind = c( -Inf, -Inf ),
										q.conn.prev = c( -Inf, -Inf ), q.conn.next = c( -Inf, mean ),
										p.ind = c( 0, 0 ),
										p.conn.prev = c( 0, 0 ), p.conn.next = c( 0, 0.5 ) ),
									CGDInterval$new(
										mean = mean,
										sd = result$x[2]^2,
										q.ind = c( mean, mean ),
										q.conn.prev = c( -Inf, mean ), q.conn.next = c( mean, Inf ),
										p.ind = c( 0.5, 0.5 ),
										p.conn.prev = c( 0, 0.5 ), p.conn.next = c( 0.5, 1 ) ),
									CGDInterval$new(
										mean = mean,
										sd = result$x[1]^2,
										q.ind = c( Inf, Inf ),
										q.conn.prev = c( mean, Inf ), q.conn.next= c( Inf, Inf ),
										p.ind = c( 1, 1 ),
										p.conn.prev = c( 0.5, 1 ), p.conn.next = c( 1, 1 ) ) )
			}
			else
			{
				message( paste( "nleqslv is failed. message:", result$message ) )
				stop( "Error: failed to make up a symmetric probability density function." )
			}
		}
		else if ( type1.type == 3 && symmetric && nrow( wp ) == 2 )
		{
			sds <- c( 1, 1 )
			e <- try( result <- nleqslv( sds, f <- function( x )
													{
														p <- pnorm( wp$q, mean, x[1]^2 )
														p.a1 <- pnorm( wp$q, mean, x[1]^2 * sqrt( 2 ) / 2 )
														p.a2 <- pnorm( wp$q, mean, x[2]^2 * sqrt( 2 ) / 2 )

														return ( p - p.a1 * sqrt( 2 ) / 2 + p.a2 * sqrt( 2 ) / 2 - wp$p )
													} ), silent = TRUE )
			if ( class( e ) == "try-error" )
			{
				stop( "Error: failed to make up a symmetric probability density function." )
			}
			else if ( result$termcd == 1 )
			{
				intervals <<- gen.t3.intervals( rep( mean, 3 ), c( result$x[1]^2, result$x[2]^2, result$x[1]^2 ) )
			}
			else
			{
				message( paste( "nleqslv is failed. message:", result$message ) )
				stop( "Error: failed to make up a symmetric probability density function." )
			}
		}
		else if ( type1.type == 3 && !symmetric && ( nrow( wp ) == 2 || nrow( wp ) == 3 ) )
		{
			# type1.type == 3、非対称、平均値同一
			# 評価のため、経路を p < 0.5 と p > 0.5 に分ける
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
				# 一方の経路に3点以上ある場合、平均値指定なしの4点経路と同じ方法で累積分布関数を構成
				l <- t3.wp4.intervals( data.frame( q = c( wp$q, mean ), p = c( wp$p, 0.5 ) ) )
				intervals <<- l$intervals
				result <- l$result

				if ( intervals[[1]]$mean == intervals[[2]]$mean )
				{
					mean <<- intervals[[2]]$mean
				}
				else
				{
					mean <<- bisection( function( x ) { p( x ) - 0.5 }, c( intervals[[1]]$mean, intervals[[2]]$mean ) )
				}
			}
			else
			{
				# 場合分けして累積分布関数を構成
				if ( j.lower == 2 && j.upper == 2 )
				{
					# ( #lower, #upper ) = ( 1, 1 )
					sds <- c( 1, 1 )
					e <- try( result <- nleqslv( sds, f <- function( x )
															{
																x.ave <- ( x[1]^2 + x[2]^2 ) / 2
																p <- pnorm( wp$q, mean, x^2 )
																p.a1 <- pnorm( wp$q, mean, x^2 * sqrt( 2 ) / 2 )
																p.a2 <- pnorm( wp$q, mean, x.ave * sqrt( 2 ) / 2 )

																return ( p - p.a1 * sqrt( 2 ) / 2 + p.a2 * sqrt( 2 ) / 2 - wp$p )
															} ), silent = TRUE )
					if ( class( e ) == "try-error" )
					{
						stop( "Error: failed to make up a probability density function." )
					}
					else if ( result$termcd == 1 )
					{
						# 成功
						sds[1] <- result$x[1]^2
						sds[2] <- ( result$x[1]^2 + result$x[2]^2 ) / 2
						sds[3] <- result$x[2]^2
					}
					else
					{
						stop( "Error: failed to make up a probability density function." )
					}
				}
				else
				{
					# ( #lower, #upper ) = ( 2, 0 ) or ( 0, 2 ) or ( 2, 1 ) or ( 1, 2 )
					# ( #lower == 2 and ( #upper == 0 or 1 (i.e. any) ) ) || ( #lower == 0 and #upper == 2 ) ⇒ wp.lower = 1
					wp.lower <- ifelse( ( j.lower == 3 || j.lower == 1 ), 1, 2 )
					wp.upper <- ifelse( ( j.lower == 3 || j.lower == 1 ), 2, 3 )
					wp.q <- c( wp$q[wp.lower], wp$q[wp.upper] )
					wp.p <- c( wp$p[wp.lower], wp$p[wp.upper] )
					xi.outer <- ifelse( j.lower == 3, 1, 2 )
					xi.inner <- ifelse( j.lower == 3, 2, 1 )
					sds <- c( 1, 1 )
					e <- try( result <- nleqslv( sds, f <- function( x )
															{
																p <- pnorm( wp.q, mean, x[xi.outer]^2 )
																p.a1 <- pnorm( wp.q, mean, x[xi.outer]^2 * sqrt( 2 ) / 2 )
																p.a2 <- pnorm( wp.q, mean, x[xi.inner]^2 * sqrt( 2 ) / 2 )

																return ( p - p.a1 * sqrt( 2 ) / 2 + p.a2 * sqrt( 2 ) / 2 - wp.p )
															} ), silent = TRUE )
					if ( class( e ) == "try-error" )
					{
						stop( "Error: failed to make up a probability density function." )
					}
					else if ( result$termcd == 1 )
					{
						if ( j.upper == 1 || j.lower == 1 )
						{
							# ( #lower, #upper ) = ( 2, 0 ) or ( 0, 2 )
							sds[1] <- result$x[1]^2
							sds[2] <- result$x[xi.inner]^2
							sds[3] <- result$x[2]^2
						}
						else
						{
							# ( #lower, #upper ) = ( 2, 1 ) or ( 1, 2 )
							# 残りの1つの標準偏差を算出
							if ( j.lower == 3 )
							{
								sds[1] <- result$x[xi.outer]^2
								sds[2] <- result$x[xi.inner]^2

								i.last <- 3
							}
							else
							{
								sds[2] <- result$x[xi.inner]^2
								sds[3] <- result$x[xi.outer]^2

								i.last <- 1
							}

							# 標準偏差の探索範囲決定 ( sd / sd.pseudo  は sd.pseudo/sds[2] と 1 の間にある)
							sd.pseudo <- sd.mqp.norm( mean, wp$q[i.last], wp$p[i.last] )
							if ( sd.pseudo > sds[2] )
							{
								# * 2, / 2 は本当は不要だけど計算誤差回避のために付けている
								sd.max <- sd.pseudo^2 / sds[2] * 2
								sd.min <- sd.pseudo / 2
							}
							else if ( sd.pseudo < sds[2] )
							{
								# * 2, / 2 は本当は不要だけど計算誤差回避のために付けている
								sd.max <- sd.pseudo * 2
								sd.min <- sd.pseudo^2 / sds[2] / 2
							}
							else # if ( sd.pseudo == sds[2] )
							{
								sds[i.last] <- result$x[xi.inner]^2
								sd.max <- NULL
								sd.min <- NULL
							}

							if ( !is.null( sd.max ) )
							{
								e <- try( result <- bisection( function( x )
																{
																	pnorm( wp$q[i.last], mean, x ) -
																	pnorm( wp$q[i.last], mean, x * sqrt( 2 ) / 2 ) *
																		sqrt( 2 ) / 2 +
																	pnorm( wp$q[i.last], mean, sds[2] * sqrt( 2 ) / 2 ) *
																		sqrt( 2 ) / 2 -
																	wp$p[i.last]
																}, c( sd.min, sd.max ) ), silent = TRUE )
								if ( class( e ) == "try-error" )
								{
									stop( "Error: failed to make up a probability density function." )
								}
								else
								{
									sds[i.last] <- result
								}
							}
						}
					}
					else
					{
						message( paste( "nleqslv is failed. message:", result$message ) )
						stop( "Error: failed to make up a probability density function." )
					}
				}

				intervals <<- gen.t3.intervals( rep( mean, 3 ), sds )

				if ( intervals[[1]]$mean == intervals[[2]]$mean )
				{
					mean <<- intervals[[2]]$mean
				}
				else
				{
					mean <<- bisection( function( x ) { p( x ) - 0.5 }, c( intervals[[1]]$mean, intervals[[2]]$mean ) )
				}
			}
		}
		else if ( type1.type == 3 && !symmetric && !is.set.mean && nrow( wp ) == 4 )
		{
			# type1.type == 3、4点経路、平均値指定なし
			l <- t3.wp4.intervals( wp )
			intervals <<- l$intervals
			result <- l$result

			mean <<- bisection( function( x ) { p( x ) - 0.5 }, c( intervals[[1]]$mean, intervals[[2]]$mean ) )
		}
		else if ( type1.type == 3 && !symmetric && ( ( is.set.mean && nrow( wp ) == 4 ) || nrow( wp ) == 6 ) )
		{
			# type1.type == 3、非対称、平均値変動

			# 仮の平均値と標準偏差を計算してから nleqslv を実行する
			mean.pseudo <- c( ( sqnorm( wp$p[2] ) * wp$q[1] - sqnorm( wp$p[1] ) * wp$q[2] ) /
									( sqnorm( wp$p[2] ) - sqnorm( wp$p[1] ) ),
								ifelse( nrow( wp ) == 4, mean,
										( sqnorm( wp$p[4] ) * wp$q[3] - sqnorm( wp$p[3] ) * wp$q[4] ) /
										( sqnorm( wp$p[4] ) - sqnorm( wp$p[3] ) ) ),
								( sqnorm( wp$p[nrow( wp )] ) * wp$q[nrow( wp ) - 1] -
									sqnorm( wp$p[nrow( wp ) - 1] ) * wp$q[nrow( wp )] ) /
									( sqnorm( wp$p[nrow( wp )] ) - sqnorm( wp$p[nrow( wp ) - 1] ) ) )
			sd.pseudo <- c( ( wp$q[2] - wp$q[1] ) / ( sqnorm( wp$p[2] ) - sqnorm( wp$p[1] ) ),
							( wp$q[4] - wp$q[3] ) / ( sqnorm( wp$p[4] ) - sqnorm( wp$p[3] ) ),
							( wp$q[nrow( wp )] - wp$q[nrow( wp ) - 1] ) /
								( sqnorm( wp$p[nrow( wp )] ) - sqnorm( wp$p[nrow( wp ) - 1] ) ) )

			# nleqslv の計算
			if ( nrow( wp ) == 4 )
			{
				e <- try( result <- nleqslv( c( mean.pseudo, sqrt( sd.pseudo[1] ), sqrt( sd.pseudo[3] ) ),
												f <- function( x )
														{
															means <- c( x[1], x[2], x[3] )
															sds <- c( x[4]^2, t3.wp5.mid.sd( x[2], wp[2,], wp[3,] ), x[5]^2 )

															c(	dp.t3( wp$q[1], means, sds, f.t3.p ) - wp$p[1],
																dp.t3( wp$q[2], means, sds, f.t3.p ) - wp$p[2],
																dp.t3( mean,	means, sds, f.t3.p ) - 0.5,
																dp.t3( wp$q[3], means, sds, f.t3.p ) - wp$p[3],
																dp.t3( wp$q[4], means, sds, f.t3.p ) - wp$p[4] )
														} ), silent = TRUE )
			}
			else # if ( nrow( wp ) == 6 )
			{
				e <- try( result <- nleqslv( c( mean.pseudo, sqrt( sd.pseudo ) ),
												f <- function( x )
														{
															means <- c( x[1], x[2], x[3] )
															sds <- c( x[4]^2, x[5]^2, x[6]^2 )

															c(	dp.t3( wp$q[1], means, sds, f.t3.p ) - wp$p[1],
																dp.t3( wp$q[2], means, sds, f.t3.p ) - wp$p[2],
																dp.t3( wp$q[3], means, sds, f.t3.p ) - wp$p[3],
																dp.t3( wp$q[4], means, sds, f.t3.p ) - wp$p[4],
																dp.t3( wp$q[5], means, sds, f.t3.p ) - wp$p[5],
																dp.t3( wp$q[6], means, sds, f.t3.p ) - wp$p[6] )
														} ), silent = TRUE )
			}

			if ( class( e ) == "try-error" )
			{
				stop( "Error: failed to make up a continuous probability density function." )
			}
			else if ( result$termcd == 1 )
			{
				means <- c( result$x[1], result$x[2], result$x[3] )
 				sds <-c( result$x[4]^2,
							ifelse( nrow( wp ) == 4,
									t3.wp5.mid.sd( result$x[2], wp[2,], wp[3,] ), result$x[5]^2 ),
							result$x[length( result$x )]^2 )

				intervals <<- gen.t3.intervals( means, sds )

				if ( means[1] == means[2] && means[2] == means[3] )
				{
					mean <<- means[2]
				}
				else
				{
					mean <<- bisection( function( x ) { p( x ) - 0.5 }, c( min( means ), max( means ) ) )
				}
			}
			else
			{
				message( paste( "nleqslv is failed. message:", result$message ) )
				stop( "Error: failed to make up a continuous probability density function." )
			}
		}

		if ( length( intervals ) > 0 )
		{
			return ( result )
		}

		####################################
		# 確率密度関数が非連続な分布を構成

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
				q.conn.prev <- c( qnorm( intervals[[length( intervals )]]$p.conn.next[1], mean, sds[i] ), wp[i,]$q )

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
						p.ind[2] <- wp[j,]$p
						q.ind[2] <- qnorm( wp[j,]$p, mean, sds[i] )
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
				p.conn.next <- c( wp[i,]$p, wp[i + 1,]$p )
				q.conn.next <- c( qnorm( wp[i,]$p, mean, sds[i] ), qnorm( wp[i + 1,]$p, mean, sds[i] ) )

				# 独立区間の終点は次の区間との接続区間の始点
				p.ind[2] <- p.conn.next[1]
				q.ind[2] <- q.conn.next[1]
			}

			# 連結区間クラスのインスタンス生成
			intervals <<- c( intervals, CGDInterval$new(
											mean = mean,
											sd = sds[i],
											q.ind = q.ind, q.conn.prev = q.conn.prev, q.conn.next = q.conn.next,
											p.ind = p.ind, p.conn.prev = p.conn.prev, p.conn.next = p.conn.next ) )
			i <- i + 1
		}

		return ( result )
	}
)

###############################################################################
# type1.type=3 の確率密度関数または累積分布関数の値を得る
# @param	x			X座標 (クォンタイル)
# @param	means		平均値のベクトル
# @param	sds			標準偏差のベクトル
# @param	f.t3		確率密度 / 累積分布用関数ハンドル ( f.t3.d / f.t3.p )
# @return	確率密度関数または累積分布関数の値
###############################################################################
dp.t3 <- function( x, means, sds, f.t3 )
{
	result <- f.t3[[2]]( x, means[2], sds[2] )

	if ( x < means[1] )
	{
		result <- result + f.t3[[1]]( x, means[1], sds[1] )
	}
	else
	{
		result <- result + f.t3[[3]]
	}

	if ( x > means[3] )
	{
		result <- result + f.t3[[1]]( x, means[3], sds[3] ) - f.t3[[3]]
	}

	return ( result )
}

###############################################################################
# type1.type=3, 4点経路で、平均値指定なし または 経路の点が平均値以上/以下に
# 偏っている場合の intervals を導出する
# @param	wp			経路
# @return	intervals, result (nleqslv の結果) のリスト
###############################################################################
t3.wp4.intervals <- function( wp )
{
	if ( nrow( wp ) != 4 )
	{
		stop( paste( "Error: nrow( wp ) must be 4 for t3.wp4.intervals( wp ). the nrow: ", nrow( wp ) ) )
	}

	# 平均値から遠い2点を仮に1番目の累積分布関数、平均値から近い2点を仮に2番目の累積分布関数で通過させ、
	# 2つの累積分布関数を縦方向にグラデーションさせる

	# 平均値から遠い2点と近い2点を調べる
	d.mean <- data.frame( i = 1:4, d = abs( 0.5 - wp$p ) )
	max.i <- 0
	max.d <- -1
	min.i <- 0
	min.d <- Inf
	for ( i in 1:4 )
	{
		if ( max.d < d.mean$d[i] )
		{
			max.i <- i
			max.d <- d.mean$d[i]
		}

		if ( min.d > d.mean$d[i] )
		{
			min.i <- i
			min.d <- d.mean$d[i]
		}
	}

	d.mean <- d.mean[-max.i,]
	if ( max.i < min.i )
	{
		d.mean <- d.mean[-( min.i - 1 ),]
	}
	else
	{
		d.mean <- d.mean[-min.i,]
	}

	if ( d.mean$d[1] < d.mean$d[2] )
	{
		wp.far <- data.frame( q = c( wp$q[max.i], wp$q[d.mean$i[2]] ), p = c( wp$p[max.i], wp$p[d.mean$i[2]] ) )
		wp.near <- data.frame( q = c( wp$q[min.i], wp$q[d.mean$i[1]] ), p = c( wp$p[min.i], wp$p[d.mean$i[1]] ) )
	}
	else
	{
		wp.far <- data.frame( q = c( wp$q[max.i], wp$q[d.mean$i[1]] ), p = c( wp$p[max.i], wp$p[d.mean$i[1]] ) )
		wp.near <- data.frame( q = c( wp$q[min.i], wp$q[d.mean$i[2]] ), p = c( wp$p[min.i], wp$p[d.mean$i[2]] ) )
	}

	# 仮の平均値・標準偏差を求める
	mean.pseudo <- c( ( sqnorm( wp.far$p[2] ) * wp.far$q[1] - sqnorm( wp.far$p[1] ) * wp.far$q[2] ) /
						( sqnorm( wp.far$p[2] ) - sqnorm( wp.far$p[1] ) ),
					( sqnorm( wp.near$p[2] ) * wp.near$q[1] - sqnorm( wp.near$p[1] ) * wp.near$q[2] ) /
						( sqnorm( wp.near$p[2] ) - sqnorm( wp.near$p[1] ) ) )
	sd.pseudo <- c( ( wp.far$q[2] - wp.far$q[1] ) / ( sqnorm( wp.far$p[2] ) - sqnorm( wp.far$p[1] ) ),
					( wp.near$q[2] - wp.near$q[1] ) / ( sqnorm( wp.near$p[2] ) - sqnorm( wp.near$p[1] ) ) )

	# nleqslv の計算
	e <- try( result <- nleqslv( c( mean.pseudo, sqrt( sd.pseudo ) ),
									f <- function( x )
											{
												p <- pnorm( wp$q, x[1], x[3]^2 )
												p.a1 <- pnorm( wp$q, x[1], x[3]^2 * sqrt( 2 ) / 2 )
												p.a2 <- pnorm( wp$q, x[2], x[4]^2 * sqrt( 2 ) / 2 )

												return ( p - p.a1 * sqrt( 2 ) / 2 + p.a2 * sqrt( 2 ) / 2 - wp$p )
											} ), silent = TRUE )
	if ( class( e ) == "try-error" )
	{
		stop( "Error: failed to make up a continuous probability density function." )
	}
	else if ( result$termcd == 1 )
	{
		means <- c( result$x[1], result$x[2], result$x[1] )
		sds <- c( result$x[3]^2, result$x[4]^2, result$x[3]^2 )
	}
	else
	{
		message( paste( "nleqslv is failed. message:", result$message ) )
		stop( "Error: failed to make up a continuous probability density function." )
	}

	return ( list( intervals = gen.t3.intervals( means, sds ), result = result ) )
}

###############################################################################
# type1.type=3 の intervals を生成する
# @param	means		平均値のベクトル
# @param	sds			標準偏差のベクトル
# @return	生成した intervals
###############################################################################
gen.t3.intervals <- function( means, sds )
{
	if ( means[1] == means[2] && means[2] == means[3] )
	{
		ps <- c( 0.5, 0.5, 0.5 )
	}
	else
	{
		ps <- c( dp.t3( means[1], means, sds, f.t3.p ),
					dp.t3( means[2], means, sds, f.t3.p ),
					dp.t3( means[3], means, sds, f.t3.p ) )
	}

	return ( list( CGDInterval$new(
						mean = means[1],
						sd = sds[1],
						q.ind = c( -Inf, -Inf ),
						q.conn.prev = c( -Inf, -Inf ), q.conn.next = c( -Inf, means[1] ),
						p.ind = c( 0, 0 ),
						p.conn.prev = c( 0, 0 ), p.conn.next = c( 0, max( ps[1], ps[2] ) ) ),
					CGDInterval$new(
						mean = means[2],
						sd = sds[2],
						q.ind = c( means[2], means[2] ),
						q.conn.prev = c( -Inf, max( qnorm( ps[1], means[2], sds[2] ), means[2] ) ),
						q.conn.next = c( min( means[2], qnorm( ps[3], means[2], sds[2] ) ), Inf ),
						p.ind = c( ps[2], ps[2] ),
						p.conn.prev = c( 0, max( ps[1], ps[2] ) ), p.conn.next = c( min( ps[2], ps[3] ), 1 ) ),
					CGDInterval$new(
						mean = means[3],
						sd = sds[3],
						q.ind = c( Inf, Inf ),
						q.conn.prev = c( means[3], Inf ), q.conn.next = c( Inf, Inf ),
						p.ind = c( 1, 1 ),
						p.conn.prev = c( min( ps[2], ps[3] ), 1 ), p.conn.next = c( 1, 1 ) ) ) )
}

###############################################################################
# type1.type=3, 5点経路 の場合の、中央 (2番目) の確率分布の標準偏差を計算する
# @param	mean2		中央の確率分布の平均値
# @param	wp2			経路の2点目のX座標 (クォンタイル)
# @param	wp3			経路の4点目 (平均値を除けば3点目) のX座標 (クォンタイル)
# @return	中央の確率分布の標準偏差
###############################################################################
t3.wp5.mid.sd <- function( mean2, wp2, wp3 )
{
	sd1 <- sd.mqp.norm( mean2, wp2$q, wp2$p )
	sd2 <- sd.mqp.norm( mean2, wp3$q, wp3$p )

	return ( ( sd1 * abs( mean2 - wp3$p ) + sd2 * abs( mean2 - wp2$p ) ) / ( abs( mean2 - wp2$p ) + abs( mean2 - wp3$p ) ) )
}

###############################################################################
#' continuous 判定
#'
#' continuous かどうかを調べる
#' @usage	a <- CGD$new()
#'			a$is.continuous()
#' @name	CGD_is.continuous
#' @return	continuous = TRUE として構成されていれば TRUE、そうでなければ FALSE
###############################################################################
NULL
CGD$methods(
	is.continuous = function()
	{
		return ( type1.type <= 2 && intervals[[1]]$p.conn.next[1] == 0 && intervals[[1]]$p.conn.next[2] == 1 )
	}
)

###############################################################################
#' symmetric 判定
#'
#' symmetric かどうかを調べる
#' @name	CGD_is.symmetric
#' @usage	a <- CGD$new()
#'			a$is.symmetric()
#' @return	symmetric = TRUE として構成されていれば TRUE、そうでなければ FALSE
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
#' uni.sigma 判定
#'
#' uni.sigma かどうかを調べる
#' @name	CGD_is.uni.sigma
#' @usage	a <- CGD$new()
#'			a$is.uni.sigma()
#' @return	uni.sigma = TRUE として構成されていれば TRUE、そうでなければ FALSE
###############################################################################
NULL
CGD$methods(
	is.uni.sigma = function()
	{
		result <- FALSE

		if ( type1.type == 2 && is.continuous() )
		{
			result <- ( intervals[[1]]$sd == intervals[[2]]$sd )
		}

		return ( result )
	}
)

###############################################################################
#' uni.mean 判定
#'
#' uni.mean (分布を構成するすべての正規分布の平均値が等しい) かどうかを調べる
#' @name	CGD_is.uni.mean
#' @usage	a <- CGD$new()
#'			a$is.uni.mean()
#' @return	構成するすべての正規分布の平均値が等しければ TRUE、そうでなければ FALSE
###############################################################################
NULL
CGD$methods(
	is.uni.mean = function()
	{
		result <- TRUE

		for ( i in length( intervals ) )
		{
			if ( intervals[[i]]$mean != mean )
			{
				result <- FALSE
				break
			}
		}

		return ( result )
	}
)

###############################################################################
#' 独立区間判定 (確率)
#'
#' 指定された確率が独立区間に入っているかどうかを調べる
#' @name	CGD_is.ind.p
#' @usage	a <- CGD$new()
#'			a$is.ind.p(p)
#' @param	p		確率のベクトル
#' @return	第1要素 (bool) に、独立区間に入っていれば TRUE、入っていなければ FALSE。
#'			第2要素 (i) に、当該 intervals のインデックス (bool = FALSE の場合は NaN) が入ったリスト
###############################################################################
NULL
CGD$methods(
	is.ind.p = function( p )
	{
		bool <- rep( FALSE, length( p ) )
		i.result <- rep( NaN, length( p ) )

		for ( i in 1:length( p ) )
		{
			if ( p[i] < 0 || p[i] > 1 )
			{
				# 確率が負または1を超えている
				warning( paste( "Warning: probability" , p[i] , "is out of range [0, 1]." ) )
				next
			}

			for ( j in 1:length( intervals ) )
			{
				if ( p[i] >= intervals[[j]]$p.ind[1] && p[i] <= intervals[[j]]$p.ind[2] )
				{
					bool[i] <- TRUE
					i.result[i] <- j
					break
				}
			}
		}

		return ( list( bool = bool, i = i.result ) )
	}
)

###############################################################################
#' 接続区間判定 (確率)
#'
#' 指定された確率が接続区間に入っているかどうかを調べる
#' @name	CGD_is.conn.p
#' @usage	a <- CGD$new()
#'			a$is.conn.p(p)
#' @param	p		確率のベクトル
#' @return	第1要素 (bool) に、接続区間に入っていれば TRUE、入っていなければ FALSE
#'			 (type1.type = 3 では、2番目の独立区間が接続区間と互いに重なることがあるが、
#'			  指定された確率が独立区間と接続区間の両方に入っているときは、 FALSE を返す)。
#'			第2要素 (i.1) に、前の intervals のインデックス
#'			 (bool = FALSE の場合は 独立区間の intervals のインデックス)。
#'			第3要素 (i.2) に、後の intervals のインデックス (i.1 + 1, bool = FALSE の場合は NaN) が入ったリスト
###############################################################################
NULL
CGD$methods(
	is.conn.p = function( p )
	{
		bool <- rep( FALSE, length( p ) )
		i.1 <- rep( NaN, length( p ) )
		i.2 <- rep( NaN, length( p ) )

		for ( i in 1:length( p ) )
		{
			if ( p[i] < 0 || p[i] > 1 )
			{
				# 確率が負または1を超えている
				warning( paste( "Warning: probability" , p[i] , "is out of range [0, 1]." ) )
				next
			}

			is.ind <- is.ind.p( p[i] )
			if ( !is.ind$bool )
			{
				bool[i] <- TRUE
				for ( j in 1:( length( intervals ) - 1 ) )
				{
					# 論理的には = は不要だが、計算誤差回避のために = を付けておく
					if ( p[i] >= intervals[[j]]$p.ind[2] && p[i] <= intervals[[j + 1]]$p.ind[1] )
					{
						i.1[i] <- j
						i.2[i] <- j + 1
						break
					}
				}
			}
			else
			{
				i.1[i] <- is.ind$i
			}
		}

		return ( list( bool = bool, i.1 = i.1, i.2 = i.2 ) )
	}
)

###############################################################################
#' 独立区間判定 (X座標 (クォンタイル) )
#'
#' 指定されたX座標 (クォンタイル) が独立区間に入っているかどうかを調べる
#' @name	CGD_is.ind.x
#' @usage	a <- CGD$new()
#'			a$is.ind.x(x)
#' @param	x		X座標 (クォンタイル) のベクトル
#' @return	第1要素 (bool) に、独立区間に入っていれば TRUE、入っていなければ FALSE。
#'			第2要素 (i) に、当該 intervals のインデックス (bool = FALSE の場合は NaN) が入ったリスト
###############################################################################
NULL
CGD$methods(
	is.ind.x = function( x )
	{
		bool <- rep( FALSE, length( x ) )
		i.result <- rep( NaN, length( x ) )

		for ( i in 1:length( x ) )
		{
			for ( j in 1:length( intervals ) )
			{
				if ( x[i] >= intervals[[j]]$q.ind[1] && x[i] <= intervals[[j]]$q.ind[2] )
				{
					bool[i] <- TRUE
					i.result[i] <- j
					break
				}
			}
		}

		return ( list( bool = bool, i = i.result ) )
	}
)

###############################################################################
#' 接続区間判定 (X座標 (クォンタイル) )
#'
#' 指定されたX座標 (クォンタイル) が接続区間に入っているかどうかを調べる
#' @name	CGD_is.conn.x
#' @usage	a <- CGD$new()
#'			a$is.conn.x(x)
#' @param	x		X座標 (クォンタイル) のベクトル
#' @return	第1要素 (bool) に、接続区間に入っていれば TRUE、入っていなければ FALSE
#'			 (type1.type = 3 では、2番目の独立区間が接続区間と互いに重なることがあるが、
#'			  指定された座標が独立区間と接続区間の両方に入っているときは、 FALSE を返す)。
#'			第2要素 (i.1) に、前の intervals のインデックス
#'			 (bool = FALSE の場合は 独立区間の intervals のインデックス)。
#'			第3要素 (i.2) に、後の intervals のインデックス (i.1 + 1, bool = FALSE の場合は NaN) が入ったリスト
###############################################################################
NULL
CGD$methods(
	is.conn.x = function( x )
	{
		bool <- rep( FALSE, length( x ) )
		i.1 <- rep( NaN, length( x ) )
		i.2 <- rep( NaN, length( x ) )

		for ( i in 1:length( x ) )
		{
			is.ind <- is.ind.x( x[i] )
			if ( !is.ind$bool )
			{
				bool[i] <- TRUE
				for ( j in 1:( length( intervals ) - 1 ) )
				{
					# 論理的には = は不要だが、計算誤差回避のために = を付けておく
					if ( x[i] >= intervals[[j]]$q.ind[2] && x[i] <= intervals[[j + 1]]$q.ind[1] )
					{
						i.1[i] <- j
						i.2[i] <- j + 1
						break
					}
				}
			}
			else
			{
				i.1[i] <- is.ind$i
			}
		}

		return ( list( bool = bool, i.1 = i.1, i.2 = i.2 ) )
	}
)

###############################################################################
#' 独立区間判定 (X座標 (クォンタイル) )
#'
#' 指定されたX座標 (クォンタイル) が独立区間に入っているかどうかを調べる
#' @name	CGD_is.ind.q
#' @usage	a <- CGD$new()
#'			a$is.ind.q(q)
#' @param	q		X座標 (クォンタイル) のベクトル
#' @return	第1要素 (bool) に、独立区間に入っていれば TRUE、入っていなければ FALSE。
#'			第2要素 (i) に、当該 intervals のインデックス (bool = FALSE の場合は NaN) が入ったリスト
###############################################################################
NULL
CGD$methods(
	is.ind.q = function( q )
	{
		return ( is.ind.x( q ) )
	}
)

###############################################################################
#' 接続区間判定 (X座標 (クォンタイル) )
#'
#' 指定されたX座標 (クォンタイル) が接続区間に入っているかどうかを調べる
#' @name	CGD_is.conn.q
#' @usage	a <- CGD$new()
#'			a$is.conn.q(q)
#' @param	q		X座標 (クォンタイル) のベクトル
#' @return	第1要素 (bool) に、接続区間に入っていれば TRUE、入っていなければ FALSE。
#'			第2要素 (i.1) に、前の intervals のインデックス
#'			 (bool = FALSE の場合は 独立区間の intervals のインデックス)。
#'			第3要素 (i.2) に、後の intervals のインデックス (i.1 + 1, bool = FALSE の場合は NaN) が入ったリスト
###############################################################################
NULL
CGD$methods(
	is.conn.q = function( q )
	{
		return ( is.conn.x( q ) )
	}
)

###############################################################################
#' 確率密度取得
#'
#' X座標 (クォンタイル) を指定して、確率密度を取得する
#' @name	CGD_d
#' @usage	a <- CGD$new()
#'			a$d(x)
#' @param	x		X座標 (クォンタイル) のベクトル
#' @return	確率密度
###############################################################################
NULL
CGD$methods(
	d = function( x )
	{
		results <- numeric()

		if ( type1.type == 2 && is.symmetric() )
		{
			x <- mean - abs( mean - x )
		}

		for ( i in 1:length( x ) )
		{
			if ( type1.type == 3 )
			{
				# type1.type == 3 の場合の計算は symmetric であってもなくても同じ

				results[i] <- dp.t3( x[i], c( intervals[[1]]$mean, intervals[[2]]$mean, intervals[[3]]$mean ),
											c( intervals[[1]]$sd, intervals[[2]]$sd, intervals[[3]]$sd ), f.t3.d )
			}
			else if ( is.continuous() )
			{
				# continuous の場合
				if ( type1.type == 1 )
				{
					results[i] <- ( dnorm( x[i], intervals[[1]]$mean, intervals[[1]]$sd ) +
									dnorm( x[i], intervals[[2]]$mean, intervals[[2]]$sd ) ) / 2
				}
				else # if ( type1.type == 2 )
				{
					results[i] <- ( 1 - pnorm( x[i], intervals[[1]]$mean, intervals[[1]]$sd ) ) *
										dnorm( x[i], intervals[[1]]$mean, intervals[[1]]$sd ) +
									pnorm( x[i], intervals[[2]]$mean, intervals[[2]]$sd ) *
									dnorm( x[i], intervals[[2]]$mean, intervals[[2]]$sd )
				}
			}
			else if ( is.symmetric() )
			{
				# symmetric の場合
				# type1.type == 2 ( type1.type == 1 は is.continuous() == TRUE になる)
				results[i] <- ( 1 - 2 * pnorm( x[i], intervals[[1]]$mean, intervals[[1]]$sd ) ) *
										dnorm( x[i], intervals[[1]]$mean, intervals[[1]]$sd ) +
									2 * pnorm( x[i], intervals[[1]]$mean, intervals[[2]]$sd ) *
										dnorm( x[i], intervals[[1]]$mean, intervals[[2]]$sd )
			}
			else
			{
				# 確率密度関数が非連続の場合
				is.conn <- is.conn.x( x[i] )
				if ( !is.conn$bool )
				{
					# 独立区間内 ⇒ 確率密度をそのまま出力
					results[i] <- dnorm( x[i], intervals[[is.conn$i.1]]$mean, intervals[[is.conn$i.1]]$sd )
				}
				else
				{
					# 接続区間内 ⇒ 区間前後の2つの分布で確率密度を負担
					j <- is.conn$i.1
					if ( x[i] > intervals[[j]]$q.manage()[2] && x[i] < intervals[[j + 1]]$q.manage()[1] )
					{
						# 負担分布なし ⇒ d( x ) = 0 (このケースは type 2 の場合に生じる)
						results[i] <- 0
						next
					}

					d.1 <- intervals[[j]]			# 確率分布1 (接続区間前を負担)
					d.2 <- intervals[[j + 1]]		# 確率分布2 (接続区間後を負担)
					x.conn <- c( d.1$q.conn.next[1], d.2$q.conn.prev[2] )
													# 接続区間の範囲のX座標 (クォンタイル)

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
									results[i] <- ( dnorm( x[i], mean, d.1$sd ) * ( x.conn[2] - x[i] ) +
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
										# P1(x) < sup ⇒ 確率分布1 が単独で負担
										results[i] <- dnorm( x[i], mean, d.1$sd ) / 2
									}
									else
									{
										# P1(x) >= sup ⇒ 負担分布なし
										results[i] <- 0
									}
								}
								else
								{
									# P2(x) >= inf
									if ( x[i] < d.1$q.conn.next[2] )
									{
										# P1(x) < sup ⇒ 確率分布1 と 2 が負担
										results[i] <- ( dnorm( x[i], mean, d.1$sd ) + dnorm( x[i], mean, d.2$sd ) ) / 2
									}
									else
									{
										# P1(x) >= sup ⇒ 確率分布2 が単独で負担
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
										# P1(x) < sup ⇒ 確率分布1 と 2 が負担
										results[i] <- ( dnorm( x[i], mean, d.1$sd ) + dnorm( x[i], mean, d.2$sd ) ) / 2
									}
									else
									{
										# P1(x) >= sup ⇒ 確率分布2 が単独で負担
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
										# P2(x) < inf ⇒ 確率分布1 が単独で負担
										results[i] <- dnorm( x[i], mean, d.1$sd ) / 2
									}
									else
									{
										# P2(x) >= inf ⇒ 確率分布1 と 2 が負担
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
									# P1(x) < sup ⇒ 確率分布1 が単独で負担
									results[i] <- dnorm( x[i], mean, d.1$sd ) / 2
								}
								else
								{
									# P1(x) >= sup ⇒ 負担分布なし
									results[i] <- 0
								}
							}
							else
							{
								# P2(x) >= inf
								if ( x[i] < d.1$q.conn.next[2] )
								{
									# P1(x) < sup ⇒ 確率分布1 と 2 が負担
									results[i] <- ( dnorm( x[i], mean, d.1$sd ) + dnorm( x[i], mean, d.2$sd ) ) / 2
								}
								else
								{
									# P1(x) >= sup ⇒ 確率分布2 が単独で負担
									results[i] <- dnorm( x[i], mean, d.2$sd ) / 2
								}
							}
						}
						else
						{
							# type 1
							if ( type1.type == 1 )
							{
								results[i] <- ( dnorm( x[i], mean, d.1$sd ) * ( x.conn[2] - x[i] ) +
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
#' @usage	a <- CGD$new()
#'			a$p(q)
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

				results[i] <- dp.t3( q[i], c( intervals[[1]]$mean, intervals[[2]]$mean, intervals[[3]]$mean ),
											c( intervals[[1]]$sd, intervals[[2]]$sd, intervals[[3]]$sd ), f.t3.p )
			}
			else if ( is.continuous() )
			{
				# continuous の場合
				if ( type1.type == 1 )
				{
					results[i] <- ( pnorm( q[i], intervals[[1]]$mean, intervals[[1]]$sd ) +
									pnorm( q[i], intervals[[2]]$mean, intervals[[2]]$sd ) ) / 2
				}
				else # if ( type1.type == 2 )
				{
					p1 <- pnorm( q[i], intervals[[1]]$mean, intervals[[1]]$sd )
					p2 <- pnorm( q[i], intervals[[2]]$mean, intervals[[2]]$sd )
					results[i] <- p1 - ( p1 * p1 - p2 * p2 ) / 2
				}
			}
			else if ( is.symmetric() )
			{
				# symmetric の場合
				# type1.type == 2
				if ( q[i] <= mean )
				{
					p1 <- pnorm( q[i], intervals[[1]]$mean, intervals[[1]]$sd )
					p2 <- pnorm( q[i], intervals[[2]]$mean, intervals[[2]]$sd )
					results[i] <- ( 1 - p1 ) * p1 + p2 * p2
				}
				else
				{
					p1 <- pnorm( 2 * intervals[[1]]$mean - q[i], intervals[[1]]$mean, intervals[[1]]$sd )
					p2 <- pnorm( 2 * intervals[[2]]$mean - q[i], intervals[[2]]$mean, intervals[[2]]$sd )
					results[i] <- 1 - ( 1 - p1 ) * p1 - p2 * p2
				}
			}
			else
			{
				# 確率密度関数が非連続の場合
				is.conn <- is.conn.q( q[i] )
				if ( !is.conn$bool )
				{
					# 独立区間内 ⇒ 確率をそのまま出力
					results[i] <- pnorm( q[i], mean, intervals[[is.conn$i.1]]$sd )
				}
				else
				{
					# 接続区間内 ⇒ 区間前後の2つの分布で確率を負担
					j <- is.conn$i.1
					if ( q[i] > intervals[[j]]$q.manage()[2] && q[i] < intervals[[j + 1]]$q.manage()[1] )
					{
						# 負担分布なし ⇒ p = 直前の分布が負担する確率の上限 = 接続区間の確率の上限と下限の平均値
						results[i] <- ( intervals[[j]]$p.conn.next[1] + intervals[[j]]$p.conn.next[2] ) / 2
						next
					}

					d.1 <- intervals[[j]]			# 確率分布1 (接続区間前を負担)
					d.2 <- intervals[[j + 1]]		# 確率分布2 (接続区間後を負担)
					x.conn <- c( d.1$q.conn.next[1], d.2$q.conn.prev[2] )
													# 接続区間のX座標 (クォンタイル)

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
									results[i] <- ( pnorm( q[i], mean, d.1$sd ) * ( x.conn[2] - q[i] ) +
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
										# P1(x) < sup ⇒ 確率分布1 が単独で負担
										results[i] <- ( d.1$p.conn.next[1] + pnorm( q[i], mean, d.1$sd ) ) / 2
									}
									else
									{
										# P1(x) >= sup ⇒ 負担分布なし
										results[i] <- ( d.1$p.conn.next[1] + d.1$p.conn.next[2] ) / 2
									}
								}
								else
								{
									# P2(x) >= inf
									if ( q[i] < d.1$q.conn.next[2] )
									{
										# P1(x) < sup ⇒ 確率分布1 と 2 が負担
										results[i] <- ( pnorm( q[i], mean, d.1$sd ) + pnorm( q[i], mean, d.2$sd ) ) / 2
									}
									else
									{
										# P1(x) >= sup ⇒ 確率分布2 が単独で負担
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
										# P1(x) < sup ⇒ 確率分布1 と 2 が負担
										results[i] <- ( pnorm( q[i], mean, d.1$sd ) + pnorm( q[i], mean, d.2$sd ) ) / 2
									}
									else
									{
										# P1(x) >= sup ⇒ 確率分布2 が単独で負担
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
										# P2(x) < inf ⇒ 確率分布1 が単独で負担
										results[i] <- ( pnorm( q[i], mean, d.1$sd ) + d.2$p.conn.prev[1] ) / 2
									}
									else
									{
										# P2(x) >= inf ⇒ 確率分布1 と 2 が負担
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
									# P1(x) < sup ⇒ 確率分布1 が単独で負担
									results[i] <- ( d.1$p.conn.next[1] + pnorm( q[i], mean, d.1$sd ) ) / 2
								}
								else
								{
									# P1(x) >= sup ⇒ 負担分布なし
									results[i] <- ( d.1$p.conn.next[1] + d.1$p.conn.next[2] ) / 2
								}
							}
							else
							{
								# P2(x) >= inf
								if ( q[i] < d.1$q.conn.next[2] )
								{
									# P1(x) < sup ⇒ 確率分布1 と 2 が負担
									results[i] <- ( pnorm( q[i], mean, d.1$sd ) + pnorm( q[i], mean, d.2$sd ) ) / 2
								}
								else
								{
									# P1(x) >= sup ⇒ 確率分布2 が単独で負担
									results[i] <- ( d.1$p.conn.next[2] + pnorm( q[i], mean, d.2$sd ) ) / 2
								}
							}
						}
						else
						{
							# type 1
							if ( type1.type == 1 )
							{
								results[i] <- ( pnorm( q[i], mean, d.1$sd ) * ( x.conn[2] - q[i] ) +
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
#' @usage	a <- CGD$new()
#'			a$q(prob, tol = .Machine$double.eps * 16)
#' @param	prob					確率のベクトル
#' @param	tol						許容誤差 (デフォルト: .Machine$double.eps * 16)
#' @return	X座標 (クォンタイル)
###############################################################################
NULL
CGD$methods(
	q = function( prob, tol = .Machine$double.eps * 16 )
	{
		results <- numeric()

		for ( i in 1:length( prob ) )
		{
			if ( prob[i] < 0 || prob[i] > 1 )
			{
				# 確率が負または1を超えている
				warning( paste( "Warning: probability" , prob[i] , "is out of range [0, 1]." ) )
				results[i] <- NaN
				next
			}
			else if ( prob[i] == 0 )
			{
				results[i] <- -Inf
				next
			}
			else if ( prob[i] == 1 )
			{
				results[i] <- Inf
				next
			}

			if ( type1.type == 3 )
			{
				if ( prob[i] == 0.5 )
				{
					results[i] <- mean
				}
				else if ( prob[i] < 0.5 )
				{
					results[i] <- bisection( function( x ) { p( x ) - prob[i] },
											c( qnorm( prob[i], mean, intervals[[1]]$sd + intervals[[2]]$sd ), mean ), tol )
				}
				else
				{
					results[i] <- bisection( function( x ) { p( x ) - prob[i] },
											c( mean, qnorm( prob[i], mean, intervals[[3]]$sd + intervals[[2]]$sd ) ), tol )
				}
			}
			else if ( is.continuous() || is.symmetric() )
			{
				# continuous or symmetric ⇒ 累積分布関数は至る所で微分可能なので、場合分けしなくても収束する
				if ( prob[i] == 0.5 )
				{
					results[i] <- mean
				}
				else if ( prob[i] < 0.5 )
				{
					min.q <- min( qnorm( prob[i], intervals[[1]]$mean, intervals[[1]]$sd ),
									qnorm( prob[i], intervals[[2]]$mean, intervals[[2]]$sd ) )
					results[i] <- bisection( function( x ) { p( x ) - prob[i] },
												c( min.q - ( mean - min.q ) * 2, mean ), tol )
				}
				else
				{
					max.q <- max( qnorm( prob[i], intervals[[1]]$mean, intervals[[1]]$sd ),
									qnorm( prob[i], intervals[[2]]$mean, intervals[[2]]$sd ) )
					results[i] <- bisection( function( x ) { p( x ) - prob[i] },
												c( mean, max.q + ( max.q - mean ) * 2 ), tol )
				}
			}
			else
			{
				# 確率密度関数が非連続の場合
				is.conn <- is.conn.p( prob[i] )
				j <- is.conn$i.1
				if ( !is.conn$bool )
				{
					# 独立区間内 ⇒ 確率をそのまま出力
					results[i] <- qnorm( prob[i], mean, intervals[[j]]$sd )
				}
				else
				{
					# 接続区間内 ⇒ 区間前後の2つの分布で確率密度を負担
					if ( intervals[[j]]$p.conn.next[1] < 0.5 && intervals[[j]]$p.conn.next[2] > 0.5 )
					{
						# type 3 ⇒ prob[i] が平均値なら 0.5。また、それより大か小かで値域を絞れる
						if ( prob[i] == 0.5 )
						{
							results[i] <- mean
						}
						else if ( prob[i] < 0.5 )
						{
							results[i] <- bisection( function( x ) { p( x ) - prob[i] },
														c( intervals[[j]]$q.conn.next[1], mean ), tol )
						}
						else
						{
							results[i] <- bisection( function( x ) { p( x ) - prob[i] },
														c( mean, intervals[[j + 1]]$q.conn.prev[2] ), tol )
						}
					}
					else
					{
						if ( intervals[[j]]$sd < intervals[[j + 1]]$sd )
						{
							# type 1 ⇒ 接続区間内は至る所で微分可能なので、場合分けしなくても収束する
							results[i] <- bisection( function( x ) { p( x ) - prob[i] },
														c( intervals[[j]]$q.conn.next[1],
															intervals[[j + 1]]$q.conn.prev[2] ), tol )
						}
						else
						{
							# type 2 ⇒ 場合分けが必要
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
									results[i] <- bisection( function( x ) { p( x ) - prob[i] },
																c( intervals[[j + 1]]$q.conn.prev[1],
																	intervals[[j]]$q.conn.next[2] ), tol )
								}
							}
							else
							{
								results[i] <- bisection( function( x ) { p( x ) - prob[i] },
															c( intervals[[j]]$q.conn.next[1],
																intervals[[j + 1]]$q.conn.prev[2] ), tol )
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
#' @usage	a <- CGD$new()
#'			a$r(n, tol = 2^(-17))
#' @param	n		サンプル数
#' @param	tol		q() に許容する誤差 (デフォルト: 2^(-17))
#' @return	ランダムサンプルのベクトル
###############################################################################
NULL
CGD$methods(
	r = function( n, tol = 2^(-17) )
	{
		return ( q( runif( n, 0, 1 ), tol ) )
	}
)

###############################################################################
#' 標準偏差取得
#'
#' 標準偏差を取得する
#' @name	CGD_sd
#' @usage	a <- CGD$new()
#'			a$sd()
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
		if ( type1.type >= 3 || is.continuous() || is.symmetric() )
		{
			# 確率密度関数が連続
			if ( type1.type == 1 )
			{
				v <- ( intervals[[1]]$sd^2 + intervals[[2]]$sd^2 ) / 2
			}
			else if ( type1.type == 3 && is.uni.mean() )
			{
				v <- ( intervals[[1]]$sd^2 * ( 1 - sqrt( 2 ) / 4 ) +
								intervals[[2]]$sd^2 * sqrt( 2 ) / 2 +
								intervals[[3]]$sd^2 * ( 1 - sqrt( 2 ) / 4 ) ) / 2
			}
			else
			{
				v <- integrate( f <-
								function( x )
								{
									( x - mean )^2 * d( x )
								}, -Inf, Inf )$value
			}
		}
		else
		{
			# 確率密度関数が不連続
			for ( i in 1:length( intervals ) )
			{
				# 独立区間の計算
				v <- v + variance.sub( intervals[[i]]$q.ind, mean, intervals[[i]]$sd )

				if ( i < length( intervals ) )
				{
					# 接続区間の計算
					d.1 <- intervals[[i]]			# 確率分布1 (接続区間前を負担)
					d.2 <- intervals[[i + 1]]		# 確率分布2 (接続区間後を負担)
					x.conn <- c( d.1$q.conn.next[1], d.2$q.conn.prev[2] )

 					if ( ( x.conn[1] < mean && x.conn[2] <= mean && d.2$sd >= d.1$sd ) ||
 							( x.conn[1] >= mean && d.2$sd <= d.1$sd ) )
					{
						# type 1 (数値積分するしかない)
						v <- v + integrate( f <-
											function( x )
											{
												( x - mean )^2 * d( x )
											}, intervals[[i]]$q.ind[2], intervals[[i + 1]]$q.ind[1] )$value
					}
					else if ( ( x.conn[1] < mean && x.conn[2] <= mean && d.2$sd < d.1$sd ) ||
								( x.conn[1] >= mean && d.2$sd > d.1$sd ) )
					{
						# type 2

						# Φ_1(x) < a_{i+1}, Φ_2(x) < b_i
						v <- v + variance.sub( c( x.conn[1], min( d.1$q.conn.next[2], d.2$q.conn.prev[1] ) ),
												mean, d.1$sd ) / 2

						# Φ_1(x) >= a_{i+1}, Φ_2(x) < b_i ⇒ v <- v + 0 (i.e. 処理なし)

						# Φ_1(x) < a_{i+1}, Φ_2(x) >= b_i
						if ( d.1$q.conn.next[2] > d.2$q.conn.prev[1]  )
						{
							v <- v + ( variance.sub( c( d.2$q.conn.prev[1], d.1$q.conn.next[2] ), mean, d.1$sd ) +
										variance.sub( c( d.2$q.conn.prev[1], d.1$q.conn.next[2] ), mean, d.2$sd ) ) / 2
						}

						# Φ_1(x) >= a_{i+1}, Φ_2(x) >= b_i
						v <- v + variance.sub( c( max( d.1$q.conn.next[2], d.2$q.conn.prev[1] ), x.conn[2] ),
												mean, d.2$sd ) / 2
					}
					else if ( d.2$sd >= d.1$sd )
					{
						# type 3a

						# x <= mean
						v <- v + variance.sub( c( x.conn[1], mean ), mean, d.1$sd )

						# x > mean, Φ_1(x) < a_{i+1}
						v <- v + ( variance.sub( c( mean, d.1$q.conn.next[2] ), mean, d.1$sd ) +
										variance.sub( c( mean, d.1$q.conn.next[2] ), mean, d.2$sd ) ) / 2

						# x > mean, Φ_1(x) >= a_{i+1}
						v <- v + variance.sub( c( d.1$q.conn.next[2], x.conn[2] ), mean, d.1$sd ) / 2
					}
					else
					{
						# type 3b

						# x <= mean, Φ_2(x) < b_i
						v <- v + variance.sub( c( x.conn[1], d.2$q.conn.prev[1] ), mean, d.1$sd ) / 2

						# x <= mean, Φ_2(x) >= b_i
						v <- v + ( variance.sub( c( d.2$q.conn.prev[1], mean ), mean, d.1$sd ) +
										variance.sub( c( d.2$q.conn.prev[1], mean ), mean, d.2$sd ) ) / 2

						# x > mean
						v <- v + variance.sub( c( mean, x.conn[2] ), mean, d.2$sd )
					}
				}
			}
		}

		# 標準偏差を出力
		m.sd <<- sqrt( v )

		return ( m.sd )
	}
)
