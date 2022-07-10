##############################################################################
# 共通関数
# @file			common.R
# @version		0.1.0
# @author		Kimitsuna-Goblin
# @copyright	Copyright (C) 2022 Ura Kimitsuna
# @license		Released under the MIT license.
#				see https://opensource.org/licenses/MIT/
##############################################################################

##############################################################################
# 文字列解釈
# @param	str 	文字列
# @param	... 	eval の引数
# @return	文字列を解釈した結果
##############################################################################
evalparse <- function( str, ... )
{
	return ( eval( parse( text = str ), ... ) )
}

##############################################################################
# 文字列解釈 (グローバルスコープ用)
# @param	str 	文字列
# @param	... 	local の引数
# @return	文字列を解釈した結果
##############################################################################
evalparse.global <- function( str )
{
	return ( eval( parse( text = str ), envir = globalenv() ) )
}


##############################################################################
# paste して print する
# @param	... 	引数
# @return	なし
##############################################################################
pp <- function( ... )
{
	return ( print( paste( ... ) ) )
}

##############################################################################
# 新しいプロット用のデバイスを開く
# @param	なし
# @return	なし
##############################################################################
dp.new <- function()
{
	dev.new()
	return( plot.new() )
}


##############################################################################
# NULL要素リスト作成
# @param	n		要素数
# @return	n個の NULL を要素とするリスト
##############################################################################
null.list <- function( n )
{
	l = list()
	l[n + 1] <- 0
	l[n + 1] <- NULL

	return ( l )
}

##############################################################################
# 表の行を間引く
# @param	table			表
# @param	interval		間引く間隔
# @param	start_row = 1	開始行
# @param	end_row = NULL	最終行 (省略時は table の最終行まで)
# @return	間引いた結果の表
##############################################################################
thinrows <- function( table, interval, start_row = 1, end_row = NULL )
{
	if ( is.null( end_row ) )
	{
		end_row = nrow( table )
	}

	return ( table[ seq( start_row, end_row, interval ), ] )
}

#############################################################################
# 昇順ソート
# @param	data	ソートするデータ
# @param	sorter	ソートのキーとなる項目
# @return	ソート済みデータ
##############################################################################
sort.data <- function( data, sorter )
{
	index <- order( sorter )
	return ( data[index,] )
}

#############################################################################
# 開始項除外
# @param	v		ベクトルデータ
# @param	num = 1 除外する項目数
# @return	残差平方和
##############################################################################
cut.first <- function( v, num = 1 )
{
	return ( v[( num + 1 ):length( v )] )	# num + 1 のカッコは必須
}

#############################################################################
# 最終項除外
# @param	v		ベクトルデータ
# @param	num = 1 除外する項目数
# @return	残差平方和
##############################################################################
cut.last <- function( v, num = 1 )
{
	return ( v[1:( length( v ) - num )] )	# length( v ) - num のカッコは必須
}

#############################################################################
# 正値の最小値取得
# @param	v		数値のベクトルデータ
# @return	0 および負数を除く最小値
##############################################################################
min.positive <- function( v )
{
	return ( min( ifelse ( v <= 0, Inf, v ) ) )
}


#############################################################################
# 度数分布のベクトルから外れ値を除外する
# @param	param					パラメータのベクトル (値はすべて数値であること)
# @param	freq					度数のベクトル (length は param と同じであること)
# @param	param.name = "param"	パラメータ名
# @param	freq.name = "param"		度数名
# @param	CT = NULL				パラメータの代表値 (NULL の場合は内部で平均値を使う)
# @return	外れ値を除外した list( param, freq )
##############################################################################
remove.outlier <- function( param, freq, param.name = "param", freq.name = "freq", CT = NULL )
{
	# CT が NULL の場合は平均値を使う
	if ( is.null( NULL ) )
	{
		CT <- sum( param * freq ) / sum( freq )
	}

	# ベクトルのトレース
	i.min <- 0
	i.max <- 0
	prev.diff.to.CT <- -CT
	is.after.CT <- FALSE
	i <- 1
	while ( i <= length( param ) )
	{
		now.diff.to.CT <- param[i] - CT
		if ( ( prev.diff.to.CT * now.diff.to.CT ) <= 0 )
		{
			# CTの部分を過ぎた (あるいは、今がちょうどCT)
			is.after.CT <- TRUE
		}

		if ( freq[i] <= 0 )
		{
			if ( is.after.CT )
			{
				# CTを過ぎた後に度数が 0 になった
				# 範囲取得終了 (これ以降の度数が 0以上 の部分は外れ値)
				i.max <- i - 1
				i <- length( param )
			}
			else
			{
				# CTを過ぎる前に度数が 0 になった
				i.min <- 0
			}
		}
		else
		{
			if ( !is.after.CT )
			{
				# CTを過ぎる前に度数が 0以上 になった
				if ( i.min == 0 )
				{
					i.min <- i
				}
			}
		}

		prev.diff.to.CT <- now.diff.to.CT
		i <- i + 1
	}
	if ( i.max == 0 )
	{
		i.max <- length( param )
	}

	# 名前付け
	result = list( param[i.min:i.max], freq[i.min:i.max] )
	names( result ) <- c( param.name, freq.name )

	return ( result )
}


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

###############################################################################
# 与えられた1点 (平均値からの相対座標) を通る確率密度関数を持つ正規分布の標準偏差を得る
# @param	a		平均値からの相対位置(σ単位)
# @param	d		その点における確率密度
# @return	標準偏差 (2個以下の実数)
###############################################################################
sd.rd.norm <- function( a, d )
{
	# σ = exp( -a^2 / 2 ) / ( sqrt( 2 * pi ) * density )
	return ( exp( -a^2 / 2 ) / ( sqrt( 2 * pi ) * d ) )
}

###############################################################################
# 与えられた平均値を取り、1点 (絶対座標) を通る確率密度関数を持つ正規分布の標準偏差を得る
# @param	mean							平均値
# @param	q  								X座標 (クォンタイル)
# @param	d								その点における確率密度
# @param	interval = c( 1e-14, 1e+14 )	探索範囲の下限と上限
# @return	標準偏差 (2個以下の実数)
###############################################################################
sd.mqd.norm <- function( mean, q, d, interval = c( 1e-14, 1e+14 ) )
{
	result.sd <- numeric()

	# 引数エラーチェック
	if ( d <= 0 )
	{
		stop( "Error: density must be positive." )
	}

	if ( interval[1] < 0 || interval[2] < 0 )
	{
		stop( "Error: each values of interval must be positive or zero." )
	}
	else if ( interval[1] > interval[2] )
	{
		a <- interval[2]
		interval[2] <- interval[1]
		interval[1] <- a
	}

	# 確率密度の最大値は 1 / ( abs( X - mean ) * sqrt( 2 * pi * e )
	sd.d.max <- abs( q - mean )
	d.max <- 1 / ( sd.d.max * sqrt( 2 * pi * exp( 1 ) ) )
	if ( d > d.max )
	{
		stop( "Error: density is too large." )
	}

	# まず、標準偏差が初等的に求まるか調べる (X が平均値 or 変曲点)
	# これらの場合、解の標準偏差は1個しかない

	# X が平均値と等しい場合 sd = 1 / sqrt( 2 * pi * Y )
	if ( q == mean )
	{
		result.sd <- 1 / sqrt( 2 * pi * d )
	}

	# X が変曲点になる場合 sd = abs( X - mean ), Y = max( dnorm )
	if ( d == d.max )
	{
		result.sd <- sd.d.max
	}

	# 標準偏差が初等的に求まった場合、結果を返す
	if ( length( result.sd ) > 0 )
	{
		return ( result.sd )
	}

	# 標準偏差の探索範囲を探索
	f <- function( sd )
	{
		dnorm( q, mean, sd ) - d
	}

	if ( interval[2] < sd.d.max || interval[1] > sd.d.max )
	{
		if ( f( interval[1] ) * f( interval[2] ) < 0 )
		{
			result.sd <- uniroot( f, interval )$root
		}
	}
	else
	{
		if ( f( interval[1] ) * f( sd.d.max ) < 0 )
		{
			result.sd <- uniroot( f, c( interval[1], sd.d.max ) )$root
		}

		if ( f( sd.d.max ) * f( interval[2] ) < 0 )
		{
			result.sd <- c( result.sd, uniroot( f, c( sd.d.max, interval[2] ) )$root )
		}
	}
	if ( length( result.sd ) == 0 )
	{
		warning( "Warning: no S.D. is found (probubly interval is too short)." )
	}

	return ( result.sd )
}

###############################################################################
# 2つのベクトルデータのリスト (2次元配列) のt検定を行う
# @param	list1						ベクトルデータのリスト1
# @param	list2						ベクトルデータのリスト2
# @param	var.equal = FALSE			等分散仮定フラグ
# @param	paired = FALSE				対応性仮定フラグ
# @param	alternative = "two.sided"	対立仮説
# @param	mu = 0						既知の母平均
# @param	conf.level = 0.95			信頼係数
# @return	t検定を行った結果のp値のベクトルデータ
##############################################################################
t.test.forList = function( list1, list2, var.equal = FALSE, paired = FALSE,
							alternative = "two.sided", mu = 0, conf.level = 0.95 )
{
	p.results <- numeric()

	for ( i in 1:min( length( list1 ), length( list2 ) ) )
	{
		t.result <- t.test( list1[[i]], list2[[i]],
							var.equal = var.equal, paired = paired,
							alternative = alternative, mu = mu, conf.level = conf.level )

		p.results <- c( p.results, t.result$p.value )
	}

	return ( p.results )
}


##############################################################################
# 決定係数算出
# @param	samples 	基準データ
# @param	expects 	予測データ
# @return	決定係数 (R2値)
# @remarks	R2値は 1 - Σ_k ( y_k - f_k )^2 / Σ_k ( y_k - y~ )^2 で算出
##############################################################################
get.R2 <- function( samples, expects )
{
	smplMean <- mean( samples )
	return ( 1 - sum( ( samples - expects )^2 )
						/ sum( ( samples - smplMean )^2 ) )
}

##############################################################################
# 自由度調整済み決定係数算出
# @param	samples 	基準データ
# @param	expects 	予測データ
# @param	p = 1		説明変数の数
# @return	自由度調整済み決定係数 (R2値)
# @remarks	R2値は 1 - { Σ_k ( y_k - f_k )^2 / (N - p - 1) }
#						/ { Σ_k ( y_k - y~ )^2 / (N - 1 ) }  で算出
##############################################################################
get.adjustedR2 <- function( samples, expects, p = 1 )
{
	smplMean <- mean( samples )
	a <- length( samples ) - p - 1
	b <- length( samples ) - 1
	return ( 1 - { sum( ( samples - expects )^2 ) / a }
				/ { sum( ( samples - smplMean )^2 ) / b } )
}

#############################################################################
# 残差平方和算出
# @param	x	  データ1
# @param	y	  データ2
# @return	残差平方和
##############################################################################
get.errSumSq <- function( x, y )
{
	return ( sum( ( x - y )^2 ) )
}


###############################################################################
# 立方根計算
# @param	x		実数値
# @return	実数 x の立方根 (実数値) を返す
###############################################################################
cbrt <- function( x )
{
	# 負数^1/3 は直接計算できないので、正負で場合分けして計算する
	return ( ifelse ( x >= 0, x^(1/3), -(-x)^(1/3) ) )
}

###############################################################################
# 三次逆関数構造取得
# @param	coef		三次関数の係数 (a0:定数項 a1:1次項 a2:2次項 a3:3次項)
# @return	三次関数の逆関数のためのカルダノ式のP, Qを得る
###############################################################################
get.Cardano.param <- function( coef )
{
	return ( list( "P" = coef$a1 / coef$a3 - ( coef$a2 / coef$a3 )^2 / 3,
					"Q" = coef$a0 / coef$a3 - coef$a1 * coef$a2 / coef$a3^2 / 3
											+ ( coef$a2 / coef$a3 )^3 * 2 / 27 ) )
}

###############################################################################
# 三次逆関数 (カルダノ式による)
# @param	coef		三次関数の係数 (1:定数項 2:1次項 3:2次項 4:3次項)
# @param	y			三次関数 y = f(x) の y の値 (ベクトル指定可)
# @return	三次関数 y = f(x) の x の値
###############################################################################
solve.三次逆関数 <- function( coef, y )
{
	# y としてスカラー実数値とベクトルが両方指定できるように、
	# リストにしてからサブ関数に渡す
	param <- get.Cardano.param( list( a0 = coef[1] - y, a1 = coef[2], a2 = coef[3], a3 = coef[4] ) )

	return ( cbrt( 0 - param$Q / 2 + sqrt( param$Q^2 / 4 + param$P^3 / 27 ) )
			+ cbrt( 0 - param$Q / 2 - sqrt( param$Q^2 / 4 + param$P^3 / 27 ) )
			- coef[3] / coef[4] / 3 )
}
