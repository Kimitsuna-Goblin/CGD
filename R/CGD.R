################################################################################################
# 連結ガウス分布 (Connected Gaussian Distribution) クラス
# @file 		CGD.R
# @version		3.0.0
# @author		Kimitsuna-Goblin
# @copyright	Copyright (C) 2023 Ura Kimitsuna
# @license		Released under the MIT license.
#				see https://opensource.org/licenses/MIT/
################################################################################################

#library( nleqslv )

################################################################################################
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

# 連結ガウス分布の種類 (1～15は連続分布)													# kind.index
kinds <- c( "Normal Distribution",															# 1
			"Mean of Median-Equaled Sigma-Differed 2 Normal Distributions",					# 2
			"Symmetric Horizontal Gradational Distribution",								# 3
			"Median-Differed Sigma-Equaled Horizontal Gradational Distribution",			# 4
			"Median-Equaled Sigma-Differed Horizontal Gradational Distribution",			# 5
			"Median-Differed Sigma-Differed Horizontal Gradational Distribution", 			# 6
			"Median-Differed Sigma-Equaled Vertical Gradational Distribution",				# 7
			"Median-Equaled Sigma-Differed Vertical Gradational Distribution",				# 8
			"Median-Differed Sigma-Differed Vertical Gradational Distribution",				# 9
			"3-Median-Differed Sigma-Equaled Vertical Gradational Distribution",			#10
			"Median-Equaled 3-Sigma-Differed Vertical Gradational Distribution",			#11
			"3-Median-Differed 3-Sigma-Differed Vertical Gradational Distribution",			#12
			"Median-Differed Sigma-Equaled Vertical-Horizontal Gradational Distribution", 	#13
			"Median-Equaled Sigma-Differed Vertical-Horizontal Gradational Distribution", 	#14
			"Median-Differed Sigma-Differed Vertical-Horizontal Gradational Distribution",	#15
			"Discontinuous Connected Gaussian Distribution" )								#16

# TeX 形式 テンプレート (累積分布関数・確率密度関数共通)
tex.form.header <- "\\begin{align}\n"
tex.form.footer <- "\n\\end{align}"

tex.val.sub.0 <-
		paste0(
			"\\\\\n",
			"& \\begin{array}{l}\n",
				"\\mu = mean; & \\sigma = sd;;\n",
			"\\end{array}" )

tex.val.sub.1 <-
		paste0(
			"\\\\\n",
			"& \\begin{array}{l}\n",
				"\\mu = mean; & \\sigma_1 = sd.1; & \\sigma_2 = sd.2;;\n",
			"\\end{array}" )

tex.val.sub.2 <-
		paste0(
			"\\\\\n",
			"& \\begin{array}{l}\n",
				"\\mu_1 = mean.1; & \\sigma_1 = sd.1;\\\\\n",
				"\\mu_2 = mean.2; & \\sigma_2 = sd.2;;\n",
			"\\end{array}" )

tex.val.sub.3 <-
		paste0(
			"\\\\\n",
			"& \\begin{array}{l}\n",
				"\\mu_1 = mean.1; & \\sigma_1 = sd.1;\\\\\n",
				"\\mu_2 = mean.2; & \\sigma_2 = sd.2;\\\\\n",
				"\\mu_3 = mean.3; & \\sigma_3 = sd.3;;\n",
			"\\end{array}" )

tex.val.sub.4 <-
		paste0(
			"\\\\\n",
			"& \\begin{array}{l}\n",
				"\\mu_{1,1} = mean.1.1; & \\sigma_{1,1} = sd.1.1;\\\\\n",
				"\\mu_{1,2} = mean.1.2; & \\sigma_{1,2} = sd.1.2;\\\\\n",
				"\\mu_{2,1} = mean.2.1; & \\sigma_{2,1} = sd.2.1;\\\\\n",
				"\\mu_{2,2} = mean.2.2; & \\sigma_{2,2} = sd.2.2;;\n",
			"\\end{array}" )

# TeX 形式 テンプレート (累積分布関数)
tex.p.sub.0 <-
		paste0(
			"\\Phi(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma^2}} ",
					"\\int_{-\\infty}^{x} \\exp \\left( -\\dfrac{(t - \\mu)^2}{2 \\sigma^2} \\right) dt;\\\\\n" )

tex.p.sub.1 <-
		paste0(
			"\\Phi_i(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma_i^2}} ",
					"\\int_{-\\infty}^{x} \\exp \\left( -\\dfrac{(t - \\mu)^2}{2 \\sigma_i^2} \\right) dt;\\\\\n" )

tex.p.sub.2 <-
		paste0(
			"\\Phi_i(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma_i^2}} ",
					"\\int_{-\\infty}^{x} \\exp \\left( -\\dfrac{(t - \\mu_i)^2}{2 \\sigma_i^2} \\right) dt;\\\\\n" )

tex.p.sub.v <-
		paste0(
			"\\Phi_i(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma_i^2}} ",
					"\\int_{-\\infty}^{x} \\exp \\left( -\\dfrac{(t - \\mu_i)^2}{2 \\sigma_i^2} \\right) dt;\\\\\n",
			"\\Phi^\\ast_i(x) &= \\dfrac{1}",
					"{\\sqrt{2 \\pi \\left( \\begin{array}{c} \\dfrac{\\sigma_i}{\\sqrt{2}} \\end{array} \\right)^2}} ",
					"\\int_{-\\infty}^{x} \\exp \\left( \\begin{array}{c} -\\dfrac{(t - \\mu_i)^2}",
					"{2 \\left( \\begin{array}{c} \\dfrac{\\sigma_i}",
					"{\\sqrt{2}} \\end{array} \\right)^2} \\end{array} \\right) dt;\\\\\n" )

tex.p.sub.3 <- c(
		paste0(
			"\\Psi_1(x) &= \\mathrm{min} \\left( ",
					"\\Phi_1(x) - \\dfrac{1}{\\sqrt{2}} \\Phi^\\ast_1(x), \\ \\dfrac{2 - \\sqrt{2}}{4} \\right);\\\\\n",
			"\\Psi_2(x) &= \\dfrac{1}{\\sqrt{2}} \\Phi^\\ast_2(x);\\\\\n",
			"\\Psi_3(x) &= \\mathrm{max} \\left( ",
					"0, \\ \\Phi_3(x) - \\dfrac{1}{\\sqrt{2}} \\Phi^\\ast_3(x) - \\dfrac{2 - \\sqrt{2}}{4} \\right);\\\\\n" ),
		paste0(
			"\\Phi_i(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma_i^2}} ",
					"\\int_{-\\infty}^{x} \\exp \\left( -\\dfrac{(t - \\mu_i)^2}{2 \\sigma_i^2} \\right) dt;\\\\\n",
			"\\Phi^\\ast_i(x) &= \\dfrac{1}",
					"{\\sqrt{2 \\pi \\left( \\begin{array}{c} \\dfrac{\\sigma_i}{\\sqrt{2}} \\end{array} \\right)^2}} ",
					"\\int_{-\\infty}^{x} \\exp \\left( \\begin{array}{c} -\\dfrac{(t - \\mu_i)^2}",
					"{2 \\left( \\begin{array}{c} \\dfrac{\\sigma_i}",
					"{\\sqrt{2}} \\end{array} \\right)^2} \\end{array} \\right) dt;\\\\\n" ) )

tex.p.sub.4 <- c(
		paste0(
			"\\Psi_i(x) &= \\Phi_{i,1}(x) - ",
					"\\dfrac{1}{\\sqrt{2}} \\Phi^\\ast_{i,1}(x) + \\dfrac{1}{\\sqrt{2}} \\Phi^\\ast_{i,2}(x);\\\\\n" ),
		paste0(
			"\\Phi_{i,j}(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma_{i,j}^2}} ",
					"\\int_{-\\infty}^{x} ",
					"\\exp \\left( -\\dfrac{(t - \\mu_{i,j})^2}{2 \\sigma_{i,j}^2} \\right) dt;\\\\\n",
			"\\Phi^\\ast_{i,j}(x) &= \\dfrac{1}",
					"{\\sqrt{2 \\pi \\left( \\begin{array}{c} \\dfrac{\\sigma_{i,j}}{\\sqrt{2}} \\end{array} \\right)^2}} ",
					"\\int_{-\\infty}^{x} ",
					"\\exp \\left( \\begin{array}{c} -\\dfrac{(t - \\mu_{i,j})^2}",
					"{2 \\left( \\begin{array}{c} \\dfrac{\\sigma_{i,j}}",
					"{\\sqrt{2}} \\end{array} \\right)^2} \\end{array} \\right) dt;\\\\\n" ) )

tex.p.all <-
		c(
		paste0( "\\Psi(x) &= \\Phi(x);\\\\\n" ),
		paste0( "\\Psi(x) &= \\dfrac{1}{2} ( \\Phi_1(x) + \\Phi_2(x) );\\\\\n" ),
		paste0( "\\Psi(x) &= \\left\\lbrace",
						"\\begin{array}{l}\n",
							"\\Phi_1(x) - \\Phi_1(x)^2 + \\Phi_2(x)^2 & (x \\leq \\mu);\\\\\n",
							"1 - (\\Phi_1(2 \\mu - x) - \\Phi_1(2 \\mu - x)^2 + \\Phi_2(2 \\mu - x)^2) & (x > \\mu);\n",
						"\\end{array} \\right.\\\\\n" ),
		rep( paste0( "\\Psi(x) &= \\Phi_1(x) - \\dfrac{1}{2} \\Phi_1(x)^2 + \\dfrac{1}{2} \\Phi_2(x)^2;\\\\\n" ), 3 ),
		rep( paste0( "\\Psi(x) &= \\Phi_1(x) - \\dfrac{1}{\\sqrt{2}} \\Phi^\\ast_1(x) + ",
							"\\dfrac{1}{\\sqrt{2}} \\Phi^\\ast_2(x);\\\\\n" ), 3 ),
		rep( paste0( "\\Psi(x) &= \\Psi_1(x) + \\Psi_2(x) + \\Psi_3(x);\\\\\n" ), 3 ),
		rep( paste0( "\\Psi(x) &= \\Psi_1(x) - \\dfrac{1}{2} \\Psi_1(x)^2 + \\dfrac{1}{2} \\Psi_2(x)^2;\\\\\n" ), 3 ),
			"" )

# TeX 形式 テンプレート (確率密度関数)
tex.d.sub.0 <-
		paste0(
			"f(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma^2}} ",
					"\\exp \\left( -\\dfrac{(x - \\mu)^2}{2 \\sigma^2} \\right);\\\\\n" )

tex.d.sub.1 <-
		paste0(
			"f_i(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma_i^2}} ",
					"\\exp \\left( -\\dfrac{(x - \\mu)^2}{2 \\sigma_i^2} \\right);\\\\\n" )

tex.d.sub.2 <-
		paste0(
			"f_i(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma_i^2}} ",
					"\\exp \\left( -\\dfrac{(x - \\mu_i)^2}{2 \\sigma_i^2} \\right);\\\\\n" )

tex.d.sub.v <-
		paste0(
			"f_i(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma_i^2}} ",
					"\\exp \\left( -\\dfrac{(x - \\mu_i)^2}{2 \\sigma_i^2} \\right);\\\\\n" )

tex.d.sub.3 <- c(
		paste0(
			"g_1(x) &= \\left\\lbrace\n",
					"\\begin{array}{l}\n",
						"\\left( 1 - \\dfrac{f_1(x)}{f_1(\\mu_1)} \\right) f_1(x) & (x \\leq \\mu_1);\\\\\n",
						"0 & (x > \\mu_1);\\\\\n",
					"\\end{array} \\right.\\\\\n",
			"g_2(x) &= \\dfrac{f_2(x)}{f_2(\\mu_2)} f_2(x);\\\\\n",
			"g_3(x) &= \\left\\lbrace\n",
					"\\begin{array}{l}\n",
						"0 & (x < \\mu_3);\\\\\n",
						"\\left( 1 - \\dfrac{f_3(x)}{f_3(\\mu_3)} \\right) f_3(x) & (x \\geq \\mu_3);\n",
					"\\end{array} \\right.\\\\\n" ),
		paste0(
			"f_i(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma_i^2}} ",
					"\\exp \\left( -\\dfrac{(x - \\mu_i)^2}{2 \\sigma_i^2} \\right);\\\\\n" ) )

tex.d.sub.4 <- c(
		paste0(
			"g_i(x) &= \\left( 1 - \\dfrac{f_{i,1}(x)}{f_{i,1}(\\mu_{i,1})} \\right) f_{i,1}(x) + ",
					"\\dfrac{f_{i,2}(x)}{f_{i,2}(\\mu_{i,2})} f_{i,2}(x);\\\\\n" ),
		paste0(
			"f_{i,j}(x) &= \\dfrac{1}{\\sqrt{2 \\pi \\sigma_{i,j}^2}} ",
					"\\exp \\left( -\\dfrac{(x - \\mu_{i,j})^2}{2 \\sigma_{i,j}^2} \\right);\\\\\n" ) )

tex.d.all <-
		c(
		paste0( "g(x) &= f(x);\\\\\n" ),
		paste0( "g(x) &= \\dfrac{1}{2} ( f_1(x) + f_2(x) );\\\\\n" ),
		paste0( "g(x) &= \\left\\lbrace\n",
						"\\begin{array}{l}\n",
							"(1 - 2 \\Phi_1(x)) f_1(x) + 2 \\Phi_2(x) f_2(x) & (x \\leq \\mu);\\\\\n",
							"1 - \\left\\lbrace (1 - 2 \\Phi_1(2 \\mu - x)) f_1(2 \\mu - x) + 2 \\Phi_2(2 \\mu - x) ",
									"f_2(2 \\mu - x) \\right\\rbrace & (x > \\mu);\n",
						"\\end{array} \\right.\\\\\n" ),
		rep( paste0( "g(x) &= \\left( 1 - \\Phi_1(x) \\right) f_1(x) + \\Phi_2(x) f_2(x);\\\\\n" ), 3 ),
		rep( paste0( "g(x) &= \\left( 1 - \\dfrac{f_1(x)}{f_1(\\mu_1)} \\right) f_1(x) + ",
							"\\dfrac{f_2(x)}{f_2(\\mu_2)} f_2(x);\\\\\n" ), 3 ),
		rep( paste0( "g(x) &= g_1(x) + g_2(x) + g_3(x) ;\\\\\n" ), 3 ),
		rep( paste0( "g(x) &= \\left( 1 - \\Psi_1(x) \\right) g_1(x) + \\Psi_2(x) g_2(x);\\\\\n" ), 3 ),
		"" )

################################################################################################
#  クラス・関数

################################################################################################
#' 正規分布の平均値からの相対位置算出
#'
#' 正規分布の累積分布関数において、与えられた確率に当たる点が、平均値から何σ離れているかを得る。
#' @export
#' @param   p           確率。
#' @return  平均値からの相対位置(\eqn{\sigma} 単位)
#' @importFrom  stats       qnorm
#' @examples
#'  sqnorm( 0.5 ) # 0
#'  sqnorm( pnorm( -2, 0, 1 ) ) # -2
#'  sqnorm( seq( 0, 1, 0.1 ) ) # increces from -Inf to Inf
################################################################################################
sqnorm <- function( p )
{
	return ( qnorm( p, 0, 1 ) )
}

################################################################################################
#' 正規分布の標準偏差算出
#'
#' 与えられた平均値を取り、与えられた1点のクォンタイルにおける確率を満たす正規分布の、
#' 標準偏差を得る。
#' @export
#' @param   mean        平均値。
#' @param   q           X座標 (クォンタイル)。平均値と同じ値を与えてはならない。
#' @param   p           そのX座標における確率 (累積分布関数の値)。 0.5 を与えてはならない。
#' @return  標準偏差
#' @examples
#'  sd.mqp.norm( 0, qnorm( 0.3, 0, 1 ), 0.3 ) # 1
#'  sd.mqp.norm( rep( 0, 5 ), 1:5, pnorm( 1:5, 0, 1 ) ) # 1 1 1 1 1
#'  sd.mqp.norm( c( -0.1, 0, 0.3 ), c( -0.3, -0.1, 0.4 ), c( 0.38, 0.47, 0.53 ) ) # [2] == [3]
################################################################################################
sd.mqp.norm <- function( mean, q, p )
{
	return ( ( q - mean ) / sqnorm( p ) )
}

################################################################################################
#' 正規分布の平均値・標準偏差算出
#'
#' 与えられた2点のクォンタイルにおける確率を満たす正規分布の、平均値と標準偏差を得る。
#' @export
#' @param   q           X座標 (クォンタイル)。要素数 2 個のベクトル。
#' @param   p           それらのX座標における確率 (累積分布関数の値)。要素数 2 個のベクトル。
#' @return  list( mean = 平均値, sd = 標準偏差 )
#' @examples
#'  ms.qp.norm( q = c( -1, 1 ), p = pnorm( c( -1, 1 ), 0, 1 ) ) # list( mean = 0, sd = 1 )
#'  ms.qp.norm( q = c(  0, 1 ), p = pnorm( c(  0, 1 ), 0, 2 ) ) # list( mean = 0, sd = 2 )
#'  ms.qp.norm( q = c( -2, 1 ), p = c( 0.3, 0.7 ) ) # list( mean = 0.5, sd = 2.86 ) (about)
################################################################################################
ms.qp.norm <- function( q, p )
{
	d <- sqnorm( p[2] ) - sqnorm( p[1] )

	return ( list( mean = ( sqnorm( p[2] ) * q[1] - sqnorm( p[1] ) * q[2] ) / d,
					sd = ( q[2] - q[1] ) / d ) )
}

################################################################################################
#' [内部関数] 部分期待値算出 (正規分布)
#'
#' ある閉区間 [x[1], x[2]] における、正規分布の部分期待値を算出する。
#' すなわち、 \eqn{\int_{x_1}^{x_2} t f(t) dt} の計算をする。
#' \link[stats]{integrate} は使用しない。
#' @param   obj.or.mean     計算対象の独立区間を持つ interval の要素
#'                           (\link[cgd]{CGDInterval} クラスオブジェクト)。
#'                          または構成要素の正規分布の平均値 (numeric)。
#' @param   sd              構成要素の正規分布の標準偏差。
#' @param   x               計算対象の閉区間の下限値 (x[1]) と上限値 (x[2])。
#'                          引数 sd と x は、第1引数の型が numeric の場合に用いられる。
#' @return  与えられた閉区間における正規分布の部分期待値。
#' @importFrom  stats       dnorm pnorm
################################################################################################
mean.nd.bound <- function( obj.or.mean, sd = NULL, x = c( -Inf, Inf ) )
{
	if ( inherits( obj.or.mean, "CGDInterval" ) )
	{
		mean <- obj.or.mean$mean
		sd <- obj.or.mean$sd
		x <- obj.or.mean$q.ind
	}
	else
	{
		mean <- as.numeric( obj.or.mean )
	}

	mean * ( pnorm( x[2], mean, sd ) - pnorm( x[1], mean, sd ) ) -
	sd^2 * ( dnorm( x[2], mean, sd ) - dnorm( x[1], mean, sd ) )
}

################################################################################################
#' [内部関数] 部分期待値算出 (接続区間)
#'
#' 不連続分布の平均値 (期待値) の計算のうち、接続区間における部分の計算をする。
#' type1.type = 1 では、 \link[stats]{integrate} は使用しない。
#' type1.type = 2 では、 \link[stats]{integrate} を使用しないときに生じる
#' 桁落ち誤差のリスクが、 \link[stats]{integrate} で生じる誤差のリスクを
#' 上回ることがあるので、 \link[stats]{integrate} を使用する場合がある。
#' type1.type = 0 では、この関数は常に 0 を返す。
#' @param   type1.type      接続区間が type 1 の場合の計算方法。
#'                          type1.type = 0 の場合、他の引数の値に関係なく、
#'                          この関数は常に 0 を返す。
#' @param   itv.1.or.median 接続区間の下側の正規分布の
#'                          \link[cgd]{CGDInterval} クラスオブジェクト。
#'                          または、分布の中央値 (numeric)。
#'                          この値は、各構成要素の正規分布の平均値でもある。
#' @param   itv.2.or.sd.1   接続区間の上側の正規分布の
#'                          \link[cgd]{CGDInterval} クラスオブジェクト。
#'                          または、下側の正規分布の標準偏差 (numeric)。
#' @param   sd.2            接続区間の上側の正規分布の標準偏差 (numeric)。
#' @param   x               接続区間の下限値 (x[1]) と上限値 (x[2])。
#'                          引数 x は、第3～第6引数の型が numeric の場合に用いられる。
#' @param   thsd.p.sum      type1.type = 2 の場合、
#'                          2つの正規分布の累積分布関数の値の差の絶対値が、
#'                          この値よりも小さい場合は、
#'                          期待値を数値積分 (\link[stats]{integrate}) で計算する
#'                           (デフォルト: 2e-4)。
#'                          この処置の目的は、累積分布関数の差の絶対値が小さいとき、
#'                          \link[stats]{integrate} を使わないで期待値を計算しようとすると、
#'                          著しい桁落ち誤差が生じることがあるので、それを防ぐためである。
#' @param   force.type1     TRUEの場合、実際の接続区間の type に関係なく、
#'                          強制的に type 1 として計算する。計算テスト用のオプション
#'                           (デフォルト: !inherits( itv.1.or.median, "CGDInterval" ))。
#' @return  与えられた接続区間における部分期待値。
#' @importFrom  stats       dnorm pnorm integrate
################################################################################################
mean.conn.bound <- function( type1.type, itv.1.or.median, itv.2.or.sd.1,
							sd.2 = NULL, x = c( -Inf, Inf ), thsd.p.sum = 2e-4,
							force.type1 = !inherits( itv.1.or.median, "CGDInterval" ) )
{
	if ( type1.type == 0 )
	{
		# type1.type = 0 では接続区間は空集合
		return ( 0 )
	}

	if ( inherits( itv.1.or.median, "CGDInterval" ) )
	{
		itv.1 <- itv.1.or.median
		itv.2 <- itv.2.or.sd.1

		median <- mean.1 <- mean.2 <- itv.1$mean
		sd.1 <- itv.1$sd
		sd.2 <- itv.2$sd

		x <- c( itv.1$q.ind[2], itv.2$q.ind[1] )
	}
	else
	{
		median <- mean.1 <- mean.2 <- as.numeric( itv.1.or.median )
		sd.1 <- as.numeric( itv.2.or.sd.1 )
	}

	if ( ( x[1] == x[2] ) || ( x[2] - x[1] == 0 ) )
	{
		# 区間の幅が 0 ならば部分期待値も 0。
		mean <- 0
	}
	else if ( sd.1 == sd.2 )
	{
		# 2つの正規分布の標準偏差が等しい ⇒ 接続区間の分布も正規分布
		mean <- mean.nd.bound( median, sd.1, x )
	}
	else
	{
		d.1 <- dnorm( x, mean.1, sd.1 )
		d.2 <- dnorm( x, mean.2, sd.2 )
		p.1 <- pnorm( x, mean.1, sd.1 )
		p.2 <- pnorm( x, mean.2, sd.2 )
		mean <- numeric()

		if ( force.type1 || ( x[2] <= median && sd.1 < sd.2 ) || ( x[1] >= median && sd.1 > sd.2 ) )
		{
			# type 1
			if ( type1.type == 1 )
			{
				mean <- ( ( x[2] + x[1] ) * ( p.2[2] - p.1[1] ) -
						  ( ( ( x[2] - mean.1 )^2 + sd.1^2 ) * ( p.1[2] - p.1[1] ) -
							( ( x[1] - mean.2 )^2 + sd.2^2 ) * ( p.2[2] - p.2[1] ) +
							sd.1^2 * ( ( x[2] - mean.1 ) * d.1[2] -
									   ( 2 * x[2] - x[1] - mean.1 ) * d.1[1] ) +
							sd.2^2 * ( ( x[2] - 2 * x[1] + mean.2 ) * d.2[2] +
									   ( x[1] - mean.2 ) * d.2[1] )
						  ) / ( x[2] - x[1] ) ) / 2
			}
			else if ( type1.type == 2 )
			{
				p.sum <- p.1 + p.2

				if ( ( p.sum[2] - p.sum[1] ) == 0 )
				{
					# 注: x[1] と x[2] の絶対値が非常に大きい/小さい場合、この処理を通ることがある。
					mean <- 0
				}
				else if ( ( p.sum[2] - p.sum[1] ) < thsd.p.sum )
				{
					# p.sum の差の絶対値が小さい場合、
					# 数値積分を使わずに計算しようとすると、
					# 累積分布関数の差の計算で著しい桁落ち誤差が生じることがあるので、数値積分で計算する。
					mean <- integrate( function( x )
										{
											x * ( ( p.sum[2] - 2 * pnorm( x, mean.1, sd.1 ) ) * dnorm( x, mean.1, sd.1 ) +
												  ( 2 * pnorm( x, mean.2, sd.2 ) - p.sum[1] ) * dnorm( x, mean.2, sd.2 ) )
										}, x[1], x[2] )$value / ( p.sum[2] - p.sum[1] )
				}
				else
				{
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
				}
			}
		}
		else if ( x[2] <= median || x[1] >= median )
		{
			# type 2
			mean <- ( mean.nd.bound( median, sd.1,
									 c( x[1], itv.1$q.conn.next[2] ) ) +
					  mean.nd.bound( median, sd.2,
									 c( itv.2$q.conn.prev[1], x[2] ) ) ) / 2

		}
		else if ( sd.2 > sd.1 )
		{
			# type 3a
			mean <- mean.nd.bound( median, sd.1, c( x[1], median ) ) +
					( mean.nd.bound( median, sd.1,
									 c( median, itv.1$q.conn.next[2] ) ) +
					  mean.nd.bound( median, sd.2, c( median, x[2] ) ) ) / 2
		}
		else
		{
			# type 3b
			mean <- ( mean.nd.bound( median, sd.1, c( x[1], median ) ) +
					  mean.nd.bound( median, sd.2,
									 c( itv.2$q.conn.prev[1], median ) ) ) / 2 +
					mean.nd.bound( median, sd.2, c( median, x[2] ) )
		}
	}

	return ( mean )
}

################################################################################################
#' [内部関数] 平均値計算用サブ関数 (type1.type = 4)
#'
#' type1.type = 4 の場合の平均値計算に用いるサブ関数。
#' \eqn{\int_{-\infty}^{\infty} x \Psi_i(x) g_i(x) dx} の計算をする。
#' ただし、
#' \eqn{\Psi_i(x) = \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi_{i,1}^*(x) + \dfrac{1}{\sqrt{2}} \Phi_{i,2}^*(x)}、
#' \eqn{g_i(x) = ( 1 - f_{i,1}(x) / f_{i,1}(\mu_{i,1}) ) f_{i,1}(x) + f_{i,2}(x)^2 / f_{i,2}(\mu_{i,2})}
#' \eqn{( i = 1, 2 )}。
#' \link[stats]{integrate} は使用しない。
#' @param   means       2つの正規分布 \eqn{N_{i,1}, N_{i,2}} の平均値。
#' @param   sds         2つの正規分布 \eqn{N_{i,1}, N_{i,2}} の標準偏差。
#' @return  \eqn{\int_{-\infty}^{\infty} x \Psi_i(x) g_i(x) dx} の値。
#' @importFrom  stats       pnorm
################################################################################################
mean.cont.t4.sub <- function( means, sds )
{
	d.mean <- means[1] - means[2]

	( ( ( 4 + sqrt( 2 ) - 2 * sqrt( 6 ) ) * sds[1] + sqrt( 2 ) * sds[2] ) / 8 +
	  sqrt( ( 2 * sds[1]^2 + sds[2]^2 ) / 8 ) *
	  exp( -d.mean^2 / ( 2 * sds[1]^2 + sds[2]^2 ) ) -
	  sqrt( sds[1]^2 + sds[2]^2 ) *
	  exp( -d.mean^2 / ( sds[1]^2 + sds[2]^2 ) ) / 4 ) / sqrt( pi ) +
	( means[1] + means[2] ) / 4 -
	d.mean *
	( sqrt( 2 ) * pnorm( -d.mean / sqrt( sds[1]^2 + sds[2]^2 / 2 ), 0, 1 ) -
	  pnorm( -sqrt( 2 ) * d.mean / sqrt( sds[1]^2 + sds[2]^2 ), 0, 1 ) ) / 2
}

################################################################################################
#' [内部関数] 平均値算出 (連続分布)
#'
#' 連続な連結ガウス分布の平均値を求める。 \link[stats]{integrate} は使用しない。
#' 本関数は type1.type = 2, symmetric = TRUE の場合に対応していないので、
#' type1.type = 2, symmetric = TRUE のとき、本関数を使用してはならない。
#' なお、そのときは mean = median である。
#' type1.type = 4 の場合、 means と sds の要素数は 4 でも 6 でも同様に計算可能。
#' @param   type1.type  type1.typeの値。
#' @param   means       intervals の構成要素の正規分布の平均値のベクトル。
#' @param   sds         intervals の構成要素の正規分布の標準偏差のベクトル。
#' @return  連続な連結ガウス分布の平均値。
################################################################################################
mean.cont <- function( type1.type, means, sds )
{
	mean <- numeric()

	if ( type1.type == 1 )
	{
		mean <- means[1]
	}
	else if ( type1.type == 2 )
	{
		mean <- ( means[1] + means[2] - ( sds[1] - sds[2] ) / sqrt( pi ) ) / 2
	}
	else if ( type1.type == 3 )
	{
		if ( means[1] == means[3] && sds[1] == sds[3] )
		{
			mean <- means[1] - ( means[1] - means[2] ) * sqrt( 2 ) / 2
		}
		else
		{
			mean <- ( ( means[1] + means[3] ) * ( 2 - sqrt( 2 ) ) -
					  ( sds[1] - sds[3] ) * sqrt( 2 ) / sqrt( pi ) ) / 4 +
					means[2] * sqrt( 2 ) / 2
		}
	}
	else if ( type1.type == 4 )
	{
		if ( length( means ) == 6 )
		{
			means <- means[c( 1, 2, 4, 5 )]
			sds <- sds[c( 1, 2, 4, 5 )]
		}

		mean <- means[1] - ( means[1] - means[2] ) * sqrt( 2 ) / 2 -
				mean.cont.t4.sub( means[1:2], sds[1:2] ) +
				mean.cont.t4.sub( means[3:4], sds[3:4] )
	}

	return ( mean )
}

################################################################################################
#' [内部関数] 部分分散算出 (正規分布)
#'
#' ある閉区間 [x[1], x[2]] において、確率分布が (局所的に) 正規分布に従うとき、
#' その閉区間における部分分散を算出する。
#' すなわち、 \eqn{\int_{x_1}^{x_2} (t - \mu)^2 f(t) dt} の計算をする。
#' ここで、 \eqn{\mu} は分布全体の平均値である。
#' \link[stats]{integrate} は使用しない。
#' @param   mean        分布全体の平均値。
#' @param   mean.nd     正規分布の平均値。
#' @param   sd.nd       正規分布の標準偏差。
#' @param   x           計算対象の閉区間の下限値 (x[1]) と上限値 (x[2])
#'                      (デフォルト: c( -Inf, Inf ))。
#' @return  与えられた閉区間における部分分散。
#' @importFrom  stats       dnorm pnorm
################################################################################################
v.nd.bound <- function( mean, mean.nd, sd.nd, x )
{
	d = dnorm( x, mean.nd, sd.nd )
	p = pnorm( x, mean.nd, sd.nd )

	if ( mean == mean.nd )
	{
		# v([x_1, x_2]) = \sd^2 * [( \Phi(x_2) - \Phi(x_1) ) - ( ( x_2 - \mu ) f(x_2) - ( x_1 - \mu ) f(x_1) )]
		dd <- ifelse( is.infinite( x ), 0, ( x - mean.nd ) * d )

		return ( sd.nd^2 * ( p[2] - p[1] - dd[2] + dd[1] ) )
	}
	else
	{
		# v([x_1, x_2]) = ( ( \mu - \mu_i )^2 + sd_i^2 ) * ( \Phi(x_2) - \Phi(x_1) ) -
		#				  \sd_i^2 * [( x_2 - 2\mu + \mu_i ) f(x_2) - ( x_1 - 2\mu + \mu_i ) f(x_1)]
		dd <- ifelse( is.infinite( x ), 0, ( x - 2 * mean + mean.nd ) * d )

		return ( ( ( mean - mean.nd )^2 + sd.nd^2 ) * ( p[2] - p[1] ) -
				 sd.nd^2 * ( dd[2] - dd[1] ) )
	}
}

################################################################################################
#' [内部関数] 分散計算用サブ関数 (type1.type = 1, 2, 3)
#'
#' 連続分布や不連続分布の接続区間の部分の分散の計算に用いるサブ関数。
#' サブ関数の意味や表現式は、 type1.type の値によって、それぞれ異なる。
#' \link[stats]{integrate} は使用しない。
#' @param   type1.type  接続区間が type 1 の場合の計算方法
#' @param   mean        分布全体の平均値。
#' @param   mean.i      intervals の構成要素の正規分布の平均値。
#' @param   sd.i        intervals の構成要素の正規分布の標準偏差。
#' @param   x           分散の計算対象範囲の下限値または上限値。
#' @param   q           接続区間のX座標の下限値または上限値 (デフォルト: x)。
#'                      type1.type = 2 の連続分布の場合は
#'                      下限値として -Inf 、上限値として Inf を与えること。
#' @param   p.sum       type1.type = 2 の場合に使用する。
#'                      2つの正規分布の累積分布関数の合計値 (デフォルト: 0)。
#' @param   k           type1.type = 3 の場合に使用する。
#'                      intervals を構成する3つの正規分布のうち、
#'                      何番目について計算するかを指定する (デフォルト: 0)。
#' @details
#'  この関数では、 type1.type の値によって、
#'  それぞれ以下の計算を \link[stats]{integrate} を使わずに計算する。
#'  最終的な分散の値はこれらの計算値の和と差および、簡単な項との和や積で表される。
#'
#'  以下の式において、 \eqn{\mu} は分布全体の平均値 (mean)、
#'  \eqn{f_i(x)} は平均値 mean.i 、標準偏差 sd.i の正規分布の確率密度関数、
#'  同じく \eqn{\Phi_i(x)} は累積分布関数、
#'  \eqn{\bar \Phi(x)} は2つの累積分布関数 \eqn{\Phi_1(x)} と \eqn{\Phi_2(x)} の平均である。
#'
#'  \describe{
#'      \item{type1.type = 1}{
#'          \deqn{3 \left \lbrace (x - \mu)^2 (q - x) \Phi_i(x) -
#'                2 \displaystyle
#'                  \int_{-\infty}^x (t - \mu) (q - t) \Phi_i(t) \ dt \right \rbrace}}
#'
#'      \item{type1.type = 2}{
#'          \deqn{\displaystyle
#'                \int_{-\infty}^x (t - \mu)^2 (\Phi_i(t) - \bar \Phi(q)) f_i(t) \ dt}}
#'
#'      \item{type1.type = 3}{
#'          \deqn{k = 1, 3 \ :
#'              \ \displaystyle
#'                \int_{-\infty}^x (t - \mu) (1 - \dfrac{f_i(t)}{f_i(\mu_i)}) f_i(t) \ dt}
#'
#'          \deqn{k = 2 \ :
#'              \ \displaystyle
#'                \int_{-\infty}^x (t - \mu) \dfrac{f_i(t)^2}{f_i(\mu_i)} f_i(t) \ dt}}
#'  }
#' @return  分散計算用サブ関数の値。
#' @importFrom  stats       dnorm pnorm
################################################################################################
v.sub <- function( type1.type, mean, mean.i, sd.i, x, q = x, p.sum = 0, k = 0 )
{
	v <- numeric()
	d.i <- dnorm( x, mean.i, sd.i )
	p.i <- pnorm( x, mean.i, sd.i )

	if ( type1.type == 1 )
	{
		v <- ( ( x - mean )^3 - 3 * q * ( mean.i - mean )^2 +
			   3 * ( mean.i - mean ) * ( mean.i^2 + sd.i^2 ) +
			   3 * sd.i^2 * ( mean.i - q ) + mean^3 - mean.i^3 ) * p.i -
			 sd.i^2 * ( 2 * ( x^2 + mean.i * x + mean.i^2 + 2 * sd.i^2 ) -
						3 * ( q + mean ) * ( x + mean.i ) + 6 * mean * q ) * d.i
	}
	else if ( type1.type == 2 )
	{
		pstar.i <- pnorm( x, mean.i, sd.i * sqrt( 2 ) / 2 )

		# 2つの正規分布の差が小さいときに生じる桁落ち誤差を緩和するために、
		# ( p.i^2 - p.sum * p.i ) の p.i はカッコの外に出してない。
		# ただし、これでも桁落ち誤差が生じることがある。
		v <- ( ( mean.i - mean )^2 + sd.i^2 ) * ( p.i^2 - p.sum * p.i ) / 2 +
			 ( mean.i - mean ) * sd.i * pstar.i / sqrt( pi ) -
			 sd.i^2 * ( x + mean.i - 2 * mean ) * ( p.i - p.sum / 2 ) * d.i -
			 sd.i^4 * d.i^2 / 2
	}
	else if ( type1.type == 3 )
	{
		dstar.i <- dnorm( x, mean.i, sd.i * sqrt( 2 ) / 2 )
		pstar.i <- pnorm( x, mean.i, sd.i * sqrt( 2 ) / 2 )

		v <- ( ( mean.i^2 + sd.i^2 / 2 ) * pstar.i -
			   ( x + mean.i ) * sd.i^2 * dstar.i / 2 -
			   mean * ( 2 * mean.i * pstar.i - sd.i^2 * dstar.i ) +
			   mean^2 * pstar.i ) * sqrt( 2 ) / 2

		if ( k == 1 || k == 3 )
		{
			v <- ( mean.i^2 + sd.i^2 ) * p.i -
				 ( x + mean.i ) * sd.i^2 * d.i -
				 2 * mean * ( mean.i * p.i - sd.i^2 * d.i ) +
				 mean^2 * p.i - v
		}
	}

	return ( v )
}

################################################################################################
#' [内部関数] 分散計算用サブ関数 (type1.type = 4)
#'
#' type1.type = 4 の場合の分散の計算に用いるサブ関数。
#' \eqn{\int_{-\infty}^{\infty} x^2 \Psi_i(x) g_i(x) dx} の計算をする。
#' ただし、
#' \eqn{\Psi_i(x) = \Phi_{i,1}(x) - \dfrac{1}{\sqrt{2}} \Phi_{i,1}^*(x) +
#'                                  \dfrac{1}{\sqrt{2}} \Phi_{i,2}^*(x)}、
#' \eqn{g_i(x) = ( 1 - f_{i,1}(x) / f_{i,1}(\mu_{i,1}) ) f_{i,1}(x) +
#'               f_{i,2}(x)^2 / f_{i,2}(\mu_{i,"})}
#' \link[stats]{integrate} は使用しない。
#' @param   mean        分布全体の平均値。
#' @param   means       2つの正規分布 \eqn{N_{i,1}, N_{i,2}} の平均値。
#' @param   sds         2つの正規分布 \eqn{N_{i,1}, N_{i,2}} の標準偏差。
#' @return  \eqn{\int_{-\infty}^{\infty} x^2 \Psi_i(x) g_i(x) dx} の値。
#' @importFrom  stats       pnorm
################################################################################################
v.sub.t4 <- function( mean, means, sds )
{
	d.mean <- means[1] - means[2]

	( ( 6 - 4 * sqrt( 2 ) ) * means[1]^2 + ( 5 - 3 * sqrt( 2 ) ) * sds[1]^2 +
	  ( 4 * sqrt( 2 ) - 2 ) * means[2]^2 + ( 2 * sqrt( 2 ) - 1 ) * sds[2]^2 ) / 8 +
	( ( 4 + sqrt( 2 ) - 2 * sqrt( 6 ) ) * means[1] * sds[1] +
	  sqrt( 2 ) * means[2] * sds[2] ) / sqrt( pi ) / 4 +
	( sqrt( 2 ) * ( 2 * ( means[1]^2 - means[2]^2 + sds[1]^2 ) - sds[2]^2 ) *
	  pnorm( d.mean / sqrt( sds[1]^2 + sds[2]^2 / 2 ), 0, 1 ) -
	  ( 2 * ( means[1]^2 - means[2]^2 ) + sds[1]^2 - sds[2]^2 ) *
	  pnorm( sqrt( 2 ) * d.mean / sqrt( sds[1]^2 + sds[2]^2 ), 0, 1 ) ) / 4 +
	( means[1] + means[2] ) *
	( sqrt( 4 * sds[1]^2 + 2 * sds[2]^2 ) *
	  exp( -d.mean^2 / ( 2 * sds[1]^2 + sds[2]^2 ) ) -
	  sqrt( sds[1]^2 + sds[2]^2 ) *
	  exp( -d.mean^2 / ( sds[1]^2 + sds[2]^2 ) ) ) / sqrt( pi ) / 4
}

################################################################################################
#' [内部関数] 部分分散算出 (接続区間)
#'
#' 不連続分布の分散の計算のうち、接続区間における部分分散を算出する。
#' type1.type = 1 では、 \link[stats]{integrate} は使用しない。
#' type1.type = 2 では、 \link[stats]{integrate} を使用しないときに生じる
#' 桁落ち誤差のリスクが、 \link[stats]{integrate} で生じる誤差のリスクを
#' 上回ることがあるので、 \link[stats]{integrate} を使用する場合がある。
#' type1.type = 0 では、この関数は常に 0 を返す。
#' @param   type1.type      接続区間が type 1 の場合の計算方法。
#'                          type1.type = 0 の場合、他の引数の値に関係なく、
#'                          この関数は常に 0 を返す。
#' @param   mean            分布全体の平均値。
#' @param   itv.1.or.median 接続区間の下側の正規分布の
#'                          \link[cgd]{CGDInterval} クラスオブジェクト。
#'                          または、分布の中央値 (numeric)。
#' @param   itv.2.or.sd.1   接続区間の上側の正規分布の
#'                          \link[cgd]{CGDInterval} クラスオブジェクト。
#'                          または、接続区間の下側の正規分布の標準偏差 (numeric)。
#' @param   sd.2            接続区間の上側の正規分布の標準偏差 (デフォルト: 0)。
#' @param   q               接続区間のX座標の下限値 (q[1]) と上限値 (q[2])
#'                           (デフォルト: list( numeric(), numeric() ))。
#' @param   x               計算対象範囲の下限値 (x[1]) と上限値 (x[2])
#'                           (デフォルト: q)。
#'                          x の範囲は q の範囲に包含されている必要がある。
#' @param   thsd.p.sum      type1.type = 2 の場合、
#'                          2つの正規分布の累積分布関数の値の差の絶対値が、
#'                          この値よりも小さい場合は、
#'                          期待値を数値積分 (\link[stats]{integrate}) で計算する
#'                           (デフォルト: 2e-4)。
#'                          この処置の目的は、累積分布関数の差の絶対値が小さいとき、
#'                          \link[stats]{integrate} を使わないで分散を計算しようとすると、
#'                          著しい桁落ち誤差が生じることがあるので、それを防ぐためである。
#' @param   force.type1     TRUEの場合、実際の接続区間の type に関係なく、
#'                          強制的に type 1 として計算する。計算テスト用のオプション
#'                           (デフォルト: !inherits( itv.1.or.median, "CGDInterval" ))。
#' @return  与えられた計算対象範囲における部分分散。
#' @importFrom  stats       pnorm integrate
################################################################################################
v.conn.bound <- function( type1.type, mean, itv.1.or.median, itv.2.or.sd.1,
							sd.2 = 0, q = list( numeric(), numeric() ), x = q,
							thsd.p.sum = 2e-4,
							force.type1 = !inherits( itv.1.or.median, "CGDInterval" ) )
{
	if ( type1.type == 0 )
	{
		# type1.type = 0 では接続区間は空集合
		return ( 0 )
	}

	if ( inherits( itv.1.or.median, "CGDInterval" ) )
	{
		itv.1 <- itv.1.or.median
		itv.2 <- itv.2.or.sd.1

		median <- mean.1 <- mean.2 <- itv.1$mean
		sd.1 <- itv.1$sd
		sd.2 <- itv.2$sd

		q <- c( itv.1$q.ind[2], itv.2$q.ind[1] )

		if ( length( x[1] ) == 0 )
		{
			x <- q
		}
	}
	else
	{
		median <- mean.1 <- mean.2 <- as.numeric( itv.1.or.median )
		sd.1 <- as.numeric( itv.2.or.sd.1 )
	}

	if ( ( q[1] == q[2] ) || ( q[2] - q[1] == 0 ) ||
		 ( x[1] == x[2] ) || ( x[2] - x[1] == 0 ) )
	{
		# 区間の幅が 0 ならば部分分散も 0。
		v <- 0
	}
	else if ( sd.1 == sd.2 )
	{
		# 2つの正規分布の標準偏差が等しい ⇒ 接続区間の分布も正規分布
		v <- v.nd.bound( mean, mean.1, sd.1, x )
	}
	else
	{
		v <- numeric()

		if ( force.type1 || ( x[2] <= median && sd.1 < sd.2 ) || ( x[1] >= median && sd.1 > sd.2 ) )
		{
			# type 1
			if ( type1.type == 1 )
			{
				v <- ( v.sub( 1, mean, mean.2, sd.2, x[2], q[1] ) -
					   v.sub( 1, mean, mean.1, sd.1, x[2], q[2] ) -
					   v.sub( 1, mean, mean.2, sd.2, x[1], q[1] ) +
					   v.sub( 1, mean, mean.1, sd.1, x[1], q[2] ) ) /
					 ( 3 * ( q[2] - q[1] ) )
			}
			else if ( type1.type == 2 )
			{
				p.sum <- pnorm( q, mean.1, sd.1 ) + pnorm( q, mean.2, sd.2 )

				if ( ( p.sum[2] - p.sum[1] ) == 0 )
				{
					v <- 0
				}
				else if ( ( p.sum[2] - p.sum[1] ) < thsd.p.sum )
				{
					# p.sum の差の絶対値が小さい場合は、
					# 数値積分を使わずに計算しようとすると、
					# サブ関数で著しい桁落ち誤差が生じることがあるので、数値積分で計算する。
					v <- integrate( function( x )
									{
										( x - mean )^2 *
										( ( p.sum[2] - 2 * pnorm( x, mean.1, sd.1 ) ) * dnorm( x, mean.1, sd.1 ) +
										  ( 2 * pnorm( x, mean.2, sd.2 ) - p.sum[1] ) * dnorm( x, mean.2, sd.2 ) )
									}, x[1], x[2] )$value / ( p.sum[2] - p.sum[1] )
				}
				else
				{
					v <- ( v.sub( 2, mean, mean.2, sd.2, x[2], q[1], p.sum[1] ) -
						   v.sub( 2, mean, mean.1, sd.1, x[2], q[2], p.sum[2] ) -
						   v.sub( 2, mean, mean.2, sd.2, x[1], q[1], p.sum[1] ) +
						   v.sub( 2, mean, mean.1, sd.1, x[1], q[2], p.sum[2] ) ) /
						 ( p.sum[2] - p.sum[1] ) * 2
				}
			}
		}
		else if ( x[2] <= median || x[1] >= median )
		{
			# type 2
			v <- ( v.nd.bound( mean, mean.1, sd.1,
								c( x[1], itv.1$q.conn.next[2] ) ) +
				   v.nd.bound( mean, mean.2, sd.2,
								c( itv.2$q.conn.prev[1], x[2] ) ) ) / 2
		}
		else if ( sd.2 > sd.1 )
		{
			# type 3a
			v <- v.nd.bound( mean, mean.1, sd.1, c( x[1], median ) ) +
				 ( v.nd.bound( mean, mean.1, sd.1,
								c( median, itv.1$q.conn.next[2] ) ) +
				   v.nd.bound( mean, mean.2, sd.2, c( median, x[2] ) ) ) / 2
		}
		else
		{
			# type 3b
			v <- ( v.nd.bound( mean, mean.1, sd.1, c( x[1], median ) ) +
				   v.nd.bound( mean, mean.2, sd.2,
								c( itv.2$q.conn.prev[1], median ) ) ) / 2 +
				 v.nd.bound( mean, mean.2, sd.2, c( median, x[2] ) )
		}
	}

	return ( v )
}

################################################################################################
#' [内部関数] 分散算出 (連続分布)
#'
#' 連続な連結ガウス分布の分散を求める。
#' \link[stats]{integrate} は使用しない。
#' type1.type = 4 の場合、 get.lv と get.uv のオプションは無効。
#' その理由は \eqn{\int_{-\infty}^{\mu} f_{i,1}(x) \Phi_{i,2}(x) dx} を
#' 数値積分を使わないで計算する方法が分からないため。
#' なお、 type1.type = 4 の場合、 means と sds の要素数は 4 でも 6 でも同様に計算可能。
#' @param   type1.type  type1.typeの値。
#' @param   means       intervals の構成要素の正規分布の平均値のベクトル。
#' @param   sds         intervals の構成要素の正規分布の標準偏差のベクトル。
#' @param   mean        分布全体の平均値
#'                       (デフォルト: mean.cont( type1.type, means, sds ))。
#' @param   symmetric   TRUE の場合、中央値を中心として左右対称な分布として扱う。
#'                      type1.type = 2 または 3 の場合に有効 (デフォルト: FALSE)。
#' @param   get.lv      TRUE の場合、下半分散を算出する (デフォルト: FALSE)。
#' @param   get.uv      TRUE の場合、上半分散を算出する (デフォルト: FALSE)。
#'                      get.lv と get.uv を両方同時に TRUE にすべきではないが、
#'                      もし両方とも TRUE の場合は、 get.lv が優先される。
#'                      両方とも FALSE の場合は、分布全体の分散を算出する。
#' @return  連続な連結ガウス分布の分散 (あるいは半分散)。
#' @importFrom  stats       dnorm pnorm
################################################################################################
v.cont <- function( type1.type, means, sds,
					mean = mean.cont( type1.type, means, sds ),
					symmetric = FALSE, get.lv = FALSE, get.uv = FALSE )
{
	v <- 0

	if ( type1.type == 1 )
	{
		if ( get.lv || get.uv )
		{
			v <- ( sds[1]^2 + sds[2]^2 ) / 4
		}
		else
		{
			v <- ( sds[1]^2 + sds[2]^2 ) / 2
		}
	}
	else if ( type1.type == 2 )
	{
		if ( symmetric )
		{
			v <- ( sds[1]^2 + sds[2]^2 ) / 2 + ( sds[1]^2 - sds[2]^2 ) / pi

			if ( get.lv || get.uv )
			{
				v <- v / 2
			}
		}
		else
		{
			if ( get.lv || get.uv )
			{
				v <- v.sub( 2, mean, means[2], sds[2], mean, -Inf, 0 ) -
					 v.sub( 2, mean, means[1], sds[1], mean, Inf, 2 )
			}

			if ( !get.lv )
			{
				v <- ( means[1]^2 + means[2]^2 + sds[1]^2 + sds[2]^2 ) / 2 - mean^2 +
					 ( sds[2] * means[2] - sds[1] * means[1] ) / sqrt( pi ) - v
			}
		}
	}
	else if ( type1.type == 3 )
	{
		if ( symmetric )
		{
			v <- sds[1]^2 + ( sds[2]^2 - sds[1]^2 ) * sqrt( 2 ) / 4

			if ( get.lv || get.uv )
			{
				v <- v / 2
			}
		}
		else if ( means[1] == means[3] && sds[1] == sds[3] )
		{
			if ( get.lv || get.uv )
			{
				d.1 <- dnorm( mean, means[1], sds[1] )
				p.1 <- pnorm( mean, means[1], sds[1] )
				dstar <- dnorm( mean, means[1:2], sds[1:2] * sqrt( 2 ) / 2 )
				pstar <- pnorm( mean, means[1:2], sds[1:2] * sqrt( 2 ) / 2 )
				d.mean <- means[1] - means[2]

				v <- ( d.mean^2 / 2 + sds[1]^2 ) * p.1 -
					 d.mean * sds[1]^2 * d.1 * sqrt( 2 ) / 2 -
					 ( ( d.mean^2 + sds[1]^2 ) * pstar[1] * sqrt( 2 ) -
					   d.mean * sds[1]^2 * dstar[1] -
					   ( d.mean^2 * ( 3 * sqrt( 2 ) - 4 ) + sds[2]^2 * sqrt( 2 ) ) * pstar[2] -
					   d.mean * sds[2]^2 * dstar[2] * ( sqrt( 2 ) - 1 ) ) / 4
			}

			if ( !get.lv )
			{
				v <- ( means[1] - means[2] )^2 * ( sqrt( 2 ) - 1 ) / 2 +
					 ( ( 4 - sqrt( 2 ) ) * sds[1]^2 + sqrt( 2 ) * sds[2]^2 ) / 4 - v
			}
		}
		else
		{
			if ( get.lv )
			{
				v <- v.sub( 3, mean, means[1], sds[1], min( mean, means[1] ), k = 1 ) +
					 v.sub( 3, mean, means[2], sds[2], mean, k = 2 )

				if ( means[3] < mean )
				{
					v <- v + v.sub( 3, mean, means[3], sds[3], mean, k = 3 ) -
						 ( means[3] - mean )^2 * ( 2 - sqrt( 2 ) ) / 4 +
						 ( means[3] - mean ) * sds[3] * sqrt( 2 ) / sqrt( pi ) / 2 -
						 sds[3]^2 * ( 4 - sqrt( 2 ) ) / 8
				}
			}
			else if ( get.uv )
			{
				v <- ( means[2] - mean )^2 * sqrt( 2 ) / 2 +
					 sds[2]^2 * sqrt( 2 ) / 4 -
					 v.sub( 3, mean, means[2], sds[2], mean, k = 2 ) +
					 ( means[3] - mean )^2 * ( 2 - sqrt( 2 ) ) / 2 +
					 sds[3]^2 * ( 4 - sqrt( 2 ) ) / 4 -
					 v.sub( 3, mean, means[3], sds[3], max( mean, means[3] ), k = 3 )

				if ( mean < means[1] )
				{
					v <- v - v.sub( 3, mean, means[1], sds[1], mean, k = 1 ) +
						 ( means[1] - mean )^2 * ( 2 - sqrt( 2 ) ) / 4 -
						 ( means[1] - mean ) * sds[1] * sqrt( 2 ) / sqrt( pi ) / 2 +
						 sds[1]^2 * ( 4 - sqrt( 2 ) ) / 8
				}
			}
			else
			{
				v <- ( ( means[1] - mean )^2 + ( means[3] - mean )^2 ) *
					 ( 2 - sqrt( 2 ) ) / 4 -
					 ( ( means[1] - mean ) * sds[1] - ( means[3] - mean ) * sds[3] ) *
					 sqrt( 2 ) / sqrt( pi ) / 2 +
					 ( sds[1]^2 + sds[3]^2 ) * ( 4 - sqrt( 2 ) ) / 8 +
					 ( ( means[2] - mean )^2 * 2 + sds[2]^2 ) * sqrt( 2 ) / 4
			}
		}
	}
	else if ( type1.type == 4 )
	{
		# For type1.type ==4, both get.lv and get.uv options are invalid.
		# Use v.cont.t4.via.integrate for lv or uv.

		if ( get.lv || get.uv )
		{
			stop( "Error: The get.lv and get.uv options are not supported yet where type1.type = 4." )
		}

		if ( length( means ) == 6 )
		{
			means <- means[c( 1, 2, 4, 5 )]
			sds <- sds[c( 1, 2, 4, 5 )]
		}

		v <- ( ( 2 - sqrt( 2 ) ) * means[1]^2 + ( 4 - sqrt( 2 ) ) * sds[1]^2 / 2 +
			 sqrt( 2 ) * ( means[2]^2 + sds[2]^2 / 2 ) ) / 2 - mean^2 -
			 v.sub.t4( mean, means[1:2], sds[1:2] ) +
			 v.sub.t4( mean, means[3:4], sds[3:4] )
	}

	return ( v )
}

################################################################################################
#' [内部関数] 数値積分による分散算出 (type1.type = 4)
#'
#' type1.type = 4 の場合に、数値積分 (\link[stats]{integrate}) を使って
#' 下半分散または上半分散を求める。分布全体の分散を求めることも可能。
#' means と sds の要素数は 4 でも 6 でも同様に計算可能。
#' @param   means       intervals の構成要素の正規分布の平均値のベクトル。
#' @param   sds         intervals の構成要素の正規分布の標準偏差のベクトル。
#' @param   mean        分布全体の平均値
#'                       (デフォルト: mean.cont( 4, means, sds ))。
#' @param   get.lv      TRUE の場合、下半分散を算出する (デフォルト: FALSE)。
#' @param   get.uv      TRUE の場合、上半分散を算出する (デフォルト: FALSE)。
#'                      get.lv と get.uv を両方同時に TRUE にすべきではないが、
#'                      もし両方とも TRUE の場合は、 get.lv が優先される。
#'                      両方とも FALSE の場合は、分布全体の分散を算出する。
#' @return  半分散 (または分散) を求めた \link[stats]{integrate} の出力リスト。
#' @importFrom  stats       dnorm pnorm integrate
################################################################################################
v.cont.t4.via.integrate <- function( means, sds, mean = mean.cont( 4, means, sds ),
									get.lv = FALSE, get.uv = FALSE )
{
	if ( length( means ) == 6 )
	{
		means <- means[c( 1, 2, 4, 5 )]
		sds <- sds[c( 1, 2, 4, 5 )]
	}

	f <- function( x )
	{
		( x - mean )^2 *
		( ( 1 - pnorm( x, means[1], sds[1] ) +
				pnorm( x, means[1], sds[1] * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2 -
				pnorm( x, means[2], sds[2] * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2 ) *
		  ( ( 1 - dnorm( x, means[1], sds[1] ) / dnorm( means[1], means[1], sds[1] ) ) *
				  dnorm( x, means[1], sds[1] ) +
				  dnorm( x, means[2], sds[2] )^2 / dnorm( means[2], means[2], sds[2] ) ) +
		  ( pnorm( x, means[3], sds[3] ) -
			pnorm( x, means[3], sds[3] * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2 +
			pnorm( x, means[4], sds[4] * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2 ) *
		  ( ( 1 - dnorm( x, means[3], sds[3] ) / dnorm( means[3], means[3], sds[3] ) ) *
				  dnorm( x, means[3], sds[3] ) +
				  dnorm( x, means[4], sds[4] )^2 / dnorm( means[4], means[4], sds[4] ) ) )
	}

	result <- list()
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

	return ( result )
}

################################################################################################
#' [内部関数] 二分法
#'
#' 二分法により方程式を解く。
#' @param   f           解を探索する関数。
#' @param   interval    解を探索する範囲のベクトル。
#' @param   tol         許容する誤差 (デフォルト: .Machine$double.eps * 16)。
#' @return  方程式 f = 0 の解。
################################################################################################
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

################################################################################################
#' [内部関数] 二分法用サブ関数
#'
#' 二分法の処理実体。
#' @param   f           解を探索する関数。
#' @param   interval    解を探索する範囲のベクトル。
#' @param   tol         許容する誤差。
#' @return  方程式 f = 0 の解。
################################################################################################
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

################################################################################################
#' 連結ガウス分布区間クラス
#'
#' 連結ガウス分布クラス (\link[cgd]{CGD}) で使用する区間を表すクラス。
#' @export      CGDInterval
#' @exportClass CGDInterval
#' @field   mean            独立区間を負担する正規分布の平均値
#' @field   sd              独立区間を負担する正規分布の標準偏差
#' @field   q.ind           独立区間の定義域 (クォンタイル)
#' @field   q.conn.prev     前の接続区間の確率内に値が収まる、この正規分布の累積密度関数の定義域
#' @field   q.conn.next     次の接続区間の確率内に値が収まる、この正規分布の累積密度関数の定義域
#' @field   p.ind           この正規分布が負担する独立確率区間
#' @field   p.conn.prev     前の接続区間の確率
#'                           (type1.type = 3 では、1個目または3個目の正規分布が寄与する区間)
#' @field   p.conn.next     次の接続区間の確率
#'                           (type1.type = 3 では、1個目または3個目の正規分布が寄与する区間)
#' @seealso \link[cgd]{CGD}
################################################################################################
CGDInterval <- setRefClass(

	# クラス名
	Class = "CGDInterval",

	# フィールド
	fields = list(
		mean = "numeric",			# 独立区間を負担する正規分布の平均値
		sd = "numeric", 			# 独立区間を負担する正規分布の標準偏差
		q.ind = "vector",			# 独立区間の定義域 (クォンタイル)
		q.conn.prev = "vector", 	# 前の接続区間の確率内に値が収まる、この正規分布の累積密度関数の定義域
		q.conn.next = "vector", 	# 次の接続区間の確率内に値が収まる、この正規分布の累積密度関数の定義域
		p.ind = "vector",			# この正規分布が負担する独立区間の確率
		p.conn.prev = "vector", 	# 前の接続区間の確率
		p.conn.next = "vector"		# 次の接続区間の確率
	)
)

# メソッド
################################################################################################
#' 負担区間取得
#'
#' 負担区間 (前の区間との接続区間～次の区間との接続区間) に対するX座標 (クォンタイル) を取得する。
#' @name CGDInterval_q.manage
#' @usage   CGDInterval$q.manage()
#' @return  負担区間のX座標 (クォンタイル)
#' @examples
#'  a <- CGD$new()
#'  a$intervals[[1]]$q.manage()
################################################################################################
NULL
CGDInterval$methods(
	q.manage = function()
	{
		return ( c( q.conn.prev[1], q.conn.next[2] ) )
	}
)

################################################################################################
#' 連結ガウス分布 (Connected Gaussian Distribution) クラス
#'
#' 複数の正規分布をX軸方向に連結したモデルを表すクラス。
#'
#' 連結ガウス分布は、
#' 理論上、任意のクォンタイルをゼロ誤差で満たす分布を構成できる。
#' そのため、何かクォンタイルが与えられたときに、
#' それを高精度に再現する分布の概形やサンプルが欲しいときなどに使うことができる。
#' ただし、確率密度関数は一般に不連続であり、歪んだ形になる。
#'
#' 単峰性や二峰性の連続分布を構成することも可能であるが、
#' 連続分布を構成するには、クォンタイルの個数が8点以下で、
#' クォンタイルの位置が過度にいびつでないことが条件になる。
#'
#' 不連続な連結ガウス分布を使って滑らかなグラフを得るには、
#' ランダムサンプルを取得して、そのヒストグラムを描画するとよい。
#'
#' また、 \link[cgd]{nls.freq} や \link[cgd]{nls.freq.all} を使うと、
#' データの度数分布をもとに、その分布に近い連続分布モデルを得ることもできる。
#' このとき、データは正規分布に従っていなくてもよい。
#' むしろ、このクラスはデータが正規分布に従わない場合
#'  (あるいは、正規分布に従う裏付けが無い場合) に用いるべきである。
#' @export      CGD
#' @exportClass CGD
#' @field   kind.index      この分布の種類のインデックス番号。
#' @field   kind            この分布の種類名。
#' @field   median          この分布の中央値。
#' @field   intervals       連結区間 (\link[cgd]{CGDInterval} クラスのリスト)。
#' @field   type1.type      接続区間が type 1 の場合の計算方法
#'                           (詳細は \link[cgd]{trace.q} の Details を参照)。
#' @field   mean            この分布の平均値。
#' @field   sd              この分布の標準偏差。
#' @field   lsd             この分布の下半標準偏差。
#' @field   usd             この分布の上半標準偏差。
#' @field   lsd.abs.error   下半標準偏差の絶対誤差の推定値。
#' @field   usd.abs.error   上半標準偏差の絶対誤差の推定値。
#'                          type1.type = 4 の場合、
#'                          半標準偏差の計算に \link[stats]{integrate} を用いるが、
#'                          それに由来する絶対誤差がこれらのフィールドに入る。
#' @seealso \link[cgd]{CGDInterval}, \link[cgd]{CGD_set.waypoints}, \link[cgd]{trace.q},
#'          \link[cgd]{CGD_nls.freq}, \link[cgd]{nls.freq}, \link[cgd]{nls.freq.all},
#'          \href{https://github.com/Kimitsuna-Goblin/CGD}{README.md} (GitHub)
################################################################################################
CGD <- setRefClass(

	# クラス名
	Class = "CGD",

	# フィールド
	fields = list(
		kind.index = "numeric", 	# この分布の種類のインデックス番号
		kind = "character", 		# この分布の種類名

		median = "numeric", 		# この分布の中央値
		intervals = "list", 		# 連結区間 (CGDInterval クラスのリスト)
		type1.type = "numeric", 	# 接続区間が type 1 の場合の計算方法

		mean = "numeric",			# この分布の平均値
		sd = "numeric", 			# この分布の標準偏差
		lsd = "numeric",			# この分布の下半標準偏差
		usd = "numeric",			# この分布の上半標準偏差
		lsd.abs.error = "numeric",	# 下半標準偏差の絶対誤差の推定値
		usd.abs.error = "numeric"	# 上半標準偏差の絶対誤差の推定値
	)
)

# メソッド
################################################################################################
#' コンストラクタ
#'
#' \link[cgd]{CGD} クラスオブジェクトを生成する。
#'
#' 処理の安定化・可読性向上のため、 type1.type 以外の引数は削除された。
#' 連結区間の構成が分かっている場合は \link[cgd]{CGD$set.intervals} を使用すること。
#' @name    CGD_initialize
#' @aliases CGD$new
#' @usage   CGD$new(type1.type = 1)
#' @param   type1.type      type 1 の場合の計算方法。 0、1、2、3、4 のいずれかを指定すること
#'                           (デフォルト: 1)。
#'                          それ以外の数値を指定した場合は警告が表示され、
#'                          type1.type はデフォルトの 1 になる。
#'                          0～4 の詳細については、 \link[cgd]{trace.q} の Details を参照。
#' @seealso \link[cgd]{CGDInterval}, \link[cgd]{CGD_set.intervals},
#'          \link[cgd]{CGD_set.waypoints}
#' @examples
#'  CGD$new()
#'  CGD$new( type1.type = 2 )
#'  CGD$new( 3 )
#'  CGD$new( 4 )
################################################################################################
NULL
CGD$methods(
	initialize = function( type1.type = 1 )
	{
		kind.index <<- 1
		kind <<- kinds[1]

		median <<- 0

		if ( length( type1.type ) == 0 || !any( type1.type == 0:4 ) )
		{
			warning( paste( "Warning: type1.type" , type1.type, "is undefined." ) )
			type1.type <<- 1
			intervals <<- c( CGDInterval$new(	mean = 0,
												sd = 1,
												q.ind = c( -Inf, Inf ),
												q.conn.prev = c( -Inf, -Inf ), q.conn.next = c( Inf, Inf ),
												p.ind = c( 0, 1 ),
												p.conn.prev = c( 0, 0 ), p.conn.next = c( 1, 1 ) ) )
		}
		else if ( any( type1.type == 0:2 ) )
		{
			type1.type <<- type1.type
			intervals <<- c( CGDInterval$new(	mean = 0,
												sd = 1,
												q.ind = c( -Inf, Inf ),
												q.conn.prev = c( -Inf, -Inf ), q.conn.next = c( Inf, Inf ),
												p.ind = c( 0, 1 ),
												p.conn.prev = c( 0, 0 ), p.conn.next = c( 1, 1 ) ) )
		}
		else if ( type1.type == 3 )
		{
			type1.type <<- type1.type
			intervals <<- gen.t3.intervals( c( 0, 0, 0 ), c( 1, 1, 1 ) )
		}
		else # if ( type1.type == 4 )
		{
			type1.type <<- type1.type
			intervals <<- list( gen.t3.intervals( c( 0, 0, 0 ), c( 1, 1, 1 ) ),
								gen.t3.intervals( c( 0, 0, 0 ), c( 1, 1, 1 ) ) )
		}

		mean <<- 0
		sd <<- usd <<- lsd <<- 1
		lsd.abs.error <<- usd.abs.error <<- 0
	}
)

################################################################################################
#' フィールド初期化
#'
#' コンストラクタを使わずに、 type1.type 以外のフィールドを初期状態にする。
#' @name    CGD_clear
#' @usage   CGD$clear(clear.intervals = TRUE)
#' @param   clear.intervals     intervals フィールドを初期化するかどうかのフラグ
#'                               (デフォルト: TRUE)。
#'                              FALSE にすると、 intervals フィールドはクリアされないが、
#'                              mean などの他のフィールドはクリアされてしまうので、
#'                              他のフィールドを明示的に再設定しないと、
#'                              オブジェクトは使い物にならなくなるので注意。
#' @return  なし
#' @examples
#'  a <- CGD$new()
#'  a$set.waypoints(
#'      data.frame( p = c( 0.25, 0.5, 0.75 ), q = c( -0.67, 0, 0.53 ) ),
#'      this.type1.type = 2, continuous = TRUE )
#'  a
#'  a$clear()
#'  a
################################################################################################
NULL
CGD$methods(
	clear = function( clear.intervals = TRUE )
	{
		median <<- NaN

		if ( clear.intervals )
		{
			intervals <<- list()
		}

		adjust.kind.index()
		mean <<- sd <<- usd <<- lsd <<- lsd.abs.error <<- usd.abs.error <<- NaN
	}
)

################################################################################################
#' 連結区間設定
#'
#' 連結区間を構成する正規分布の情報 (平均値、標準偏差、独立区間、接続区間) が
#' 分かっている場合に、 intervals フィールドを設定する。
#' クラスの他のフィールド (kind.index, kind, median, mean, sd など) も、
#' intervals の内容に応じて、適切に設定される。
#' @name    CGD_set.intervals
#' @usage   CGD$set.intervals(new.intervals, this.type1.type = NULL)
#' @param   new.intervals       連結区間 (\link[cgd]{CGDInterval} クラスのリスト)。
#' @param   this.type1.type     フィールドの type1.type に設定する値。
#'                              0、1、2、3、4 のいずれかを指定すること。
#'                              NULL の場合は type1.type の値を変更しない (デフォルト: NULL)。
#'                              0、1、2、3、4 以外の数値を指定した場合はエラーになる。
#'                              0～4 の詳細については、 \link[cgd]{trace.q} の Details を参照。
#' @return  分布の種類のインデックス番号
#' @seealso \link[cgd]{CGDInterval}, \link[cgd]{trace.q}, \link[cgd]{CGD_set.waypoints},
#'          \link[cgd]{nls.freq}, \link[cgd]{nls.freq.all}
#' @examples
#'  a1 <- CGD$new()
#'  a2 <- CGD$new()
#'  a3 <- CGD$new()
#'
#'  a1$set.waypoints(
#'  data.frame(
#'      p = c( 0.2, 0.5, 0.6, 0.7 ),
#'      q = c( qnorm( c( 0.2, 0.5, 0.6 ), 0, 1 ), 0.5 ) ),
#'  this.type1.type = 1 )
#'
#'  a2$set.intervals( a1$intervals )
#'  a3$set.intervals( a1$intervals, this.type1.type = 2 )
#'
#'  a1$sd
#'  plot( seq( 0.1, 0.6, 0.001 ), a1$d( seq( 0.1, 0.6, 0.001 ) ), type = "l" )
#'
#'  ## a2 is as same as a1
#'  a2$sd
#'  plot( seq( 0.1, 0.6, 0.001 ), a2$d( seq( 0.1, 0.6, 0.001 ) ), type = "l" )
#'
#'  ## a3 is a little different from a1
#'  a3$sd
#'  plot( seq( 0.1, 0.6, 0.001 ), a3$d( seq( 0.1, 0.6, 0.001 ) ), type = "l" )
################################################################################################
NULL
CGD$methods(
	set.intervals = function( new.intervals, this.type1.type = NULL )
	{
		# intervals フィールド設定
		#
		#	この処理は、フィールド初期化よりも先に実行する必要がある
		#	 (厳密には、フィールド初期化の前に new.intervals を使った何らかの処理を実行しておく必要がある)。
		#
		#	理由は、例えば、呼び出し元で a$set.intervals( new.intervals = list( CGDInterval$new( mean = a$median, ...
		#	のように、 CGDInterval$new() の引数に、自身のフィールドの値を渡している場合に、問題が生じるためである。
		#
		#	この場合、 CGDInterval$new() が実行されるタイミングは、 set.intervals() の処理に入る前ではなく、
		#	set.intervals() 関数の {} 内のコードに new.intervals が最初に現れた時点になる。
		#
		#	したがって、フィールドを初期化したあとに new.intervals を初めてコードに書いてしまうと、
		#	実際の new() の処理では、 set.intervals() を呼び出す時点のフィールドの値ではなく、
		#	初期化した後のフィールドの値が使われてしまう (上の例では、 CGDInterval$new() の引数 mean は NaN になる)。
		#	そのため、意図しないエラーが生じたり、誤った結果が得られることがある。
		#
		#	この問題を解決するには、フィールド初期化の前に new.intervals を使う処理を書いておけばよい。
		#	そうすれば、上の例でも、 CGDInterval$new() の引数 mean に、元の a$median の値が渡される。
		intervals <<- new.intervals

		# フィールドを初期化
		clear( FALSE )

		# type1.type フィールド設定
		if ( !is.null( this.type1.type ) )
		{
			if ( any( this.type1.type == 0:4 ) )
			{
				type1.type <<- this.type1.type
			}
			else
			{
				stop( paste( "Error: type1.type" , this.type1.type, "is undefined." ) )
			}
		}

		# median フィールド設定
		median <<- get.adjusted.median()

		# 分布の種類設定
		adjust.kind.index()

		# 平均値・標準偏差設定
		adjust.mean.sd()
	}
)

################################################################################################
#' 中央値取得
#'
#' \link[cgd]{CGD} クラスオブジェクトの intervals フィールドの内容から、
#' その連結ガウス分布の中央値を取得する。
#' 通常、このメソッドは、クラスの利用者が直接呼び出す必要はない。
#' 中央値を使用するには、 median フィールドを使えば十分である。
#' このメソッドは、クラス内部の処理で intervals フィールドを直接書き換えたときに、
#' median フィールドの値を再設定するために用意している。
#' @name    CGD_get.adjusted.median
#' @usage   CGD$get.adjusted.median()
#' @return  連結ガウス分布の中央値 (intervals 未設定時は NaN)
################################################################################################
NULL
CGD$methods(
	get.adjusted.median = function()
	{
		median.adj <- NaN

		if ( length( intervals ) == 0 )
		{
			return ( NaN )
		}

		if ( is.uni.mean() )
		{
			if ( type1.type == 4 )
			{
				median.adj <- intervals[[1]][[1]]$mean
			}
			else
			{
				median.adj <- intervals[[1]]$mean
			}
		}
		else
		{
			means <- intervals.mean()
			median.adj <- bisection( function( x ) { p( x ) - 0.5 }, c( min( means ), max( means ) ) )
		}

		return ( median.adj )
	}
)

################################################################################################
#' 構成要素の各正規分布の平均値取得
#'
#' intervals の各正規分布の mean の値をベクトルに整列して返す。
#' @name    CGD_intervals.mean
#' @usage   CGD$intervals.mean()
#' @return  intervals の mean の値を並べたベクトル
#' @examples
#'  a <- CGD$new()
#'  a$set.waypoints(
#'      data.frame( p = c( 0.25, 0.5, 0.75 ), q = c( -0.67, 0, 0.53 ) ),
#'      this.type1.type = 2, continuous = TRUE )
#'  a$intervals.mean()
#'
#'  a$set.waypoints(
#'      data.frame(
#'          p = c( 0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9 ),
#'          q = c( -1.40, -0.96, -0.61, -0.30, 0.32, 0.72, 1.23, 2.21 ) ),
#'  this.type1.type = 4 )
#'  a$intervals.mean()
################################################################################################
NULL
CGD$methods(
	intervals.mean = function()
	{
		if ( length( intervals ) == 0 )
		{
			return ( numeric() )
		}
		else
		{
			if ( type1.type == 4 )
			{
				return ( c( intervals[[1]][[1]]$mean, intervals[[1]][[2]]$mean, intervals[[1]][[3]]$mean,
							intervals[[2]][[1]]$mean, intervals[[2]][[2]]$mean, intervals[[2]][[3]]$mean ) )
			}
			else
			{
				return ( vapply( 1:length( intervals ), function( i ) intervals[[i]]$mean, 0 ) )
			}
		}
	}
)

################################################################################################
#' 独立区間の定義域取得
#'
#' intervals フィールドの q.ind の値を順に並べた data.frame を取得する。
#' @name    CGD_intervals.q.ind
#' @usage   CGD$intervals.q.ind()
#' @return  intervals の
#'          q.ind[1] を並べたベクトルと q.ind[2] を並べたベクトルの data.frame
#' @examples
#'  a <- CGD$new()
#'  a$set.waypoints(
#'      data.frame( p = c( 0.25, 0.5, 0.75 ), q = c( -0.67, 0, 0.53 ) ),
#'      this.type1.type = 1 )
#'  a$intervals.q.ind()
#'
#'  a$set.waypoints(
#'      data.frame(
#'          p = c( 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ),
#'          q = c( -1.40, -0.96, -0.61, -0.30, 0, 0.32, 0.72, 1.23, 2.21 ) ),
#'  this.type1.type = 2 )
#'  a$intervals.q.ind()
################################################################################################
NULL
CGD$methods(
	intervals.q.ind = function()
	{
		if ( type1.type == 4 )
		{
			return ( data.frame(
						q.ind.1 =
							c( intervals[[1]][[1]]$q.ind[1], intervals[[1]][[2]]$q.ind[1], intervals[[1]][[3]]$q.ind[1],
								intervals[[2]][[1]]$q.ind[1], intervals[[2]][[2]]$q.ind[1], intervals[[2]][[3]]$q.ind[1] ),
						q.ind.2 =
							c( intervals[[1]][[1]]$q.ind[2], intervals[[1]][[2]]$q.ind[2], intervals[[1]][[3]]$q.ind[2],
								intervals[[2]][[1]]$q.ind[2], intervals[[2]][[2]]$q.ind[2], intervals[[2]][[3]]$q.ind[2] ) ) )
		}
		else
		{
			return ( data.frame(
						q.ind.1 = vapply( 1:length( intervals ), function( i ) intervals[[i]]$q.ind[1], 0 ),
						q.ind.2 = vapply( 1:length( intervals ), function( i ) intervals[[i]]$q.ind[2], 0 ) ) )
		}
	}
)

################################################################################################
#' 独立区間の確率取得
#'
#' intervals フィールドの p.ind の値を順に並べた data.frame を取得する。
#' @name    CGD_intervals.p.ind
#' @usage   CGD$intervals.p.ind()
#' @return  intervals の
#'          p.ind[1] を並べたベクトルと p.ind[2] を並べたベクトルの data.frame
#' @examples
#'  a <- CGD$new()
#'  a$set.waypoints(
#'      data.frame( p = c( 0.25, 0.5, 0.75 ), q = c( -0.67, 0, 0.53 ) ),
#'      this.type1.type = 1 )
#'  a$intervals.p.ind()
#'
#'  a$set.waypoints(
#'      data.frame(
#'          p = c( 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ),
#'          q = c( -1.40, -0.96, -0.61, -0.30, 0, 0.32, 0.72, 1.23, 2.21 ) ),
#'  this.type1.type = 2 )
#'  a$intervals.p.ind()
################################################################################################
NULL
CGD$methods(
	intervals.p.ind = function()
	{
		if ( type1.type == 4 )
		{
			return ( data.frame(
						p.ind.1 =
							c( intervals[[1]][[1]]$p.ind[1], intervals[[1]][[2]]$p.ind[1], intervals[[1]][[3]]$p.ind[1],
								intervals[[2]][[1]]$p.ind[1], intervals[[2]][[2]]$p.ind[1], intervals[[2]][[3]]$p.ind[1] ),
						p.ind.2 =
							c( intervals[[1]][[1]]$p.ind[2], intervals[[1]][[2]]$p.ind[2], intervals[[1]][[3]]$p.ind[2],
								intervals[[2]][[1]]$p.ind[2], intervals[[2]][[2]]$p.ind[2], intervals[[2]][[3]]$p.ind[2] ) ) )
		}
		else
		{
			return ( data.frame(
						p.ind.1 = vapply( 1:length( intervals ), function( i ) intervals[[i]]$p.ind[1], 0 ),
						p.ind.2 = vapply( 1:length( intervals ), function( i ) intervals[[i]]$p.ind[2], 0 ) ) )
		}
	}
)

################################################################################################
#' 構成要素の各正規分布の標準偏差取得
#'
#' intervals フィールドの sd の値をベクトルに整列して返す。
#' @name    CGD_intervals.sd
#' @usage   CGD$intervals.sd()
#' @return  intervals の sd の値を並べたベクトル
#' @examples
#'  a <- CGD$new()
#'  a$set.waypoints(
#'      data.frame( p = c( 0.25, 0.5, 0.75 ), q = c( -0.67, 0, 0.53 ) ),
#'      this.type1.type = 2, continuous = TRUE )
#'  a$intervals.sd()
#'
#'  a$set.waypoints(
#'      data.frame(
#'          p = c( 0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9 ),
#'          q = c( -1.40, -0.96, -0.61, -0.30, 0.32, 0.72, 1.23, 2.21 ) ),
#'  this.type1.type = 4 )
#'  a$intervals.sd()
################################################################################################
NULL
CGD$methods(
	intervals.sd = function()
	{
		if ( length( intervals ) == 0 )
		{
			return ( numeric() )
		}
		else
		{
			if ( type1.type == 4 )
			{
				return ( c( intervals[[1]][[1]]$sd, intervals[[1]][[2]]$sd, intervals[[1]][[3]]$sd,
							intervals[[2]][[1]]$sd, intervals[[2]][[2]]$sd, intervals[[2]][[3]]$sd ) )
			}
			else
			{
				return ( vapply( 1:length( intervals ), function( i ) intervals[[i]]$sd, 0 ) )
			}
		}
	}
)

################################################################################################
#' 分布の種類のインデックス番号設定
#'
#' 分布の種類のインデックス番号のフィールドを正しく設定する。
#' この関数を呼び出す前に intervals フィールドと type1.type フィールドを
#' 設定しておく必要がある。
#' 通常、このメソッドは、クラスの利用者が直接呼び出す必要はない。
#' @name    CGD_adjust.kind.index
#' @usage   CGD$adjust.kind.index()
#' @return  分布の種類のインデックス番号 (フィールドにも同じ値が設定される)
################################################################################################
NULL
CGD$methods(
	adjust.kind.index = function()
	{
		index <- integer()

		if ( length( intervals ) > 0 )
		{
			index <- length( kinds )

			if ( is.uni.mean() && is.uni.sigma() )
			{
				index <- 1L # Normal Distribution
			}
			else if ( type1.type <= 2 )
			{
				if ( type1.type == 1 && is.continuous() )
				{
					index <- 2L # Mean of Median-Equaled Sigma-Differed 2 Normal Distributions
				}
				else if ( type1.type == 2 && is.symmetric() )
				{
					index <- 3L # Symmetric Horizontal Gradational Distribution
				}
				else if ( type1.type == 2 && is.continuous() )
				{
					if ( is.uni.sigma() )
					{
						index <- 4L # Median-Differed Sigma-Equaled Horizontal Gradational Distribution
					}
					else if ( is.uni.mean() )
					{
						index <- 5L # Median-Equaled Sigma-Differed Horizontal Gradational Distribution
					}
					else
					{
						index <- 6L # Median-Differed Sigma-Differed Horizontal Gradational Distribution
					}
				}
				else if ( intervals[[1]]$p.conn.prev[2] == 0 && intervals[[length( intervals )]]$p.conn.next[1] == 1 )
				{
					index <- length( kinds )	# Discontinuous Connected Gaussian Distribution
				}
			}
			else if ( type1.type == 3 )
			{
				if ( is.v.grad() )
				{
					if ( is.uni.sigma() )
					{
						index <- 7L # Median-Differed Sigma-Equaled Vertical Gradational Distribution
					}
					else if ( is.uni.mean() )
					{
						index <- 8L # Median-Equaled Sigma-Differed Vertical Gradational Distribution
					}
					else
					{
						index <- 9L # Median-Differed Sigma-Differed Vertical Gradational Distribution
					}
				}
				else
				{
					if ( is.uni.sigma() )
					{
						index <- 10L	# 3-Median-Differed Sigma-Equaled Vertical Gradational Distribution
					}
					else if ( is.uni.mean() )
					{
						index <- 11L	# Median-Equaled 3-Sigma-Differed Vertical Gradational Distribution
					}
					else
					{
						index <- 12L	# Median-Differed 3-Sigma-Differed Vertical Gradational Distribution
					}
				}
			}
			else if ( type1.type == 4 )
			{
				if ( is.uni.sigma() )
				{
					index <- 13L	# Median-Differed Sigma-Equaled Vertical-Horizontal Gradational Distribution
				}
				else if ( is.uni.mean() )
				{
					index <- 14L	# Median-Equaled Sigma-Differed Vertical-Horizontal Gradational Distribution
				}
				else
				{
					index <- 15L	# Median-Differed Sigma-Differed Vertical-Horizontal Gradational Distribution
				}
			}
		}

		kind.index <<- index
		if ( length( index ) == 0 )
		{
			kind <<- character()
		}
		else
		{
			kind <<- kinds[index]
		}

		return ( index )
	}
)

################################################################################################
#' 平均値・標準偏差設定
#'
#' 連結ガウス分布クラス (\link[cgd]{CGD}) の平均値と標準偏差のフィールドを
#' 正しく設定する。
#' この関数を呼び出す前に intervals フィールドと type1.type フィールドを
#' 設定し、かつ adjust.kind.index メソッドを呼び出しておく必要がある。
#' 通常、このメソッドは、クラスの利用者が直接呼び出す必要はない。
#' @name    CGD_adjust.mean.sd
#' @usage   CGD$adjust.mean.sd()
#' @return  この分布の種類のインデックス番号 (フィールドにも同じ値が設定される)
################################################################################################
NULL
CGD$methods(
	adjust.mean.sd = function()
	{
		means <- intervals.mean()
		sds <- intervals.sd()

		if ( length( kind.index ) == 0 )
		{
			# No data
			mean <<- sd <<- lsd <<- usd <<- NaN
		}
		else if ( kind.index == 1L )
		{
			# Normal Distribution
			mean <<- means[1]
			sd <<- lsd <<- usd <<- sds[1]
			lsd.abs.error <<- usd.abs.error <<- 0
		}
		else if ( kind.index == length( kinds ) )
		{
			# Discontinuous Distribution

			# 平均値を求める
			mean <<- sum( vapply( intervals, mean.nd.bound, 0 ) ) +
					 sum( vapply( 1:( length( intervals ) - 1 ),
									function( i )
									{
										mean.conn.bound( type1.type,
															intervals[[i]],
															intervals[[i + 1]] )
									}, 0 ) )

			# 分散を求める
			lv <- sum( vapply( intervals,
								function ( itv )
								{
									if ( itv$q.ind[1] < mean )
									{
										v.nd.bound( mean, itv$mean, itv$sd,
													c( itv$q.ind[1], min( itv$q.ind[2], mean ) ) )
									}
									else
									{
										0
									}
								}, 0 ) ) +
					sum( vapply( 1:( length( intervals ) - 1 ),
								function( i )
								{
									if ( intervals[[i]]$q.ind[2] < mean )
									{
										v.conn.bound( type1.type, mean,
														intervals[[i]],
														intervals[[i + 1]],
														x = c( intervals[[i]]$q.ind[2],
																min( intervals[[i + 1]]$q.ind[1], mean ) ) )
									}
									else
									{
										0
									}
								}, 0 ) )

			uv <- sum( vapply( intervals,
								function ( itv )
								{
									if ( mean < itv$q.ind[2] )
									{
										v.nd.bound( mean, itv$mean, itv$sd,
													c( max( mean, itv$q.ind[1] ), itv$q.ind[2] ) )
									}
									else
									{
										0
									}
								}, 0 ) ) +
					sum( vapply( 1:( length( intervals ) - 1 ),
								function( i )
								{
									if ( mean < intervals[[i + 1]]$q.ind[1] )
									{
										v.conn.bound( type1.type, mean,
														intervals[[i]],
														intervals[[i + 1]],
														x = c( max( mean, intervals[[i]]$q.ind[2] ),
																intervals[[i + 1]]$q.ind[1] ) )
									}
									else
									{
										0
									}
								}, 0 ) )

			# 標準偏差算出
			sd <<- sqrt( lv + uv )
			lsd <<- sqrt( 2 * lv )
			usd <<- sqrt( 2 * uv )
			lsd.abs.error <<- usd.abs.error <<- 0
		}
		else
		{
			# Continuous Distribution

			if ( type1.type == 1 || type1.type == 2 && is.symmetric() ||
				 type1.type == 3 && is.uni.mean() && is.v.grad() )
			{
				# Symmetric Distribution (S.D. = lower S.D = upper S.D.)
				mean <<- median

				sd <<- lsd <<- usd <<- sqrt( v.cont( type1.type, means, sds, mean, symmetric = TRUE ) )
				lsd.abs.error <<- usd.abs.error <<- 0
			}
			else
			{
				# Asymmetric Distribution
				mean <<- mean.cont( type1.type, means, sds )

				if ( type1.type == 4 )
				{
					sd <<- sqrt( v.cont( 4, means, sds, mean ) )

					lv <- v.cont.t4.via.integrate( means, sds, mean, get.lv = TRUE )
					lsd <<- sqrt( 2 * lv$value )
					lsd.abs.error <<- sqrt( 2 * lv$abs.error )

					uv <- v.cont.t4.via.integrate( means, sds, mean, get.uv = TRUE )
					usd <<- sqrt( 2 * uv$value )
					usd.abs.error <<- sqrt( 2 * uv$abs.error )
				}
				else
				{
					symmetric = is.symmetric()
					lv <- v.cont( type1.type, means, sds, mean, symmetric, get.lv = TRUE )
					uv <- v.cont( type1.type, means, sds, mean, symmetric, get.uv = TRUE )

					sd <<- sqrt( lv + uv )
					lsd <<- sqrt( 2 * lv )
					usd <<- sqrt( 2 * uv )
					lsd.abs.error <<- usd.abs.error <<- 0
				}
			}
		}
	}
)

################################################################################################
#' 分位点トレース
#'
#' 累積分布関数で分位点をトレースする \link[cgd]{CGD} クラスオブジェクトを生成する。
#' 本関数の本体は \link[cgd]{CGD_set.waypoints} メソッドである。
#' @export
#' @param   quantiles           分位点を指定する
#'                              data.frame( q = X座標 (クォンタイル), p = 確率 )。
#'                              X座標 (クォンタイル) は昇順にソートしておくこと。
#'                              不連続分布を構成する場合は、
#'								中央値 (p = 0.5 の点) を必ず与えること。
#' @param   continuous          TRUE にすると、 type1.type = 1 または 2 のとき、
#'                              独立区間を [0, 0] と [1, 1] の 2点にして、
#'                              確率密度関数が全区間 \eqn{(-\infty, \infty)} で連続になるように
#'                              構成を試みる (デフォルト: FALSE)。
#'                              type1.type = 1 または 2 のときのみ有効。
#' @param   symmetric           TRUE にすると、確率密度関数が
#'                              中央値を中心として左右対称になるように試みる (デフォルト: FALSE)。
#'                              type1.type = 1 または 2 のときのみ有効。
#'                              ただし、 type1.type = 2, symmetric = TRUE の確率密度関数は、
#'                              一般に、中央値の点で微分できない関数になる。
#'                              type1.type = 2, symmetric = TRUE のときは、
#'                              必ず中央値 (p = 0.5 の点) を quantiles に指定する必要がある。
#' @param   v.grad              TRUE にすると、 type1.type = 3 のとき、
#'                              裾部の左右の標準偏差が等しい、上下2つの正規分布による
#'                              縦方向グラデーションの分布を構成する (デフォルト: FALSE)。
#' @param   uni.sigma           TRUE にすると、連続分布に対して、
#'                              intervals の各正規分布の標準偏差を強制的に等しくする
#'                               (デフォルト: FALSE)。
#'                              経路の構成点が少ない場合のみ有効
#'                               (type1.type = 2 では 3点、
#'                                type1.type = 3 では 3～4点 (v.grad = TRUE の場合は 3点のみ)、
#'                                type1.type = 4 では 5点 の場合に有効)。
#' @param   diff.mean           TRUE にすると、 intervals の各正規分布の平均値が
#'                              異なる値になるよう試みる (デフォルト: FALSE)。
#'                              不連続分布および type1.type = 1 または symmetric = TRUE では無効。
#'                              なお、 diff.mean = TRUE としても、
#'                              与えられた分位点の条件によっては、
#'                              各要素の平均値がすべて等しくなることもあり得る。
#' @param   control             3nleqslv に渡す、同関数の control オプションのリスト
#'                               (デフォルト: list())。
#'                              詳細は \link[nleqslv]{nleqslv} の Control options を参照。
#'                              デフォルトは空だが、分位点の個数が少ない場合など、
#'                              条件不足で "Jacobian is singular" のエラーになる可能性が高い場合は
#'                              allowSingular = TRUE が暗黙のうちに設定される。
#'                              ただし、引数 control のリストに
#'                              allowSingular が与えられている場合は、引数のリストを優先する。
#' @param   type1.type          フィールドの type1.type に設定する値。
#'                              0、1、2、3、4 のいずれかを指定すること (デフォルト: 1)。
#'                              0、1、2、3、4 以外の数値を指定した場合はエラーになる。
#'                              詳細は Details を参照。
#' @return  nleqslv() を内部で実行した場合はその結果。それ以外は NULL
#' @importFrom  nleqslv     nleqslv
#' @importFrom  methods     new
#' @seealso \link[cgd]{CGD_set.waypoints},
#'          \href{https://github.com/Kimitsuna-Goblin/CGD}{README.md} (GitHub)
#' @details
#'  \subsection{type1.type}{
#'          type1.type フィールドは
#'          連結ガウス分布の累積分布関数を決定するためのオプションである。
#'
#'          連結ガウス分布には、連続分布と不連続分布があるが、
#'          連続分布は不連続分布の拡張である。そのため、まず、不連続分布から説明する。
#'
#'          \bold{不連続分布}では、
#'          確率密度関数や累積分布関数の定義域 \eqn{[-\infty, \infty]} において、
#'          1つの正規分布が独立的に分布を負担する
#'          \bold{独立区間}の定義域 \eqn{[\alpha_i, \beta_i]} と、
#'          2つの正規分布が分布を負担し合う
#'          \bold{接続区間}の定義域 \eqn{(\beta_i, \alpha_{i+1})} が交互に現れる。
#'
#'          \bold{接続区間}は、その定義域の範囲に
#'          中央値 \eqn{m} を含むかどうかという条件と、
#'          前後の独立区間を負担する正規分布の標準偏差の大小の条件によって、
#'          次の4つの type に分類される。
#'
#'          \itemize{
#'              \item type 1 : 定義域に \eqn{m} を含まない。標準偏差は山側の方が裾側よりも大。
#'              \item type 2 : 定義域に \eqn{m} を含まない。標準偏差は山側の方が裾側よりも小。
#'              \item type 3a : 定義域に \eqn{m} を含む。標準偏差は前側の方が後側よりも小。
#'              \item type 3b : 定義域に \eqn{m} を含む。標準偏差は前側の方が後側よりも大。
#'          }
#'
#'          本パッケージでは、 type 2/3a/3b の接続区間では、連結ガウス分布は
#'          原則として、2つの正規分布の平均とする
#'           (ただし、累積分布関数の単調増加性を保証するために、細かい場合分けがある。
#'            定義式は省略)。
#'
#'          一方、 type 1 の接続区間では、 type1.type のオプションに応じて、
#'          以下のように2つの正規分布を混合する。
#'
#'          \bold{type1.type = 1} は、不連続分布を構成する最も単純な方法であり、
#'                          2つの正規分布の混合比率を線形的に変えて混合する。
#'                          また、 continuous = TRUE または symmetric = TRUE
#'                          のオプションにより、
#'                          左右対称な連続分布である
#'                          \bold{「平均値が等しい2つの正規分布の平均」} が構成できる
#'                           (ただし、これは後述の type1.type = 2 のオプションと異なり、
#'                            type 1 の接続区間の拡張ではなく、
#'                            type 2/3a/3b の接続区間の拡張である)。
#'
#'          \bold{type1.type = 2} は、 1 と同様、不連続分布を構成する方法であるが、
#'                          混合比率の変化は線形的ではない。
#'                          このオプションでは、接続区間 \eqn{(\beta_1, \alpha_2)} を
#'                          \eqn{\beta_1 = -\infty, \alpha_2 = \infty} と拡張すれば、
#'                          連続分布も構成できるように工夫している。
#'                          連続分布を構成するには、 continuous = TRUE のオプションを使う。
#'                          このオプションでは、2つの確率密度関数による
#'                          \bold{「横方向グラデーション」} が構成できる。
#'                          この分布の確率密度関数の形は
#'                          \eqn{x = -\infty} の点から \eqn{x = \infty} の点に向かって、
#'                          横方向に徐々に変化していくようなイメージになる。
#'                          また、 symmetric = TRUE のオプションにより、
#'                          x = 中央値 の点で二つに線対称に折り返した、
#'                          左右対称な連続分布を構成できる
#'                           (ただし、文字通り「折り返して」いるため、
#'                            x = 中央値 の点において微分可能ではない)。
#'
#'          \bold{type1.type = 3} は連続分布に特化した、
#'                          \bold{「縦方向グラデーション」} の構成方法である。
#'                          このオプションでは、確率密度関数の形は、
#'                          中央値から遠い裾部から、中央値に近い山部に向かって、
#'                          縦方向に徐々に変化していくようなイメージになる。
#'                          v.grad = TRUE のオプションでは、
#'                          構成要素の正規分布は裾部と山部の2つになる。
#'                          v.grad = FALSE のオプションでは、
#'                          裾部の両側が異なる分布になるので、構成要素の正規分布は3つになる。
#'
#'          \bold{type1.type = 4} は正規分布の連結ではなく、
#'                          2つの連続な連結ガウス分布を連結した、
#'                          \bold{「縦横グラデーション」} の構成方法である。
#'                          このオプションでは、
#'                          2つの type1.type = 3, v.grad = TRUE (縦方向グラデーション) の分布を
#'                          type1.type = 2, continuous = TRUE (横方向グラデーション) で連結する。
#'                          この構成方法は、
#'                          本パッケージの連続分布の構成方法の中では、最も自由度が高い。
#'
#'          \bold{type1.type = 0} は不連続分布に特化した構成方法である。
#'                          このオプションでは、
#'                          経路の隣接する2点 \eqn{(q_i, p_i), (q_{i+1}, p_{i+1})}
#'                          を通る正規分布の累積分布関数をそのまま連続的に並べることによって、
#'                          連結ガウス分布を構成する。
#'                          その結果、接続区間の定義域は空集合となり、
#'                          全定義域 \eqn{x \in [-\infty, \infty]} の \eqn{x} が、
#'                          必ずいずれかの独立区間に含まれることになる。
#'
#'          \bold{連続分布}は、不連続分布の独立区間の定義域を特別な2つの区間
#'          \eqn{[-\infty, -\infty], [\infty, \infty]} のみとし、
#'          それら2つの区間を接続する接続区間の定義域を
#'          \eqn{(-\infty, \infty)} と拡張することによって、
#'          確率密度関数が \eqn{x \in (-\infty, \infty)} の範囲で
#'          連続になるように定義した分布である
#'           (ただし、 type1.type = 2, symmetric = TRUE のオプションでは、
#'            独立区間の定義域は
#'            \eqn{[-\infty, -\infty], [\mu, \mu], [\infty, \infty]} の3つ、
#'            接続区間の定義域は \eqn{(-\infty, \mu), (\mu, \infty)} の2つ)。
#'
#'          \itemize{
#'              \item type1.type = 1 : continuous or symmetric = TRUE \eqn{\Rightarrow} 3点
#'              \item type1.type = 2 : continuous = TRUE \eqn{\Rightarrow} 3～4点、
#'                                     symmetric = TRUE \eqn{\Rightarrow} 3点
#'                                      (必ず確率 0.5 の点を含めること)
#'              \item type1.type = 3 : v.grad = TRUE \eqn{\Rightarrow} 3～4点、
#'                                     v.grad = FALSE \eqn{\Rightarrow} 3～6点
#'              \item type1.type = 4 : 5～8点
#'          }
#'  }
#'
#'  \subsection{累積分布関数の定義式}{
#'          不連続分布では、
#'          独立区間については、
#'          \eqn{i} 番目の独立区間の
#'          定義域が \eqn{[\alpha_i, \beta_i]}、確率が \eqn{[a_i, b_i]} のとき、
#'          累積分布関数は \eqn{\Phi_i( \alpha_i ) = a_i, \Phi_i( \beta_i ) = b_i} を満たす
#'          正規分布の累積分布関数 \eqn{\Phi_i} とする。
#'
#'          ただし、
#'          type1.type = 1, 2 の場合は、
#'          すべての正規分布の平均値を連結ガウス分布の中央値 \eqn{m} に統一するが、
#'          type1.type = 0 の場合は、
#'          それぞれの正規分布の平均値を統一しない (接続区間が無いため、統一できない)。
#'
#'          接続区間については、
#'          \eqn{i} 番目の接続区間が type 1 のとき、定義域 \eqn{(\beta_i, \alpha_{i+1})} における
#'          累積分布関数 \eqn{\Psi_i(x)} を以下の式によって定める。
#'
#'          \describe{
#'              \item{type1.type = 1}{\deqn{
#'                  \Psi_i( x ) =
#'                      \dfrac{ \alpha_{i+1} - x }{ \alpha_{i+1} - \beta_i } \Phi_i( x ) +
#'                      \dfrac{ x - \beta_i }{ \alpha_{i+1} - \beta_i } \Phi_{i+1}( x )}}
#'
#'              \item{type1.type = 2}{\deqn{
#'                  \Psi_i( x ) = \dfrac{ \bar \Phi_i( \alpha_{i+1} ) -
#'                      \bar \Phi_i( x ) }{ \bar \Phi_i( \alpha_{i+1} ) -
#'                      \bar \Phi_i( \beta_i ) } \Phi_i( x ) +
#'                      \dfrac{ \bar \Phi_i( x ) - \bar \Phi_i( \beta_i ) }
#'                      { \bar \Phi_i( \alpha_{i+1} ) - \bar \Phi_i( \beta_i ) } \Phi_{i+1}( x )}}
#'          }
#'
#'          ここで、 \eqn{\Phi_i, \Phi_{i+1}} は
#'          当該接続区間の前後の独立区間を負担する正規分布の累積分布関数である。
#'          \eqn{\bar \Phi_i} は \eqn{\Phi_i} と \eqn{\Phi_{i+1}} の平均である。
#'
#'          連続分布では、累積分布関数 \eqn{\Psi(x)} を以下のように定める。
#'
#'          \describe{
#'              \item{type1.type = 1, continuous or symmetric = TRUE}{\deqn{
#'                  \Psi( x ) = \dfrac{\Phi_1( x ) + \Phi_2( x )}{2}}}
#'
#'              \item{type1.type = 2, continuous = TRUE}{\deqn{
#'                  \Psi( x ) = \displaystyle \int_{-\infty}^{x}
#'                      \left \lbrace ( 1 - \dfrac{f_1(t)}{\Phi_1(t)} ) f_1(t)
#'                      + \dfrac{f_2(t)}{\Phi_2(t)} f_2(t) \right \rbrace dt \\
#'                  = \Phi_1( x ) - \dfrac{\Phi_1( x )^2}{2} + \dfrac{\Phi_2( x )^2}{2}
#'                      \qquad \quad \ \ \ }}
#'
#'              \item{type1.type = 2, symmetric = TRUE}{\deqn{
#'                  \Psi( x ) = \genfrac{\lbrace}{}{0pt}{0}
#'                      { \Psi_1( x ) = \Phi_1( x ) - \Phi_1( x )^2 + \Phi_2( x )^2
#'                                      \quad ( x \leq \mu ) }
#'                      { \Psi_2( x ) = 1 - \Psi_1( 2\mu - x )
#'                                      \qquad \quad \, \, \ \ \ \ \ \ \ ( x > \mu ) }}}
#'
#'              \item{type1.type = 3, v.grad = TRUE}{\deqn{
#'                  \Psi(x) = \displaystyle \int_{-\infty}^{x}
#'                      \left \lbrace ( 1 - \dfrac{f_1(t)}{f_1(\mu_1)} ) f_1(t)
#'                      + \dfrac{f_2(t)}{f_2(\mu_2)} f_2(t) \right \rbrace dt \\
#'                  = \Phi_1(x) - \dfrac{\Phi^*_1(x)}{\sqrt{2}} + \dfrac{\Phi^*_2(x)}{\sqrt{2}}
#'                      \qquad \qquad \quad \ \ }}
#'
#'              \item{type1.type = 3, v.grad = FALSE}{\deqn{
#'                  \Psi(x) = \displaystyle \int_{-\infty}^{\min( x, \mu_1 )}
#'                      ( 1 - \dfrac{f_1(t)}{f_1(\mu_1)} ) f_1(t) \ dt
#'                      + \displaystyle \int_{-\infty}^x \dfrac{f_2(t)^2}{f_2(\mu_2)} \ dt
#'                      + \displaystyle \int_{\min( x, \mu_3 )}^x
#'                          ( 1 - \dfrac{f_3(t)}{f_3(\mu_3)} ) f_3(t) \ dt \\
#'                  \quad \ \ \ = \min( \Phi_1(x) - \dfrac{\Phi^*_1(x)}{\sqrt{2}}, \
#'                                      \dfrac{2 - \sqrt{2}}{4} )
#'                      + \dfrac{\Phi^*_2(x)}{\sqrt{2}}
#'                      + \max( 0, \ \Phi_3(x) -
#'                                   \dfrac{\Phi^*_3(x)}{\sqrt{2}} - \dfrac{2 - \sqrt{2}}{4} ) }}
#'
#'              \item{type1.type = 4}{\deqn{
#'                  \Psi(x) = \Psi_1(x) - \dfrac{\Psi_1(x)^2}{2} +
#'                                        \dfrac{\Psi_2(x)^2}{2} \qquad \qquad \quad \\
#'                  \Psi_i(x) =
#'                      \Phi_{i,1}(x) - \dfrac{\Phi^*_{i,1}(x)}{\sqrt{2}} +
#'                                      \dfrac{\Phi^*_{i,2}(x)}{\sqrt{2}}
#'                      \quad (i = 1, 2)}}
#'          }
#'
#'          ただし、\eqn{\Phi_i} および \eqn{\Phi_{i,j}} は各構成要素の正規分布の累積分布関数、
#'          \eqn{f_i} は i 番目の構成要素の正規分布の確率密度関数、
#'          \eqn{\mu_i} は i 番目の構成要素の正規分布の平均値、
#'          \eqn{\Phi^*_i} および \eqn{\Phi^*_{i,j}} は
#'          正規分布 \eqn{N(\mu_i, (\sigma_i / \sqrt{2})^2)} および
#'          \eqn{N(\mu_{i,j}, (\sigma_{i,j} / \sqrt{2})^2)} の累積分布関数である。
#'  }
#' @examples
#'  ## Discontinuous Example:
#'  ##  For discontinuous distribution, you can set waypoints as any.
#'  ##  The type1.type must be 1 or 2 or 0.
#'  a <- trace.q(
#'      data.frame(
#'          p = c( 0.2, 0.5, 0.6, 0.7 ),
#'          q = c( qnorm( c( 0.2, 0.5, 0.6 ), 0, 1 ), 0.5 ) ),
#'      type1.type = 1 )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  a <- trace.q(
#'      data.frame(
#'          p = c( 0.2, 0.5, 0.6, 0.7 ),
#'          q = c( qnorm( c( 0.2, 0.5, 0.6 ), 0, 1 ), 0.5 ) ),
#'      type1.type = 0 )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## Mean of 2 Normal Distributions Example:
#'  ##  The type1.type = 1, continuous = TRUE option-set gives mean of 2 normal distributions.
#'  ##  The number of p of waypoints must be 3.
#'  ##  And it is better for every point to take different distance from the mean.
#'  a <- trace.q(
#'      data.frame(
#'          p = c( 0.1, 0.5, 0.6 ),
#'          q = c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.6, 0, 0.75 ) ) ),
#'      type1.type = 1, continuous = TRUE )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## Horizontal Gradational Example:
#'  ##  The type1.type = 2, continuous = TRUE option-set
#'  ##  gives a horizontal gradational distribution.
#'  ##  The number of p of the waypoints must be 3 or 4.
#'  a <- trace.q(
#'      data.frame( p = c( 0.25, 0.5, 0.75 ), q = c( -0.67, 0, 0.53 ) ),
#'      type1.type = 2, continuous = TRUE )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## symmetric Example:
#'  ##  This option provides a distribution with a symmetric continuous
#'  ##  probability density function for type1.type = 2
#'  ##   (and you can use this option for type1.type = 1 as same meaning of "continuous").
#'  ##  With this option, the number of p of waypoints must be 3. And one of p[i] must be 0.5.
#'  ##  And it is recommended that two of quantiles are set as on the same side of
#'  ##  the probability density function as below.
#'  a <- trace.q(
#'      data.frame( p = c( 0.25, 0.4, 0.5 ), q = c( -0.67, -0.15, 0 ) ),
#'      type1.type = 2, symmetric = TRUE )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## uni.sigma Example:
#'  ##  This option constructs a distribution in which the standard deviations
#'  ##  of the normal distributions of the components are all equal.
#'  ##  Since this option reduces the degrees of freedom,
#'  ##  you can specify only limited number of quantile-probability points.
#'  a <- trace.q(
#'      data.frame( p = c( 0.25, 0.4, 0.5 ), q = c( -0.64, -0.25, 0 ) ),
#'      type1.type = 2, continuous = TRUE, uni.sigma = TRUE )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## Vertical Gradational Example:
#'  ##  The type1.type = 3, v.grad = TRUE option-set gives a vertical gradational distribution.
#'  ##  The number of p of waypoints must be 3 or 4.
#'  ##
#'  ##  Where number of p is 3, you should give each of 3 waypoints a specific role.
#'  ##  One is to specify a waypoint on the tail, one is for a waypoint on the head
#'  ##  and the other is to specify the median of the whole distribution.
#'  a <- trace.q(
#'      data.frame( p = c( 0.1, 0.4, 0.5 ), q = c( -1.28, -0.23, 0 ) ),
#'      type1.type = 3, v.grad = TRUE )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  a <- trace.q(
#'      data.frame( p = c( 0.1, 0.4, 0.6, 0.9 ), q = c( -1.92, -0.20, 0.20, 1.92 ) ),
#'      type1.type = 3, v.grad = TRUE )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ##  diff.mean = TRUE option force to make different all standard deviations
#'  ##  of the normal distributions of the components.
#'  ##  But even with this option, some of the standard deviations can be equal sometimes.
#'
#'  a <- trace.q(
#'      data.frame( p = c( 0.1, 0.3, 0.5 ), q = c( -1.28, -0.42, 0 ) ),
#'      type1.type = 3, v.grad = TRUE, diff.mean = TRUE )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## 3-Median/Sigma-Differed Vertical Gradational Example:
#'  ##  The type1.type = 3 without v.grad option also gives a vertical gradational distribution
#'  ##  but the normal distributions of both tail sides will be different each other.
#'  ##  The number of p of waypoints must be from 3 to 6.
#'  a <- trace.q(
#'      data.frame( p = c( 0.1, 0.4, 0.6, 0.9 ), q = c( -1.92, -0.20, 0.20, 1.92 ) ),
#'      type1.type = 3, v.grad = FALSE )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## Vertical-Horizontal Gradational Examples:
#'  ##  The type1.type = 4 option gives a vertical-horizontal gradational distribution.
#'  ##  The number of p of waypoints must be from 5 to 8.
#'  a <- trace.q(
#'      data.frame(
#'          p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
#'          q = c( -1.38, -0.76, -0.28, 0.02, 0.36, 1.10, 2.79 ) ),
#'      type1.type = 4 )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  a <- trace.q(
#'      data.frame(
#'          p = c( 0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9 ),
#'          q = c( -1.40, -0.96, -0.61, -0.30, 0.32, 0.72, 1.23, 2.21 ) ),
#'      type1.type = 4 )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
################################################################################################
trace.q <- function( quantiles, continuous = FALSE, symmetric = FALSE, v.grad = FALSE,
							uni.sigma = FALSE, diff.mean = FALSE, control = list(), type1.type = 1 )
{
	obj <- CGD$new()
	obj$set.waypoints( quantiles, continuous, symmetric, v.grad,
						uni.sigma, diff.mean, control, type1.type )
	return ( obj )
}

################################################################################################
#' 経路設定
#'
#' 累積分布関数の経路 (クォンタイル) を設定する。
#' ジェネレータ関数 \link[cgd]{trace.q} はこのメソッドを関数化したものである。
#' @name    CGD_set.waypoints
#' @usage   CGD$set.waypoints(waypoints, continuous = FALSE, symmetric = FALSE,
#'                            v.grad = FALSE, uni.sigma = FALSE, diff.mean = FALSE,
#'                            control = list(), this.type1.type = NULL )
#' @param   waypoints           経路の data.frame( q = 経路のX座標 (クォンタイル), p = 確率 )。
#'                              X座標 (クォンタイル) は昇順にソートしておくこと。
#'                              不連続分布を構成する場合は、
#'								中央値 (p = 0.5 の点) を必ず与えること。
#' @param   continuous          TRUE にすると、 type1.type = 1 または 2 のとき、
#'                              独立区間を [0, 0] と [1, 1] の 2点にして、
#'                              確率密度関数が全区間 \eqn{(-\infty, \infty)} で連続になるように
#'                              構成を試みる (デフォルト: FALSE)。
#'                              type1.type = 1 または 2 のときのみ有効。
#' @param   symmetric           TRUE にすると、確率密度関数が、
#'                              中央値を中心として左右対称になるように試みる (デフォルト: FALSE)。
#'                              type1.type = 1 または 2 のときのみ有効。
#'                              ただし、 type1.type = 2, symmetric = TRUE の確率密度関数は、
#'                              一般に、中央値の点で微分できない関数になる。
#'                              type1.type = 2, symmetric = TRUE のときは、
#'                              必ず中央値 (p = 0.5 の点) を waypoints に指定する必要がある。
#' @param   v.grad              TRUE にすると、 type1.type = 3 のとき、
#'                              裾部の左右の標準偏差が等しい、上下2つの正規分布による
#'                              縦方向グラデーションの分布を構成する (デフォルト: FALSE)。
#' @param   uni.sigma           TRUE にすると、連続分布に対して、
#'                              構成要素の各正規分布の標準偏差を強制的に等しくする
#'                               (デフォルト: FALSE)。
#'                              経路の構成点が少ない場合のみ有効
#'                               (type1.type = 1 では、本パッケージでは無効。
#'                                type1.type = 2 では 3点、type1.type = 3 では 3～4点
#'                                 (v.grad = TRUE の場合は 3点のみ)、
#'                                type1.type = 4 では 5点 の場合に有効)。
#' @param   diff.mean           TRUE にすると、各構成要素の正規分布の平均値が
#'                              異なるようになるように試みる (デフォルト: FALSE)。
#'                              不連続分布および type1.type = 1 または symmetric = TRUE では無効。
#'                              なお、 diff.mean = TRUE としても、
#'                              与えられた経路の条件によっては、
#'                              各要素の平均値がすべて等しくなることもあり得る。
#' @param   control             nleqslv に渡す、同関数の control オプションのリスト
#'                               (デフォルト: list())。
#'                              詳細は \link[nleqslv]{nleqslv} の Control options を参照。
#'                              デフォルトは空だが、条件不足のため "Jacobian is singular" の
#'                              エラーになる可能性が高い場合は allowSingular = TRUE が
#'                              暗黙のうちに設定される。
#'                              ただし、引数 control のリストに
#'                              allowSingular が与えられている場合は、引数のリストを優先する。
#' @param   this.type1.type     フィールドの type1.type に設定する値。
#'                              0、1、2、3、4 のいずれかを指定すること。
#'                              NULL の場合は type1.type の値を変更しない (デフォルト: NULL)。
#'                              0、1、2、3、4 以外の数値を指定した場合はエラーになる。
#'                              詳細は Details を参照。
#' @return  nleqslv() を内部で実行した場合はその結果。それ以外は NULL
#' @importFrom  nleqslv     nleqslv
#' @importFrom  methods     new
#' @importFrom  stats       pnorm qnorm
#' @seealso \link[cgd]{trace.q}, \href{https://github.com/Kimitsuna-Goblin/CGD}{README.md} (GitHub)
#' @details
#'  \subsection{type1.type}{
#'          type1.type フィールドは
#'          連結ガウス分布の累積分布関数を決定するためのオプションである。
#'
#'          連結ガウス分布には、連続分布と不連続分布があるが、
#'          連続分布は不連続分布の拡張である。そのため、まず、不連続分布から説明する。
#'
#'          \bold{不連続分布}では、
#'          確率密度関数や累積分布関数の定義域 \eqn{[-\infty, \infty]} において、
#'          1つの正規分布が独立的に分布を負担する
#'          \bold{独立区間}の定義域 \eqn{[\alpha_i, \beta_i]} と、
#'          2つの正規分布が分布を負担し合う
#'          \bold{接続区間}の定義域 \eqn{(\beta_i, \alpha_{i+1})} が交互に現れる。
#'
#'          \bold{接続区間}は、その定義域の範囲に
#'          中央値 \eqn{m} を含むかどうかという条件と、
#'          前後の独立区間を負担する正規分布の標準偏差の大小の条件によって、
#'          次の4つの type に分類される。
#'
#'          \itemize{
#'              \item type 1 : 定義域に \eqn{m} を含まない。標準偏差は山側の方が裾側よりも大。
#'              \item type 2 : 定義域に \eqn{m} を含まない。標準偏差は山側の方が裾側よりも小。
#'              \item type 3a : 定義域に \eqn{m} を含む。標準偏差は前側の方が後側よりも小。
#'              \item type 3b : 定義域に \eqn{m} を含む。標準偏差は前側の方が後側よりも大。
#'          }
#'
#'          本パッケージでは、 type 2/3a/3b の接続区間では、連結ガウス分布は
#'          原則として、2つの正規分布の平均とする
#'           (ただし、累積分布関数の単調増加性を保証するために、細かい場合分けがある。
#'            定義式は省略)。
#'
#'          一方、 type 1 の接続区間では、 type1.type のオプションに応じて、
#'          以下のように2つの正規分布を混合する。
#'
#'          \bold{type1.type = 1} は、不連続分布を構成する最も単純な方法であり、
#'                          2つの正規分布の混合比率を線形的に変えて混合する。
#'                          また、 continuous = TRUE または symmetric = TRUE
#'                          のオプションにより、
#'                          左右対称な連続分布である
#'                          \bold{「平均値が等しい2つの正規分布の平均」} が構成できる
#'                           (ただし、これは後述の type1.type = 2 のオプションと異なり、
#'                            type 1 の接続区間の拡張ではなく、
#'                            type 2/3a/3b の接続区間の拡張である)。
#'
#'          \bold{type1.type = 2} は、 1 と同様、不連続分布を構成する方法であるが、
#'                          混合比率の変化は線形的ではない。
#'                          このオプションでは、接続区間 \eqn{(\beta_1, \alpha_2)} を
#'                          \eqn{\beta_1 = -\infty, \alpha_2 = \infty} と拡張すれば、
#'                          連続分布も構成できるように工夫している。
#'                          連続分布を構成するには、 continuous = TRUE のオプションを使う。
#'                          このオプションでは、2つの確率密度関数による
#'                          \bold{「横方向グラデーション」} が構成できる。
#'                          この分布の確率密度関数の形は
#'                          \eqn{x = -\infty} の点から \eqn{x = \infty} の点に向かって、
#'                          横方向に徐々に変化していくようなイメージになる。
#'                          また、 symmetric = TRUE のオプションにより、
#'                          x = 中央値 の点で二つに線対称に折り返した、
#'                          左右対称な連続分布を構成できる
#'                           (ただし、文字通り「折り返して」いるため、
#'                            x = 中央値 の点において微分可能ではない)。
#'
#'          \bold{type1.type = 3} は連続分布に特化した、
#'                          \bold{「縦方向グラデーション」} の構成方法である。
#'                          このオプションでは、確率密度関数の形は、
#'                          中央値から遠い裾部から、中央値に近い山部に向かって、
#'                          縦方向に徐々に変化していくようなイメージになる。
#'                          v.grad = TRUE のオプションでは、
#'                          構成要素の正規分布は裾部と山部の2つになる。
#'                          v.grad = FALSE のオプションでは、
#'                          裾部の両側が異なる分布になるので、構成要素の正規分布は3つになる。
#'
#'          \bold{type1.type = 4} は正規分布の連結ではなく、
#'                          2つの連続な連結ガウス分布を連結した、
#'                          \bold{「縦横グラデーション」} の構成方法である。
#'                          このオプションでは、
#'                          2つの type1.type = 3, v.grad = TRUE (縦方向グラデーション) の分布を
#'                          type1.type = 2, continuous = TRUE (横方向グラデーション) で連結する。
#'                          この構成方法は、
#'                          本パッケージの連続分布の構成方法の中では、最も自由度が高い。
#'
#'          \bold{type1.type = 0} は不連続分布に特化した構成方法である。
#'                          このオプションでは、
#'                          経路の隣接する2点 \eqn{(q_i, p_i), (q_{i+1}, p_{i+1})}
#'                          を通る正規分布の累積分布関数をそのまま連続的に並べることによって、
#'                          連結ガウス分布を構成する。
#'                          その結果、接続区間の定義域は空集合となり、
#'                          全定義域 \eqn{x \in [-\infty, \infty]} の \eqn{x} が、
#'                          必ずいずれかの独立区間に含まれることになる。
#'
#'          \bold{連続分布}は、不連続分布の独立区間の定義域を特別な2つの区間
#'          \eqn{[-\infty, -\infty], [\infty, \infty]} のみとし、
#'          それら2つの区間を接続する接続区間の定義域を
#'          \eqn{(-\infty, \infty)} と拡張することによって、
#'          確率密度関数が \eqn{x \in (-\infty, \infty)} の範囲で
#'          連続になるように定義した分布である
#'           (ただし、 type1.type = 2, symmetric = TRUE のオプションでは、
#'            独立区間の定義域は
#'            \eqn{[-\infty, -\infty], [\mu, \mu], [\infty, \infty]} の3つ、
#'            接続区間の定義域は \eqn{(-\infty, \mu), (\mu, \infty)} の2つ)。
#'
#'          \itemize{
#'              \item type1.type = 1 : continuous or symmetric = TRUE \eqn{\Rightarrow} 3点
#'              \item type1.type = 2 : continuous = TRUE \eqn{\Rightarrow} 3～4点、
#'                                     symmetric = TRUE \eqn{\Rightarrow} 3点
#'                                      (必ず確率 0.5 の点を含めること)
#'              \item type1.type = 3 : v.grad = TRUE \eqn{\Rightarrow} 3～4点、
#'                                      v.grad = FALSE \eqn{\Rightarrow} 3～6点
#'              \item type1.type = 4 : 5～8点
#'          }
#'  }
#'
#'  \subsection{累積分布関数の定義式}{
#'          不連続分布では、
#'          独立区間については、
#'          \eqn{i} 番目の独立区間の
#'          定義域が \eqn{[\alpha_i, \beta_i]}、確率が \eqn{[a_i, b_i]} のとき、
#'          累積分布関数は \eqn{\Phi_i( \alpha_i ) = a_i, \Phi_i( \beta_i ) = b_i} を満たす
#'          正規分布の累積分布関数 \eqn{\Phi_i} とする。
#'
#'          ただし、
#'          type1.type = 1, 2 の場合は、
#'          すべての正規分布の平均値を連結ガウス分布の中央値 \eqn{m} に統一するが、
#'          type1.type = 0 の場合は、
#'          それぞれの正規分布の平均値を統一しない (接続区間が無いため、統一できない)。
#'
#'          接続区間については、
#'          \eqn{i} 番目の接続区間が type 1 のとき、定義域 \eqn{(\beta_i, \alpha_{i+1})} における
#'          累積分布関数 \eqn{\Psi_i(x)} を以下の式によって定める。
#'
#'          \describe{
#'              \item{type1.type = 1}{\deqn{
#'                  \Psi_i( x ) = \dfrac{ \alpha_{i+1} - x }{ \alpha_{i+1} - \beta_i } \Phi_i( x ) +
#'                      \dfrac{ x - \beta_i }{ \alpha_{i+1} - \beta_i } \Phi_{i+1}( x )}}
#'
#'              \item{type1.type = 2}{\deqn{
#'                  \Psi_i( x ) = \dfrac{ \bar \Phi_i( \alpha_{i+1} ) -
#'                      \bar \Phi_i( x ) }{ \bar \Phi_i( \alpha_{i+1} ) -
#'                      \bar \Phi_i( \beta_i ) } \Phi_i( x ) +
#'                      \dfrac{ \bar \Phi_i( x ) - \bar \Phi_i( \beta_i ) }
#'                      { \bar \Phi_i( \alpha_{i+1} ) - \bar \Phi_i( \beta_i ) } \Phi_{i+1}( x )}}
#'          }
#'
#'          ここで、 \eqn{\Phi_i, \Phi_{i+1}} は
#'          当該接続区間の前後の独立区間を負担する正規分布の累積分布関数である。
#'          \eqn{\bar \Phi_i} は \eqn{\Phi_i} と \eqn{\Phi_{i+1}} の平均である。
#'
#'          連続分布では、累積分布関数 \eqn{\Psi(x)} を以下のように定める。
#'
#'          \describe{
#'              \item{type1.type = 1, continuous or symmetric = TRUE}{\deqn{
#'                  \Psi( x ) = \dfrac{\Phi_1( x ) + \Phi_2( x )}{2}}}
#'
#'              \item{type1.type = 2, continuous = TRUE}{\deqn{
#'                  \Psi( x ) = \displaystyle \int_{-\infty}^{x}
#'                      \left \lbrace ( 1 - \dfrac{f_1(t)}{\Phi_1(t)} ) f_1(t)
#'                      + \dfrac{f_2(t)}{\Phi_2(t)} f_2(t) \right \rbrace dt \\
#'                  = \Phi_1( x ) - \dfrac{\Phi_1( x )^2}{2} + \dfrac{\Phi_2( x )^2}{2}
#'                      \qquad \quad \ \ \ }}
#'
#'              \item{type1.type = 2, symmetric = TRUE}{\deqn{
#'                  \Psi( x ) = \genfrac{\lbrace}{}{0pt}{0}
#'                      { \Psi_1( x ) = \Phi_1( x ) - \Phi_1( x )^2 + \Phi_2( x )^2
#'                                      \quad ( x \leq \mu ) }
#'                      { \Psi_2( x ) = 1 - \Psi_1( 2\mu - x )
#'                                      \qquad \quad \, \, \ \ \ \ \ \ \ ( x > \mu ) }}}
#'
#'              \item{type1.type = 3, v.grad = TRUE}{\deqn{
#'                  \Psi(x) = \displaystyle \int_{-\infty}^{x}
#'                      \left \lbrace ( 1 - \dfrac{f_1(t)}{f_1(\mu_1)} ) f_1(t)
#'                      + \dfrac{f_2(t)}{f_2(\mu_2)} f_2(t) \right \rbrace dt \\
#'                  = \Phi_1(x) - \dfrac{\Phi^*_1(x)}{\sqrt{2}} + \dfrac{\Phi^*_2(x)}{\sqrt{2}}
#'                      \qquad \qquad \quad \ \ }}
#'
#'              \item{type1.type = 3, v.grad = FALSE}{\deqn{
#'                  \Psi(x) = \displaystyle \int_{-\infty}^{\min( x, \mu_1 )}
#'                      ( 1 - \dfrac{f_1(t)}{f_1(\mu_1)} ) f_1(t) \ dt
#'                      + \displaystyle \int_{-\infty}^x \dfrac{f_2(t)^2}{f_2(\mu_2)} \ dt
#'                      + \displaystyle \int_{\min( x, \mu_3 )}^x
#'                                      ( 1 - \dfrac{f_3(t)}{f_3(\mu_3)} ) f_3(t) \ dt \\
#'                  \quad \ \ \ = \min( \Phi_1(x) - \dfrac{\Phi^*_1(x)}{\sqrt{2}}, \
#'                                      \dfrac{2 - \sqrt{2}}{4} )
#'                      + \dfrac{\Phi^*_2(x)}{\sqrt{2}}
#'                      + \max( 0, \ \Phi_3(x) - \dfrac{\Phi^*_3(x)}{\sqrt{2}} -
#'                                               \dfrac{2 - \sqrt{2}}{4} ) }}
#'
#'              \item{type1.type = 4}{\deqn{
#'                  \Psi(x) = \Psi_1(x) - \dfrac{\Psi_1(x)^2}{2} +
#'                                        \dfrac{\Psi_2(x)^2}{2} \qquad \qquad \quad \\
#'                  \Psi_i(x) =
#'                      \Phi_{i,1}(x) - \dfrac{\Phi^*_{i,1}(x)}{\sqrt{2}} +
#'                                      \dfrac{\Phi^*_{i,2}(x)}{\sqrt{2}} \quad (i = 1, 2)}}
#'          }
#'
#'          ただし、\eqn{\Phi_i} および \eqn{\Phi_{i,j}} は各構成要素の正規分布の累積分布関数、
#'          \eqn{f_i} は i 番目の構成要素の正規分布の確率密度関数、
#'          \eqn{\mu_i} は i 番目の構成要素の正規分布の平均値、
#'          \eqn{\Phi^*_i} および \eqn{\Phi^*_{i,j}} は
#'          正規分布 \eqn{N(\mu_i, (\sigma_i / \sqrt{2})^2)} および
#'          \eqn{N(\mu_{i,j}, (\sigma_{i,j} / \sqrt{2})^2)} の累積分布関数である。
#'  }
#' @examples
#'  ## Discontinuous Example:
#'  ##  For discontinuous distribution, you can set waypoints as any.
#'  ##  The type1.type must be 1 or 2 or 0.
#'  a <- CGD$new()
#'  a$set.waypoints(
#'      data.frame(
#'          p = c( 0.2, 0.5, 0.6, 0.7 ),
#'          q = c( qnorm( c( 0.2, 0.5, 0.6 ), 0, 1 ), 0.5 ) ),
#'      this.type1.type = 1 )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  a$set.waypoints(
#'      data.frame(
#'          p = c( 0.2, 0.5, 0.6, 0.7 ),
#'          q = c( qnorm( c( 0.2, 0.5, 0.6 ), 0, 1 ), 0.5 ) ),
#'      this.type1.type = 0 )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## Mean of 2 Normal Distributions Example:
#'  ##  The type1.type = 1, continuous = TRUE option-set gives mean of 2 normal distributions.
#'  ##  The number of p of waypoints must be 3.
#'  ##  And it is better for every point to take different distance from the median (p = 0.5).
#'  a$set.waypoints(
#'      data.frame(
#'          p = c( 0.1, 0.5, 0.6 ),
#'          q = c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.6, 0, 0.75 ) ) ),
#'      this.type1.type = 1, continuous = TRUE )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## Horizontal Gradational Example:
#'  ##  The type1.type = 2, continuous = TRUE option-set
#'  ##  gives a horizontal gradational distribution.
#'  ##  The number of p of the waypoints must be 3 or 4.
#'  a$set.waypoints(
#'      data.frame( p = c( 0.25, 0.5, 0.75 ), q = c( -0.67, 0, 0.53 ) ),
#'      this.type1.type = 2, continuous = TRUE )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## symmetric Example:
#'  ##  This option provides a distribution with a symmetric continuous
#'  ##  probability density function for type1.type = 2
#'  ##   (and you can use this option for type1.type = 1 as same meaning of "continuous").
#'  ##  With this option, the number of p of waypoints must be 3. And one of p[i] must be 0.5.
#'  ##  And it is recommended that two of quantiles are set as on the same side of
#'  ##  the probability density function as below.
#'  a$set.waypoints(
#'      data.frame( p = c( 0.25, 0.4, 0.5 ), q = c( -0.67, -0.15, 0 ) ),
#'      this.type1.type = 2, symmetric = TRUE )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## uni.sigma Example:
#'  ##  This option constructs a distribution in which the standard deviations
#'  ##  of the normal distributions of the components are all equal.
#'  ##  Since this option reduces the degrees of freedom,
#'  ##  you can specify only limited number of quantile-probability points.
#'  a$set.waypoints(
#'      data.frame( p = c( 0.25, 0.4, 0.5 ), q = c( -0.64, -0.25, 0 ) ),
#'      this.type1.type = 2, continuous = TRUE, uni.sigma = TRUE )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## Vertical Gradational Example:
#'  ##  The type1.type = 3, v.grad = TRUE option-set gives a vertical gradational distribution.
#'  ##  The number of p of waypoints must be 3 or 4.
#'  ##
#'  ##  Where number of p is 3, you should give each of 3 waypoints a specific role.
#'  ##  One is to specify a waypoint on the tail, one is for a waypoint on the head
#'  ##  and the other is to specify the median of the whole distribution.
#'  a$set.waypoints(
#'      data.frame( p = c( 0.1, 0.4, 0.5 ), q = c( -1.28, -0.23, 0 ) ),
#'      this.type1.type = 3, v.grad = TRUE )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  a$set.waypoints(
#'      data.frame( p = c( 0.1, 0.4, 0.6, 0.9 ), q = c( -1.92, -0.20, 0.20, 1.92 ) ),
#'      this.type1.type = 3, v.grad = TRUE )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ##  diff.mean = TRUE option force to make asymmetric the distribution.
#'
#'  a$set.waypoints(
#'      data.frame( p = c( 0.1, 0.3, 0.5 ), q = c( -1.28, -0.42, 0 ) ),
#'      this.type1.type = 3, v.grad = TRUE, diff.mean = TRUE )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## 3-Median/Sigma-Differed Vertical Gradational Example:
#'  ##  The type1.type = 3 without v.grad option also gives a vertical gradational distribution
#'  ##  but the normal distributions of both tail sides will be different each other.
#'  ##  The number of p of waypoints must be from 3 to 6.
#'  a$set.waypoints(
#'      data.frame( p = c( 0.1, 0.4, 0.6, 0.9 ), q = c( -1.92, -0.20, 0.20, 1.92 ) ),
#'      this.type1.type = 3, v.grad = FALSE )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  ## Vertical-Horizontal Gradational Examples:
#'  ##  The type1.type = 4 option gives a vertical-horizontal gradational distribution.
#'  ##  The number of p of waypoints must be from 5 to 8.
#'  a$set.waypoints(
#'      data.frame(
#'          p = c( 0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9 ),
#'          q = c( -1.38, -0.76, -0.28, 0.02, 0.36, 1.10, 2.79 ) ),
#'      this.type1.type = 4 )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'
#'  a$set.waypoints(
#'      data.frame(
#'          p = c( 0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9 ),
#'          q = c( -1.40, -0.96, -0.61, -0.30, 0.32, 0.72, 1.23, 2.21 ) ),
#'      this.type1.type = 4 )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
################################################################################################
NULL
CGD$methods(
	set.waypoints = function( waypoints, continuous = FALSE, symmetric = FALSE, v.grad = FALSE,
								uni.sigma = FALSE, diff.mean = FALSE, control = list(), this.type1.type = NULL )
	{
		result <- NULL

		# フィールドを初期化
		clear( TRUE )

		if ( !is.null( this.type1.type ) )
		{
			if ( any( this.type1.type == 0:4 ) )
			{
				type1.type <<- this.type1.type
			}
			else
			{
				stop( paste( "Error: type1.type" , this.type1.type, "is undefined." ) )
			}
		}
		else if ( !any( type1.type == 0:4 ) )
		{
			stop( paste( "Error: type1.type" , type1.type, "is undefined." ) )
		}

		# 確率 p の範囲チェック
		if ( length( waypoints$p[( waypoints$p < 0 | waypoints$p > 1 )] ) > 0 )
		{
			warning( paste( "Warning: Probability" ,
								waypoints$p[( waypoints$p < 0 | waypoints$p > 1 )], "is out of range [0, 1]." ) )
		}

		# X座標が±∞で、確率が 0 または 1 でない場合は警告
		if ( length( waypoints$q[( waypoints$q == -Inf & waypoints$p != 0 ) |
									( waypoints$q == Inf & waypoints$p != 1 )]	) > 0 )
		{
			warning( "Warning: There is a row which q is infinite (-Inf or Inf) but p is not 0 or 1." )
		}

		# 中央値を除いた経路の data.frame を取得
		wp.order <- order( waypoints$p )
		if ( nrow( waypoints ) > 1 )
		{
			# X座標が確率に対して昇順に並んでいなければエラー
			if ( any( waypoints$p[wp.order[1:( nrow( waypoints ) - 1 )]] >= waypoints$p[wp.order[2:nrow( waypoints )]] ) ||
					any( waypoints$q[wp.order[1:( nrow( waypoints ) - 1 )]] >= waypoints$q[wp.order[2:nrow( waypoints )]] ) )
			{
				stop( "Error: Order of q is not along with that of p." )
			}
		}

		# wp に p = 0, 1, 0.5 の点を除いた昇順ソート済みの経路の点を設定
		q.ordered <- waypoints$q[wp.order]
		p.ordered <- waypoints$p[wp.order]
		wp.with.median <- data.frame( q = q.ordered[p.ordered > 0 & p.ordered < 1 & q.ordered > -Inf & q.ordered < Inf],
									  p = p.ordered[p.ordered > 0 & p.ordered < 1 & q.ordered > -Inf & q.ordered < Inf] )

		wp <- wp.with.median[( wp.with.median$p != 0.5 ),]
		if ( nrow( wp ) == 0 )
		{
			warning( "Warning: No waypoints other than (p = 0, 0.5, 1) are given." )
		}

		# 中央値取得・エラーチェック
		is.set.median <- any( waypoints$p == 0.5 )
		if ( is.set.median )
		{
			median <<- waypoints$q[waypoints$p == 0.5][1]
		}

		if ( !is.set.median )
		{
			if ( any( type1.type == 1:2 ) && !continuous && !symmetric )
			{
				stop( "Error: The point of p = 0.5 must be given for discontinous model of type1.type = 1 or 2." )
			}
			else if ( type1.type == 2 && symmetric )
			{
				stop( "Error: The point of p = 0.5 must be given when type1.type = 2 and symmetric = TRUE." )
			}
		}

		point.num <- nrow( wp.with.median ) # 有効な経路上の点の個数

		if ( type1.type == 0 && point.num < 2 )
		{
			stop( "Error: 2 and more quantiles are needed for type1.type = 0." )
		}

		if ( continuous && !( ( type1.type == 1 && point.num == 3 ) ||
								( type1.type == 2 && ( ( point.num == 3 ) || point.num == 4 ) ) ) )
		{
			stop( "Error: Illegal number of quantiles or illegal type1.type for continuous = TRUE." )
		}

		if ( symmetric && !( ( type1.type == 1 || type1.type == 2 ) && point.num == 3 ) )
		{
			stop( "Error: Illegal number of quantiles or illegal type1.type for symmetric = TRUE." )
		}

		if ( v.grad && !( type1.type == 3 && ( point.num == 3 || point.num == 4 ) ) )
		{
			stop( "Error: Illegal number of quantiles or illegal type1.type for v.grad = TRUE." )
		}

		if ( uni.sigma && !( ( type1.type == 2 && point.num == 3 ) ||
								( type1.type == 3 && ( point.num == 3 || ( !v.grad && point.num == 4 ) ) ) ||
								( type1.type == 4 && point.num == 5 ) ) )
		{
			stop( "Error: Illegal number of quantiles or illegal type1.type for uni.sigma = TRUE." )
		}

		if ( diff.mean && ( type1.type == 1 || ( type1.type == 2 && !continuous ) || ( type1.type == 2 && symmetric ) ) )
		{
			stop( "Error: Illegal options for diff.mean = TRUE." )
		}

		if ( type1.type == 3 && !( point.num > 2 && point.num < 7 ) )
		{
			stop( "Error: Illegal number of quantiles for type1.type == 3." )
		}

		if ( type1.type == 4 && !( point.num > 4 && point.num < 9 ) )
		{
			stop( "Error: Illegal number of quantiles for type1.type == 4." )
		}

		####################################
		# 確率密度関数が連続な分布を構成

		#  nleqslv では、反復計算中に標準偏差が負値になって警告が出るのを防ぐために、標準偏差を2乗して計算する
		#	(2乗した方が、収束も1乗より少し速くなるようだ)

		# 分布構成の最大パス数を決定
		is.discontinuous <- is.max.point <- is.middle.max.point <- is.only.allowSingular <- FALSE

		retry.msg <- NULL	# 一旦構成に失敗してリトライするときの個別メッセージ

		if ( ( type1.type == 1 || type1.type == 2 ) && !continuous && !symmetric )
		{
			is.discontinuous <- TRUE
		}
		else
		{
			if ( ( type1.type == 1 && point.num == 3 ) ||
				( type1.type == 2 && point.num == 4 ) ||
				( type1.type == 3 && v.grad && point.num == 4 ) ||
				( type1.type == 3 && !v.grad && point.num == 6 ) ||
				( type1.type == 4 && point.num == 8 ) )
			{
				is.max.point <- TRUE
			}

			if ( ( type1.type == 3 && !is.set.median && point.num == 4 ) ||
				( type1.type == 4 && point.num == 6 ) )
			{
				is.middle.max.point <- TRUE
			}

			if ( type1.type == 4 && point.num == 7 )
			{
				is.only.allowSingular <- TRUE
			}
		}

		if ( type1.type == 3 && v.grad && point.num == 3 && is.set.median && wp.with.median$p[2] != 0.5 )
		{
			max.retry <- 2
		}
		else
		{
			max.retry <- 1
		}

		if ( uni.sigma || diff.mean || is.discontinuous || is.max.point || is.middle.max.point || is.only.allowSingular )
		{
			max.retry <- max.retry - 1
		}

		# 分布構成
		for ( retry in 0:max.retry )
		{
			x.0 <- NULL
			f <- NULL
			result.list <- NULL

			# nleqslv の初期値および目的関数定義
			if ( type1.type == 1 && ( continuous || symmetric ) )
			{
				if ( !is.set.median )
				{
					pseudo.mean <- ms.qp.norm( wp$q[c( 1, 3 )], wp$p[c( 1, 3 )] )$mean
					x.0 <- c( pseudo.mean, 0.9, 1.1 )

					mean.1 <- 1
					sd.1 <- 2
					sd.2 <- 3
				}
				else
				{
					x.0 <- c( 0.9, 1.1 )

					mean.1 <- 0
					sd.1 <- 1
					sd.2 <- 2
				}

				f <- function( x )
				{
					( pnorm( wp$q, ifelse( mean.1 == 0, median, x[mean.1] ), x[sd.1]^2 ) +
					  pnorm( wp$q, ifelse( mean.1 == 0, median, x[mean.1] ), x[sd.2]^2 ) ) / 2 - wp$p
				}

				result.list <- function( result )
				{
					list(
						CGDInterval$new(
							mean = ifelse( mean.1 == 0, median, result$x[mean.1] ),
							sd = result$x[sd.1]^2,
							q.ind = c( -Inf, -Inf ), q.conn.prev = c( -Inf, -Inf ), q.conn.next = c( -Inf, Inf ),
							p.ind = c( 0, 0 ), p.conn.prev = c( 0, 0 ), p.conn.next = c( 0, 1 ) ),
						CGDInterval$new(
							mean = ifelse( mean.1 == 0, median, result$x[mean.1] ),
							sd = result$x[sd.2]^2,
							q.ind = c( Inf, Inf ), q.conn.prev = c( -Inf, Inf ), q.conn.next = c( Inf, Inf ),
							p.ind = c( 1, 1 ), p.conn.prev = c( 0, 1 ), p.conn.next = c( 1, 1 ) ) )
				}
			}
			else if ( type1.type == 2 && continuous )
			{
				if ( !is.set.median )
				{
					pseudo.mean <- ms.qp.norm( wp$q[c( 1, 3 )], wp$p[c( 1, 3 )] )$mean
				}
				else
				{
					pseudo.mean <- median
				}

				if ( uni.sigma )
				{
					# 標準偏差を同一にして、平均を変えて連結
					# 仮の標準偏差を計算してから nleqslv を実行する
					if ( is.set.median )
					{
						x.0 <- c( 0, sqrt( ( sd.mqp.norm( pseudo.mean, wp$q[1], wp$p[1] )
												+ sd.mqp.norm( pseudo.mean, wp$q[2], wp$p[2] ) ) * 15 / 32 ) )
						mean.1 <- 0
						mean.2 <- 0
						sd.1 <- 2
					}
					else
					{
						x.0 <- c( pseudo.mean, pseudo.mean,
									sqrt( ( sd.mqp.norm( pseudo.mean, wp$q[1], wp$p[1] )
												+ sd.mqp.norm( pseudo.mean, wp$q[2], wp$p[2] ) ) * 15 / 32 ) )
						mean.1 <- 1
						mean.2 <- 2
						sd.1 <- 3

						# 実際の自由度 (< 3) よりも変数 (= 3) が多いので allowSingular = TRUE とする。
						if ( is.null( control$allowSingular ) )
						{
							control <- append( control, list( allowSingular = TRUE ) )
						}
					}

					f <- function( x )
					{
						p1 <- pnorm( wp$q, ifelse( mean.1 == 0, median - x[1], x[mean.1] ), x[sd.1]^2 )
						p2 <- pnorm( wp$q, ifelse( mean.2 == 0, median + x[1], x[mean.2] ), x[sd.1]^2 )

						return ( p1 - ( p1 * p1 - p2 * p2 ) / 2 - wp$p )
					}

					result.list <- function( result )
					{
						list(
							CGDInterval$new(
								mean = ifelse( mean.1 == 0, median - result$x[1], result$x[mean.1] ),
								sd = result$x[sd.1]^2,
								q.ind = c( -Inf, -Inf ),
								q.conn.prev = c( -Inf, -Inf ), q.conn.next = c( -Inf, Inf ),
								p.ind = c( 0, 0 ),
								p.conn.prev = c( 0, 0 ), p.conn.next = c( 0, 1 ) ),
							CGDInterval$new(
								mean = ifelse( mean.2 == 0, median + result$x[1], result$x[mean.2] ),
								sd = result$x[sd.1]^2,
								q.ind = c( Inf, Inf ),
								q.conn.prev = c( -Inf, Inf ), q.conn.next = c( Inf, Inf ),
								p.ind = c( 1, 1 ),
								p.conn.prev = c( 0, 1 ), p.conn.next = c( 1, 1 ) ) )
					}
				}
				else
				{
					if ( point.num == 3 )
					{
						if ( !diff.mean && retry < max.retry )
						{
							# 各正規分布の平均値を統一して、標準偏差を変えて連結
							if ( is.set.median )
							{
								x.0 <- c( 1, 1 )
							}
							else
							{
								x.0 <- c( pseudo.mean, 1, 1 )
							}

							mean.1 <- ifelse( is.set.median, 0, 1 )
							mean.2 <- ifelse( is.set.median, 0, 1 )
							sd.1 <- mean.2 + 1
							sd.2 <- mean.2 + 2
						}
						else
						{
							# 4点経路として計算 (条件不足のため allowSingular = TRUE とする)
							wp <- data.frame( q = wp.with.median$q[c( 1, 2, 2, 3 )],
											  p = wp.with.median$p[c( 1, 2, 2, 3 )] )
							if ( is.null( control$allowSingular ) )
							{
								control <- append( control, list( allowSingular = TRUE ) )
							}

							x.0 <- c( pseudo.mean, pseudo.mean, 1, 1 )

							mean.1 <- 1
							mean.2 <- 2
							sd.1 <- 3
							sd.2 <- 4
						}
					}
					else # if ( point.num == 4 )
					{
						# 4点経路
						# 各正規分布の仮の平均値と標準偏差を計算してから nleqslv を実行する
						wp <- wp.with.median
						pseudos <- list( ms.qp.norm( wp$q[1:2], wp$p[1:2] ),
										 ms.qp.norm( wp$q[3:4], wp$p[3:4] ) )
						x.0 <- c( pseudos[[1]]$mean, pseudos[[2]]$mean,
								  sqrt( pseudos[[1]]$sd ), sqrt( pseudos[[2]]$sd ) )

						mean.1 <- 1
						mean.2 <- 2
						sd.1 <- 3
						sd.2 <- 4
					}

					f <- function( x )
					{
						p1 <- pnorm( wp$q, ifelse( mean.1 == 0, median, x[mean.1] ), x[sd.1]^2 )
						p2 <- pnorm( wp$q, ifelse( mean.2 == 0, median, x[mean.2] ), x[sd.2]^2 )

						return ( p1 - ( p1 * p1 - p2 * p2 ) / 2 - wp$p )
					}

					result.list <- function( result )
					{
						list(
							CGDInterval$new(
								mean = ifelse( mean.1 == 0, median, result$x[mean.1] ),
								sd = result$x[sd.1]^2,
								q.ind = c( -Inf, -Inf ),
								q.conn.prev = c( -Inf, -Inf ), q.conn.next = c( -Inf, Inf ),
								p.ind = c( 0, 0 ),
								p.conn.prev = c( 0, 0 ), p.conn.next = c( 0, 1 ) ),
							CGDInterval$new(
								mean = ifelse( mean.2 == 0, median, result$x[mean.2] ),
								sd = result$x[sd.2]^2,
								q.ind = c( Inf, Inf ),
								q.conn.prev = c( -Inf, Inf ), q.conn.next = c( Inf, Inf ),
								p.ind = c( 1, 1 ),
								p.conn.prev = c( 0, 1 ), p.conn.next = c( 1, 1 ) ) )
					}
				}
			}
			else if ( type1.type == 2 && symmetric )
			{
				# symmetric の場合、高速化のために、経路の確率をすべて 0.5 以下にそろえて計算する
				wp$p <- 0.5 - abs( 0.5 - wp$p )
				wp$q <- median - abs( median - wp$q )

				x.0 <- c( 1, 1 )

				mean.1 <- 0
				sd.1 <- 1
				sd.2 <- 2

				f <- function( x )
				{
					p1 <- pnorm( wp$q, median, x[sd.1]^2 )
					p2 <- pnorm( wp$q, median, x[sd.2]^2 )

					return( ( 1 - p1 ) * p1 + p2 * p2 - wp$p )
				}

				result.list <- function( result )
				{
					list(
						CGDInterval$new(
							mean = median,
							sd = result$x[1]^2,
							q.ind = c( -Inf, -Inf ),
							q.conn.prev = c( -Inf, -Inf ), q.conn.next = c( -Inf, median ),
							p.ind = c( 0, 0 ),
							p.conn.prev = c( 0, 0 ), p.conn.next = c( 0, 0.5 ) ),
						CGDInterval$new(
							mean = median,
							sd = result$x[2]^2,
							q.ind = c( median, median ),
							q.conn.prev = c( -Inf, median ), q.conn.next = c( median, Inf ),
							p.ind = c( 0.5, 0.5 ),
							p.conn.prev = c( 0, 0.5 ), p.conn.next = c( 0.5, 1 ) ),
						CGDInterval$new(
							mean = median,
							sd = result$x[1]^2,
							q.ind = c( Inf, Inf ),
							q.conn.prev = c( median, Inf ), q.conn.next= c( Inf, Inf ),
							p.ind = c( 1, 1 ),
							p.conn.prev = c( 0.5, 1 ), p.conn.next = c( 1, 1 ) ) )
				}
			}
			else if ( type1.type == 3 && v.grad )
			{
				# type1.type == 3、縦方向グラデーション
				if ( uni.sigma )
				{
					# 中央値以外の2点を通る正規分布の平均値と標準偏差を仮の値とする
					pseudo <- ms.qp.norm( wp$q[1:2], wp$p[1:2] )

					x.0 <- c( pseudo$mean, ifelse( is.set.median, median, pseudo$mean ), sqrt( pseudo$sd ) )
					wp <- wp.with.median

					f <- function( x )
					{
						p <- pnorm( wp$q, x[1], x[3]^2 )
						p.a1 <- pnorm( wp$q, x[1], x[3]^2 * sqrt( 2 ) / 2 )
						p.a2 <- pnorm( wp$q, x[2], x[3]^2 * sqrt( 2 ) / 2 )

						return ( p - p.a1 * sqrt( 2 ) / 2 + p.a2 * sqrt( 2 ) / 2 - wp$p )
					}

					result.list <- function( result )
					{
						gen.t3.intervals( c( result$x[1], result$x[2], result$x[1] ), rep( result$x[3]^2, 3 ) )
					}
				}
				else if ( point.num == 3 && retry < max.retry )
				{
					if ( is.set.median && !diff.mean && retry == 0 )
					{
						# 中央値指定あり・3点経路
						# 2つの正規分布の平均値を全体の中央値に統一して、
						# 中央値以外の2点を累積分布関数が通るように標準偏差を求める
						x.0 <- c( 1, 1 )

						f <- function( x )
						{
							p <- pnorm( wp$q, median, x[1]^2 )
							p.a1 <- pnorm( wp$q, median, x[1]^2 * sqrt( 2 ) / 2 )
							p.a2 <- pnorm( wp$q, median, x[2]^2 * sqrt( 2 ) / 2 )

							return ( p - p.a1 * sqrt( 2 ) / 2 + p.a2 * sqrt( 2 ) / 2 - wp$p )
						}

						result.list <- function( result )
						{
							gen.t3.intervals( rep( median, 3 ),
											  c( result$x[1]^2, result$x[2]^2, result$x[1]^2 ) )
						}
					}
					else
					{
						# より歪んだ (あるいは中央値の指定がない) 3点経路
						#	この処理は特殊なループ処理なので、他のパスとは別に nleqslv まで独立して行う

						# 経路の1点 (2点目を最優先) で2つの正規分布が交差し、
						# 他の2点を累積分布関数が通るように平均値と標準偏差を求める
						# 歪みが小さければ、2点目で交差させて構成できるが、
						# 歪みが大きい場合は、他の点で交差しないと失敗することがある
						wp <- wp.with.median

						successed <- FALSE
						for ( i in c( 2, 3, 1 ) )
						{
							if ( wp$p[i] == 0.5 )
							{
								next
							}

							p.i <- c( 1:3 )[-i]
							means <- c( ms.qp.norm( wp$q[c( p.i[1], i )], wp$p[c( p.i[1], i )] )$mean,
										ms.qp.norm( wp$q[c( i, p.i[2] )], wp$p[c( i, p.i[2] )] )$mean )

							e <- try( result <- nleqslv( means, f <- function( x )
																{
																	x <- ifelse( sd.mqp.norm( x, wp$q[i], wp$p[i] ) < 0,
																					2 * wp$q[i] - x, x )
																	sds <- sd.mqp.norm( x, wp$q[i], wp$p[i] )

																	p <- pnorm( wp$q[p.i], x[1], sds[1] )
																	p.a1 <- pnorm( wp$q[p.i], x[1], sds[1] * sqrt( 2 ) / 2 )
																	p.a2 <- pnorm( wp$q[p.i], x[2], sds[2] * sqrt( 2 ) / 2 )

																	return ( p - p.a1 * sqrt( 2 ) / 2 +
																				 p.a2 * sqrt( 2 ) / 2 - wp$p[p.i] )
																}, control = control ), silent = TRUE )
							if ( inherits( e, "try-error" ) )
							{
								stop( "Error: Failed to construct a continuous probability density function." )
							}
							else if ( result$termcd == 1 )
							{
								means <- ifelse( sd.mqp.norm( result$x, wp$q[i], wp$p[i] ) < 0,
												 2 * wp$q[i] - result$x, result$x )
								sds <- sd.mqp.norm( means, wp$q[i], wp$p[i] )

								set.intervals( gen.t3.intervals( c( means[1], means[2], means[1] ),
																	c( sds[1], sds[2], sds[1] ) ) )
								successed <- TRUE
								break
							}

							message( paste( "nleqslv has once failed. Message:", result$message ) )
							message( paste( "Message: 3-point-tracing crossing at #", i, "point has failed.",
											"The result may distort heavily." ) )
						}

						if ( successed )
						{
							break
						}

						# 失敗したら4点経路としてリトライ
						message( paste( "Message: All of tries of 3-point-tracing have failed.",
										"4-point-tracing with allowSingular = TRUE has been retried." ) )
						next
					}
				}
				else # if ( point.num == 4 || retry == max.retry )
				{
					# 4点経路として構築試行
					if ( point.num == 3 )
					{
						wp <- data.frame( q = wp.with.median$q[c( 1, 2, 2, 3 )], p = wp.with.median$p[c( 1, 2, 2, 3 )] )

						# 条件不足のため allowSingular = TRUE とする
						if ( is.null( control$allowSingular ) )
						{
							control <- append( control, list( allowSingular = TRUE ) )
						}
					}
					else
					{
						wp <- wp.with.median
					}

					l <- t3.v.grad.wp4.intervals( wp, control )

					set.intervals( l$intervals )
					result <- l$result
					break
				}
			}
			else if ( type1.type == 3 && !v.grad )
			{
				if ( ( point.num == 3 || point.num == 4 ) && is.set.median && retry < max.retry )
				{
					# type1.type == 3、非対称、中央値指定あり
					# 経路の p < 0.5 と p > 0.5 のそれぞれの点の数によって、場合分けする
					wp <- wp.with.median[( wp.with.median$p != 0.5 ),]
					num.lower <- length( wp$p[wp$p < 0.5] )
					num.upper <- length( wp$p[wp$p > 0.5] )

					if ( num.lower >= 3 || num.upper >= 3 )
					{
						# 一方の側に3点以上ある場合、
						# 中央値指定なしの4点経路と同じ方法で累積分布関数を構成
						next
					}
					else
					{
						# 場合分けして累積分布関数を構成
						if ( num.lower == 1 && num.upper == 1 )
						{
							# ( #lower, #upper ) = ( 1, 1 )
							# 山の中央部の標準偏差は上下の裾部の標準偏差の平均とする
							x.0 <- c( 1, 1 )

							f <- function( x )
							{
								x.ave <- ( x[1]^2 + x[2]^2 ) / 2
								p <- pnorm( wp$q, median, x^2 )
								p.a1 <- pnorm( wp$q, median, x^2 * sqrt( 2 ) / 2 )
								p.a2 <- pnorm( wp$q, median, x.ave * sqrt( 2 ) / 2 )

								return ( p - p.a1 * sqrt( 2 ) / 2 + p.a2 * sqrt( 2 ) / 2 - wp$p )
							}

							result.list <- function( result )
							{
								sds <- c( result$x[1]^2, ( result$x[1]^2 + result$x[2]^2 ) / 2, result$x[2]^2 )
								gen.t3.intervals( rep( median, 3 ), sds )
							}
						}
						else
						{
							# ( #lower, #upper ) = ( 2, 0 ) or ( 0, 2 ) or ( 2, 1 ) or ( 1, 2 )

							# 2点ある側の点が、中央値に近いか遠いかに注意して、その2点を通る分布の標準偏差を得る
							#	2点ある側のうち、 q の値が小さい方のインデックスを wp.lower, 大きい方を wp.upper とし、
							#	中央値から遠い方のインデックスを xi.outer, 近い方を xi.inner とする
							#	 (同じインデックスなのに変数名を統一してないのは、あとの可読性のため)

							# ( #lower == 2 and ( #upper == 0 or 1 (i.e. any) ) ) || ( #lower == 0 and #upper == 2 )
							# ⇒ wp.lower = 1
							wp.lower <- ifelse( ( num.lower == 2 || num.lower == 0 ), 1, 2 )
							wp.upper <- ifelse( ( num.lower == 2 || num.lower == 0 ), 2, 3 )
							wp.q <- c( wp$q[wp.lower], wp$q[wp.upper] )
							wp.p <- c( wp$p[wp.lower], wp$p[wp.upper] )
							xi.outer <- ifelse( num.lower == 2, 1, 2 )
							xi.inner <- ifelse( num.lower == 2, 2, 1 )

							x.0 <- c( 1, 1 )

							f <- function( x )
							{
								p <- pnorm( wp.q, median, x[xi.outer]^2 )
								p.a1 <- pnorm( wp.q, median, x[xi.outer]^2 * sqrt( 2 ) / 2 )
								p.a2 <- pnorm( wp.q, median, x[xi.inner]^2 * sqrt( 2 ) / 2 )

								return ( p - p.a1 * sqrt( 2 ) / 2 + p.a2 * sqrt( 2 ) / 2 - wp.p )
							}

							result.list <- function( result )
							{
								sds <- numeric()

								if ( num.upper == 0 || num.lower == 0 )
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
									if ( num.lower == 2 )
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

									# エラーチェック (最後の標準偏差は算出可能か？)
									#	未計算の確率の残りが 0 より大きく ( 2 - sqrt(2) ) / 4 より小さければ算出可能
									p.remain <- wp$p[i.last] -
												pnorm( wp$q[i.last], median, sds[2] * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2
									if ( wp$p[i.last] < 0.5 )
									{
										if ( p.remain <= 0 )
										{
											# 最後の点の確率が低すぎるため、構成できない
											# => もっと最後の点のX座標が小さければ、構成できるかも知れない
											message( paste( "Message: The probability of the lowest quantile",
															"is too small. Or, the lowest quantile (x-coordinate)",
															"may be too near from the median of the data." ) )
											return ( NULL )
										}
										else if ( p.remain >= ( 0.5 - 0.25 * sqrt( 2 ) ) )
										{
											# 最後の点の確率が高すぎる (0.5に近すぎる) ため、構成できない
											# => もっと最後の点のX座標が中央値に近ければ、構成できるかも知れない
											message( paste( "Message: The probability of the lowest quantile",
															"is too near to 0.5. Or, the lowest quantile (x-coordinate)",
															"may be too far from the median of the data." ) )
											return ( NULL )
										}
									}
									else
									{
										if ( p.remain <= ( 0.5 - 0.25 * sqrt( 2 ) ) )
										{
											# 最後の点の確率が低すぎる (0.5に近すぎる) ため、構成できない
											# => もっと最後の点のX座標が中央値に近ければ、構成できるかも知れない
											message( paste( "Message: The probability of the highest quantile",
															"is too near to 0.5. Or, the highest quantile (x-coordinate)",
															"may be too far from the median of the data." ) )
											return ( NULL )
										}
										else if ( p.remain >= ( 1 - sqrt( 2 ) / 2 ) )
										{
											# 最後の点の確率が高すぎるため、構成できない
											# => もっと最後の点のX座標が大きければ、構成できるかも知れない
											message( paste( "Message: The probability of the highest quantile",
															"is too large. Or, the highest quantile (x-coordinate)",
															"may be too near from the median of the data." ) )
											return ( NULL )
										}
									}

									# 標準偏差の探索範囲決定
									if ( sds[2] == sd.mqp.norm( median, wp$q[i.last], wp$p[i.last] ) )
									{
										# 残りの1つの標準偏差は、山の中央部の標準偏差と等しい
										sds[i.last] <- sds[2]
									}
									else
									{
										# 残りの1つの標準偏差は、山の中央部の標準偏差と異なる
										if ( wp$p[i.last] < 0.5 )
										{
											sd.sup <- sd.mqp.norm( median, wp$q[i.last], ( 2 + sqrt( 2 ) ) * p.remain )
											sd.inf <- sd.mqp.norm( median, wp$q[i.last], p.remain )
										}
										else
										{
											sd.sup <- sd.mqp.norm( median, wp$q[i.last], ( 2 + sqrt( 2 ) ) * p.remain )
											sd.inf <- sd.mqp.norm( median, wp$q[i.last], p.remain + sqrt( 2 ) / 2 )
										}

										sds[i.last] <- bisection(
														function( x )
														{
															pnorm( wp$q[i.last], median, x ) -
															pnorm( wp$q[i.last], median, x * sqrt( 2 ) / 2 ) * sqrt( 2 ) / 2 -
															p.remain
														}, c( sd.inf, sd.sup ) )
									}
								}

								return ( gen.t3.intervals( rep( median, 3 ), sds ) )
							}
						}
					}
				}
				else if ( point.num == 3 && !is.set.median && !diff.mean && !uni.sigma && retry < max.retry )
				{
					# type1.type == 3、非対称、中央値指定なし、3点経路

					# 各正規分布の仮の平均値と標準偏差を計算
					# 経路の点を中央値に近い順に並べ、それを用いて、仮の平均値と標準偏差を計算し、
					# さらに、計算結果の山側の正規分布の標準偏差の算出にも用いる
					wp.near <- data.frame( q = wp$q[order( abs( wp$p - 0.5 ) )],
											p = wp$p[order( abs( wp$p - 0.5 ) )] )

					pseudos <- list( ms.qp.norm( wp.near$q[1:2], wp.near$p[1:2] ),
									ms.qp.norm( wp.near$q[2:3], wp.near$p[2:3]) )

					x.0 <- c( ( pseudos[[1]]$mean + pseudos[[2]]$mean ) / 2,
								sqrt( pseudos[[1]]$sd ), sqrt( pseudos[[2]]$sd ) )

					f <- function( x )
					{
						dp.t3( wp$q, rep( x[1], 3 ), c( x[2]^2, t3.mid.sd( x[1], wp.near ), x[3]^2 ), f.t3.p ) - wp$p
					}

					result.list <- function( result )
					{
						gen.t3.intervals( rep( result$x[1], 3 ),
											c( result$x[2]^2, t3.mid.sd( result$x[1], wp.near ), result$x[3]^2 ) )
					}

					retry.msg <- paste( "Message: Failed to construct a continuous probability density function",
										"without allowSingular option.",
										"Constructing with allowSingular option has been retried." )
				}
				else if ( point.num == 4 || ( point.num == 3 && retry == max.retry ) )
				{
					# type1.type == 3、4点経路として処理
					if ( point.num == 3 )
					{
						wp <- data.frame( q = wp.with.median$q[c( 1, 2, 2, 3 )],
										  p = wp.with.median$p[c( 1, 2, 2, 3 )] )

						# 条件不足のため allowSingular = TRUE とする
						if ( is.null( control$allowSingular ) )
						{
							control <- append( control, list( allowSingular = TRUE ) )
						}
					}
					else
					{
						wp <- wp.with.median
					}

					l <- t3.wp4.intervals( wp, uni.sigma, control )
					set.intervals( l$intervals )
					result <- l$result
					break
				}
				else if ( point.num == 5 && retry < max.retry )
				{
					# 各正規分布の仮の平均値と標準偏差を計算
					# 山側の正規分布の標準偏差は、中央値以外で、中央値に近い2点を通る正規分布の標準偏差の重み付き平均値とする。
					wp <- wp.with.median[( wp.with.median$p != 0.5 ),]
					wp.near <- data.frame( q = wp$q[order( abs( wp$p - 0.5 ) )][1:2],
										   p = wp$p[order( abs( wp$p - 0.5 ) )][1:2] )

					pseudos <- list( ms.qp.norm( wp.with.median$q[1:2], wp.with.median$p[1:2] ),
									 ms.qp.norm( wp.with.median$q[4:5], wp.with.median$p[4:5] ) )
					pseudo.2.mean <- ifelse( is.set.median, median,
												( wp.near$q[1] * ( wp.near$p[2] - 0.5 ) -
												  wp.near$q[2] * ( wp.near$p[1] - 0.5 ) ) /
												( wp.near$p[2] - wp.near$p[1] ) )

					means <- c( pseudos[[1]]$mean, pseudo.2.mean, pseudos[[2]]$mean )
					sds <- c( pseudos[[1]]$sd, pseudos[[2]]$sd )

					x.0 <- c( means, sqrt( sds ) )
					wp <- wp.with.median

					f <- function( x )
					{
						means <- c( x[1], x[2], x[3] )
						sds <- c( x[4]^2, t3.mid.sd( x[2], wp.near ), x[5]^2 )

						c( dp.t3( wp$q, means, sds, f.t3.p ) - wp$p )
					}

					result.list <- function( result )
					{
						gen.t3.intervals( result$x[1:3],
											c( result$x[4]^2, t3.mid.sd( result$x[2], wp.near ), result$x[5]^2 ) )
					}
				}
				else # if ( point.num == 6 || retry == max.retry )
				{
					# type1.type == 3、6点経路として計算
					# 各正規分布の仮の平均値と標準偏差を計算
					if ( point.num == 5 )
					{
						wp <- wp.with.median[( wp.with.median$p != 0.5 ),]
						wp.near <- data.frame( q = wp$q[order( abs( wp$p - 0.5 ) )][1:2],
												p = wp$p[order( abs( wp$p - 0.5 ) )][1:2] )

						pseudo.2.mean <- ifelse( is.set.median, median,
											( wp.near$q[1] * ( wp.near$p[2] - 0.5 ) -
											  wp.near$q[2] * ( wp.near$p[1] - 0.5 ) ) /
											( wp.near$p[2] - wp.near$p[1] ) )

						pseudos.2 <- list( mean = pseudo.2.mean,
											sd = t3.mid.sd( pseudo.2.mean, wp.near ) )
					}
					else
					{
						pseudos.2 <- ms.qp.norm( wp$q[3:4], wp$p[3:4] )
					}

					pseudos <- list( ms.qp.norm( wp$q[1:2], wp$p[1:2] ), pseudos.2,
										ms.qp.norm( wp$q[( nrow( wp ) - 1 ):nrow( wp )], wp$p[( nrow( wp ) - 1 ):nrow( wp )] ) )

					means <- c( pseudos[[1]]$mean, pseudos[[2]]$mean, pseudos[[3]]$mean )
					sds <- c( pseudos[[1]]$sd, pseudos[[2]]$sd, pseudos[[3]]$sd )

					x.0 <- c( means, sqrt( sds ) )

					if ( point.num == 5 )
					{
						wp <- data.frame( q = wp.with.median$q[c( 1, 2, 3, 3, 4, 5 )],
										  p = wp.with.median$p[c( 1, 2, 3, 3, 4, 5 )] )

						# 条件不足のため allowSingular = TRUE とする
						if ( is.null( control$allowSingular ) )
						{
							control <- append( control, list( allowSingular = TRUE ) )
						}
					}
					else
					{
						wp <- wp.with.median
					}

					f <- function( x )
					{
						c( dp.t3( wp$q, x[1:3], x[4:6]^2, f.t3.p ) - wp$p )
					}

					result.list <- function( result )
					{
						gen.t3.intervals( result$x[1:3], result$x[4:6]^2 )
					}
				}
			}
			else if ( type1.type == 4 && ( point.num == 5 || point.num == 6 ) )
			{
				# type1.type == 4、5～6点経路
				#	まず、上下の正規分布の平均値が等しい縦横グラデーションの構成を試し、
				#	ダメなら上下の正規分布の平均値が異なる縦横グラデーションを構成する

				# 各正規分布の仮の平均値と標準偏差を計算 (trace.q に control はあえて渡してない)
				# 6点経路の場合、2つの type1.type = 3 分布の仮の平均値は 3, 4点目 とする
				if ( any( wp.with.median$p[1:3] == 0.5 ) )
				{
					wp.d.1 <- wp.with.median[1:3,]
				}
				else
				{
					d.1.p <- c( wp.with.median$p[1:2], 0.5 )
					wp.d.1 <- data.frame( q = wp.with.median$q[order( d.1.p )], p = d.1.p[order( d.1.p )] )
				}
				e <- try( d.1 <- trace.q( wp.d.1, type1.type = 3, v.grad = TRUE,
											uni.sigma = uni.sigma, diff.mean = diff.mean ), silent = TRUE )
				if ( inherits( e, "try-error" ) )
				{
					stop( "Error: Failed to determine the drop-in parameters of the lower distribution." )
				}

				if ( any( wp.with.median$p[nrow( wp.with.median ) + ( -2:0 )] == 0.5 ) )
				{
					wp.d.2 <- wp.with.median[nrow( wp.with.median ) + ( -2:0 ),]
				}
				else
				{
					d.2.p <- c( 0.5, wp.with.median$p[nrow( wp.with.median ) + ( -1:0 )] )
					wp.d.2 <- data.frame( q = wp.with.median$q[order( d.2.p ) + nrow( wp.with.median ) - 3],
											p = d.2.p[order( d.2.p )] )
				}
				e <- try( d.2 <- trace.q( wp.d.2, type1.type = 3, v.grad = TRUE,
											uni.sigma = uni.sigma, diff.mean = diff.mean ), silent = TRUE )
				if ( inherits( e, "try-error" ) )
				{
					stop( "Error: Failed to determine the drop-in parameters of the upper distribution." )
				}

				if ( uni.sigma )
				{
					wp <- wp.with.median

					# length( x.0 ) == 5
					x.0 <- c( d.1$intervals[[1]]$mean, d.1$intervals[[2]]$mean,
								d.2$intervals[[1]]$mean, d.2$intervals[[2]]$mean,
								sqrt( ( d.1$intervals[[2]]$sd + d.2$intervals[[2]]$sd ) / 2 ) )

					f <- function( x )
					{
						sd.i <- x[5]^2
						p.1 <-	f.t3.p[[1]]( wp$q, x[1], sd.i ) + f.t3.p[[2]]( wp$q, x[2], sd.i )
						p.2 <-	f.t3.p[[1]]( wp$q, x[3], sd.i ) + f.t3.p[[2]]( wp$q, x[4], sd.i )

						p <- p.1 - p.1^2 / 2 + p.2^2 / 2
						return ( p - wp$p )
					}

					result.list <- function( result )
					{
						means <- result$x[1:4]
						sds <- rep( result$x[5]^2, 4 )

						return( list( gen.t3.intervals( c( means[1], means[2], means[1] ),
														c( sds[1], sds[2], sds[1] ) ),
									  gen.t3.intervals( c( means[3], means[4], means[3] ),
														c( sds[3], sds[4], sds[3] ) ) ) )
					}
				}
				else if ( point.num == 5 && !diff.mean && retry < max.retry )
				{
					# length( x.0 ) == 4
					x.0 <- sqrt( c( d.1$intervals[[1]]$sd, d.1$intervals[[2]]$sd,
									d.2$intervals[[1]]$sd, d.2$intervals[[2]]$sd ) )

					f <- function( x )
					{
						sds <- x^2
						p.1 <-	f.t3.p[[1]]( wp$q, median, sds[1] ) +
								f.t3.p[[2]]( wp$q, median, sds[2] )
						p.2 <-	f.t3.p[[1]]( wp$q, median, sds[3] ) +
								f.t3.p[[2]]( wp$q, median, sds[4] )

						p <- p.1 - p.1^2 / 2 + p.2^2 / 2
						return ( p - wp$p )
					}

					result.list <- function( result )
					{
						means <- rep( median, 4 )
						sds <- result$x[1:4]^2

						return( list( gen.t3.intervals( c( means[1], means[2], means[1] ),
														c( sds[1], sds[2], sds[1] ) ),
									  gen.t3.intervals( c( means[3], means[4], means[3] ),
														c( sds[3], sds[4], sds[3] ) ) ) )
					}
				}
				else
				{
					if ( point.num == 5 )
					{
						wp <- data.frame( q = wp.with.median$q[c( 1, 2, 3, 3, 4, 5 )],
											p = wp.with.median$p[c( 1, 2, 3, 3, 4, 5 )] )

						# 条件不足のため allowSingular = TRUE とする
						if ( is.null( control$allowSingular ) )
						{
							control <- append( control, list( allowSingular = TRUE ) )
						}
					}
					else
					{
						wp <- wp.with.median
					}

					# length( x.0 ) == 6
					x.0 <- c( d.1$mean, d.2$mean,
								sqrt( c( d.1$intervals[[1]]$sd, d.1$intervals[[2]]$sd,
											d.2$intervals[[1]]$sd, d.2$intervals[[2]]$sd ) ) )
					f <- function( x )
					{
						sds <- x[3:6]^2
						p.1 <-	f.t3.p[[1]]( wp$q, x[1], sds[1] ) + f.t3.p[[2]]( wp$q, x[1], sds[2] )
						p.2 <-	f.t3.p[[1]]( wp$q, x[2], sds[3] ) + f.t3.p[[2]]( wp$q, x[2], sds[4] )

						p <- p.1 - p.1^2 / 2 + p.2^2 / 2
						return ( p - wp$p )
					}

					result.list <- function( result )
					{
						means <- c( result$x[1], result$x[1], result$x[2], result$x[2] )
						sds <- result$x[3:6]^2

						return( list( gen.t3.intervals( c( means[1], means[2], means[1] ),
														c( sds[1], sds[2], sds[1] ) ),
									  gen.t3.intervals( c( means[3], means[4], means[3] ),
														c( sds[3], sds[4], sds[3] ) ) ) )
					}
				}
			}
			else if ( type1.type == 4 ) # && ( point.num == 7 || point.num == 8 )
			{
				# type1.type == 4、7～8点経路
				#	上下の正規分布の平均値が異なる縦横グラデーションを構成する

				# 各正規分布の仮の平均値と標準偏差を計算
				#	7～8点経路では、2つの type1.type = 3 分布の経路はX座標・確率とも、指定された通りとする
				#	type1.type = 3 分布は、4点経路は成功率が低いので、3点で構成する。失敗したら個別構成

				wp.q <- c( wp$q[1:3], ifelse( rep( point.num == 7, 2 ), wp.with.median$q[4:4], wp.with.median$q[4:5] ),
							wp$q[( nrow( wp ) - 2 ):nrow( wp )] )

				wp.p <- c( wp$p[1:3], ifelse( rep( point.num == 7, 2 ), wp.with.median$p[4:4], wp.with.median$p[4:5] ),
							wp$p[( nrow( wp ) - 2 ):nrow( wp )] )

				e <- try( d.1 <- trace.q( data.frame( q = wp.q[1:3], p = wp.p[1:3] ),
											type1.type = 3, v.grad = TRUE ), silent = TRUE )
				if ( inherits( e, "try-error" ) )
				{
					message( paste( "Message: Failed to the 3-point-trace algorithm for the drop-in parameters",
									"of the lower distribution.",
									"So the combination of 2-point-trace algorithms has been used instead." ) )
					pseudos <- list( ms.qp.norm( wp.q[1:2], wp.p[1:2] ), ms.qp.norm( wp.q[2:3], wp.p[2:3] ) )

					d.1 <- CGD$new( type1.type = 3 )
					d.1$set.intervals( gen.t3.intervals( c( pseudos[[1]]$mean, pseudos[[2]]$mean, pseudos[[1]]$mean ),
														c( pseudos[[1]]$sd, pseudos[[2]]$sd, pseudos[[1]]$sd ) ) )
				}

				e <- try( d.2 <- trace.q( data.frame( q = wp.q[6:8], p = wp.p[6:8] ),
											type1.type = 3, v.grad = TRUE ), silent = TRUE )
				if ( inherits( e, "try-error" ) )
				{
					message( paste( "Message: Failed to the 3-point-trace algorithm for the drop-in parameters",
									"of the upper distribution.",
									"So the combination of 2-point-trace algorithms has been used instead." ) )
					pseudos <- list( ms.qp.norm( wp.q[7:8], wp.p[7:8] ), ms.qp.norm( wp.q[5:6], wp.p[5:6] ) )

					d.2 <- CGD$new( type1.type = 3 )
					d.2$set.intervals( gen.t3.intervals( c( pseudos[[1]]$mean, pseudos[[2]]$mean, pseudos[[1]]$mean ),
														c( pseudos[[1]]$sd, pseudos[[2]]$sd, pseudos[[1]]$sd ) ) )
				}

				# 7点経路では、条件不足のため allowSingular = TRUE とする
				if ( point.num == 7 && is.null( control$allowSingular ) )
				{
					control <- append( control, list( allowSingular = TRUE ) )
				}

				x.0 <- c(	d.1$intervals[[1]]$mean, sqrt( d.1$intervals[[1]]$sd ),
							d.1$intervals[[2]]$mean, sqrt( d.1$intervals[[2]]$sd ),
							d.2$intervals[[1]]$mean, sqrt( d.2$intervals[[1]]$sd ),
							d.2$intervals[[2]]$mean, sqrt( d.2$intervals[[2]]$sd ) )

				f <- function( x )
				{
					sds <- x[seq( 2, 8, 2 )]^2
					p.1 <-	f.t3.p[[1]]( wp.q, x[1], sds[1] ) + f.t3.p[[2]]( wp.q, x[3], sds[2] )
					p.2 <-	f.t3.p[[1]]( wp.q, x[5], sds[3] ) + f.t3.p[[2]]( wp.q, x[7], sds[4] )

					p <- p.1 - p.1^2 / 2 + p.2^2 / 2
					return ( p - wp.p )
				}

				result.list <- function( result )
				{
					list( gen.t3.intervals( c( result$x[1], result$x[3], result$x[1] ),
											c( result$x[2], result$x[4], result$x[2] )^2 ),
						  gen.t3.intervals( c( result$x[5], result$x[7], result$x[5] ),
											c( result$x[6], result$x[8], result$x[6] )^2 ) )
				}
			}

			# nleqslv の計算
			if ( !is.null( x.0 ) )
			{
				intervals.result <- NULL

				e <- try( result <- nleqslv( x.0, f, control = control ), silent = TRUE )
				if ( inherits( e, "try-error" ) )
				{
					stop( "Error: Failed to construct a continuous probability density function." )
				}
				else if ( result$termcd == 1 )
				{
					intervals.result <- result.list( result )
				}

				if ( !is.null( intervals.result ) )
				{
					set.intervals( intervals.result )
					break
				}
				else
				{
					if ( retry < max.retry )
					{
						if ( result$termcd != 1 )
						{
							message( paste( "nleqslv has once failed. Message:", result$message ) )
						}

						if ( is.null( retry.msg ) )
						{
							message( paste( "Message: Failed to construct",
											"a mean-equaled continuous probability density function.",
											"Constructing mean-differed one has been retried." ) )
						}
						else
						{
							message( retry.msg )
						}
					}
					else
					{
						message( paste( "nleqslv has failed. Message:", result$message ) )
						if ( symmetric )
						{
							stop( "Error: Failed to construct a symmetric probability density function." )
						}
						else
						{
							stop( "Error: Failed to construct a continuous probability density function." )
						}
					}
				}
			}
		}

		if ( length( intervals ) > 0 )
		{
			return ( result )
		}

		####################################
		# 確率密度関数が不連続な分布を構成

		# 経路上の点を通る正規分布の平均値と標準偏差を取得
		if ( type1.type == 0 )
		{
			# type1.type = 0 の場合は、中央値の点も、他の点と同様に処理する
			wp <- wp.with.median

			ms <- unname( unlist( lapply( as.list( 1:( nrow( wp ) - 1 ) ),
											function( i )
											{
												ms.qp.norm( wp$q[i:( i + 1 )], wp$p[i:( i + 1 )] )
											} ) ) )
			means <- ms[seq( 1, length( ms ) - 1, 2 )]
			sds <- ms[seq( 2, length( ms ), 2 )]
		}
		else
		{
			means <- rep( median, nrow( wp ) )
			sds <- sd.mqp.norm( means, wp$q, wp$p )
		}

		# 連結区間を設定
		#	ループ回数の上限は wp の行数ではなく、
		#	sds (あるいは means) のベクトル長とする。
		#	type1.type = 1 では、それらの値は等しいが、
		#	type1.type = 0 では、sds のベクトル長は wp の行数より 1 小さいことに注意
		#	 (type1.type = 0 では、
		#	  wp の最後の点は必ず、最後-1 の点と同じ独立区間に入るので、それで良い)。
		i <- 1
		intervals.tmp <- list()
		while ( i <= length( sds ) )
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
				# 前の区間との接続区間の確率・定義域を取得
				if ( type1.type == 0 )
				{
					p.conn.prev <- c( wp[i,]$p, wp[i,]$p )
					q.conn.prev <- c( wp[i,]$q, wp[i,]$q )
				}
				else
				{
					p.conn.prev <- intervals.tmp[[length( intervals.tmp )]]$p.conn.next
					q.conn.prev <- c( qnorm( intervals.tmp[[length( intervals.tmp )]]$p.conn.next[1], median, sds[i] ),
									  wp[i,]$q )
				}

				# 独立区間の確率・定義域の下限は前の区間との接続区間の確率・定義域の上限
				p.ind <- c( p.conn.prev[2], 1 )
				q.ind <- c( q.conn.prev[2], Inf )
			}

			# 標準偏差を変えずに次の経路上の点を通れるか探索
			if ( i <= length( sds ) )
			{
				j <- i + 1
				extended.to <- 0	# 延長先の区間
				while ( j <= length( sds ) )
				{
					if ( means[i] == means[j] && sds[i] == sds[j] )
					{
						# 平均値と標準偏差が次の経路上の点を通る正規分布のそれらと等しい場合
						# 独立区間を次の経路上の点まで延長
						p.ind[2] <- wp[j,]$p
						q.ind[2] <- qnorm( wp[j,]$p, median, sds[i] )
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
			if ( i == length( sds ) )
			{
				# 経路の最後の点まで来たので、次の区間は無し
				p.conn.next <- c( 1, 1 )
				q.conn.next <- c( Inf, Inf )

				# 経路の最後の点まで通過できた場合、独立区間の定義域は無限大まで
				p.ind[2] <- 1
				q.ind[2] <- Inf
			}
			else
			{
				if ( type1.type == 0 )
				{
					p.conn.next <- c( wp[i + 1,]$p, wp[i + 1,]$p )
					q.conn.next <- c( wp[i + 1,]$q, wp[i + 1,]$q )
				}
				else
				{
					p.conn.next <- c( wp[i,]$p, wp[i + 1,]$p )
					q.conn.next <- c( qnorm( wp[i,]$p, median, sds[i] ),
									  qnorm( wp[i + 1,]$p, median, sds[i] ) )
				}

				# 独立区間の定義域・確率の上限は次の区間との接続区間の定義域・確率の下限
				p.ind[2] <- p.conn.next[1]
				q.ind[2] <- q.conn.next[1]
			}

			# 連結区間クラスのインスタンス生成
			intervals.tmp <- c( intervals.tmp, CGDInterval$new(
													mean = means[i],
													sd = sds[i],
													q.ind = q.ind, q.conn.prev = q.conn.prev, q.conn.next = q.conn.next,
													p.ind = p.ind, p.conn.prev = p.conn.prev, p.conn.next = p.conn.next ) )
			i <- i + 1
		}
		set.intervals( intervals.tmp )

		return ( result )
	}
)

################################################################################################
#' [内部関数] 確率密度関数・累積分布関数計算 (type1.type=3)
#'
#' type1.type=3 の確率密度関数または累積分布関数の値を得る
#' @param   x           X座標 (クォンタイル) のベクトル。
#' @param   means       intervals の平均値のベクトル。
#' @param   sds         intervals の標準偏差のベクトル。
#' @param   f.t3        確率密度 / 累積分布用関数ハンドル ( f.t3.d / f.t3.p )。
#' @return  確率密度関数または累積分布関数の値のベクトル
################################################################################################
dp.t3 <- function( x, means, sds, f.t3 )
{
	results <- vapply( x, function( x )
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
	}, 0 )

	return ( results )
}

################################################################################################
#' [内部関数] intervals 導出 (4点経路, 2つの正規分布の縦方向グラデーション)
#'
#' 4点経路の場合に、 2つの正規分布の縦方向グラデーションの intervals を導出する
#' @param   wp              経路。
#' @param   control         nleqslv に渡す設定。
#' @return  intervals, result (nleqslv の結果) のリスト
#' @importFrom  stats       pnorm
################################################################################################
t3.v.grad.wp4.intervals <- function( wp, control )
{
	if ( nrow( wp ) != 4 )
	{
		stop( paste( "Error: nrow( wp ) must be 4 for t3.v.grad.wp4.intervals( wp ). The nrow: ", nrow( wp ) ) )
	}

	# 経路を中央値に近い順に並び替えて、
	# 中央値から遠い2点を通る正規分布と、中央値に近い2点を通る正規分布の平均値と標準偏差をそれぞれ算出し、
	# それらの平均値と標準偏差を nleqslv のための仮の値とする
	wp <- data.frame( q = wp$q[order( abs( wp$p - 0.5 ) )], p = wp$p[order( abs( wp$p - 0.5 ) )] )
	if ( wp$p[2] == 0.5 )
	{
		# 中央値の点が2つある場合は、中央値から遠い点を通る正規分布と、中央値に近い点を通る正規分布を使う
		# そして、条件不足のため allowSingular = TRUE とする
		pseudo.far <- ms.qp.norm( wp$q[c( 2, 4 )], wp$p[c( 2, 4 )] )
		pseudo.near <- ms.qp.norm( wp$q[c( 1, 3 )], wp$p[c( 1, 3 )] )

		if ( is.null( control$allowSingular ) )
		{
			control <- append( control, list( allowSingular = TRUE ) )
		}
	}
	else
	{

		pseudo.far <- ms.qp.norm( wp$q[3:4], wp$p[3:4] )
		pseudo.near <- ms.qp.norm( wp$q[1:2], wp$p[1:2] )
	}

	x.0 <- c( pseudo.far$mean, pseudo.near$mean, sqrt( pseudo.far$sd ), sqrt( pseudo.near$sd ) )

	f <- function( x )
	{
		p <- pnorm( wp$q, x[1], x[3]^2 )
		p.a1 <- pnorm( wp$q, x[1], x[3]^2 * sqrt( 2 ) / 2 )
		p.a2 <- pnorm( wp$q, x[2], x[4]^2 * sqrt( 2 ) / 2 )

		return ( p - p.a1 * sqrt( 2 ) / 2 + p.a2 * sqrt( 2 ) / 2 - wp$p )
	}

	# nleqslv の計算
	e <- try( result <- nleqslv( x.0, f, control = control ), silent = TRUE )
	if ( inherits( e, "try-error" ) )
	{
		stop( "Error: Failed to construct a continuous probability density function." )
	}
	else if ( result$termcd == 1 )
	{
		means <- c( result$x[1], result$x[2], result$x[1] )
		sds <- c( result$x[3]^2, result$x[4]^2, result$x[3]^2 )
	}
	else
	{
		message( paste( "nleqslv has failed. Message:", result$message ) )
		stop( "Error: Failed to construct a continuous probability density function." )
	}

	return ( list( intervals = gen.t3.intervals( means, sds ), result = result ) )
}

################################################################################################
# type1.type=3, v.grad = FALSE, 4点経路の intervals を導出する
# @param    wp              経路。
# @param    uni.sigma       標準偏差を揃えるかどうかのフラグ。
# @param    control         nleqslv に渡す設定。
# @return   intervals, result (nleqslv の結果) のリスト
################################################################################################
t3.wp4.intervals <- function( wp, uni.sigma, control )
{
	if ( nrow( wp ) != 4 )
	{
		stop( paste( "Error: nrow( wp ) must be 4 for t3.wp4.intervals( wp ). The nrow: ", nrow( wp ) ) )
	}

	# 経路を中央値に近い順に並び替えて、
	# 中央値から遠い2点を通る正規分布と、中央値に近い2点を通る正規分布の平均値と標準偏差をそれぞれ算出し、
	# それらの平均値を nleqslv のための仮の値とする

	wp <- data.frame( q = wp$q[order( abs( wp$p - 0.5 ) )], p = wp$p[order( abs( wp$p - 0.5 ) )] )

	if ( wp$p[2] == 0.5 )
	{
		# 中央値の点が2つある場合は、中央値から遠い点を通る正規分布と、中央値に近い点を通る正規分布を使う
		# そして、条件不足のため allowSingular = TRUE とする
		pseudos <- list( ms.qp.norm( wp$q[c( 1, 3 )], wp$p[c( 1, 3 )] ), ms.qp.norm( wp$q[c( 2, 4 )], wp$p[c( 2, 4 )] ) )

		if ( is.null( control$allowSingular ) )
		{
			control <- append( control, list( allowSingular = TRUE ) )
		}
	}
	else
	{
		pseudos <- list( ms.qp.norm( wp$q[1:2], wp$p[1:2] ), ms.qp.norm( wp$q[3:4], wp$p[3:4] ) )
	}

	pseudos.2 <- list( mean = ( pseudos[[1]]$mean + pseudos[[2]]$mean ) / 2,
						sd = ( pseudos[[1]]$sd + pseudos[[2]]$sd ) / 2 )

	if ( uni.sigma )
	{
		e <- try( result <- nleqslv( c( pseudos[[1]]$mean, pseudos.2$mean, pseudos[[2]]$mean,
										sqrt( pseudos.2$sd ) ),
										f <- function( x )
										{
											means <- x[1:3]
											sds <- rep( x[4]^2, 3 )
											return ( dp.t3( wp$q, means, sds, f.t3.p ) - wp$p )
										}, control = control ), silent = TRUE )
		if ( inherits( e, "try-error" ) )
		{
			stop( "Error: Failed to construct a continuous probability density function." )
		}
		else if ( result$termcd == 1 )
		{
			means <- result$x[1:3]
			sds <- rep( result$x[4]^2, 3 )
		}
		else
		{
			message( paste( "nleqslv has failed. Message:", result$message ) )
			stop( "Error: Failed to construct a continuous probability density function." )
		}
	}
	else
	{
		e <- try( result <- nleqslv( c( pseudos.2$mean,
										sqrt( pseudos[[1]]$sd ), sqrt( pseudos.2$sd ), sqrt( pseudos[[2]]$sd ) ),
										f <- function( x )
										{
											means <- rep( x[1], 3 )
											sds <- x[2:4]^2
											return ( dp.t3( wp$q, means, sds, f.t3.p ) - wp$p )
										}, control = control ), silent = TRUE )
		if ( inherits( e, "try-error" ) )
		{
			stop( "Error: Failed to construct a continuous probability density function." )
		}
		else if ( result$termcd == 1 )
		{
			means <- rep( result$x[1], 3 )
			sds <- result$x[2:4]^2
		}
		else
		{
			# 失敗したら t3.v.grad.wp4.intervals でリトライ
			message( paste( "nleqslv has once failed. Message:", result$message ) )
			message( paste( "Message: Failed to construct a continuous probability density function",
							"with v.grad = FALSE. Constructing with v.grad = TRUE has been retried." ) )
			e <- try( l <- t3.v.grad.wp4.intervals( wp, control ), silent = TRUE )
			if ( inherits( e, "try-error" ) )
			{
				# それも失敗したら6点経路としてリトライ (条件不足のため allowSingular = TRUE とする)
				message( paste( "Message: Failed to construct a continuous probability density function",
								"with v.grad = TRUE. Constructing with allowSingular = TRUE has been retried." ) )

				wp <- data.frame( q = c( wp$q[1:2], wp$q ), p = c( wp$p[1:2], wp$p ) )

				if ( is.null( control$allowSingular ) )
				{
					control <- append( control, list( allowSingular = TRUE ) )
				}
				e <- try( result <- nleqslv( c( pseudos[[1]]$mean, pseudos.2$mean, pseudos[[2]]$mean,
												sqrt( pseudos[[1]]$sd ), sqrt( pseudos.2$sd ), sqrt( pseudos[[2]]$sd ) ),
												f <- function( x )
												{
													means <- x[1:3]
													sds <- x[4:6]^2
													return ( dp.t3( wp$q, means, sds, f.t3.p ) - wp$p )
												}, control = control ), silent = TRUE )
				if ( inherits( e, "try-error" ) )
				{
					stop( "Error: Failed to construct a continuous probability density function." )
				}
				else if ( result$termcd == 1 )
				{
					means <- result$x[1:3]
					sds <- result$x[4:6]^2
				}
				else
				{
					message( paste( "The last nleqslv has failed. Message:", result$message ) )
					stop( "Error: Failed to construct a continuous probability density function." )
				}
			}
			else
			{
				return ( l )
			}
		}
	}

	return ( list( intervals = gen.t3.intervals( means, sds ), result = result ) )
}

################################################################################################
#' type1.type=3 用 intervals 生成
#'
#' type1.type=3 の intervals を生成する。
#' @export
#' @param   means       平均値のベクトル。
#' @param   sds         標準偏差のベクトル。
#' @return  生成した intervals
#' @importFrom  methods     new
#' @importFrom  stats       qnorm
#' @examples
#'  gen.t3.intervals( means = c( 1.625, 0.032, -1.37 ), sds = rep( 0.828, 3 ) )
################################################################################################
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
						p.conn.prev = c( 0, 0 ), p.conn.next = c( 0, ps[1] ) ),
					CGDInterval$new(
						mean = means[2],
						sd = sds[2],
						q.ind = c( means[2], means[2] ),
						q.conn.prev = c( -Inf, qnorm( ps[1], means[2], sds[2] ) ),
						q.conn.next = c( qnorm( ps[3], means[2], sds[2] ), Inf ),
						p.ind = c( ps[2], ps[2] ),
						p.conn.prev = c( 0, ps[1] ), p.conn.next = c( ps[3], 1 ) ),
					CGDInterval$new(
						mean = means[3],
						sd = sds[3],
						q.ind = c( Inf, Inf ),
						q.conn.prev = c( means[3], Inf ), q.conn.next = c( Inf, Inf ),
						p.ind = c( 1, 1 ),
						p.conn.prev = c( ps[3], 1 ), p.conn.next = c( 1, 1 ) ) ) )
}

################################################################################################
#' [内部関数] 山側の正規分布の標準偏差算出
#'
#' type1.type=3, 5点経路の場合および、中央値の指定がない3点経路の場合の、
#' 山側 (2番目) の正規分布の標準偏差を計算する。
#'
#' nleqslv でワーニングが出ないようにするために、この関数では、
#' wp.near と mean.2 の位置関係がおかしい場合は、内部で勝手にクォンタイルの値を書き換えている。
#' そのため、この関数は nleqslv で標準偏差を探索する目的以外に用いてはならない。
#' @param   mean.2      山側の正規分布の平均値。
#' @param   wp.near     中央値に近い経路の2点 (以上) の座標。
#' @return  山側の正規分布の標準偏差
################################################################################################
t3.mid.sd <- function( mean.2, wp.near )
{
	sd.1 <- sd.mqp.norm( mean.2, wp.near$q[1], wp.near$p[1] )
	sd.2 <- sd.mqp.norm( mean.2, wp.near$q[2], wp.near$p[2] )

	if ( sd.1 < 0 )
	{
		sd.1 <- sd.mqp.norm( mean.2, 2 * mean.2 - wp.near$q[1], wp.near$p[1] )
	}

	if ( sd.2 < 0 )
	{
		sd.2 <- sd.mqp.norm( mean.2, 2 * mean.2 - wp.near$q[2], wp.near$p[2] )
	}

	dist.1 <- abs( mean.2 - wp.near$q[1] )
	dist.2 <- abs( mean.2 - wp.near$q[2] )

	return ( ( sd.1 * dist.2 + sd.2 * dist.1 ) / ( dist.1 + dist.2 ) )
}


################################################################################################
#' 度数分布による連続な連結ガウス分布構成
#'
#' 非線形最小二乗法 (\link[stats]{nls}) によって、
#' 与えられた度数分布に最も近くなるように、連続な連結ガウス分布を構成する。
#'
#' この関数内では、度数分布の外れ値の除外は行わない。外れ値の除外は、必要に応じて、前処理で行うこと。
#' 度数の合計 (total) に外れ値の分を含めるかどうかは、ライブラリの利用者の判断に委ねられる。
#' そのため、引数 total は省略できない。
#' @export
#' @param   x               X座標のベクトル。最低 3 個以上の要素を含み、昇順に並べておくこと。
#'                          また、値に重複がないようにすること。
#' @param   freq            X座標に対する度数分布のベクトル。要素数は x と同数であること。
#' @param   total           度数の合計。
#'                          前処理で除外された外れ値の数を合計に含めるかどうかは、
#'                          この関数の使用者の判断に委ねられる。そのため、この引数は省略できない。
#' @param   start           \link[stats]{nls} に渡す初期値のリスト (デフォルト: NULL)。
#'                          デフォルト (NULL) の場合は、
#'                          内部で各正規分布の平均値や局地的な標準偏差などをある程度計算して、
#'                          暫定的なパラメータを初期値とする。
#'                          もし、自分で初期値を与える場合は、
#'                          構成要素の正規分布の平均値 (mean または mean.1, mean.2, mean.3 または
#'                          mean.1.1, mean.1.2, mean.2.1, mean.2,2) と
#'                          標準偏差の平方根
#'                           (sqrt.sd または sqrt.sd.1, sqrt.sd.2, sqrt.sd.3 または
#'                            sqrt.sd.1.1, sqrt.sd.1.2, sqrt.sd.2.1, sqrt.sd.2.2)
#'                          をリストとして与えること。
#'                          なお、リストのテンプレートが
#'                          \link[cgd]{nls.start.template} で得られるので、
#'                          初期値は、そのテンプレートの各変数に与えてやればよい。
#' @param   control         \link[stats]{nls} に渡す設定。
#'                          詳細は \link[stats]{nls.control} を参照 (デフォルト: list())。
#' @param   set.by.start    \link[stats]{nls} を実行せず、
#'                          start の値をそのまま使って分布を構成するフラグ。
#'                          start の値は、引数で与えられた場合は、それをそのまま使い、
#'                          引数で与えられていない場合は、
#'                          内部で計算した暫定的な初期値を使う (デフォルト: FALSE)。
#' @param   kind            生成する分布の種類を特定する変数 (1要素のみ有効) (デフォルト: NULL)。
#'                          変数の種類は、 \link[cgd]{CGD} クラスオブジェクト か、
#'                          cgd:::kinds の要素の文字列か、またはそのインデックス番号が有効。
#'                          kind に NULL でない有効な引数を与えると、
#'                          以下の normal から uni.mean までの 6 個の引数は無視され、
#'                          kind で指定された分布の種類に沿うように、
#'                          それらのオプションの値が内部で決定される。
#' @param   normal          正規分布で近似するフラグ。
#'                          normal = TRUE にすると、
#'                          type1.type を除く以降の 4 個の引数は無視され、
#'                          結果の分布は正規分布になる。
#'                          type1.type は既設定値が
#'                           (引数 type1.type が与えられていれば、その値を上書きした結果)
#'                          1 または 2 であればそのまま。
#'                          それ以外は 1 に設定される (デフォルト: FALSE)。
#' @param   symmetric       type1.type = 2, symmetric = TRUE で近似するフラグ。
#'                          symmetric = TRUE にすると、
#'                          type1.type, uni.sigma, uni.mean の引数は無視される
#'                           (デフォルト: FALSE)。
#' @param   v.grad          type1.type = 3, v.grad = TRUE で近似するフラグ。
#'                          v.grad = TRUE にすると、 type1.type の引数は無視され、
#'                          type1.type = 3 に設定される (デフォルト: FALSE)
#' @param   type1.type      フィールドの type1.type に設定する値。
#'                          1、2、3、4 のいずれかを指定すること。
#'                          分布の種類は type1.type の設定値によって、
#'                              1: 平均値が等しい2つの正規分布の平均
#'                                  (uni.sigma, uni.mean の設定は無効)、
#'                              2: 横方向グラデーション、
#'                              3:  (歪んだ) 縦方向グラデーション、
#'                              4: 縦横グラデーション
#'                          となる (デフォルト: 2)。
#'                          1、2、3、4 以外の数値を指定した場合はエラーになる。
#'                          本関数では、 type1.type = 0 は指定できない。
#' @param   uni.sigma       構成要素の正規分布の標準偏差をすべて等しくするかどうかのフラグ。
#'                          uni.sigma = TRUE にすると、
#'                          uni.mean の引数は無視され、 uni.mean = FALSE になる。
#'                          type1.type = 1 では無効 (デフォルト: FALSE)。
#' @param   uni.mean        構成要素の正規分布の平均値をすべて等しくするかどうかのフラグ。
#'                          type1.type = 1 では、 FALSE の設定は無効
#'                           (注: 平均値が異なる2つの正規分布の平均は、
#'                            本パッケージにおける不連続な連結ガウス分布の拡張にはならない)
#'                           (デフォルト: TRUE)。
#' @param   ...             その他、 \link[stats]{nls} の各引数を指定できる。
#'                          詳細は \link[stats]{nls} の Arguments を参照。
#' @return  生成した CGD クラスオブジェクト
#' @importFrom  methods     new
#' @seealso \link[stats]{nls}, \link[stats]{nls.control},
#'          \link[cgd]{CGD_nls.freq}, \link[cgd]{nls.freq.all}, \link[cgd]{nls.start.template}
#' @examples
#'  ## preparing
#'  x <- seq( -2, 2, 0.2 )
#'  freq <- c( 15.164, 22.923, 25.134, 27.631, 37.239, 40.464,
#'          47.126, 79.469, 109.966, 118.241, 111.333, 78.674,
#'          46.921, 41.026, 36.975, 27.403, 25.493, 22.838,
#'          14.992, 11.468, 9.174 )
#'  total <- sum( freq )
#'
#'  plot.freq.and.d <- function( a, x, freq, total )
#'  {
#'      xlim <- c( min( x ), max( x ) )
#'      ylim <- c( 0, max( cgd::get.d( x, freq, total ) ) * 1.2 )
#'      plot( x, cgd::get.d( x, freq, total ), xlim = xlim, ylim = ylim, xlab = "", ylab = "" )
#'      par( new = TRUE )
#'      plot( seq( min( x ), max( x ), 0.01 ), a$d( seq( min( x ), max( x ), 0.01 ) ),
#'      type = "l", xlim = xlim, ylim = ylim )
#'  }
#'
#'  ## examples
#'  a <- nls.freq( x, freq, total, normal = TRUE )
#'  a
#'  plot.freq.and.d( a, x, freq, total )
#'
#'  a <- nls.freq( x, freq, total, type1.type = 1 )
#'  a
#'  plot.freq.and.d( a, x, freq, total )
#'
#'  a <- nls.freq( x, freq, total ) ## Remark: default value of type1.type = 2 for nls.freq()
#'  a
#'  plot.freq.and.d( a, x, freq, total )
#'
#'  a <- nls.freq( x, freq, total, type1.type = 3, uni.sigma = TRUE )
#'  a
#'  plot.freq.and.d( a, x, freq, total )
#'
#'  a <- nls.freq( x, freq, total,
#'                 kind = "Median-Equaled Sigma-Differed Vertical Gradational Distribution" )
#'  a
#'  plot.freq.and.d( a, x, freq, total )
#'
#'  a <- nls.freq( x, freq, total, kind = 13, control = list( warnOnly = TRUE ) )
#'  a
#'  plot.freq.and.d( a, x, freq, total )
#'
#'  ## you can set start parameters if you want
#'  start.list <- nls.start.template( a )
#'  start.list
#'
#'  start.list$mean.1.1 <- -0.671
#'  start.list$mean.1.2 <- -0.198
#'  start.list$mean.2.1 <- 0.293
#'  start.list$mean.2.2 <- -0.198
#'  start.list$sqrt.sd <- sqrt( 0.640 )     ## sqrt.sd is the sqrt of the standard deviation.
#'  a <- nls.freq( x, freq, total, kind = 13, start = start.list )
#'  a
#'  plot.freq.and.d( a, x, freq, total )
#'
#'  a <- nls.freq( x, freq, total, type1.type = 4, uni.mean = FALSE )
#'  a
#'  plot.freq.and.d( a, x, freq, total )
################################################################################################
nls.freq <- function( x, freq, total, start = NULL, control = list(), set.by.start = FALSE,
						kind = NULL, normal = FALSE, symmetric = FALSE, v.grad = FALSE,
						type1.type = 2, uni.sigma = FALSE, uni.mean = TRUE, ... )
{
	obj <- CGD$new()
	obj$nls.freq( x, freq, total, start, control, set.by.start, kind,
							normal, symmetric, v.grad, type1.type,
							uni.sigma, uni.mean, ... )
	return ( obj )
}

################################################################################################
#' 度数分布による連続な連結ガウス分布構成
#'
#' 非線形最小二乗法 (\link[stats]{nls}) によって、
#' 与えられた度数分布に最も近くなるように、連続な連結ガウス分布を構成する。
#'
#' この関数内では、度数分布の外れ値の除外は行わない。外れ値の除外は、必要に応じて、前処理で行うこと。
#' 度数の合計 (total) に外れ値の分を含めるかどうかは、ライブラリの利用者の判断に委ねられる。
#' そのため、引数 total は省略できない。
#' @name    CGD_nls.freq
#' @usage   CGD$nls.freq(x, freq, total, start = NULL, control = list(),
#'                       set.by.start = FALSE, kind = NULL, normal = FALSE,
#'                       symmetric = FALSE, v.grad = FALSE, this.type1.type = NULL,
#'                       uni.sigma = FALSE, uni.mean = TRUE, ...)
#' @param   x               X座標のベクトル。最低 3 個以上の要素を含み、昇順に並べておくこと。
#'                          また、値に重複がないようにすること。
#' @param   freq            X座標に対する度数分布のベクトル。要素数は x と同数であること。
#' @param   total           度数の合計。
#'                          前処理で除外された外れ値の数を合計に含めるかどうかは、
#'                          この関数の使用者の判断に委ねられる。そのため、この引数は省略できない。
#' @param   start           \link[stats]{nls} に渡す初期値のリスト (デフォルト: NULL)。
#'                          デフォルト (NULL) の場合は、
#'                          内部で各正規分布の平均値や局地的な標準偏差などをある程度計算して、
#'                          暫定的なパラメータを初期値とする。
#'                          もし、自分で初期値を与える場合は、
#'                          構成要素の正規分布の平均値 (mean または mean.1, mean.2, mean.3 または
#'                          mean.1.1, mean.1.2, mean.2.1, mean.2,2) と
#'                          標準偏差の平方根
#'                           (sqrt.sd または sqrt.sd.1, sqrt.sd.2, sqrt.sd.3 または
#'                            sqrt.sd.1.1, sqrt.sd.1.2, sqrt.sd.2.1, sqrt.sd.2.2)
#'                          をリストとして与えること。
#'                          なお、リストのテンプレートが
#'                          \link[cgd]{nls.start.template} で得られるので、
#'                          初期値は、そのテンプレートの各変数に与えてやればよい。
#' @param   control         \link[stats]{nls} に渡す設定。
#'                          詳細は \link[stats]{nls.control} を参照 (デフォルト: list())。
#' @param   set.by.start    \link[stats]{nls} を実行せず、
#'                          start の値をそのまま使って分布を構成するフラグ。
#'                          start の値は、引数で与えられた場合は、それをそのまま使い、
#'                          引数で与えられていない場合は、
#'                          内部で計算した暫定的な初期値を使う (デフォルト: FALSE)。
#' @param   kind            生成する分布の種類を特定する変数 (1要素のみ有効) (デフォルト: NULL)。
#'                          変数の種類は、 \link[cgd]{CGD} クラスオブジェクト か、
#'                          cgd:::kinds の要素の文字列か、またはそのインデックス番号が有効。
#'                          kind に NULL でない有効な引数を与えると、
#'                          以下の normal から uni.mean までの 6 個の引数は無視され、
#'                          kind で指定された分布の種類に沿うように、
#'                          それらのオプションの値が内部で決定される。
#' @param   normal          正規分布で近似するフラグ。
#'                          normal = TRUE にすると、
#'                          type1.type を除く以降の 4 個の引数は無視され、
#'                          結果の分布は正規分布になる。
#'                          type1.type は既設定値が
#'                           (引数 type1.type が与えられていれば、その値を上書きした結果)
#'                          1 または 2 であればそのまま。
#'                          それ以外は 1 に設定される (デフォルト: FALSE)。
#' @param   symmetric       type1.type = 2, symmetric = TRUE で近似するフラグ。
#'                          symmetric = TRUE にすると、
#'                          this.type1.type, uni.sigma, uni.mean の引数は無視される
#'                           (デフォルト: FALSE)。
#' @param   v.grad          type1.type = 3, v.grad = TRUE で近似するフラグ。
#'                          v.grad = TRUE にすると、
#'                          this.type1.type の引数は無視され、
#'                          type1.type = 3 に設定される (デフォルト: FALSE)
#' @param   this.type1.type フィールドの type1.type に設定する値。
#'                          1、2、3、4 のいずれかを指定すること。
#'                          NULL の場合は type1.type の値を変更しない。
#'                          分布の種類は type1.type の設定値によって、
#'                              1: 平均値が等しい2つの正規分布の平均
#'                                  (uni.sigma, uni.mean の設定は無効)、
#'                              2: 横方向グラデーション、
#'                              3:  (歪んだ) 縦方向グラデーション、
#'                              4: 縦横グラデーション
#'                          となる (デフォルト: NULL)。
#'                          1、2、3、4 以外の数値を指定した場合はエラーになる。
#'                          本関数では、 type1.type = 0 は指定できない。
#' @param   uni.sigma       構成要素の正規分布の標準偏差をすべて等しくするかどうかのフラグ。
#'                          uni.sigma = TRUE にすると、
#'                          uni.mean の引数は無視され、 uni.mean = FALSE になる。
#'                          type1.type = 1 では無効 (デフォルト: FALSE)。
#' @param   uni.mean        構成要素の正規分布の平均値をすべて等しくするかどうかのフラグ。
#'                          type1.type = 1 では、 FALSE の設定は無効
#'                           (注: 平均値が異なる2つの正規分布の平均は、
#'                            本パッケージにおける不連続な連結ガウス分布の拡張にはならない)
#'                           (デフォルト: TRUE)。
#' @param   ...             その他、 \link[stats]{nls} の各引数を指定できる。
#'                          詳細は \link[stats]{nls} の Arguments を参照。
#' @return  \link[stats]{nls} の実行結果
#' @seealso \link[stats]{nls}, \link[stats]{nls.control},
#'          \link[cgd]{nls.freq}, \link[cgd]{nls.freq.all}, \link[cgd]{nls.start.template}
#' @examples
#'  ## preparing
#'  x <- seq( -2, 2, 0.2 )
#'  freq <- c( 15.164, 22.923, 25.134, 27.631, 37.239, 40.464,
#'          47.126, 79.469, 109.966, 118.241, 111.333, 78.674,
#'          46.921, 41.026, 36.975, 27.403, 25.493, 22.838,
#'          14.992, 11.468, 9.174 )
#'  total <- sum( freq )
#'
#'  plot.freq.and.d <- function( a, x, freq, total )
#'  {
#'      xlim <- c( min( x ), max( x ) )
#'      ylim <- c( 0, max( cgd::get.d( x, freq, total ) ) * 1.2 )
#'      plot( x, cgd::get.d( x, freq, total ), xlim = xlim, ylim = ylim, xlab = "", ylab = "" )
#'      par( new = TRUE )
#'      plot( seq( min( x ), max( x ), 0.01 ), a$d( seq( min( x ), max( x ), 0.01 ) ),
#'      type = "l", xlim = xlim, ylim = ylim )
#'  }
#'
#'  ## examples
#'  a <- CGD$new()
#'  a$nls.freq( x, freq, total, normal = TRUE )
#'  a
#'  plot.freq.and.d( a, x, freq, total )
#'
#'  a$nls.freq( x, freq, total, this.type1.type = 1 )
#'  a
#'  plot.freq.and.d( a, x, freq, total )
#'
#'  a$nls.freq( x, freq, total, this.type1.type = 2 )
#'  a
#'  plot.freq.and.d( a, x, freq, total )
#'
#'  a$nls.freq( x, freq, total, this.type1.type = 3, uni.sigma = TRUE )
#'  a
#'  plot.freq.and.d( a, x, freq, total )
#'
#'  a$nls.freq( x, freq, total,
#'              kind = "Median-Equaled Sigma-Differed Vertical Gradational Distribution" )
#'  a
#'  plot.freq.and.d( a, x, freq, total )
#'
#'  a$nls.freq( x, freq, total, kind = 13, control = list( warnOnly = TRUE ) )
#'  a
#'  plot.freq.and.d( a, x, freq, total )
#'
#'  ## you can set start parameters if you want
#'  start.list <- nls.start.template( a )
#'  start.list
#'
#'  start.list$mean.1.1 <- -0.671
#'  start.list$mean.1.2 <- -0.198
#'  start.list$mean.2.1 <- 0.293
#'  start.list$mean.2.2 <- -0.198
#'  start.list$sqrt.sd <- sqrt( 0.640 )     ## sqrt.sd is the sqrt of the standard deviation.
#'  a$nls.freq( x, freq, total, kind = 13, start = start.list )
#'  a
#'  plot.freq.and.d( a, x, freq, total )
#'
#'  a$nls.freq( x, freq, total, this.type1.type = 4, uni.mean = FALSE )
#'  a
#'  plot.freq.and.d( a, x, freq, total )
################################################################################################
NULL
CGD$methods(
	nls.freq = function( x, freq, total, start = NULL, control = list(), set.by.start = FALSE, kind = NULL,
							normal = FALSE, symmetric = FALSE, v.grad = FALSE, this.type1.type = NULL,
							uni.sigma = FALSE, uni.mean = TRUE, ... )
	{
		result <- NULL

		# フィールドを初期化
		clear( TRUE )

		# 引数チェック
		if ( length( x ) != length( freq ) )
		{
			stop( "Error: The lengths of x and freq are different." )
		}

		if ( length( x ) < 3 )
		{
			stop( "Error: The lengths of x and freq are too short." )
		}

		if ( !all( x[1:length( x ) - 1] < x[2:length( x )] ) )
		{
			stop( "Error: x must have been sorted in ascending order, and must not contain the same numbers." )
		}

		if ( sum( freq ) > total )
		{
			stop( "Error: total is smaller than the sum of freq." )
		}

		# kind 指定時の各オプション設定
		index <- NaN
		if ( !is.null( kind ) )
		{
			index <- cgd.kind.index( kind )[1]
		}

		if ( !is.na( index ) )
		{
			normal		<- ( index == 1 )
			symmetric	<- ( index == 3 )
			v.grad		<- any( index == 7:9 )
			uni.sigma	<- ( index > 3 && ( index %% 3 == 1 ) )
			uni.mean	<- ( index == 3 || ( index %% 3 != 0 ) )

			if ( index == 2 )
			{
				this.type1.type <- 1
			}
			else if ( any( index == 3:6 ) )
			{
				this.type1.type <- 2
			}
			else if ( any( index == 7:12 ) )
			{
				this.type1.type <- 3
			}
			else if ( any( index == 13:15 ) )
			{
				this.type1.type <- 4
			}
		}

		# type1.type 設定
		if ( is.null( this.type1.type ) )
		{
			this.type1.type <- type1.type
		}

		if ( normal )
		{
			if ( this.type1.type == 1 || this.type1.type == 2 )
			{
				type1.type <<- this.type1.type
			}
			else
			{
				type1.type <<- 1
			}
		}
		else if ( symmetric )
		{
			type1.type <<- 2
		}
		else if ( v.grad )
		{
			type1.type <<- 3
		}
		else if ( this.type1.type == 1 || this.type1.type == 2 || this.type1.type == 3 || this.type1.type == 4 )
		{
			type1.type <<- this.type1.type
		}
		else
		{
			stop( paste( "Error: type1.type" , this.type1.type, "is not allowed for nls.freq." ) )
		}

		if ( uni.sigma )
		{
			uni.mean <- FALSE	# uni.sigma = TRUE ⇒ uni.mean = FALSE
		}

		# nls 実行
		params <- get.nls.params( x, freq, total,
									normal, symmetric, v.grad, type1.type,
									uni.sigma, uni.mean )
		if ( is.null( start ) )
		{
			start <- params$start
		}

		if ( set.by.start )
		{
			# 初期値をそのまま出力する
			coefs <- unlist( start )
		}
		else
		{
			e <- try( result <- nls( params$format, data = list( d = get.d( x, freq, total ), x = x ),
													start = start,
													control = control, ... ), silent = TRUE )
			if ( inherits( e, "try-error" ) )
			{
				stop( paste( "nls has failed. Message:", e ) )
			}
			else
			{
				coefs <- coef( result )
			}
		}

		set.intervals( get.intervals.with.nls.coef( coefs, normal, symmetric, v.grad,
													uni.sigma, uni.mean, type1.type ) )

		return ( result )
	}
)

################################################################################################
#' 確率密度関数の目標値取得
#'
#' 度数分布から確率密度関数の目標値を得る。
#' @export
#' @param   x       X座標のベクトル。
#'                  最低 3 個以上の要素を含み、昇順に並べておくこと。
#'                  また、値に重複がないようにすること。
#' @param   freq    度数分布のベクトル。要素数は x と同数であること。
#' @param   total   度数の合計。
#' @return  座標 x における確率密度関数の目標値のベクトル
#' @examples
#'  x <- seq( -2, 2, 0.2 )
#'  freq <- c( 15.164, 22.923, 25.134, 27.631, 37.239, 40.464,
#'          47.126, 79.469, 109.966, 118.241, 111.333, 78.674,
#'          46.921, 41.026, 36.975, 27.403, 25.493, 22.838,
#'          14.992, 11.468, 9.174 )
#'  total <- sum( freq )
#'  cgd::get.d( x, freq, total )
#'  plot( x, cgd::get.d( x, freq, total ) )
################################################################################################
get.d <- function( x, freq, total )
{
	d <- vapply( 1:length( x ), function( i )
	{
		if ( i == 1 )
		{
			x.width.lower <- ( x[2] - x[1] ) / 2
		}
		else
		{
			x.width.lower <- ( x[i] - x[i - 1] ) / 2
		}

		if ( i == length( x ) )
		{
			x.width.upper <- ( x[i] - x[i - 1] ) / 2
		}
		else
		{
			x.width.upper <- ( x[i + 1] - x[i] ) / 2
		}

		freq[i] / total / ( x.width.lower + x.width.upper )
	}, 0 )

	return ( d )
}

################################################################################################
#' [内部関数] nls 用パラメータ取得
#'
#' 設定値をもとに、 nls に渡す主要な引数を取得する
#' @param   x               X座標のベクトル。
#'                          最低 3 個以上の要素を含み、昇順に並んでいて、値の重複はない。
#' @param   freq            度数分布のベクトル。
#' @param   total           度数の合計。
#' @param   normal          正規分布で近似するフラグ。
#' @param   symmetric       type1.type = 2, symmetric = TRUE で近似するフラグ。
#' @param   v.grad          type1.type = 3, v.grad = TRUE で近似するフラグ。
#' @param   type1.type      フィールドの type1.type の値。
#' @param   uni.sigma       構成要素の正規分布の標準偏差をすべて等しくするかどうかのフラグ。
#' @param   uni.mean        構成要素の正規分布の平均値をすべて等しくするかどうかのフラグ。
#' @return  nls に渡す format と start のリスト
#' @importFrom  stats       dnorm pnorm
################################################################################################
get.nls.params <- function( x, freq, total, normal, symmetric, v.grad, type1.type, uni.sigma, uni.mean )
{
	fm <- NULL
	start <- list()

	data.mean <- sum( x * freq ) / total

	# 標準偏差の初期値を得るために、度数分布を四分割する
	xf <- separate.two.four( x, freq, data.mean )

	# uni.sigma = TRUE のときは、必ず構成要素の正規分布の平均値が異なるようになることから、
	# 初めから裾部だけの標準偏差を計算しておいて、それを標準偏差の初期値として与える
	if ( uni.sigma )
	{
		length.1 <- length( xf$x[[1]] )
		length.4 <- length( xf$x[[4]] )

		sd.1 <- sqrt( sum( ( xf$x[[1]] - xf$x[[1]][length.1] )^2 * freq[1:length.1] ) /
						sum( freq[1:length.1] ) )

		sd.4 <- sqrt( sum( ( xf$x[[4]] - xf$x[[4]][1] )^2 * freq[length( freq ) - length.4 + 1:length.4] ) /
						sum( freq[length( freq ) - length.4 + 1:length.4] ) )

		sqrt.sd.for.uni.sigma <- sqrt( ( sd.1 + sd.4 ) / 2 )
	}

	if ( normal )
	{
		# 正規分布
		fm <- d ~ dnorm( x, mean, sqrt.sd^2 )

		start <- list( mean = data.mean, sqrt.sd = ( sum( ( x - data.mean )^2 * freq ) / total )^0.25 )
	}
	else
	{
		if ( type1.type == 1 )
		{
			# 平均値が等しい2つの正規分布の平均
			fm <- d ~ ( dnorm( x, mean, sqrt.sd.1^2 ) + dnorm( x, mean, sqrt.sd.2^2 ) ) / 2
			start <- list( mean = data.mean,
							sqrt.sd.1 = ( sum( ( xf$x.outer - data.mean )^2 * xf$freq.outer ) / sum( xf$freq.outer ) )^0.25,
							sqrt.sd.2 = ( sum( ( xf$x.inner - data.mean )^2 * xf$freq.inner ) / sum( xf$freq.inner ) )^0.25 )
		}
		else if ( type1.type == 2 )
		{
			# 横方向グラデーション
			if ( symmetric )
			{
				fm <- d ~ ( 1 - 2 * pnorm( mean + abs( mean - x ), mean, sqrt.sd.1^2 ) ) *
									dnorm( mean + abs( mean - x ), mean, sqrt.sd.1^2 ) +
							2 * pnorm( mean + abs( mean - x ), mean, sqrt.sd.2^2 ) *
								dnorm( mean + abs( mean - x ), mean, sqrt.sd.2^2 )

				start <- list( mean = data.mean,
								sqrt.sd.1 = ( sum( ( xf$x.outer - data.mean )^2 * xf$freq.outer ) / sum( xf$freq.outer ) )^0.25,
								sqrt.sd.2 = ( sum( ( xf$x.inner - data.mean )^2 * xf$freq.inner ) / sum( xf$freq.inner ) )^0.25 )
			}
			else if ( uni.sigma )
			{
				fm <- d ~ ( 1 - pnorm( x, mean.1, sqrt.sd^2 ) ) * dnorm( x, mean.1, sqrt.sd^2 ) +
							pnorm( x, mean.2, sqrt.sd^2 ) * dnorm( x, mean.2, sqrt.sd^2 )

				start <- list( mean.1 = data.mean, mean.2 = data.mean, sqrt.sd = sqrt.sd.for.uni.sigma )
			}
			else if ( uni.mean )
			{
				fm <- d ~ ( 1 - pnorm( x, mean, sqrt.sd.1^2 ) ) * dnorm( x, mean, sqrt.sd.1^2 ) +
							pnorm( x, mean, sqrt.sd.2^2 ) * dnorm( x, mean, sqrt.sd.2^2 )

				start <- list( mean = data.mean,
								sqrt.sd.1 = ( sum( ( xf$x.lower - data.mean )^2 * xf$freq.lower ) / sum( xf$freq.lower ) )^0.25,
								sqrt.sd.2 = ( sum( ( xf$x.upper - data.mean )^2 * xf$freq.upper ) / sum( xf$freq.upper ) )^0.25 )
			}
			else
			{
				fm <- d ~ ( 1 - pnorm( x, mean.1, sqrt.sd.1 ) ) * dnorm( x, mean.1, sqrt.sd.1 ) +
							pnorm( x, mean.2, sqrt.sd.2 ) * dnorm( x, mean.2, sqrt.sd.2 )

				start <- list( mean.1 = data.mean, mean.2 = data.mean,
								sqrt.sd.1 = ( sum( ( xf$x.lower - data.mean )^2 * xf$freq.lower ) / sum( xf$freq.lower ) )^0.25,
								sqrt.sd.2 = ( sum( ( xf$x.upper - data.mean )^2 * xf$freq.upper ) / sum( xf$freq.upper ) )^0.25 )
			}
		}
		else if ( type1.type == 3 )
		{
			if ( v.grad )
			{
				# 縦方向グラデーション
				if ( uni.sigma )
				{
					fm <- d ~ dp.t3( x, c( mean.1, mean.2, mean.1 ), c( sqrt.sd^2, sqrt.sd^2, sqrt.sd^2 ), f.t3.d )

					start <- list( mean.1 = data.mean, mean.2 = data.mean, sqrt.sd = sqrt.sd.for.uni.sigma )
				}
				else if ( uni.mean )
				{
					fm <- d ~ dp.t3( x, c( mean, mean, mean ), c( sqrt.sd.1^2, sqrt.sd.2^2, sqrt.sd.1^2 ), f.t3.d )

					start <- list( mean = data.mean,
								sqrt.sd.1 = ( sum( ( xf$x.outer - data.mean )^2 * xf$freq.outer ) / sum( xf$freq.outer ) )^0.25,
								sqrt.sd.2 = ( sum( ( xf$x.inner - data.mean )^2 * xf$freq.inner ) / sum( xf$freq.inner ) )^0.25 )
				}
				else
				{
					fm <- d ~ dp.t3( x, c( mean.1, mean.2, mean.1 ), c( sqrt.sd.1^2, sqrt.sd.2^2, sqrt.sd.1^2 ), f.t3.d )

					start <- list( mean.1 = data.mean, mean.2 = data.mean,
								sqrt.sd.1 = ( sum( ( xf$x.outer - data.mean )^2 * xf$freq.outer ) / sum( xf$freq.outer ) )^0.25,
								sqrt.sd.2 = ( sum( ( xf$x.inner - data.mean )^2 * xf$freq.inner ) / sum( xf$freq.inner ) )^0.25 )
				}
			}
			else
			{
				# 歪んだ縦方向グラデーション
				if ( uni.sigma )
				{
					fm <- d ~ dp.t3( x, c( mean.1, mean.2, mean.3 ), c( sqrt.sd^2, sqrt.sd^2, sqrt.sd^2 ), f.t3.d )

					start <- list( mean.1 = data.mean, mean.2 = data.mean, mean.3 = data.mean,
									sqrt.sd = sqrt.sd.for.uni.sigma )
				}
				else if ( uni.mean )
				{
					fm <- d ~ dp.t3( x, c( mean, mean, mean ), c( sqrt.sd.1^2, sqrt.sd.2^2, sqrt.sd.3^2 ), f.t3.d )

					start <- list( mean = data.mean,
								sqrt.sd.1 = ( sum( ( xf$x[[1]] - data.mean )^2 * xf$freq[[1]] ) / sum( xf$freq[[1]] ) )^0.25,
								sqrt.sd.2 = ( sum( ( xf$x.inner - data.mean )^2 * xf$freq.inner ) / sum( xf$freq.inner ) )^0.25,
								sqrt.sd.3 = ( sum( ( xf$x[[4]] - data.mean )^2 * xf$freq[[4]] ) / sum( xf$freq[[4]] ) )^0.25 )
				}
				else
				{
					fm <- d ~ dp.t3( x, c( mean.1, mean.2, mean.3 ), c( sqrt.sd.1^2, sqrt.sd.2^2, sqrt.sd.3^2 ), f.t3.d )

					start <- list( mean.1 = data.mean, mean.2 = data.mean, mean.3 = data.mean,
								sqrt.sd.1 = ( sum( ( xf$x[[1]] - data.mean )^2 * xf$freq[[1]] ) / sum( xf$freq[[1]] ) )^0.25,
								sqrt.sd.2 = ( sum( ( xf$x.inner - data.mean )^2 * xf$freq.inner ) / sum( xf$freq.inner ) )^0.25,
								sqrt.sd.3 = ( sum( ( xf$x[[4]] - data.mean )^2 * xf$freq[[4]] ) / sum( xf$freq[[4]] ) )^0.25 )
				}
			}
		}
		else if ( type1.type == 4 )
		{
			# 縦横グラデーション
			if ( uni.sigma )
			{
				fm <- d ~ ( 1 - f.t3.p[[1]]( x, mean.1.1, sqrt.sd^2 ) - f.t3.p[[2]]( x, mean.1.2, sqrt.sd^2 ) ) *
								( f.t3.d[[1]]( x, mean.1.1, sqrt.sd^2 ) + f.t3.d[[2]]( x, mean.1.2, sqrt.sd^2 ) ) +
							( f.t3.p[[1]]( x, mean.2.1, sqrt.sd^2 ) + f.t3.p[[2]]( x, mean.2.2, sqrt.sd^2 ) ) *
								( f.t3.d[[1]]( x, mean.2.1, sqrt.sd^2 ) + f.t3.d[[2]]( x, mean.2.2, sqrt.sd^2 ) )

				start <- list( mean.1.1 = data.mean, mean.1.2 = data.mean,
								mean.2.1 = data.mean, mean.2.2 = data.mean,
								sqrt.sd = sqrt.sd.for.uni.sigma )
			}
			else if ( uni.mean )
			{
				fm <- d ~ ( 1 - f.t3.p[[1]]( x, mean, sqrt.sd.1.1^2 ) - f.t3.p[[2]]( x, mean, sqrt.sd.1.2^2 ) ) *
								( f.t3.d[[1]]( x, mean, sqrt.sd.1.1^2 ) + f.t3.d[[2]]( x, mean, sqrt.sd.1.2^2 ) ) +
							( f.t3.p[[1]]( x, mean, sqrt.sd.2.1^2 ) + f.t3.p[[2]]( x, mean, sqrt.sd.2.2^2 ) ) *
								( f.t3.d[[1]]( x, mean, sqrt.sd.2.1^2 ) + f.t3.d[[2]]( x, mean, sqrt.sd.2.2^2 ) )

				start <- list( mean = data.mean,
								sqrt.sd.1.1 = ( sum( ( xf$x[[1]] - data.mean )^2 * xf$freq[[1]] ) / sum( xf$freq[[1]] ) )^0.25,
								sqrt.sd.1.2 = ( sum( ( xf$x[[2]] - data.mean )^2 * xf$freq[[2]] ) / sum( xf$freq[[2]] ) )^0.25,
								sqrt.sd.2.1 = ( sum( ( xf$x[[3]] - data.mean )^2 * xf$freq[[3]] ) / sum( xf$freq[[3]] ) )^0.25,
								sqrt.sd.2.2 = ( sum( ( xf$x[[4]] - data.mean )^2 * xf$freq[[4]] ) / sum( xf$freq[[4]] ) )^0.25 )
			}
			else
			{
				fm <- d ~ ( 1 - f.t3.p[[1]]( x, mean.1.1, sqrt.sd.1.1^2 ) - f.t3.p[[2]]( x, mean.1.2, sqrt.sd.1.2^2 ) ) *
								( f.t3.d[[1]]( x, mean.1.1, sqrt.sd.1.1^2 ) + f.t3.d[[2]]( x, mean.1.2, sqrt.sd.1.2^2 ) ) +
							( f.t3.p[[1]]( x, mean.2.1, sqrt.sd.2.1^2 ) + f.t3.p[[2]]( x, mean.2.2, sqrt.sd.2.2^2 ) ) *
								( f.t3.d[[1]]( x, mean.2.1, sqrt.sd.2.1^2 ) + f.t3.d[[2]]( x, mean.2.2, sqrt.sd.2.2^2 ) )

				start <- list( mean.1.1 = data.mean, mean.1.2 = data.mean,
								mean.2.1 = data.mean, mean.2.2 = data.mean,
								sqrt.sd.1.1 = ( sum( ( xf$x[[1]] - data.mean )^2 * xf$freq[[1]] ) / sum( xf$freq[[1]] ) )^0.25,
								sqrt.sd.1.2 = ( sum( ( xf$x[[2]] - data.mean )^2 * xf$freq[[2]] ) / sum( xf$freq[[2]] ) )^0.25,
								sqrt.sd.2.1 = ( sum( ( xf$x[[3]] - data.mean )^2 * xf$freq[[3]] ) / sum( xf$freq[[3]] ) )^0.25,
								sqrt.sd.2.2 = ( sum( ( xf$x[[4]] - data.mean )^2 * xf$freq[[4]] ) / sum( xf$freq[[4]] ) )^0.25 )
			}
		}
	}

	return ( list( format = fm, start = start ) )
}

################################################################################################
#' [内部関数] 度数分布四分割
#'
#' 度数分布をデータの平均値で二分割し、さらにそれぞれをX座標の個数で二分割する。
#' ちょうど平均値と等しいX座標がある場合は、
#' リストの2番目と3番目のベクトルの両方にその度数を入れる。
#' 二分割した結果、個数が奇数だった場合は、リストの前後のベクトルの両方に中央の度数を入れる。
#' @param   x               X座標のベクトル。
#'                          最低 3 個以上の要素を含み、昇順に並んでいて、値の重複はない。
#' @param   freq            度数分布のベクトル。
#' @param   data.mean       データの平均値。
#' @return  四分割したX座標と度数分布のベクトルのリスト
#' @importFrom  utils       head tail
################################################################################################
separate.two.four <- function( x, freq, data.mean )
{
	xs <- freqs <- list( numeric(), numeric(), numeric(), numeric() )

	xs.lower <- x[x <= data.mean]
	xs.upper <- x[x >= data.mean]
	freqs.lower <- freq[x <= data.mean]
	freqs.upper <- freq[x >= data.mean]

	xs[[1]] <- head( xs.lower, ceiling( length( xs.lower ) / 2 ) )
	xs[[2]] <- tail( xs.lower, ceiling( length( xs.lower ) / 2 ) )
	xs[[3]] <- head( xs.upper, ceiling( length( xs.upper ) / 2 ) )
	xs[[4]] <- tail( xs.upper, ceiling( length( xs.upper ) / 2 ) )

	freqs[[1]] <- head( freqs.lower, ceiling( length( freqs.lower ) / 2 ) )
	freqs[[2]] <- tail( freqs.lower, ceiling( length( freqs.lower ) / 2 ) )
	freqs[[3]] <- head( freqs.upper, ceiling( length( freqs.upper ) / 2 ) )
	freqs[[4]] <- tail( freqs.upper, ceiling( length( freqs.upper ) / 2 ) )

	if ( xs[[2]][length( xs[[2]] )] == xs[[3]][1] )
	{
		xs.inner <- c( head( xs[[2]], length( xs[[2]] ) - 1 ), xs[[3]] )
		freqs.inner <- c( head( freqs[[2]], length( freqs[[2]] ) - 1 ), freqs[[3]] )
	}
	else
	{
		xs.inner <- c( xs[[2]], xs[[3]] )
		freqs.inner <- c( freqs[[2]], freqs[[3]] )
	}

	xs.outer <- c( xs[[1]], xs[[4]] )
	freqs.outer <- c( freqs[[1]], freqs[[4]] )

	return ( list( x = xs, freq = freqs,
					x.lower = xs.lower, x.upper = xs.upper,
					freq.lower = freqs.lower, freq.upper = freqs.upper,
					x.outer = xs.outer, x.inner = xs.inner,
					freq.outer = freqs.outer, freq.inner = freqs.inner ) )
}

################################################################################################
#' [内部関数] nls の実行結果による intervals 取得
#'
#' nls の実行結果をもとに、 intervals を取得する
#' @param   coefs       nls の実行結果の coef (名前付き double 型ベクトル)。
#' @param   normal      正規分布で近似するフラグ。
#' @param   symmetric   type1.type = 2, symmetric = TRUE で近似するフラグ。
#' @param   v.grad      type1.type = 3, v.grad = TRUE で近似するフラグ。
#' @param   uni.sigma   type1.type = 2, uni.sigma = TRUE で近似するフラグ。
#' @param   uni.mean    構成要素の正規分布の平均値をすべて同じ値に揃えるかどうかのフラグ。
#' @param   type1.type  フィールドの type1.type の値。
#' @return  intervals のリスト
#' @importFrom  methods     new
################################################################################################
get.intervals.with.nls.coef <- function( coefs, normal, symmetric, v.grad, uni.sigma, uni.mean, type1.type )
{
	intervals <- list()
	if ( normal )
	{
		# 正規分布
		intervals <- list( CGDInterval$new(
								mean = unname( coefs["mean"] ),
								sd = unname( coefs["sqrt.sd"] )^2,
								q.ind = c( -Inf, Inf ),
								q.conn.prev = c( -Inf, -Inf ), q.conn.next = c( Inf, Inf ),
								p.ind = c( 0, 1 ),
								p.conn.prev = c( 0, 0 ), p.conn.next = c( 1, 1 ) ) )
	}
	else if ( symmetric )
	{
		# 左右対称・横方向グラデーション
		mean <- unname( coefs["mean"] )

		intervals <- list( CGDInterval$new(
								mean = mean,
								sd = unname( coefs["sqrt.sd.1"] )^2,
								q.ind = c( -Inf, -Inf ),
								q.conn.prev = c( -Inf, -Inf ), q.conn.next = c( -Inf, mean ),
								p.ind = c( 0, 0 ),
								p.conn.prev = c( 0, 0 ), p.conn.next = c( 0, 0.5 ) ),
							CGDInterval$new(
								mean = mean,
								sd = unname( coefs["sqrt.sd.2"] )^2,
								q.ind = c( mean, mean ),
								q.conn.prev = c( -Inf, mean ), q.conn.next = c( mean, Inf ),
								p.ind = c( 0.5, 0.5 ),
								p.conn.prev = c( 0, 0.5 ), p.conn.next = c( 0.5, 1 ) ),
							CGDInterval$new(
								mean = mean,
								sd = unname( coefs["sqrt.sd.1"] )^2,
								q.ind = c( Inf, Inf ),
								q.conn.prev = c( mean, Inf ), q.conn.next= c( Inf, Inf ),
								p.ind = c( 1, 1 ),
								p.conn.prev = c( 0.5, 1 ), p.conn.next = c( 1, 1 ) ) )
	}
	else if ( type1.type == 1 || type1.type == 2 )
	{
		# 平均値が等しい2つの正規分布の平均, 横方向グラデーション
		if ( type1.type == 1 )
		{
			mean.1 <- mean.2 <- unname( coefs["mean"] )

			sd.1 <- unname( coefs["sqrt.sd.1"] )^2
			sd.2 <- unname( coefs["sqrt.sd.2"] )^2
		}
		else # ( type1.type == 2 )
		{
			if ( uni.mean )
			{
				mean.1 <- mean.2 <- unname( coefs["mean"] )
			}
			else
			{
				mean.1 <- unname( coefs["mean.1"] )
				mean.2 <- unname( coefs["mean.2"] )
			}

			if ( uni.sigma )
			{
				sd.1 <- sd.2 <- unname( coefs["sqrt.sd"] )^2
			}
			else
			{
				sd.1 <- unname( coefs["sqrt.sd.1"] )^2
				sd.2 <- unname( coefs["sqrt.sd.2"] )^2
			}
		}

		intervals <- list( CGDInterval$new(
								mean = mean.1,
								sd = sd.1,
								q.ind = c( -Inf, -Inf ),
								q.conn.prev = c( -Inf, -Inf ), q.conn.next = c( -Inf, Inf ),
								p.ind = c( 0, 0 ),
								p.conn.prev = c( 0, 0 ), p.conn.next = c( 0, 1 ) ),
							CGDInterval$new(
								mean = mean.2,
								sd = sd.2,
								q.ind = c( Inf, Inf ),
								q.conn.prev = c( -Inf, Inf ), q.conn.next = c( Inf, Inf ),
								p.ind = c( 1, 1 ),
								p.conn.prev = c( 0, 1 ), p.conn.next = c( 1, 1 ) ) )
	}
	else if ( type1.type == 3 )
	{
		# (歪んでない/歪んだ) 縦方向グラデーション
		if ( uni.sigma )
		{
			mean.3.coef <- ifelse( v.grad, "mean.1", "mean.3" )

			intervals <- gen.t3.intervals(	c(	unname( coefs["mean.1"] ),
												unname( coefs["mean.2"] ),
												unname( coefs[mean.3.coef] ) ),
											rep( unname( coefs["sqrt.sd"] )^2, 3 ) )
		}
		else if ( uni.mean )
		{
			sqrt.sd.3.coef <- ifelse( v.grad, "sqrt.sd.1", "sqrt.sd.3" )

			intervals <- gen.t3.intervals( rep( unname( coefs["mean"] ), 3 ),
											c(	unname( coefs["sqrt.sd.1"] )^2,
												unname( coefs["sqrt.sd.2"] )^2,
												unname( coefs[sqrt.sd.3.coef] )^2 ) )
		}
		else
		{
			mean.3.coef <- ifelse( v.grad, "mean.1", "mean.3" )
			sqrt.sd.3.coef <- ifelse( v.grad, "sqrt.sd.1", "sqrt.sd.3" )

			intervals <- gen.t3.intervals(	c(	unname( coefs["mean.1"] ),
												unname( coefs["mean.2"] ),
												unname( coefs[mean.3.coef] ) ),
											c(	unname( coefs["sqrt.sd.1"] )^2,
												unname( coefs["sqrt.sd.2"] )^2,
												unname( coefs[sqrt.sd.3.coef] )^2 ) )
		}
	}
	else if ( type1.type == 4 )
	{
		# 縦横グラデーション
		if ( uni.sigma )
		{
			intervals <- list( gen.t3.intervals( c( unname( coefs["mean.1.1"] ),
													unname( coefs["mean.1.2"] ),
													unname( coefs["mean.1.1"] ) ),
												rep( unname( coefs["sqrt.sd"] )^2, 3 ) ),
								gen.t3.intervals( c( unname( coefs["mean.2.1"] ),
													unname( coefs["mean.2.2"] ),
													unname( coefs["mean.2.1"] ) ),
												rep( unname( coefs["sqrt.sd"] )^2, 3 ) ) )
		}
		else if ( uni.mean )
		{
			intervals <- list( gen.t3.intervals( rep( unname( coefs["mean"] ), 3 ),
												c(	unname( coefs["sqrt.sd.1.1"] )^2,
													unname( coefs["sqrt.sd.1.2"] )^2,
													unname( coefs["sqrt.sd.1.1"] )^2 ) ),
								gen.t3.intervals( rep( unname( coefs["mean"] ), 3 ),
												c(	unname( coefs["sqrt.sd.2.1"] )^2,
													unname( coefs["sqrt.sd.2.2"] )^2,
													unname( coefs["sqrt.sd.2.1"] )^2 ) ) )
		}
		else
		{
			intervals <- list( gen.t3.intervals( c( unname( coefs["mean.1.1"] ),
													unname( coefs["mean.1.2"] ),
													unname( coefs["mean.1.1"] ) ),
												c(	unname( coefs["sqrt.sd.1.1"] )^2,
													unname( coefs["sqrt.sd.1.2"] )^2,
													unname( coefs["sqrt.sd.1.1"] )^2 ) ),
								gen.t3.intervals( c( unname( coefs["mean.2.1"] ),
													unname( coefs["mean.2.2"] ),
													unname( coefs["mean.2.1"] ) ),
												c(	unname( coefs["sqrt.sd.2.1"] )^2,
													unname( coefs["sqrt.sd.2.2"] )^2,
													unname( coefs["sqrt.sd.2.1"] )^2 ) ) )
		}
	}

	return ( intervals )
}

################################################################################################
#' 度数分布による連続な連結ガウス分布構成 (全種類)
#'
#' \link[cgd]{nls.freq} のすべてのオプションを試して、
#' 与えられた度数分布に最も近くなるように、連続な連結ガウス分布を構成する。
#' 度数分布との近さは相関係数 (\link[stats]{cor}) によって調べる。
#'
#' 出力の cgd は次の順番に並ぶ。
#'   1: 正規分布、
#'   2: 2つの正規分布の平均、
#'   3: 左右対称な横方向グラデーション、
#'   4: 左右の正規分布の平均値が異なり、標準偏差が等しい横方向グラデーション、
#'   5: 左右の正規分布の平均値が等しく、標準偏差が異なる横方向グラデーション、
#'   6: 左右の正規分布の平均と標準偏差が異なる横方向グラデーション、
#'   7: 裾側と山側で平均値が異なり、標準偏差が等しい縦方向グラデーション、
#'   8: 裾側と山側で平均値が等しく、標準偏差が異なる縦方向グラデーション、
#'   9: 裾側と山側で平均値と標準偏差が異なる縦方向グラデーション、
#'  10: 裾側の左右と山側でそれぞれ平均値が異なり、裾側と山側で標準偏差が等しい縦方向グラデーション、
#'  11: 裾側と山側で平均値が等しく、裾側の左右と山側でそれぞれ標準偏差が異なる縦方向グラデーション、
#'  12: 裾側の左右と山側でそれぞれ平均値と標準偏差が異なる縦方向グラデーション、
#'  13: 4つの正規分布の平均値がすべて異なり、標準偏差がすべて等しい縦横方向グラデーション、
#'  14: 4つの正規分布の平均値がすべて等しく、標準偏差がすべて異なる縦横方向グラデーション、
#'  15: 4つの正規分布の平均値と標準偏差がすべて異なる縦横方向グラデーション。
#'
#' 相関係数が同率首位の分布がある場合、出力の best には、上の並び順の早い方を優先する。
#' @export
#' @param   x               X座標のベクトル。
#'                          最低 3 個以上の要素を含み、昇順に並べておくこと。また、値に重複がないこと。
#' @param   freq            X座標に対する度数分布のベクトル。要素数は x と同数であること。
#' @param   total           度数の合計。
#'                          前処理で除外された外れ値の数を合計に含めるかどうかは、
#'                          この関数の使用者の判断に委ねられる。そのため、この引数は省略できない。
#' @param   start           \link[stats]{nls} に渡す初期値のリスト (リストのリスト)。
#'                          start を与える場合は、
#'                          length( cgd:::kinds ) - 1 個の要素からなるリストを用意し、
#'                          上の並び順に従って、
#'                          \link[stats]{nls} に与える start のリストを入れること
#'                           (デフォルト: NULL)。
#'                          なお、ここで使用するリストのリストを作成するための補助ツールとして、
#'                          \link[cgd]{init.start.list} が利用できる。
#'                          また、各要素のリストのテンプレートが
#'                          \link[cgd]{nls.start.template} で得られる。
#'                          それらのツールの具体的な使用方法は examples を参照のこと。
#' @param   control         \link[stats]{nls} に渡す設定。詳細は \link[stats]{nls.control} を参照
#'                           (デフォルト: list( maxiter = 300, warnOnly = TRUE ) )。
#' @param   method          \link[stats]{cor} で用いる相関係数名。
#'                          NULL の場合、\link[stats]{cor} のデフォルトを用いる (デフォルト: NULL )。
#' @param   ...             その他、 \link[stats]{nls} の各引数を指定できる。
#'                          詳細は \link[stats]{nls} の Arguments を参照。
#' @return  list(   best        = 与えられた度数分布に最も近い \link[cgd]{CGD} クラスオブジェクト,
#'                  best.cor    = 与えられた度数分布に対する best の相関係数,
#'                  cgd         = length( cgd:::kinds ) - 1 個の \link[cgd]{CGD} クラスオブジェクトのリスト,
#'                  cors        = 与えられた度数分布に対する cgd の各要素の相関係数のベクトル )
#' @importFrom  methods     new
#' @importFrom  stats       cor
#' @seealso \link[cgd]{CGD_nls.freq}, \link[cgd]{nls.freq}, \link[stats]{cor},
#'          \link[cgd]{init.start.list}, \link[cgd]{nls.start.template}
#' @examples
#'  ## preparing
#'  plot.freq.and.d <- function( a, x, freq, total )
#'  {
#'      xlim <- c( min( x ), max( x ) )
#'      ylim <- c( 0, max( cgd::get.d( x, freq, total ) ) * 1.2 )
#'      plot( x, cgd::get.d( x, freq, total ), xlim = xlim, ylim = ylim, xlab = "", ylab = "" )
#'      par( new = TRUE )
#'      plot( seq( min( x ), max( x ), 0.01 ), a$d( seq( min( x ), max( x ), 0.01 ) ),
#'            type = "l", xlim = xlim, ylim = ylim )
#'  }
#'
#'  x <- seq( -2, 2, 0.2 )
#'  freq <- c( 15.164, 22.923, 25.134, 27.631, 37.239, 40.464,
#'          47.126, 79.469, 109.966, 118.241, 111.333, 78.674,
#'          46.921, 41.026, 36.975, 27.403, 25.493, 22.838,
#'          14.992, 11.468, 9.174 )
#'  total <- sum( freq )
#'
#'  ## try nls.freq.all
#'  result <- nls.freq.all( x, freq, total )
#'
#'  ## show the results
#'  result
#'  result$cgd
#'  result$best
#'  result$best.cor
#'  result$cor
#'
#'  ## the degree of freedom of cgd:::kinds[13] is larger than that of cgd:::kinds[10]
#'  ## but in this example, the result$cor[[13]] is worse than result$cor[[10]].
#'  result$cor[[10]]
#'  result$cor[[13]]    ## smaller than result$cor[[10]]
#'
#'  result$cgd[[10]]$kind ## "3-Median-Differed Sigma-Equaled Vertical Gradational Distribution"
#'  result$cgd[[13]]$kind ## "Median-Differed Sigma-Equaled Vertical-Horizontal Gradational Distri..."
#'
#'  ## see the plots
#'  plot.freq.and.d( result$cgd[[10]], x, freq, total )
#'  plot.freq.and.d( result$cgd[[13]], x, freq, total )
#'
#'  ## so we are going to retry nls.freq.all
#'  ## with setting the start of cgd:::kinds[13] as the result$cgd[[10]].
#'  start.list <- init.start.list()
#'  start.list[[13]] <- nls.start.template( result$cgd[[13]] )
#'  start.list[[13]]    ## checking the names of parameters for cgd:::kinds[13]
#'
#'  ## checking the values of result$cgd[[10]]
#'  result$cgd[[10]]$intervals.mean()   ## -0.6709851 -0.1977602  0.2928311 (about)
#'  result$cgd[[10]]$intervals.sd() ## 0.6400895 0.6400895 0.6400895 (about)
#'
#'  ## setting the start parameters of cgd:::kinds[13]
#'  start.list[[13]]$mean.1.1 <- -0.6709851
#'  start.list[[13]]$mean.1.2 <- -0.1977602
#'  start.list[[13]]$mean.2.1 <- 0.2928311
#'  start.list[[13]]$mean.2.2 <- -0.1977602
#'  start.list[[13]]$sqrt.sd <- sqrt( 0.6400895 )   ## the sqrt of the standard deviation.
#'
#'  ## now retry nls.freq.all
#'  result <- nls.freq.all( x, freq, total, start = start.list )
#'  result$cor
#'  result$cor[[10]]
#'  result$cor[[13]]    ## grater than result$cor[[10]]
#'
#'  ## see the suitability of new result$cgd[[13]]
#'  plot.freq.and.d( result$cgd[[13]], x, freq, total )
#'
#'  ## but result$cgd[[15]] is the best
#'  result$best$kind.index  ## 15
#'  result$cor[[15]]    ## grater than result$cor[[13]]
#'  plot.freq.and.d( result$cgd[[15]], x, freq, total )
################################################################################################
nls.freq.all <- function( x, freq, total, start = NULL,
						  control = list( maxiter = 300, warnOnly = TRUE ), method = NULL, ... )
{
	# start 用リスト作成
	if ( is.null( start ) )
	{
		start <- init.start.list()
	}

	# nls実行 (エラー時はメッセージ表示するが、処理は継続)
	cgds <- list()
	for ( i in 1:length( start ) )
	{
		cgds[[i]] <- CGD$new()
		try( cgds[[i]]$nls.freq( x, freq, total, start = start[[i]], control = control, kind = i, ... ) )
	}

	# 結果をまとめる
	if ( is.null( method ) )
	{
		cor.f <- function( cd, fd, m ) cor( cd, fd )
	}
	else
	{
		cor.f <- function( cd, fd, m ) cor( cd, fd, method = m )
	}

	cors <- unlist( lapply( cgds, function( cgd )
									{
										if ( is.null( cgd ) )
										{
											return ( NaN )
										}

										if ( length( cgd$intervals ) == 0 )
										{
											return ( NaN )
										}

										return ( cor.f( cgd$d( x ), get.d( x, freq, total ), method ) )
									} ) )

	best.cor <- max( cors, na.rm = TRUE )

	return ( list( best = cgds[( !is.nan( cors ) ) & ( cors == best.cor )][[1]],
					best.cor = best.cor, cgd = cgds, cor = cors ) )
}

################################################################################################
#' nls.freq.all の start 用リスト初期化
#'
#' 要素数が length( cgd:::kinds ) - 1 と等しい NULL のリストを得る。
#' \link[cgd]{nls.freq.all} の初期値 start を自分で指定したいときは、
#' この関数の戻り値のリストを得て、
#' そのリストに対して、初期値の指定が必要な所に初期値の list を与えればよい。
#'
#' リストの何番目の要素にどういった初期値を指定すればよいかは、
#' \link[cgd]{nls.freq.all} の出力 cgd の並び順または cgd:::kinds を参照すること。
#' また、 \link[cgd]{nls.start.template} で、リストの各要素のテンプレートが得られる。
#' @export
#' @return  length( cgd:::kinds ) - 1 個の NULL が入ったリスト
#' @seealso \link[cgd]{nls.freq.all}, \link[cgd]{nls.start.template}
#' @examples
#'  ## preparing
#'  x <- seq( -2, 2, 0.2 )
#'  freq <- c( 15.164, 22.923, 25.134, 27.631, 37.239, 40.464,
#'          47.126, 79.469, 109.966, 118.241, 111.333, 78.674,
#'          46.921, 41.026, 36.975, 27.403, 25.493, 22.838,
#'          14.992, 11.468, 9.174 )
#'  total <- sum( freq )
#'
#'  ## getting the initial list
#'  start.list <- init.start.list()
#'
#'  ## setting the start parameters of cgd:::kinds[13]
#'  start.list[[13]] <- nls.start.template( 13 )
#'  start.list[[13]]    ## checking the names of parameters for cgd:::kinds[13]
#'
#'  start.list[[13]]$mean.1.1 <- -0.671
#'  start.list[[13]]$mean.1.2 <- -0.198
#'  start.list[[13]]$mean.2.1 <- 0.293
#'  start.list[[13]]$mean.2.2 <- -0.198
#'  start.list[[13]]$sqrt.sd <- sqrt( 0.640 )   ## the sqrt of the standard deviation.
#'
#'  ## try nls.freq.all
#'  result <- nls.freq.all( x, freq, total, start = start.list )
#'  result$cor
################################################################################################
init.start.list <- function()
{
	start.list <- list()
	for ( i in 1:( length( kinds ) - 1 ) )
	{
		start.list[i] <- list( NULL )
	}

	return ( start.list )
}

################################################################################################
#' nls の start 用テンプレート取得
#'
#' \link[cgd]{nls.freq} や \link[cgd]{nls.freq.all} の初期値 start を
#' 自分で指定したいとき、初期値の list のテンプレートを得る。
#' @export
#' @param   target      テンプレートの対象となる分布の種類を特定する変数 (1要素のみ有効)。
#'                      変数の種類は \link[cgd]{CGD} クラスオブジェクト または、
#'                      cgd:::kinds の要素の文字列 または、そのインデックス番号が有効。
#' @return  リストのテンプレート
#'           (平均値 (mean) は 0 、標準偏差の平方根 (sqrt.sd) は 1 になっている)
#' @seealso \link[cgd]{nls.freq}, \link[cgd]{nls.freq.all}, \link[cgd]{init.start.list}
#' @examples
#'  ## preparing
#'  x <- seq( -2, 2, 0.2 )
#'  freq <- c( 15.164, 22.923, 25.134, 27.631, 37.239, 40.464,
#'          47.126, 79.469, 109.966, 118.241, 111.333, 78.674,
#'          46.921, 41.026, 36.975, 27.403, 25.493, 22.838,
#'          14.992, 11.468, 9.174 )
#'  total <- sum( freq )
#'
#'  ## setting the start parameters
#'  start.list <- nls.start.template( 13 )
#'  start.list  ## checking the names of parameters for cgd:::kinds[13]
#'
#'  start.list$mean.1.1 <- -0.671
#'  start.list$mean.1.2 <- -0.198
#'  start.list$mean.2.1 <- 0.293
#'  start.list$mean.2.2 <- -0.198
#'  start.list$sqrt.sd <- sqrt( 0.640 ) ## sqrt.sd is the square root of the standard deviation.
#'
#'  ## try nls.freq
#'  nls.freq( x, freq, total, start = start.list, kind = 13 )
################################################################################################
nls.start.template <- function( target )
{
	kind.index <- cgd.kind.index( target )[1]

	if ( is.na( kind.index ) || all( kind.index != 1:( length( kinds ) - 1 ) ) )
	{
		warning( paste( target, "is not suitable for cgd.kind.index." ) )
		return ( NULL )
	}

	temp <- list()
	if ( kind.index == 1 )
	{
		temp <- list( mean = 0, sqrt.sd = 1 )
	}
	else if ( kind.index == 2 )
	{
		temp <- list( mean = 0, sqrt.sd.1 = 1, sqrt.sd.2 = 1 )
	}
	else if ( kind.index == 3 )
	{
		temp <- list( mean = 0, sqrt.sd.1 = 1, sqrt.sd.2 = 1 )
	}
	else if ( kind.index == 4 )
	{
		temp <- list( mean.1 = 0, mean.2 = 0, sqrt.sd = 1 )
	}
	else if ( kind.index == 5 )
	{
		temp <- list( mean = 0, sqrt.sd.1 = 1, sqrt.sd.2 = 1 )
	}
	else if ( kind.index == 6 )
	{
		temp <- list( mean.1 = 0, mean.2 = 0, sqrt.sd.1 = 1, sqrt.sd.2 = 1 )
	}
	else if ( kind.index == 7 )
	{
		temp <- list( mean.1 = 0, mean.2 = 0, sqrt.sd = 1 )
	}
	else if ( kind.index == 8 )
	{
		temp <- list( mean = 0, sqrt.sd.1 = 1, sqrt.sd.2 = 1 )
	}
	else if ( kind.index == 9 )
	{
		temp <- list( mean.1 = 0, mean.2 = 0, sqrt.sd.1 = 1, sqrt.sd.2 = 1 )
	}
	else if ( kind.index == 10 )
	{
		temp <- list( mean.1 = 0, mean.2 = 0, mean.3 = 0, sqrt.sd = 1 )
	}
	else if ( kind.index == 11 )
	{
		temp <- list( mean = 0, sqrt.sd.1 = 1, sqrt.sd.2 = 1, sqrt.sd.3 = 1 )
	}
	else if ( kind.index == 12 )
	{
		temp <- list( mean.1 = 0, mean.2 = 0, mean.3 = 0, sqrt.sd.1 = 1, sqrt.sd.2 = 1, sqrt.sd.3 = 1 )
	}
	else if ( kind.index == 13 )
	{
		temp <- list( mean.1.1 = 0, mean.1.2 = 0, mean.2.1 = 0, mean.2.2 = 0, sqrt.sd = 1 )
	}
	else if ( kind.index == 14 )
	{
		temp <- list( mean = 0, sqrt.sd.1.1 = 1, sqrt.sd.1.2 = 1, sqrt.sd.2.1 = 1, sqrt.sd.2.2 = 1 )
	}
	else if ( kind.index == 15 )
	{
		temp <- list( mean.1.1 = 0, mean.1.2 = 0, sqrt.sd.1.1 = 1, sqrt.sd.1.2 = 1,
						mean.2.1 = 0, mean.2.2 = 0, sqrt.sd.2.1 = 1, sqrt.sd.2.2 = 1 )
	}

	return ( temp )
}

################################################################################################
#' kinds のインデックス取得
#'
#' 分布の種類を表すインデックス番号を取得する。
#' @export
#' @param   targets     分布の種類を特定する変数
#'                       (ベクトルまたはリストまたは \link[cgd]{CGD} クラスオブジェクト)。
#'                      ベクトルまたはリストの要素は
#'                      \link[cgd]{CGD} クラスオブジェクト か、
#'                      cgd:::kinds の要素の文字列か、またはそのインデックス番号であること。
#' @return  分布の種類 (kinds) のインデックス番号のベクトルを返す。
#'          引数に NULL が与えられた場合は NULL を返し、
#'          それ以外で対象となるインデックス番号が存在しない場合は NA を返す。
#' @importFrom  stats   complete.cases
#' @seealso \link[cgd]{cgd.kind}
#' @examples
#'  cgd.kind.index( cgd:::kinds )
#'  cgd.kind.index( 1:16 )
#'  cgd.kind.index( 6 )
#'  cgd.kind.index( "3-Median-Differed 3-Sigma-Differed Vertical Gradational Distribution" )
#'
#'  a <- CGD$new()
#'  cgd.kind.index( a )
#'
#'  x <- seq( -2, 2, 0.2 )
#'  freq <- c( 0.057, 0.277, 1.002, 3.178, 9.646, 22.109,
#'          42.723, 80.646, 117.625, 139.181, 162.319, 150.870,
#'          109.947, 78.736, 46.616, 21.058, 9.211, 3.466,
#'          0.976, 0.260, 0.061 )
#'  total <- sum( freq )
#'
#'  cgds <- nls.freq.all( x, freq, total )
#'  cgd.kind.index( cgds$cgd )
################################################################################################
cgd.kind.index <- function( targets )
{
	if ( inherits( targets, "CGD" ) )
	{
		targets <- list( targets )
	}

	results <- unlist( lapply( as.list( targets ), function( target )
	{
		if ( inherits( target, "CGD" ) )
		{
			result <- target$kind.index
		}
		else if ( is.numeric( target[[1]] ) )
		{
			if ( complete.cases( target ) && any( target == 1:length( kinds ) ) )
			{
				result <- as.integer( target )
			}
			else
			{
				result <- as.integer( NA )
			}
		}
		else
		{
			if ( complete.cases( target ) )
			{
				result <- ( 1:length( kinds ) )[( kinds == target )]
			}
			else
			{
				result <- as.integer( NA )
			}
		}

		return ( result )
	} ) )

	return ( results )
}

################################################################################################
#' 分布の種類取得
#'
#' 分布の種類を表す文字列を取得する。
#' @export
#' @param   target      分布の種類を特定する変数
#'                       (ベクトルまたはリストまたは \link[cgd]{CGD} クラスオブジェクト)。
#'                      ベクトルまたはリストの要素は、
#'                      \link[cgd]{CGD} クラスオブジェクト か、
#'                      cgd:::kinds の要素の文字列か、またはそのインデックス番号であること。
#' @return  分布の種類を表す文字列
#' @seealso \link[cgd]{cgd.kind.index}
#' @examples
#'  cgd.kind( cgd:::kinds )
#'  cgd.kind( 1:16 )
#'  cgd.kind( 6 )
#'  cgd.kind( "3-Median-Differed 3-Sigma-Differed Vertical Gradational Distribution" )
#'
#'  a <- CGD$new()
#'  cgd.kind( a )
#'
#'  x <- seq( -2, 2, 0.2 )
#'  freq <- c( 0.057, 0.277, 1.002, 3.178, 9.646, 22.109,
#'          42.723, 80.646, 117.625, 139.181, 162.319, 150.870,
#'          109.947, 78.736, 46.616, 21.058, 9.211, 3.466,
#'          0.976, 0.260, 0.061 )
#'  total <- sum( freq )
#'
#'  cgds <- nls.freq.all( x, freq, total )
#'  cgd.kind( cgds$cgd )
################################################################################################
cgd.kind <- function( target )
{
	return ( kinds[cgd.kind.index( target )] )
}

################################################################################################
#' continuous 判定
#'
#' type1.type が 1 または 2 であって、
#' continuous = TRUE として構成されているかどうかを調べる
#'  (分布が連続かどうかを調べる訳ではない)。
#' @name    CGD_is.continuous
#' @usage   CGD$is.continuous()
#' @return  continuous = TRUE として構成されていれば TRUE、そうでなければ FALSE
#' @examples
#'  a <- CGD$new()
#'  a$kind  ## Normal Distribution
#'  a$is.continuous()   ## FALSE
#'                      ## (a normal distribution is continuous
#'                      ##  but it has been not generated with continuous = TRUE option.)
#'  a$set.waypoints(
#'      data.frame( p = c( 0.25, 0.5, 0.75 ), q = c( -0.67, 0, 0.53 ) ),
#'      this.type1.type = 2, continuous = TRUE )
#'  a$kind  ## Median-Equaled Sigma-Differed Horizontal Gradational Distribution
#'  a$is.continuous()   ## TRUE
################################################################################################
NULL
CGD$methods(
	is.continuous = function()
	{
		return ( any( type1.type == 1:2 ) && intervals[[1]]$p.conn.next[1] == 0 && intervals[[1]]$p.conn.next[2] == 1 )
	}
)

################################################################################################
#' symmetric 判定
#'
#' type1.type が 1 または 2 であって、
#' symmetric = TRUE として構成されているかどうかを調べる
#'  (分布が左右対称かどうかを調べる訳ではない)。
#' @name    CGD_is.symmetric
#' @usage   CGD$is.symmetric()
#' @return  symmetric = TRUE として構成されていれば TRUE、そうでなければ FALSE
#' @examples
#'  a <- CGD$new()
#'  a$kind  ## Normal Distribution
#'  a$is.symmetric()    ## FALSE
#'                      ## (a normal distribution is symmetric
#'                      ##  but it has been not generated with symmetric = TRUE option.)
#'  a$set.waypoints(
#'      data.frame( p = c( 0.25, 0.5, 0.70 ), q = c( -0.67, 0, 0.35 ) ),
#'      this.type1.type = 2, symmetric = TRUE )
#'  a$kind  ## Symmetric Horizontal Gradational Distribution
#'  a$is.symmetric()    ## TRUE
################################################################################################
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
							intervals[[1]]$sd == intervals[[length( intervals )]]$sd ) )
	}
)

################################################################################################
#' v.grad 判定
#'
#' v.grad = TRUE として構成されているかどうかを調べる。
#' @name    CGD_is.v.grad
#' @usage   CGD$is.v.grad()
#' @return  v.grad = TRUE として構成されていれば TRUE、そうでなければ FALSE
#' @examples
#'  a <- CGD$new()
#'  a$kind  ## Normal Distribution
#'  a$is.v.grad()   ## FALSE
#'
#'  a$set.waypoints(
#'      data.frame( p = c( 0.1, 0.4, 0.6, 0.9 ), q = c( -1.92, -0.20, 0.20, 1.92 ) ),
#'      this.type1.type = 3, v.grad = TRUE )
#'  a$kind  ## Median-Differed Sigma-Differed Vertical Gradational Distribution
#'  a$is.v.grad()   ## TRUE
################################################################################################
NULL
CGD$methods(
	is.v.grad = function()
	{
		return ( type1.type == 3 &&
					intervals[[1]]$mean == intervals[[length( intervals )]]$mean &&
					intervals[[1]]$sd == intervals[[length( intervals )]]$sd )
	}
)

################################################################################################
#' uni.sigma 判定
#'
#' 分布を構成するすべての正規分布の標準偏差が等しいかどうかを調べる。
#' @name    CGD_is.uni.sigma
#' @usage   CGD$is.uni.sigma()
#' @return  分布を構成するすべての正規分布の標準偏差が等しければ TRUE、そうでなければ FALSE
#' @examples
#'  a <- CGD$new()
#'  a$kind  ## Normal Distribution
#'  a$is.uni.sigma()    ## TRUE
#'
#'  a$set.waypoints(
#'      data.frame( p = c( 0.1, 0.4, 0.6, 0.9 ), q = c( -1.92, -0.20, 0.20, 1.92 ) ),
#'      this.type1.type = 3, v.grad = TRUE )
#'  a$kind  ## Median-Differed Sigma-Differed Vertical Gradational Distribution
#'  a$is.uni.sigma()    ## FALSE
#'
#'  a$set.waypoints(
#'      data.frame( p = c( 0.25, 0.5, 0.70 ), q = c( -0.67, 0, 0.53 ) ),
#'      this.type1.type = 2, continuous = TRUE, uni.sigma = TRUE )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
#'  a$is.uni.sigma()    ## TRUE
################################################################################################
NULL
CGD$methods(
	is.uni.sigma = function()
	{
		sds <- intervals.sd()
		return ( all( sds == sds[1] ) )
	}
)

################################################################################################
#' uni.mean 判定
#'
#' 分布を構成するすべての正規分布の平均値が等しいかどうかを調べる。
#' @name    CGD_is.uni.mean
#' @usage   CGD$is.uni.mean()
#' @return  構成するすべての正規分布の平均値が等しければ TRUE、そうでなければ FALSE
#' @examples
#'  a <- CGD$new()
#'  a$kind  ## Normal Distribution
#'  a$is.uni.mean() ## TRUE
#'
#'  a$set.waypoints(
#'      data.frame( p = c( 0.1, 0.4, 0.6, 0.9 ), q = c( -1.92, -0.20, 0.20, 1.92 ) ),
#'      this.type1.type = 3, v.grad = TRUE )
#'  a$kind  ## Median-Differed Sigma-Differed Vertical Gradational Distribution
#'  a$is.uni.mean() ## FALSE
#'
#'  a <- CGD$new()
#'  a$set.waypoints(
#'      data.frame(
#'          p = c( 0.2, 0.5, 0.6, 0.7 ),
#'          q = c( qnorm( c( 0.2, 0.5, 0.6 ), 0, 1 ), 0.5 ) ),
#'      this.type1.type = 1 )
#'  a$kind  ## Discontinuous Connected Gaussian Distribution
#'  a$is.uni.mean() ## TRUE
################################################################################################
NULL
CGD$methods(
	is.uni.mean = function()
	{
		means <- intervals.mean()
		return ( all( means == means[1] ) )
	}
)

################################################################################################
#' 独立区間判定 (確率)
#'
#' 指定された確率が独立区間に入っているかどうかを調べる。
#' @name    CGD_is.ind.p
#' @usage   CGD$is.ind.p(p)
#' @param   p       確率のベクトル。
#' @return  第1要素 (bool) に、独立区間に入っていれば TRUE、入っていなければ FALSE。
#'          第2要素 (i) に、当該 intervals のインデックス番号が入ったリスト。
#'               bool = FALSE の場合は NaN。
#' @examples
#'  a <- CGD$new()
#'  a$set.waypoints(
#'  data.frame(
#'      p = c( 0.2, 0.5, 0.6, 0.7 ),
#'      q = c( qnorm( c( 0.2, 0.5, 0.6 ), 0, 1 ), 0.5 ) ),
#'  this.type1.type = 1 )
#'  a$is.ind.p( c( 0, 0.6, 0.65, 0.7, 1 ) )
################################################################################################
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
				warning( paste( "Warning: Probability" , p[i] , "is out of range [0, 1]." ) )
				next
			}

			if ( type1.type == 4 )
			{
				if ( p[i] == 0 )
				{
					bool[i] <- TRUE
					i.result[i] <- 1
				}
				else if ( p[i] == 1 )
				{
					bool[i] <- TRUE
					i.result[i] <- 2
				}
			}
			else
			{
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
		}

		return ( list( bool = bool, i = i.result ) )
	}
)

################################################################################################
#' 接続区間判定 (確率)
#'
#' 指定された確率が接続区間に入っているかどうかを調べる。
#' @name    CGD_is.conn.p
#' @usage   CGD$is.conn.p(p)
#' @param   p       確率のベクトル。
#' @return  第1要素 (bool) に、接続区間に入っていれば TRUE、入っていなければ FALSE。
#'              ただし、 type1.type = 3 では、
#'              2番目の独立区間が接続区間と互いに重なることがあるが、
#'              指定された確率が独立区間と接続区間の両方に入っているときは FALSE を返す。
#'          第2要素 (i.1) に、前の intervals のインデックス番号。
#'              bool = FALSE の場合は 独立区間の intervals のインデックス番号。
#'          第3要素 (i.2) に、後の intervals のインデックス番号が入ったリスト。
#'              bool = FALSE の場合は NaN。
#' @examples
#'  a <- CGD$new()
#'  a$set.waypoints(
#'  data.frame(
#'      p = c( 0.2, 0.5, 0.6, 0.7 ),
#'      q = c( qnorm( c( 0.2, 0.5, 0.6 ), 0, 1 ), 0.5 ) ),
#'  this.type1.type = 1 )
#'  a$is.conn.p( c( 0, 0.6, 0.65, 0.7, 1 ) )
################################################################################################
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
				warning( paste( "Warning: Probability" , p[i] , "is out of range [0, 1]." ) )
				next
			}

			is.ind <- is.ind.p( p[i] )
			if ( !is.ind$bool )
			{
				bool[i] <- TRUE
				if ( type1.type == 4 )
				{
					i.1[i] <- 1
					i.2[i] <- 2
				}
				else
				{
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
			}
			else
			{
				i.1[i] <- is.ind$i
			}
		}

		return ( list( bool = bool, i.1 = i.1, i.2 = i.2 ) )
	}
)

################################################################################################
#' 独立区間判定 (X座標 (クォンタイル) )
#'
#' 指定されたX座標 (クォンタイル) が独立区間に入っているかどうかを調べる。
#' @name    CGD_is.ind.x
#' @usage   CGD$is.ind.x(x)
#' @param   x       X座標 (クォンタイル) のベクトル。
#' @return  第1要素 (bool) に、独立区間に入っていれば TRUE、入っていなければ FALSE。
#'          第2要素 (i) に、当該 intervals のインデックス番号が入ったリスト。
#'               bool = FALSE の場合は NaN。
#' @examples
#'  a <- CGD$new()
#'  a$set.waypoints(
#'  data.frame(
#'      p = c( 0.2, 0.5, 0.6, 0.7 ),
#'      q = c( qnorm( c( 0.2, 0.5, 0.6 ), 0, 1 ), 0.5 ) ),
#'  this.type1.type = 1 )
#'  a$is.ind.x( c( -Inf, -0.3, 0.3, 3.0, Inf ) )
################################################################################################
NULL
CGD$methods(
	is.ind.x = function( x )
	{
		bool <- rep( FALSE, length( x ) )
		i.result <- rep( NaN, length( x ) )

		for ( i in 1:length( x ) )
		{
			if ( type1.type == 4 )
			{
				if ( x[i] == -Inf )
				{
					bool[i] <- TRUE
					i.result[i] <- 1
				}
				else if ( x[i] == Inf )
				{
					bool[i] <- TRUE
					i.result[i] <- 2
				}
			}
			else
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
		}

		return ( list( bool = bool, i = i.result ) )
	}
)

################################################################################################
#' 接続区間判定 (X座標 (クォンタイル) )
#'
#' 指定されたX座標 (クォンタイル) が接続区間に入っているかどうかを調べる。
#' @name    CGD_is.conn.x
#' @usage   CGD$is.conn.x(x)
#' @param   x       X座標 (クォンタイル) のベクトル。
#' @return  第1要素 (bool) に、接続区間に入っていれば TRUE、入っていなければ FALSE。
#'              ただし、type1.type = 3 では、
#'              2番目の独立区間が接続区間と互いに重なることがあるが、
#'              指定された座標が独立区間と接続区間の両方に入っているときは FALSE を返す。
#'          第2要素 (i.1) に、前の intervals のインデックス番号。
#'              bool = FALSE の場合は 独立区間の intervals のインデックス。
#'          第3要素 (i.2) に、後の intervals のインデックス番号が入ったリスト。
#'              bool = FALSE の場合は NaN。
#' @examples
#'  a <- CGD$new()
#'  a$set.waypoints(
#'  data.frame(
#'      p = c( 0.2, 0.5, 0.6, 0.7 ),
#'      q = c( qnorm( c( 0.2, 0.5, 0.6 ), 0, 1 ), 0.5 ) ),
#'  this.type1.type = 1 )
#'  a$is.conn.x( c( -Inf, -0.3, 0.3, 3.0, Inf ) )
################################################################################################
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
				if ( type1.type == 4 )
				{
					i.1[i] <- 1
					i.2[i] <- 2
				}
				else
				{
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
			}
			else
			{
				i.1[i] <- is.ind$i
			}
		}

		return ( list( bool = bool, i.1 = i.1, i.2 = i.2 ) )
	}
)

################################################################################################
#' 独立区間判定 (X座標 (クォンタイル) )
#'
#' 指定されたX座標 (クォンタイル) が独立区間に入っているかどうかを調べる。
#' @name    CGD_is.ind.q
#' @usage   CGD$is.ind.q(q)
#' @param   q       X座標 (クォンタイル) のベクトル。
#' @return  第1要素 (bool) に、独立区間に入っていれば TRUE、入っていなければ FALSE。
#'          第2要素 (i) に、当該 intervals のインデックス番号が入ったリスト。
#'               bool = FALSE の場合は NaN。
#' @examples
#'  a <- CGD$new()
#'  a$set.waypoints(
#'  data.frame(
#'      p = c( 0.2, 0.5, 0.6, 0.7 ),
#'      q = c( qnorm( c( 0.2, 0.5, 0.6 ), 0, 1 ), 0.5 ) ),
#'  this.type1.type = 1 )
#'  a$is.ind.q( c( -Inf, -0.3, 0.3, 3.0, Inf ) )
################################################################################################
NULL
CGD$methods(
	is.ind.q = function( q )
	{
		return ( is.ind.x( q ) )
	}
)

################################################################################################
#' 接続区間判定 (X座標 (クォンタイル) )
#'
#' 指定されたX座標 (クォンタイル) が接続区間に入っているかどうかを調べる。
#' @name    CGD_is.conn.q
#' @usage   CGD$is.conn.q(q)
#' @param   q       X座標 (クォンタイル) のベクトル。
#' @return  第1要素 (bool) に、接続区間に入っていれば TRUE、入っていなければ FALSE。
#'              ただし、type1.type = 3 では、
#'              2番目の独立区間が接続区間と互いに重なることがあるが、
#'              指定された座標が独立区間と接続区間の両方に入っているときは FALSE を返す。
#'          第2要素 (i.1) に、前の intervals のインデックス番号。
#'              bool = FALSE の場合は 独立区間の intervals のインデックス。
#'          第3要素 (i.2) に、後の intervals のインデックス番号が入ったリスト。
#'              bool = FALSE の場合は NaN。
#' @examples
#'  a <- CGD$new()
#'  a$set.waypoints(
#'  data.frame(
#'      p = c( 0.2, 0.5, 0.6, 0.7 ),
#'      q = c( qnorm( c( 0.2, 0.5, 0.6 ), 0, 1 ), 0.5 ) ),
#'  this.type1.type = 1 )
#'  a$is.conn.q( c( -Inf, -0.3, 0.3, 3.0, Inf ) )
################################################################################################
NULL
CGD$methods(
	is.conn.q = function( q )
	{
		return ( is.conn.x( q ) )
	}
)

################################################################################################
#' 確率密度取得
#'
#' X座標 (クォンタイル) を指定して、確率密度を取得する。
#' @name    CGD_d
#' @usage   CGD$d(x)
#' @param   x       X座標 (クォンタイル) のベクトル。
#' @return  x における確率密度関数の値
#' @importFrom  stats       dnorm pnorm
#' @examples
#'  a <- CGD$new()
#'  a$set.waypoints(
#'      data.frame( p = c( 0.25, 0.5, 0.75 ), q = c( -0.67, 0, 0.53 ) ),
#'      this.type1.type = 2, continuous = TRUE )
#'  a$d( c( -0.67, 0, 0.53 ) )
#'  plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
################################################################################################
NULL
CGD$methods(
	d = function( x )
	{
		if ( type1.type == 2 && is.symmetric() )
		{
			x <- median - abs( median - x )
		}

		results <- vapply( x, function( x )
		{
			if ( type1.type == 4 )
			{
				# type1.type == 4
				p.1 <- f.t3.p[[1]]( x, intervals[[1]][[1]]$mean, intervals[[1]][[1]]$sd ) +
					   f.t3.p[[2]]( x, intervals[[1]][[2]]$mean, intervals[[1]][[2]]$sd )
				p.2 <- f.t3.p[[1]]( x, intervals[[2]][[1]]$mean, intervals[[2]][[1]]$sd ) +
					   f.t3.p[[2]]( x, intervals[[2]][[2]]$mean, intervals[[2]][[2]]$sd )

				result <- ( 1 - p.1 ) * ( f.t3.d[[1]]( x, intervals[[1]][[1]]$mean, intervals[[1]][[1]]$sd ) +
										  f.t3.d[[2]]( x, intervals[[1]][[2]]$mean, intervals[[1]][[2]]$sd ) ) +
							p.2 * ( f.t3.d[[1]]( x, intervals[[2]][[1]]$mean, intervals[[2]][[1]]$sd ) +
									f.t3.d[[2]]( x, intervals[[2]][[2]]$mean, intervals[[2]][[2]]$sd ) )
			}
			else if ( type1.type == 3 )
			{
				# type1.type == 3 の場合の計算は v.grad であってもなくても同じ

				result <- dp.t3( x, c( intervals[[1]]$mean, intervals[[2]]$mean, intervals[[3]]$mean ),
									c( intervals[[1]]$sd, intervals[[2]]$sd, intervals[[3]]$sd ), f.t3.d )
			}
			else if ( is.continuous() )
			{
				# continuous の場合
				if ( type1.type == 1 )
				{
					result <- ( dnorm( x, intervals[[1]]$mean, intervals[[1]]$sd ) +
								dnorm( x, intervals[[2]]$mean, intervals[[2]]$sd ) ) / 2
				}
				else # if ( type1.type == 2 )
				{
					result <- ( 1 - pnorm( x, intervals[[1]]$mean, intervals[[1]]$sd ) ) *
									dnorm( x, intervals[[1]]$mean, intervals[[1]]$sd ) +
									pnorm( x, intervals[[2]]$mean, intervals[[2]]$sd ) *
									dnorm( x, intervals[[2]]$mean, intervals[[2]]$sd )
				}
			}
			else if ( is.symmetric() )
			{
				# symmetric の場合
				# type1.type == 2 ( type1.type == 1 は is.continuous() == TRUE になる)
				result <- ( 1 - 2 * pnorm( x, intervals[[1]]$mean, intervals[[1]]$sd ) ) *
									dnorm( x, intervals[[1]]$mean, intervals[[1]]$sd ) +
								2 * pnorm( x, intervals[[1]]$mean, intervals[[2]]$sd ) *
									dnorm( x, intervals[[1]]$mean, intervals[[2]]$sd )
			}
			else
			{
				# 確率密度関数が不連続の場合
				is.conn <- is.conn.x( x )
				if ( !is.conn$bool )
				{
					# 独立区間内 ⇒ 確率密度をそのまま出力
					result <- dnorm( x, intervals[[is.conn$i.1]]$mean, intervals[[is.conn$i.1]]$sd )
				}
				else
				{
					# 接続区間内 ⇒ 区間前後の2つの分布で確率密度を負担
					j <- is.conn$i.1
					if ( x > intervals[[j]]$q.manage()[2] && x < intervals[[j + 1]]$q.manage()[1] )
					{
						# 負担分布なし ⇒ d( x ) = 0 (このケースは type 2 の場合に生じる)
						return ( 0 )
					}

					d.1 <- intervals[[j]]			# 確率分布1 (接続区間前を負担)
					d.2 <- intervals[[j + 1]]		# 確率分布2 (接続区間後を負担)
					x.conn <- c( d.1$q.conn.next[1], d.2$q.conn.prev[2] )
													# 接続区間の範囲のX座標 (クォンタイル)

					if ( x.conn[1] < median )		# 中央値より絶対小でなければならない。「以下」は不可
					{
						if ( x.conn[2] <= median )
						{
							# 中央値をまたがない
							if ( d.2$sd >= d.1$sd )
							{
								# type 1
								if ( type1.type == 1 )
								{
									result <- ( dnorm( x, median, d.1$sd ) * ( x.conn[2] - x ) +
												dnorm( x, median, d.2$sd ) * ( x - x.conn[1] ) +
												pnorm( x, median, d.2$sd ) - pnorm( x, median, d.1$sd ) ) /
												( x.conn[2] - x.conn[1] )
								}
								else # if ( type1.type == 2 )
								{
									p1.conn1 <- pnorm( x.conn[1], median, d.1$sd )
									p2.conn1 <- pnorm( x.conn[1], median, d.2$sd )
									psum.conn1 <- p1.conn1 + p2.conn1

									p1.conn2 <- pnorm( x.conn[2], median, d.1$sd )
									p2.conn2 <- pnorm( x.conn[2], median, d.2$sd )
									psum.conn2 <- p1.conn2 + p2.conn2

									result <- ( ( psum.conn2 - 2 * pnorm( x, median, d.1$sd ) ) *
													dnorm( x, median, d.1$sd ) +
												( 2 * pnorm( x, median, d.2$sd ) - psum.conn1 ) *
													dnorm( x, median, d.2$sd ) ) /
												( psum.conn2 - psum.conn1 )
								}
							}
							else
							{
								# type 2
								if ( x < d.2$q.conn.prev[1] )
								{
									# P2(x) < inf
									if ( x < d.1$q.conn.next[2] )
									{
										# P1(x) < sup ⇒ 確率分布1 が単独で負担
										result <- dnorm( x, median, d.1$sd ) / 2
									}
									else
									{
										# P1(x) >= sup ⇒ 負担分布なし
										#	注: v1.2.0 以降、
										#		負担分布なしの場合には前の処理で return ( 0 ) とするようにしたため、
										#		この処理は使われなくなったが、今後の注意のために残しておく。
										result <- 0
									}
								}
								else
								{
									# P2(x) >= inf
									if ( x < d.1$q.conn.next[2] )
									{
										# P1(x) < sup ⇒ 確率分布1 と 2 が負担
										result <- ( dnorm( x, median, d.1$sd ) + dnorm( x, median, d.2$sd ) ) / 2
									}
									else
									{
										# P1(x) >= sup ⇒ 確率分布2 が単独で負担
										result <- dnorm( x, median, d.2$sd ) / 2
									}
								}
							}
						}
						else
						{
							# 中央値をまたぐ
							if ( d.2$sd >= d.1$sd )
							{
								# type 3a
								if ( x <= median )
								{
									result <- dnorm( x, median, d.1$sd )
								}
								else
								{
									if ( x < d.1$q.conn.next[2] )
									{
										# P1(x) < sup ⇒ 確率分布1 と 2 が負担
										result <- ( dnorm( x, median, d.1$sd ) + dnorm( x, median, d.2$sd ) ) / 2
									}
									else
									{
										# P1(x) >= sup ⇒ 確率分布2 が単独で負担
										result <- dnorm( x, median, d.2$sd ) / 2
									}
								}
							}
							else
							{
								# type 3b
								if ( x <= median )
								{
									if ( x < d.2$q.conn.prev[1] )
									{
										# P2(x) < inf ⇒ 確率分布1 が単独で負担
										result <- dnorm( x, median, d.1$sd ) / 2
									}
									else
									{
										# P2(x) >= inf ⇒ 確率分布1 と 2 が負担
										result <- ( dnorm( x, median, d.1$sd ) + dnorm( x, median, d.2$sd ) ) / 2
									}
								}
								else
								{
									result <- dnorm( x, median, d.2$sd )
								}
							}
						}
					}
					else	# ( x.conn[1] >= median )	# 中央値より絶対大でなくてもよい。「以上」でよい
					{
						if ( d.2$sd >= d.1$sd )
						{
							# type 2
							if ( x < d.2$q.conn.prev[1] )
							{
								# P2(x) < inf
								if ( x < d.1$q.conn.next[2] )
								{
									# P1(x) < sup ⇒ 確率分布1 が単独で負担
									result <- dnorm( x, median, d.1$sd ) / 2
								}
								else
								{
									# P1(x) >= sup ⇒ 負担分布なし
									#	注: v1.2.0 以降、
									#		負担分布なしの場合には前の処理で return ( 0 ) とするようにしたため、
									#		この処理は使われなくなったが、今後の注意のために残しておく。
									result <- 0
								}
							}
							else
							{
								# P2(x) >= inf
								if ( x < d.1$q.conn.next[2] )
								{
									# P1(x) < sup ⇒ 確率分布1 と 2 が負担
									result <- ( dnorm( x, median, d.1$sd ) + dnorm( x, median, d.2$sd ) ) / 2
								}
								else
								{
									# P1(x) >= sup ⇒ 確率分布2 が単独で負担
									result <- dnorm( x, median, d.2$sd ) / 2
								}
							}
						}
						else
						{
							# type 1
							if ( type1.type == 1 )
							{
								result <- ( dnorm( x, median, d.1$sd ) * ( x.conn[2] - x ) +
											dnorm( x, median, d.2$sd ) * ( x - x.conn[1] ) +
											pnorm( x, median, d.2$sd ) - pnorm( x, median, d.1$sd ) ) /
											( x.conn[2] - x.conn[1] )
							}
							else # if ( type1.type == 2 )
							{
								p1.conn1 <- pnorm( x.conn[1], median, d.1$sd )
								p2.conn1 <- pnorm( x.conn[1], median, d.2$sd )
								psum.conn1 <- p1.conn1 + p2.conn1

								p1.conn2 <- pnorm( x.conn[2], median, d.1$sd )
								p2.conn2 <- pnorm( x.conn[2], median, d.2$sd )
								psum.conn2 <- p1.conn2 + p2.conn2

								result <- ( ( psum.conn2 - 2 * pnorm( x, median, d.1$sd ) ) *
												dnorm( x, median, d.1$sd ) +
											( 2 * pnorm( x, median, d.2$sd ) - psum.conn1 ) *
												dnorm( x, median, d.2$sd ) ) /
											( psum.conn2 - psum.conn1 )
							}
						}
					}
				}
			}

			return ( result )
		}, 0 )

		return ( results )
	}
)

################################################################################################
#' 確率取得
#'
#' X座標 (クォンタイル) を指定して、確率を取得する。
#' @name    CGD_p
#' @usage   CGD$p(q)
#' @param   q                       X座標 (クォンタイル) のベクトル。
#' @return  X座標 (クォンタイル) q における確率
#' @importFrom  stats       pnorm
#' @examples
#'  a <- CGD$new()
#'  a$set.waypoints(
#'      data.frame( p = c( 0.25, 0.5, 0.75 ), q = c( -0.67, 0, 0.53 ) ),
#'      this.type1.type = 2, continuous = TRUE )
#'  a$p( c( -0.67, 0, 0.53 ) )
#'  plot( seq( -3, 3, 0.01 ), a$p( seq( -3, 3, 0.01 ) ), type = "l" )
################################################################################################
NULL
CGD$methods(
	p = function( q )
	{
		results <- vapply( q, function( q )
		{
			if ( type1.type == 4 )
			{
				# type1.type == 4
				p.1 <- f.t3.p[[1]]( q, intervals[[1]][[1]]$mean, intervals[[1]][[1]]$sd ) +
					   f.t3.p[[2]]( q, intervals[[1]][[2]]$mean, intervals[[1]][[2]]$sd )
				p.2 <- f.t3.p[[1]]( q, intervals[[2]][[1]]$mean, intervals[[2]][[1]]$sd ) +
					   f.t3.p[[2]]( q, intervals[[2]][[2]]$mean, intervals[[2]][[2]]$sd )

				result <- p.1 - p.1^2 / 2 + p.2^2 / 2
			}
			else if ( type1.type == 3 )
			{
				# type1.type == 3 の場合の計算は v.grad であってもなくても同じ

				result <- dp.t3( q, c( intervals[[1]]$mean, intervals[[2]]$mean, intervals[[3]]$mean ),
									c( intervals[[1]]$sd, intervals[[2]]$sd, intervals[[3]]$sd ), f.t3.p )
			}
			else if ( is.continuous() )
			{
				# continuous の場合
				if ( type1.type == 1 )
				{
					result <- ( pnorm( q, intervals[[1]]$mean, intervals[[1]]$sd ) +
								pnorm( q, intervals[[2]]$mean, intervals[[2]]$sd ) ) / 2
				}
				else # if ( type1.type == 2 )
				{
					p1 <- pnorm( q, intervals[[1]]$mean, intervals[[1]]$sd )
					p2 <- pnorm( q, intervals[[2]]$mean, intervals[[2]]$sd )
					result <- p1 - ( p1 * p1 - p2 * p2 ) / 2
				}
			}
			else if ( is.symmetric() )
			{
				# symmetric の場合
				# type1.type == 2
				if ( q <= median )
				{
					p1 <- pnorm( q, intervals[[1]]$mean, intervals[[1]]$sd )
					p2 <- pnorm( q, intervals[[2]]$mean, intervals[[2]]$sd )
					result <- ( 1 - p1 ) * p1 + p2 * p2
				}
				else
				{
					p1 <- pnorm( 2 * intervals[[1]]$mean - q, intervals[[1]]$mean, intervals[[1]]$sd )
					p2 <- pnorm( 2 * intervals[[2]]$mean - q, intervals[[2]]$mean, intervals[[2]]$sd )
					result <- 1 - ( 1 - p1 ) * p1 - p2 * p2
				}
			}
			else
			{
				# 確率密度関数が不連続の場合
				is.conn <- is.conn.q( q )
				if ( !is.conn$bool )
				{
					# 独立区間内 ⇒ 確率をそのまま出力
					result <- pnorm( q, intervals[[is.conn$i.1]]$mean, intervals[[is.conn$i.1]]$sd )
				}
				else
				{
					# 接続区間内 ⇒ 区間前後の2つの分布で確率を負担
					j <- is.conn$i.1
					if ( q > intervals[[j]]$q.manage()[2] && q < intervals[[j + 1]]$q.manage()[1] )
					{
						# 負担分布なし ⇒ p = 直前の分布が負担する確率の上限 = 接続区間の確率の上限と下限の平均
						return ( ( intervals[[j]]$p.conn.next[1] + intervals[[j]]$p.conn.next[2] ) / 2 )
					}

					d.1 <- intervals[[j]]			# 確率分布1 (接続区間前を負担)
					d.2 <- intervals[[j + 1]]		# 確率分布2 (接続区間後を負担)
					x.conn <- c( d.1$q.conn.next[1], d.2$q.conn.prev[2] )
													# 接続区間のX座標 (クォンタイル)

					if ( x.conn[1] < median )		# 中央値より絶対小でなければならない。「以下」は不可
					{
						if ( d.1$q.conn.next[2] <= median )
						{
							# 中央値をまたがない
							if ( d.2$sd >= d.1$sd )
							{
								# type 1
								if ( type1.type == 1 )
								{
									result <- ( pnorm( q, median, d.1$sd ) * ( x.conn[2] - q ) +
												pnorm( q, median, d.2$sd ) * ( q - x.conn[1] ) ) /
												( x.conn[2] - x.conn[1] )
								}
								else # if ( type1.type == 2 )
								{
									p1.conn1 <- pnorm( x.conn[1], median, d.1$sd )
									p2.conn1 <- pnorm( x.conn[1], median, d.2$sd )
									psum.conn1 <- p1.conn1 + p2.conn1

									p1.conn2 <- pnorm( x.conn[2], median, d.1$sd )
									p2.conn2 <- pnorm( x.conn[2], median, d.2$sd )
									psum.conn2 <- p1.conn2 + p2.conn2

									p1.q <- pnorm( q, median, d.1$sd )
									p2.q <- pnorm( q, median, d.2$sd )

									result <- ( ( psum.conn2 - p1.q ) * p1.q + ( p2.q - psum.conn1 ) * p2.q ) /
												( psum.conn2 - psum.conn1 )
								}
							}
							else
							{
								# type 2
								if ( q < d.2$q.conn.prev[1] )
								{
									# P2(x) < inf
									if ( q < d.1$q.conn.next[2] )
									{
										# P1(x) < sup ⇒ 確率分布1 が単独で負担
										result <- ( d.1$p.conn.next[1] + pnorm( q, median, d.1$sd ) ) / 2
									}
									else
									{
										# P1(x) >= sup ⇒ 負担分布なし
										#	注: v1.2.0 以降、負担分布なしの場合には前の処理で
										#		接続区間の確率の上限と下限の平均を return するようにしたため、
										#		この処理は使われなくなったが、今後の注意のために残しておく。
										result <- ( d.1$p.conn.next[1] + d.1$p.conn.next[2] ) / 2
									}
								}
								else
								{
									# P2(x) >= inf
									if ( q < d.1$q.conn.next[2] )
									{
										# P1(x) < sup ⇒ 確率分布1 と 2 が負担
										result <- ( pnorm( q, median, d.1$sd ) + pnorm( q, median, d.2$sd ) ) / 2
									}
									else
									{
										# P1(x) >= sup ⇒ 確率分布2 が単独で負担
										result <- ( d.1$p.conn.next[2] + pnorm( q, median, d.2$sd ) ) / 2
									}
								}
							}
						}
						else
						{
							# 中央値をまたぐ
							if ( d.2$sd >= d.1$sd )
							{
								# type 3a
								if ( q <= median )
								{
									result <- pnorm( q, median, d.1$sd )
								}
								else
								{
									if ( q < d.1$q.conn.next[2] )
									{
										# P1(x) < sup ⇒ 確率分布1 と 2 が負担
										result <- ( pnorm( q, median, d.1$sd ) + pnorm( q, median, d.2$sd ) ) / 2
									}
									else
									{
										# P1(x) >= sup ⇒ 確率分布2 が単独で負担
										result <- ( d.1$p.conn.next[2] + pnorm( q, median, d.2$sd ) ) / 2
									}
								}
							}
							else
							{
								# type 3b
								if ( q <= median )
								{
									if ( q < d.2$q.conn.prev[1] )
									{
										# P2(x) < inf ⇒ 確率分布1 が単独で負担
										result <- ( pnorm( q, median, d.1$sd ) + d.2$p.conn.prev[1] ) / 2
									}
									else
									{
										# P2(x) >= inf ⇒ 確率分布1 と 2 が負担
										result <- ( pnorm( q, median, d.1$sd ) + pnorm( q, median, d.2$sd ) ) / 2
									}
								}
								else
								{
									result <- pnorm( q, median, d.2$sd )
								}
							}
						}
					}
					else	# ( x.conn[1] >= median )	# 中央値より絶対大でなくてもよい。「以上」でよい
					{
						if ( d.2$sd >= d.1$sd )
						{
							# type 2
							if ( q < d.2$q.conn.prev[1] )
							{
								# P2(x) < inf
								if ( q < d.1$q.conn.next[2] )
								{
									# P1(x) < sup ⇒ 確率分布1 が単独で負担
									result <- ( d.1$p.conn.next[1] + pnorm( q, median, d.1$sd ) ) / 2
								}
								else
								{
									# P1(x) >= sup ⇒ 負担分布なし
									#	注: v1.2.0 以降、負担分布なしの場合には前の処理で
									#		接続区間の確率の上限と下限の平均を return するようにしたため、
									#		この処理は使われなくなったが、今後の注意のために残しておく。
									result <- ( d.1$p.conn.next[1] + d.1$p.conn.next[2] ) / 2
								}
							}
							else
							{
								# P2(x) >= inf
								if ( q < d.1$q.conn.next[2] )
								{
									# P1(x) < sup ⇒ 確率分布1 と 2 が負担
									result <- ( pnorm( q, median, d.1$sd ) + pnorm( q, median, d.2$sd ) ) / 2
								}
								else
								{
									# P1(x) >= sup ⇒ 確率分布2 が単独で負担
									result <- ( d.1$p.conn.next[2] + pnorm( q, median, d.2$sd ) ) / 2
								}
							}
						}
						else
						{
							# type 1
							if ( type1.type == 1 )
							{
								result <- ( pnorm( q, median, d.1$sd ) * ( x.conn[2] - q ) +
											pnorm( q, median, d.2$sd ) * ( q - x.conn[1] ) ) /
											( x.conn[2] - x.conn[1] )
							}
							else # if ( type1.type == 2 )
							{
								p1.conn1 <- pnorm( x.conn[1], median, d.1$sd )
								p2.conn1 <- pnorm( x.conn[1], median, d.2$sd )
								psum.conn1 <- p1.conn1 + p2.conn1

								p1.conn2 <- pnorm( x.conn[2], median, d.1$sd )
								p2.conn2 <- pnorm( x.conn[2], median, d.2$sd )
								psum.conn2 <- p1.conn2 + p2.conn2

								p1.q <- pnorm( q, median, d.1$sd )
								p2.q <- pnorm( q, median, d.2$sd )

								result <- ( ( psum.conn2 - p1.q ) * p1.q + ( p2.q - psum.conn1 ) * p2.q ) /
											( psum.conn2 - psum.conn1 )
							}
						}
					}
				}
			}

			return ( result )
		}, 0 )

		return ( results )
	}
)

################################################################################################
#' X座標 (クォンタイル) 取得
#'
#' 確率を指定して、X座標 (クォンタイル) を取得する。
#' @name    CGD_q
#' @usage   CGD$q(prob, tol = .Machine$double.eps * 16)
#' @param   prob            確率のベクトル。
#' @param   tol             許容誤差 (デフォルト: .Machine$double.eps * 16)。
#' @return  確率 prob に当たるX座標 (クォンタイル)
#' @importFrom  stats       qnorm
#' @examples
#'  a <- CGD$new()
#'  a$set.waypoints(
#'      data.frame( p = c( 0.25, 0.5, 0.75 ), q = c( -0.67, 0, 0.53 ) ),
#'      this.type1.type = 2, continuous = TRUE )
#'  a$q( c( 0.25, 0.5, 0.75 ) )
#'  plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" )
################################################################################################
NULL
CGD$methods(
	q = function( prob, tol = .Machine$double.eps * 16 )
	{
		means <- intervals.mean()
		sds <- intervals.sd()

		if ( type1.type == 4 )
		{
			min.mean <- min( means )
			max.mean <- max( means )
			min.sd <- min( sds[1], sds[2] * sqrt( 2 ) / 2, sds[4], sds[5] * sqrt( 2 ) / 2 )
			max.sd <- max( sds[1], sds[2] * sqrt( 2 ) / 2, sds[4], sds[5] * sqrt( 2 ) / 2 )
			qs <- numeric( 4 )
		}
		else if ( type1.type == 3 )
		{
			sds.star <- sds * sqrt( 2 ) / 2

			max.mean.a <- max( means[1:2] )
			min.mean.a <- min( means[1:2] )
			max.mean.b <- max( means[2:3] )
			min.mean.b <- min( means[2:3] )

			max.sd.a <- max( sds[1], sds.star[2] )
			#min.sd.a <- min( sds[1], sds.star[2] ) # 使わない
			min.sd.a.2 <- min( sds.star[1], sds.star[2] )
			max.sd.b <- max( sds[3], sds.star[2] )
			min.sd.b <- min( sds[3], sds.star[2] )
			min.sd.b.2 <- min( sds.star[3], sds.star[2] )
			qs <- numeric( 6 )
		}
		else if ( is.continuous() || is.symmetric() )
		{
			qs <- numeric( 2 )
		}

		results <- vapply( prob, function( prob )
		{
			if ( prob < 0 || prob > 1 )
			{
				# 確率が負または1を超えている
				warning( paste( "Warning: Probability" , prob, "is out of range [0, 1]." ) )
				return ( NaN )
			}
			else if ( prob == 0 )
			{
				return ( -Inf )
			}
			else if ( prob == 1 )
			{
				return ( Inf )
			}
			else if ( prob == 0.5 )
			{
				result <- median
			}

			if ( is.uni.mean() && is.uni.sigma() )
			{
				# 正規分布
				result <- qnorm( prob, means[1], sds[1] )
			}
			else
			{
				if ( type1.type == 4 )
				{
					p.2.m.sqrt2 <- ( 2 - sqrt( 2 ) ) * prob
					qs[1] <- qnorm( p.2.m.sqrt2, min.mean, min.sd )
					qs[2] <- qnorm( p.2.m.sqrt2, min.mean, max.sd )
					qs[3] <- qnorm( p.2.m.sqrt2 + sqrt( 2 ) - 1, max.mean, min.sd )
					qs[4] <- qnorm( p.2.m.sqrt2 + sqrt( 2 ) - 1, max.mean, max.sd )

					if ( prob < 0.5 )
					{
						result <- bisection( function( x ) { p( x ) - prob }, c( min( qs[1:2] ), median ), tol )
					}
					else
					{
						result <- bisection( function( x ) { p( x ) - prob }, c( median, max( qs[3:4] ) ), tol )
					}
				}
				else if ( type1.type == 3 )
				{
					if ( prob >= p( means[1] ) && prob <= p( means[3] ) )
					{
						result <- qnorm( sqrt( 2 ) * ( prob - 0.5 ) + 0.5, means[2], sds.star[2] )
					}
					else
					{
						if ( prob < 0.5 )
						{
							qs[1] <- median
							qs[2] <- qnorm( ( 2 - sqrt( 2 ) ) * prob, min.mean.a, max.sd.a )
							qs[3] <- qnorm( prob, min.mean.b, max.sd.b )
							qs[4] <- median
							qs[5] <- qnorm( prob, max.mean.a, min.sd.a.2 )
							qs[6] <- qnorm( ( 2 - sqrt( 2 ) ) * prob + sqrt( 2 ) - 1, max.mean.b, min.sd.b )
							result <- bisection( function( x ) { p( x ) - prob }, c( min( qs[1:3] ), max( qs[4:6] ) ), tol )
						}
						else
						{
							# 高速化のための median の位置に注意
							qs[1] <- qnorm( ( 2 - sqrt( 2 ) ) * prob, min.mean.a, sds[1] )
							qs[2] <- qnorm( ( 2 - sqrt( 2 ) ) * prob, min.mean.a, sds.star[2] )
							qs[3] <- qnorm( prob, min.mean.b, min.sd.b.2 )
							qs[4] <- median
							qs[5] <- qnorm( prob, max.mean.a, max.sd.a )
							qs[6] <- qnorm( ( 2 - sqrt( 2 ) ) * prob + sqrt( 2 ) - 1, max.mean.b, max.sd.b )
							result <- bisection( function( x ) { p( x ) - prob }, c( min( qs[1:4] ), max( qs[4:6] ) ), tol )
						}
					}
				}
				else if ( is.continuous() || is.symmetric() )
				{
					# type1.type = 1, 2, continuous or symmetric ⇒
					# 累積分布関数の上限と下限は、構成要素の正規分布の累積分布関数で押さえられており、
					# それら2つの累積分布関数の値に挟まれた間から外に出ることはない
					qs[1] <- qnorm( prob, means[1], sds[1] )
					qs[2] <- qnorm( prob, means[2], sds[2] )
					if ( qs[1]	== qs[2] )
					{
						result <- qs[1]
					}
					else
					{
						result <- bisection( function( x ) { p( x ) - prob }, qs, tol )
					}
				}
				else
				{
					# 確率密度関数が不連続の場合
					is.conn <- is.conn.p( prob )
					j <- is.conn$i.1
					if ( !is.conn$bool )
					{
						# 独立区間内 ⇒ クォンタイルをそのまま出力
						result <- qnorm( prob, intervals[[j]]$mean, intervals[[j]]$sd )
					}
					else
					{
						# 接続区間内 ⇒ 区間前後の2つの分布で確率密度を負担
						if ( intervals[[j]]$p.conn.next[1] < 0.5 && intervals[[j]]$p.conn.next[2] > 0.5 )
						{
							# type 3 ⇒ prob が中央値より大か小かで値域を絞れる
							if ( prob < 0.5 )
							{
								result <- bisection( function( x ) { p( x ) - prob },
													 c( intervals[[j]]$q.conn.next[1], median ), tol )
							}
							else # if ( prob > 0.5 )
							{
								result <- bisection( function( x ) { p( x ) - prob },
													 c( median, intervals[[j + 1]]$q.conn.prev[2] ), tol )
							}
						}
						else
						{
							if ( intervals[[j]]$sd < intervals[[j + 1]]$sd )
							{
								# type 1 ⇒ 接続区間内は至る所で微分可能かつ単調増加なので、場合分けしなくても収束する
								result <- bisection( function( x ) { p( x ) - prob },
														c( intervals[[j]]$q.conn.next[1],
															intervals[[j + 1]]$q.conn.prev[2] ), tol )
							}
							else
							{
								# type 2 ⇒ 場合分けが必要
								dm <- ( intervals[[j]]$p.conn.next[1] + intervals[[j]]$p.conn.next[2] ) / 2 - prob
								if ( dm == 0 )
								{
									# prob が接続区間の確率の上限と下限の平均 ⇒ 負担分布なしの可能性あり
									if ( intervals[[j]]$q.conn.next[2] < intervals[[j + 1]]$q.conn.prev[1] )
									{
										# 負担分布なし
										# ⇒ 累積分布関数の逆写像は幅のある区間となり、一点に定まらない。
										# その区間は
										#	[intervals[[j]]$q.manage()[2], intervals[[j + 1]]$q.manage()[1]] になる。
										# この場合は、区間の平均値を出力しておく。
										result <- ( intervals[[j]]$q.manage()[2] + intervals[[j + 1]]$q.manage()[1] ) / 2
									}
									else
									{
										# 負担分布あり ⇒ X座標が接続区間の中点とは限らないので、その周りの範囲で方程式を解く
										result <- bisection( function( x ) { p( x ) - prob },
																c( intervals[[j + 1]]$q.conn.prev[1],
																	intervals[[j]]$q.conn.next[2] ), tol )
									}
								}
								else
								{
									result <- bisection( function( x ) { p( x ) - prob },
															c( intervals[[j]]$q.conn.next[1],
																intervals[[j + 1]]$q.conn.prev[2] ), tol )
								}
							}
						}
					}
				}
			}

			return ( result )
		}, 0 )

		return ( results )
	}
)

################################################################################################
#' ランダムサンプル取得
#'
#' ランダムサンプルを取得する。
#' @name    CGD_r
#' @usage   CGD$r(n, tol = 2^(-17))
#' @param   n       サンプル数。
#' @param   tol     q() に許容する誤差 (デフォルト: 2^(-17))。
#' @return  ランダムサンプルのベクトル
#' @examples
#'  a <- CGD$new()
#'  a$set.waypoints(
#'  data.frame( p = c( 0.25, 0.5, 0.75 ), q = c( -0.67, 0, 0.53 ) ),
#'  this.type1.type = 2, continuous = TRUE )
#'  hist( a$r( 400 ) )
################################################################################################
NULL
CGD$methods(
	r = function( n, tol = 2^(-17) )
	{
		return ( q( runif( n, 0, 1 ), tol ) )
	}
)

################################################################################################
#' TeX 形式表示
#'
#' 累積分布関数 \eqn{\Psi(x)} と確率密度関数 \eqn{g(x)} を
#' TeX 形式で表示する (連続分布の場合のみ有効)。
#' @name    CGD_tex
#' @usage   CGD$tex(decimal = 6, write.comma = TRUE)
#' @param   decimal         小数点以下桁数 (デフォルト: 6)。
#' @param   write.comma     論文のように "," と "." を付けるかどうかのフラグ
#'                           (デフォルト: TRUE)。
#' @return  なし
#' @examples
#'  a <- CGD$new()
#'  a$set.waypoints(
#'  data.frame( p = c( 0.25, 0.5, 0.75 ), q = c( -0.67, 0, 0.53 ) ),
#'  this.type1.type = 2, continuous = TRUE )
#'  a$tex()
#'
#'  x <- seq( -2, 2, 0.2 )
#'  freq <- c( 15.164, 22.923, 25.134, 27.631, 37.239, 40.464,
#'          47.126, 79.469, 109.966, 118.241, 111.333, 78.674,
#'          46.921, 41.026, 36.975, 27.403, 25.493, 22.838,
#'          14.992, 11.468, 9.174 )
#'  total <- sum( freq )
#'  a$nls.freq( x, freq, total, this.type1.type = 1 )
#'  a$tex( 3, FALSE )
################################################################################################
NULL
CGD$methods(
	tex = function( decimal = 6, write.comma = TRUE )
	{
		if ( decimal < 0 || decimal > 16 )
		{
			decimal <- 6
		}

		tex.body <- ""
		if ( kind.index == 1 )
		{
			tex.body <- paste0( tex.p.all[kind.index], tex.d.all[kind.index], "\\\\\n",
								tex.p.sub.0, tex.d.sub.0 )
		}
		else if ( type1.type == 1 || ( type1.type == 2 && is.symmetric() ) )
		{
			tex.body <- paste0( tex.p.all[kind.index], tex.d.all[kind.index], "\\\\\n",
								tex.p.sub.1, tex.d.sub.1 )
		}
		else if ( type1.type == 2 )
		{
			tex.body <- paste0( tex.p.all[kind.index], tex.d.all[kind.index], "\\\\\n",
								tex.p.sub.2, tex.d.sub.2 )
		}
		else if ( type1.type == 3 && is.v.grad() )
		{
			tex.body <- paste0( tex.p.all[kind.index], tex.d.all[kind.index], "\\\\\n",
								tex.p.sub.v, tex.d.sub.v )
		}
		else if ( type1.type == 3 )
		{
			tex.body <- paste0( tex.p.all[kind.index], tex.d.all[kind.index], "\\\\\n",
								tex.p.sub.3[1], tex.d.sub.3[1], "\\\\\n", tex.p.sub.3[2], tex.d.sub.3[2] )
		}
		else if ( type1.type == 4 )
		{
			tex.body <- paste0( tex.p.all[kind.index], tex.d.all[kind.index], "\\\\\n",
								tex.p.sub.4[1], tex.d.sub.4[1], "\\\\\n", tex.p.sub.4[2], tex.d.sub.4[2] )
		}

		tex.form <- paste0( tex.body, get.tex.val( .self, decimal ) )

		if ( write.comma )
		{
			tex.form <- paste0( tex.form.header,
								gsub( ";", ",", gsub( ";;", ".", tex.form ) ),
								tex.form.footer )
		}
		else
		{
			tex.form <- paste0( tex.form.header,
								gsub( ";", "", tex.form ),
								tex.form.footer )
		}

		writeLines( tex.form )
	}
)

################################################################################################
#' 累積分布関数の TeX 形式表示
#'
#' 累積分布関数 \eqn{\Psi(x)} を TeX 形式で表示する (連続分布の場合のみ有効)。
#' @name    CGD_tex.p
#' @usage   CGD$tex.p(decimal = 6, write.comma = TRUE)
#' @param   decimal         小数点以下桁数 (デフォルト: 6)。
#' @param   write.comma     論文のように "," と "." を付けるかどうかのフラグ
#'                           (デフォルト: TRUE)。
#' @return  なし
#' @examples
#'  a <- CGD$new()
#'  a$set.waypoints(
#'  data.frame( p = c( 0.25, 0.5, 0.75 ), q = c( -0.67, 0, 0.53 ) ),
#'  this.type1.type = 2, continuous = TRUE )
#'  a$tex.p()
#'
#'  x <- seq( -2, 2, 0.2 )
#'  freq <- c( 15.164, 22.923, 25.134, 27.631, 37.239, 40.464,
#'          47.126, 79.469, 109.966, 118.241, 111.333, 78.674,
#'          46.921, 41.026, 36.975, 27.403, 25.493, 22.838,
#'          14.992, 11.468, 9.174 )
#'  total <- sum( freq )
#'  a$nls.freq( x, freq, total, this.type1.type = 1 )
#'  a$tex.p( 3, FALSE )
################################################################################################
NULL
CGD$methods(
	tex.p = function( decimal = 6, write.comma = TRUE )
	{
		if ( decimal < 0 || decimal > 16 )
		{
			decimal <- 6
		}

		tex.body <- ""
		if ( kind.index == 1 )
		{
			tex.body <- paste0( tex.p.all[kind.index], "\\\\\n", tex.p.sub.0 )
		}
		else if ( type1.type == 1 || ( type1.type == 2 && is.symmetric() ) )
		{
			tex.body <- paste0( tex.p.all[kind.index], "\\\\\n", tex.p.sub.1 )
		}
		else if ( type1.type == 2 )
		{
			tex.body <- paste0( tex.p.all[kind.index], "\\\\\n", tex.p.sub.2 )
		}
		else if ( type1.type == 3 && is.v.grad() )
		{
			tex.body <- paste0( tex.p.all[kind.index], "\\\\\n", tex.p.sub.v )
		}
		else if ( type1.type == 3 )
		{
			tex.body <- paste0( tex.p.all[kind.index], "\\\\\n",
								tex.p.sub.3[1], "\\\\\n", tex.p.sub.3[2] )
		}
		else if ( type1.type == 4 )
		{
			tex.body <- paste0( tex.p.all[kind.index], "\\\\\n",
								tex.p.sub.4[1], "\\\\\n", tex.p.sub.4[2] )
		}

		tex.form <- paste0( tex.body, get.tex.val( .self, decimal ) )

		if ( write.comma )
		{
			tex.form <- paste0( tex.form.header,
								gsub( ";", ",", gsub( ";;", ".", tex.form ) ),
								tex.form.footer )
		}
		else
		{
			tex.form <- paste0( tex.form.header,
								gsub( ";", "", tex.form ),
								tex.form.footer )
		}

		writeLines( tex.form )
	}
)

################################################################################################
#' 確率密度関数の TeX 形式表示
#'
#' 確率密度関数 \eqn{g(x)} を TeX 形式で表示する (連続分布の場合のみ有効)。
#' @name    CGD_tex.d
#' @usage   CGD$tex.d(decimal = 6, write.comma = TRUE)
#' @param   decimal         小数点以下桁数 (デフォルト: 6)。
#' @param   write.comma     論文のように "," と "." を付けるかどうかのフラグ
#'                           (デフォルト: TRUE)。
#' @return  なし
#' @examples
#'  a <- CGD$new()
#'  a$set.waypoints(
#'  data.frame( p = c( 0.25, 0.5, 0.75 ), q = c( -0.67, 0, 0.53 ) ),
#'  this.type1.type = 2, continuous = TRUE )
#'  a$tex.d()
#'
#'  x <- seq( -2, 2, 0.2 )
#'  freq <- c( 15.164, 22.923, 25.134, 27.631, 37.239, 40.464,
#'          47.126, 79.469, 109.966, 118.241, 111.333, 78.674,
#'          46.921, 41.026, 36.975, 27.403, 25.493, 22.838,
#'          14.992, 11.468, 9.174 )
#'  total <- sum( freq )
#'  a$nls.freq( x, freq, total, this.type1.type = 1 )
#'  a$tex.d( 3, FALSE )
################################################################################################
NULL
CGD$methods(
	tex.d = function( decimal = 6, write.comma = TRUE )
	{
		if ( decimal < 0 || decimal > 16 )
		{
			decimal <- 6
		}

		tex.body <- ""
		if ( kind.index == 1 )
		{
			tex.body <- paste0( tex.d.all[kind.index], "\\\\\n", tex.d.sub.0 )
		}
		else if ( type1.type == 1 || ( type1.type == 2 && is.symmetric() ) )
		{
			tex.body <- paste0( tex.d.all[kind.index], "\\\\\n", tex.d.sub.1 )
		}
		else if ( type1.type == 2 )
		{
			# type1.type == 2 では、式に Ψ_i(x) が出てくるので、 tex.p.sub.2 も必要
			tex.body <- paste0( tex.d.all[kind.index], "\\\\\n", tex.p.sub.2, tex.d.sub.2 )
		}
		else if ( type1.type == 3 && is.v.grad() )
		{
			tex.body <- paste0( tex.d.all[kind.index], "\\\\\n", tex.d.sub.v )
		}
		else if ( type1.type == 3 )
		{
			tex.body <- paste0( tex.d.all[kind.index], "\\\\\n",
								tex.d.sub.3[1], "\\\\\n", tex.d.sub.3[2] )
		}
		else if ( type1.type == 4 )
		{
			# type1.type == 4 では、式に Ψ_i(x) が出てくるので、 tex.p.sub.4 も必要
			tex.body <- paste0( tex.d.all[kind.index], "\\\\\n",
								tex.p.sub.4[1], tex.d.sub.4[1], "\\\\\n", tex.p.sub.4[2], tex.d.sub.4[2] )
		}

		tex.form <- paste0( tex.body, get.tex.val( .self, decimal ) )

		if ( write.comma )
		{
			tex.form <- paste0( tex.form.header,
								gsub( ";", ",", gsub( ";;", ".", tex.form ) ),
								tex.form.footer )
		}
		else
		{
			tex.form <- paste0( tex.form.header,
								gsub( ";", "", tex.form ),
								tex.form.footer )
		}

		writeLines( tex.form )
	}
)

################################################################################################
#' [内部関数] 変数部の TeX 形式取得
#'
#' 平均値と標準偏差の TeX 形式を取得する
#' @param   cgd         \link[cgd]{CGD} クラスオブジェクト。
#' @param   decimal     小数点以下桁数。
#' @return  平均値と標準偏差の TeX 形式
################################################################################################
get.tex.val = function( cgd, decimal )
{
	formatter <- sprintf( "%%.%df", decimal )

	if ( cgd$kind.index == 1 )
	{
		tex <- sub( "mean", sprintf( formatter, cgd$mean ), tex.val.sub.0 )
		tex <- sub( "sd", sprintf( formatter, cgd$sd ), tex )
	}
	else if ( cgd$type1.type == 1 || ( cgd$type1.type == 2 && cgd$is.symmetric() ) )
	{
		tex <- sub( "mean", sprintf( formatter, cgd$mean ), tex.val.sub.1 )
		tex <- sub( "sd.1", sprintf( formatter, cgd$intervals[[1]]$sd ), tex )
		tex <- sub( "sd.2", sprintf( formatter, cgd$intervals[[2]]$sd ), tex )
	}
	else if ( cgd$type1.type == 2 || ( cgd$type1.type == 3 && cgd$is.v.grad() ) )
	{
		tex <- sub( "mean.1", sprintf( formatter, cgd$intervals[[1]]$mean ), tex.val.sub.2 )
		tex <- sub( "sd.1", sprintf( formatter, cgd$intervals[[1]]$sd ), tex )

		if ( cgd$is.uni.mean() )
		{
			tex <- sub( "mean.2", "\\\\mu_1", tex )
		}
		else
		{
			tex <- sub( "mean.2", sprintf( formatter, cgd$intervals[[2]]$mean ), tex )
		}

		if ( cgd$is.uni.sigma() )
		{
			tex <- sub( "sd.2", "\\\\sigma_1", tex )
		}
		else
		{
			tex <- sub( "sd.2", sprintf( formatter, cgd$intervals[[2]]$sd ), tex )
		}
	}
	else if ( cgd$type1.type == 3 )
	{
		tex <- sub( "mean.1", sprintf( formatter, cgd$intervals[[1]]$mean ), tex.val.sub.3 )
		tex <- sub( "sd.1", sprintf( formatter, cgd$intervals[[1]]$sd ), tex )

		if ( cgd$is.uni.mean() )
		{
			tex <- sub( "mean.2", "\\\\mu_1", tex )
			tex <- sub( "mean.3", "\\\\mu_1", tex )
		}
		else
		{
			tex <- sub( "mean.2", sprintf( formatter, cgd$intervals[[2]]$mean ), tex )
			tex <- sub( "mean.3", sprintf( formatter, cgd$intervals[[3]]$mean ), tex )
		}

		if ( cgd$is.uni.sigma() )
		{
			tex <- sub( "sd.2", "\\\\sigma_1", tex )
			tex <- sub( "sd.3", "\\\\sigma_1", tex )
		}
		else
		{
			tex <- sub( "sd.2", sprintf( formatter, cgd$intervals[[2]]$sd ), tex )
			tex <- sub( "sd.3", sprintf( formatter, cgd$intervals[[3]]$sd ), tex )
		}
	}
	else # if ( cgd$type1.type == 4 )
	{
		tex <- sub( "mean.1.1", sprintf( formatter, cgd$intervals[[1]][[1]]$mean ), tex.val.sub.4 )
		tex <- sub( "sd.1.1", sprintf( formatter, cgd$intervals[[1]][[1]]$sd ), tex )

		if ( cgd$is.uni.mean() )
		{
			tex <- sub( "mean.1.2", "\\\\mu_{1,1}", tex )
			tex <- sub( "mean.2.1", "\\\\mu_{1,1}", tex )
			tex <- sub( "mean.2.2", "\\\\mu_{1,1}", tex )
		}
		else
		{
			tex <- sub( "mean.1.2", sprintf( formatter, cgd$intervals[[1]][[2]]$mean ), tex )
			tex <- sub( "mean.2.1", sprintf( formatter, cgd$intervals[[2]][[1]]$mean ), tex )
			tex <- sub( "mean.2.2", sprintf( formatter, cgd$intervals[[2]][[2]]$mean ), tex )
		}

		if ( cgd$is.uni.sigma() )
		{
			tex <- sub( "sd.1.2", "\\\\sigma_{1,1}", tex )
			tex <- sub( "sd.2.1", "\\\\sigma_{1,1}", tex )
			tex <- sub( "sd.2.2", "\\\\sigma_{1,1}", tex )
		}
		else
		{
			tex <- sub( "sd.1.2", sprintf( formatter, cgd$intervals[[1]][[2]]$sd ), tex )
			tex <- sub( "sd.2.1", sprintf( formatter, cgd$intervals[[2]][[1]]$sd ), tex )
			tex <- sub( "sd.2.2", sprintf( formatter, cgd$intervals[[2]][[2]]$sd ), tex )
		}
	}

	return ( tex )
}
