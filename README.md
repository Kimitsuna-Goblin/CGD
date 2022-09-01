# 連結ガウス分布 - Connected Gaussian Distribution

連結ガウス分布とは、
正規分布 (ガウス分布) を区間で切断し、隙間を持たせてX軸方向に並べ、
累積分布関数が連続になるように連結させた確率分布です。

連結ガウス分布を使えば、どんなクォンタイルを指定しても、それをゼロ誤差で再現する分布をすぐに構成できます。

平均値が等しく、分散が異なる正規分布を連結させることによって、
どんな経路 (X座標 (クォンタイル) とその点における確率) を指定しても、
累積分布関数がその経路上のすべての点をゼロ誤差で通過する確率分布を構成できるからです。

……と書くと、なんとなく凄そうな気がするかも知れませんが、
連結ガウス分布の確率密度関数は、一般に不連続で、いびつで、自然界には絶対に存在しなさそうな分布になります。
連結する隙間の部分に、凄いひずみを作るからです。

とはいえ、連結ガウス分布は、確率分布としての要件を満たしますし、(局所的な) 正規分布の混合だけで構成しますので、
もしかしたら、何かの役に立つかも知れません。

特に、ランダムサンプルを取得してヒストグラムを描けば、かなり滑らかなグラフになるので、
たとえば、何かクォンタイルが与えられていて、それを高い精度で近似する確率分布のグラフが欲しいときなどは、
ランダムサンプルとヒストグラムを使えば、わりと良い感じのグラフが描けるだろうと思います。

ともかく、これはそういった分布を構成する、R言語のソースファイルです。


## 使い方 - How to use

<pre>
> source( "CGD.stand-alone.R" )    # ソースファイルを読み込みます
> a <- CGD$new()    # 連結ガウス分布クラスのオブジェクトを生成します
>
>    # 連結分布の構成が既知の場合は (ほとんど無いと思いますが)
>    # new() の引数に
>    #       mean = 平均値,
>    #       intervals = 連結分布の構成 (CGDInterval オブジェクトの list)
>    # を与えてください
>    #
>    # new() の引数に type1.type = 2 を指定すると、
>    # 確率密度関数のひずみの形が少しだけマシになるかも知れません
>
> a$set.waypoints(
>   data.frame(
>     p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
>     q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ) )
> # set.waypoints() : 経路 (X座標 (あるいはクォンタイル) とその点における確率) を指定し、
> #                   指定されたすべての点を通過する累積分布関数を持つ連結ガウス分布を構成します
> #                   p = 0.5 (平均値) の点は必ず指定してください
>
> dev.new(); plot.new()
> plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
> # d() : X座標を指定して、確率密度を返します
>
> dev.new(); plot.new()
> plot( seq( -3, 3, 0.01 ), a$p( seq( -3, 3, 0.01 ) ), type = "l" )
> # p() : X座標を指定して、確率を返します
>
> dev.new(); plot.new()
> plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" )
> # q() : 確率を指定して、X座標 (クォンタイル) を返します
> #       確率が同一となるX座標が、ある区間内に無限に存在し、
> #       一意に定まらない場合は、該当区間の中点の座標を返します
>
> dev.new(); plot.new()
> sample <- a$r( 1000 )
> hist( sample )
> # r() : ランダムサンプルを生成します (高速化は全然やってません)
> #       でも、もしかしたらこれが一番役に立つのかも？
>
> a    # オブジェクトの内部構造を表示します

Reference class object of class "CGD"
Field "mean":
[1] 0
Field "intervals":
[[1]]
Reference class object of class "CGDInterval"
Field "sd":
[1] 1
Field "q.ind":
[1]      -Inf 0.2533471
Field "q.conn.prev":
[1] -Inf -Inf
Field "q.conn.next":
[1] 0.2533471 0.5244005
Field "p.ind":
[1] 0.0 0.6
Field "p.conn.prev":
[1] 0 0
Field "p.conn.next":
[1] 0.6 0.7

[[2]]
Reference class object of class "CGDInterval"
Field "sd":
[1] 0.9534697
Field "q.ind":
[1] 0.5 Inf
Field "q.conn.prev":
[1] 0.2415588 0.5000000
Field "q.conn.next":
[1] Inf Inf
Field "p.ind":
[1] 0.7 1.0
Field "p.conn.prev":
[1] 0.6 0.7
Field "p.conn.next":
[1] 1 1

Field "m.sd":
[1] -Inf
> # 各要素の意味はソースファイルのコメントを参照してください
> #
> # m.sd はこの連結ガウス分布の標準偏差です
> # 計算に時間がかかるので、計算結果をメンバに持たせています
> # m.sd は set.waypoints() で -Inf に初期化され、sd() メソッドを呼び出すと、計算結果と同じ値が設定されます
> # 次に sd() を呼んだときは、まじめに計算せずに m.sd の値をそのまま返します
> # 標準偏差を得るには sd() を使い、m.sd は直接参照しないでください
</pre>


## ファイル構成 - Files

このソフトウェア (ライブラリ) は以下のファイルから成り立ちます。
<br>
なお、以下のソースファイルはすべて4文字タブで整形しています。

[CGD.stand-alone.R](https://github.com/Kimitsuna-Goblin/CGD/blob/master/CGD.stand-alone.R) - メインのR言語のソースファイルです。単独で動きます。

[CGD.need-nleqslv.R](https://github.com/Kimitsuna-Goblin/CGD/blob/master/CGD.need-nleqslv.R) - 同じく、メインのR言語のソースファイルです。
非線形連立方程式の nleqslv ライブラリが必要です。連続な確率密度関数を持つ連結ガウス分布を構成できます。

[common.R](https://github.com/Kimitsuna-Goblin/CGD/blob/master/common.R) - おまけの自作ライブラリ (抜粋公開版) です。

著作者のローカルPC環境では、CGD.stand-alone.R の元になった GCD.R は、この common.R を参照していました。
ですが、 common.R の中にある GCD.R に必要な関数は、すべて CGD.stand-alone.R と CGD.need-nleqslv.R にコピーしました。

なので、 common.R は必要ないのですが、
もしかすると、このファイルの中に誰かの役に立つ関数があるかも知れないので、おまけで公開しました。


## 連結ガウス分布の構成方法 (一般の場合)<BR> - How to construct a Connected Gaussian Distribution (generally)

一般に、連結ガウス分布は次のようにして構成することができる。

1. まず、確率が取りうる値の全体である $[0, 1]$ 区間の部分閉区間を有限個 ( $n$ 個) 持つ集合 $P$ を、閉区間同士が互いに重なり合わないようにして用意する。
ただし、閉区間のどれか1つに必ず 0 が含まれ、どれか1つに必ず 1 が含まれるようにする。

つまり、

$$
P = \left\lbrace P_i \subset [0, 1] \mid P_i=[a_i, b_i] ( 1 \leq i \leq n, 0 \leq a_i \leq b_i \leq 1 ), a_1=0, b_n=1,
      i \neq j \Rightarrow P_i \cap P_j = \varnothing \right\rbrace
$$

を満たすような、閉区間の集合 $P$ を用意する (便宜上、 $P$ の先頭の要素 $P_1$ に 0 が含まれ、最後の要素 $P_n$ に 1 が含まれるものとした)。

各要素 $P_i$ は、1点のみからなる区間 $[a_i, a_i] ( 0 \leq a_i \leq 1 )$ でもよい。

それぞれの閉区間 $P_i$ は昇順に並んでいることが望ましい。
すなわち、 $1 \leq i \leq n - 1$ に対して、 $b_i ＜ a_{i+1}$ となっていることが望ましい
 (以下、 $P$ はこの条件を満たすものとする)。

ここで、予め、累積分布関数の経路 (あるいは、クォンタイルとその確率。以下、経路と言う)
$W = \left\lbrace ( x_j, p_j ) \in ( \mathbb{R}, [0, 1] ) \mid 1 \leq j \leq m \right\rbrace$
が与えられているときは、
その経路上のすべての確率 $p_j ( 1 \leq j \leq m )$ の値が、いずれかの $P_i$ に必ず含まれるように取る
 (便宜上、経路 $W$ の確率 $\left\lbrace p_j \right\rbrace$ は昇順に並んでいるものとする)。

この際、もし、 $n$ を $m$ と等しく取り、それぞれ1つの閉区間 $P_i ( 1 \leq i \leq n = m )$ に確率 $p_i$ が1つだけ含まれるように $P$ を用意するならば、
後の工程が確実に実行できる。
ただし、後の工程が実行可能であれば、1つの閉区間 $P_i$ に複数の確率 $p_j, p_{j+1}, p_{j+2}, \cdots$ が含まれるように用意してもよい。


2. 閉区間の集合 $P$ の要素数と等しい、 $n$ 個の正規分布からなる集合 $N = \left\lbrace N_1, N_2, \cdots, N_n \right\rbrace$ を用意して、
その要素の正規分布 $N_i ( 1 \leq i \leq n )$ に、
閉区間 $P_i$ をそれぞれ1つずつ割り当てる。

このとき、集合 $N$ が以下の2つの条件を満たすように、正規分布 $N_i$ を用意する。

+ 任意の2つの $N_i, N_j ( i \neq j )$ について、
$N_i, N_j$ に閉区間 $P_i, P_j$ が割り当てられているとき、
$N_i, N_j$ の累積分布関数 $\Phi_i( x ), \Phi_j( x )$ について、
それぞれの区間に対する定義域 $\lbrace x \mid \Phi_i( x ) \in P_i \rbrace$ と
$\lbrace x \mid \Phi_j( x ) \in P_j \rbrace$ が、互いに重ならないこと。
逆関数を使って言い換えれば、
$\left\lbrace \Phi_i^{-1}( p ) \mid p \in P_i \right\rbrace ∩ \left\lbrace \Phi_j^{-1}( p ) \mid p \in P_j \right\rbrace = \varnothing$
であること。
+ 経路 $W = \left\lbrace ( x_j, p_j ) \right\rbrace$ が与えられており、
経路上の確率 $p_j$ が閉区間 $P_i$ に含まれているときは、
その区間が割り当てられている確率分布 $N_i$ の累積分布関数 $\Phi_i$ が、必ずその経路上の点 $( x_j, p_j )$ を通過すること。
すなわち、 $p_j \in P_i \Rightarrow \Phi_i( x_j ) = p_j$ が成り立つこと。

なお、集合 $N$ の要素の中には、 $N_i = N_j ( i \neq j )$ なる同一の分布が存在してもよい。

閉区間 $P_i$ がある程度の幅を持ち、 $P_i$ の中に複数個の経路上の確率 $p_j, p_{j+1}, p_{j+2}, ...$ を含んでいる場合は、
特別な場合を除いて、一般に、上の2つの条件を満たす正規分布の集合 $N$ を構成することはできない。
しかし、すべての閉区間 $P_i$ が高々1個の経路上の確率 $p_j$ しか含まない場合、特に、すべての閉区間 $P_i$ が1点のみからなる場合は、
2つの条件を満たす正規分布の集合 $N$ を必ず構成することができる (自明であろう)。

さて、これまでの工程によって、与えられた経路上の点をすべて通過する、累積分布関数の不連続な断片が構成できたので、あとは、それらの断片を連続になるように接続すればよい。
そこで、最終的な工程として、次のように断片の接続を行う。

以下、記述を簡単にするため、
閉区間 $P_i = [a_i, b_i]$ に対し、
その区間が割り当てられた正規分布 $N_i$ の累積分布関数 $\Phi_i( x )$ の定義域を
$[\alpha_i, \beta_i]$ と書く。
同様に、 $P_{i+1} = [a_{i+1}, b_{i+1}]$ に対する $\Phi_{i+1}( x )$ の定義域を $[\alpha_{i+1}, \beta_{i+1}]$ と書く。
また、2つの閉区間 $P_i = [a_i, b_i], P_{i+1} = [a_{i+1}, b_{i+1}]$ の間の開区間 $(b_i, a_{i+1})$ を $Q_i$ と書く。

3. 開区間 $Q_i = (b_i, a_{i+1})$ $( 1 \leq i \leq n - 1 )$ に対して、
$\Psi_i( \beta_i ) = b_i, \Psi_i( \alpha_{i+1} ) = a_{i+1}$ を満たすような、
区間 $[\beta_i, \alpha_{i+1}]$ において単調増加する連続関数 $\Psi_i(x) = f_i( x ) \Phi_i( x ) + g_i( x ) \Phi_{i+1}( x )$ を適当に定める。
このとき、連続関数 $f_i( x ), g_i( x )$ はできるだけシンプルな関数であることが望ましい。
注意点として、 $x$ が $\beta_i < x < \alpha_{i+1}$ の範囲を動くとき、
$\Psi_i(x)$ の値は必ず $Q_i$ の範囲の中に収まらなければならず、決して $Q_i$ の範囲外に出てはならない。

以上のように関数 $\Phi_i( x ), \Psi_i( x )$ を定めたとき、
関数

$$
\Phi_{CGD}(x)=
\begin{cases}
\Phi_i(x) & \textrm{where } x \in [\alpha_i, \beta_i], \quad i = 1, \cdots, n \\
\Psi_i(x) & \textrm{where } x \in (\beta_i, \alpha_{i+1}), \quad i = 1, \cdots, n - 1
\end{cases}
$$

は確率分布の累積分布関数としての要件を満たす。
以上のように構成した累積分布関数を持つ確率分布を「連結ガウス分布」と呼ぶ。

これまで述べてきた閉区間 $P_i$ と開区間 $Q_i$ の呼称について、
閉区間 $P_i =  [a_i, b_i]$ は、1つの正規分布の累積分布関数 $\Phi_i(x)$ が、単独で独立的に $\Phi_{CGD}(x)$ を負担している区間であることから、これを「独立区間」と呼ぶ。
それに対して、開区間 $Q_i = (b_i, a_{i+1})$ は、2つの独立区間を接続していることから、これを「接続区間」と呼ぶ。
また、接続区間 $Q_i$ を負担する関数 $\Psi_i(x)$ を「接続関数」と呼ぶ。


## 本ライブラリにおける連結ガウス分布の構成方法<BR> - The way to construct a Connected Gaussian Distribution in this library

### 独立区間と正規分布の構成 - Constructing indipendent intervals and normal distributions

本ライブラリでは、以下のように独立区間を構成する。

+ 累積分布関数の経路が与えられたとき (通常は与えられる)、原則として、経路上の点は、1点のみからなる独立区間 $[a_i, a_i]$ とする。
ただし、以下の2つの場合を例外とする。

    + 区間 $[0, 1]$ の両端である点 0 および点 1 は、1点のみからなる独立区間 $[0, 0], [1, 1]$ とはせずに、
      少なくとも、他の1つの経路上の確率を含む区間 $[0, b_1], [a_n, 1] ( b_1 > 0, a_n < 1 )$ とする。

    + 2つの連続する経路の点が、同じ正規分布の累積分布関数上の点となる場合は、
      それら2つの点の確率を1つの独立区間に含める。ただし、独立区間の始点と終点は、いずれかの経路の確率 (または 0 か 1) とする。

独立区間を負担する正規分布は、以下のように構成する。

+ すべての正規分布の平均値は、経路で与えられた点 $( x, 0.5 )$ の $x$ の値とする。

+ 先頭の独立区間 $[0, b_1]$ に対しては、累積分布関数が $b_1$ とそのときのX座標 $x_{b_1}$ を通る正規分布を採用し、
それ以外の独立区間 $[a_i, b_i]$ に対しては、累積分布関数が $a_i$ とそのときのX座標 $x_{a_i}$ を通る正規分布を採用する。

ところで、正規分布 $N( \mu, \sigma^2 )$ の累積分布関数 $\Phi_{\mu, \sigma^2}( x )$ について、
X座標が平均値 $\mu$ から標準偏差 $\sigma$ の $n$ 倍離れた位置の確率 $\Phi_{\mu, \sigma^2}( \mu + n \sigma )$ の値は、
$\mu, \sigma$ の値に関係なく、任意の有限の $n$ の値 $( n \neq 0 )$ に対して一意に定まる
 (誤差関数 $\mathrm{erf}( x )$ を使って書いた正規分布の累積分布関数の式に
  $x = \mu + n \sigma$ を代入すると、 $\mu$ と $\sigma$ が消去できる。
  誤差関数の式はウィキペディアの [「正規分布」](https://ja.wikipedia.org/wiki/%E6%AD%A3%E8%A6%8F%E5%88%86%E5%B8%83) を参照)。

したがって、ある確率 $p \in [0, 1]$ が与えられたとき、 $\Phi_{\mu, \sigma^2}( x ) = p$ を満たす $x$ が、
平均値 $\mu$ から標準偏差 $\sigma$ の何倍離れているかという問題の解、
つまり、 $x = \mu + n \sigma$ と置いたとき、 $\Phi_{\mu, \sigma^2}( \mu + n \sigma ) = p$ となるような $n$ の値は、
$\mu, \sigma$ の値とは無関係であり、
標準正規分布 $N( 0, 1 )$ の累積分布関数 $\Phi( x )$ の逆関数を使うと、 $n = \Phi^{-1}( p )$ と表せる。

このことを利用すれば、正規分布の平均値 $\mu$ を1つの値に固定したとき、
$x = q ( \neq \mu )$ のときに確率 $p$ を取る正規分布の標準偏差 $\sigma$ は、
標準正規分布の累積分布関数 $\Phi( x )$ の逆関数を使って、

$$
\sigma = \dfrac{ q - \mu }{ \Phi^{-1}( p ) } \quad ( q \neq \mu, 0 < p < 1 )
$$

で求められることが分かる。
この式を使えば、平均値を固定したとき、累積分布関数がある1点を通るような正規分布の標準偏差が直に得られるので、
上に述べたような正規分布を容易に見つけることができる。
本ライブラリでは、この式を使って正規分布を構成し、連結ガウス分布の累積分布関数 $\Phi_{CGD}(x)$ の独立区間の部分を得る。


### 接続区間の構成 - Constructing connecting intervals

連結ガウス分布の累積分布関数 $\Phi_{CGD}(x)$ の接続区間の部分は、以下のように、type 1, type 2, type 3a, type 3b の4つの場合に分けて構成する。

以下、独立区間 $P_i = [a_i, b_i]$ を負担する正規分布 $N_i$ の標準偏差を $\sigma_i$ とし、
平均値はすべての $N_i$ に対して $\mu$ で固定する。 $N_i$ の累積分布関数を $\Phi_i( x )$ とする。
また、接続区間 $Q_i = ( b_i, a_{i + 1} )$ について、
両端の確率に対するX座標を $\beta_i, \alpha_{i+1}$ とする ( $\Phi_i( \beta_i ) = b_i, \Phi_{i + 1}( \alpha_{i+1} ) = a_{i + 1}$ )。

#### Type 1 - 接続区間 $Q_i = ( b_i, a_{i + 1} )$ が平均値 $\mu$ を含まない場合 その1
+ 接続区間 $Q_i$ の範囲が平均値 $\mu$ よりも小さく、標準偏差が $\sigma_i < \sigma_{i + 1}$ の場合
+ 接続区間 $Q_i$ の範囲が平均値 $\mu$ よりも大きく、標準偏差が $\sigma_i > \sigma_{i + 1}$ の場合

この場合はすべての $x \in (\beta_i, \alpha_{i+1})$ に対して、
累積分布関数 $\Phi_i( x ), \Phi_{i + 1}( x )$ の値が両方とも接続区間 $Q_i$ の範囲内に収まり、範囲外に出ることがない。
そのため、4つの場合の中で、最も自由に接続関数を構成することできる。

本ライブラリのバージョン 1.0.0 (または、バージョン 1.1.x のデフォルト) では、
以下のように、単純に $\Phi_i( x )$ と $\Phi_{i+1}( x )$ を混合する。

$$
\Psi_i( x ) = \dfrac{ \alpha_{i+1} - x }{ \alpha_{i+1} - \beta_i } \Phi_i( x ) + \dfrac{ x - \beta_i }{ \alpha_{i+1} - \beta_i } \Phi_{i+1}( x )
$$

このとき、確率密度関数 $f( x )$ は上の式の右辺を $x$ で微分すると得られ、

$$
f( x ) = \dfrac{ \alpha_{i+1} - x }{ \alpha_{i+1} - \beta_i } f_i( x ) + \dfrac{ x - \beta_i }{ \alpha_{i+1} - \beta_i } f_{i+1}( x )
            + \dfrac{ \Phi_{i+1}( x ) - \Phi_i( x ) }{ \alpha_{i+1} - \beta_i }
$$

となる。ただし、 $f_i( x ), f_{i+1}( x )$ は正規分布 $N_i,N_{i+1}$ の確率密度関数である。

このとき、連結ガウス分布の確率密度関数 $f( x )$ は $x = \beta_i, \alpha_{i+1}$ の2点で不連続になる。

それから、この式では、独立区間が $P_1 = [0, 0]$ または $P_n = [1, 1]$ のとき、係数の分母が $\pm\infty$ になるので、確率密度が計算できない。

この欠点を解消するために、
バージョン 1.1.x 以上では、コンストラクタ new() の引数に type1.type = 2 を指定することで、
接続関数が

$$
\Psi_i( x ) = \dfrac{ \Phi_i( \alpha_{i+1} ) - \Phi_i( x ) }{ \Phi_i( \alpha_{i+1} ) - b_i } \Phi_i( x )
                + \dfrac{ \Phi_{i+1}( x ) - \Phi_{i+1}( \beta_i ) }{ a_{i+1} - \Phi_{i+1}( \beta_i ) } \Phi_{i+1}( x )
$$

のように定義される。

このとき、確率密度関数 $f( x )$ は

$$
f( x ) = \dfrac{ \Phi_i( \alpha_{i+1} ) - 2\Phi_i( x ) }{ \Phi_i( \alpha_{i+1} ) - b_i } f_i( x )
                + \dfrac{ 2\Phi_{i+1}( x ) - \Phi_{i+1}( \beta_i ) }{ a_{i+1} - \Phi_{i+1}( \beta_i ) } f_{i+1}( x )
$$

となる。
このように定義すると、独立区間が $P_1 = [0, 0], P_2 = [1, 1]$ の2個のみの場合には、
全区間 $(-\infty, \infty)$ で連続な確率密度関数を作ることができる。

[CGD.need-nleqslv.R](https://github.com/Kimitsuna-Goblin/CGD/blob/master/CGD.need-nleqslv.R) を使うと、
連続な確率密度関数を作ることができる。

ただし、連続な確率密度関数を持つ連結ガウス分布の場合、
与えられた経路の点は、独立区間内ではなく、接続区間内に入る。
そのため、不連続な場合と比べて構成するのが難しく、経路の条件によっては、構成不可能な場合もある。
CGD.need-nleqslv.R では、連続な確率密度関数を持つ連結ガウス分布を構成するために、
非線形連立方程式ライブラリの nleqslv を使用している。

#### Type 2 - 接続区間 $Q_i = ( b_i, a_{i + 1} )$ が平均値 $\mu$ を含まない場合 その2
+ 接続区間 $Q_i$ の範囲が平均値 $\mu$ よりも小さく、標準偏差が $\sigma_i \geq \sigma_{i + 1}$ の場合
+ 接続区間 $Q_i$ の範囲が平均値 $\mu$ よりも大きく、標準偏差が $\sigma_i \leq \sigma_{i + 1}$ の場合

この場合は一部の $x \in (\beta_i, \alpha_{i+1})$ に対して、 $\Phi_i( x )$ と $\Phi_{i+1}( x )$ の値が
接続区間 $Q_i$ の範囲外に出てしまう。
そのため、接続関数 $\Psi_i( x )$ を構成する際は、値が $Q_i$ の範囲から出ないように注意する必要がある。
なお、範囲外に出るケースは、 $\Phi_i( \alpha_{i+1} ) > a_{i + 1}$ または $\Phi_{i+1}( \beta_i ) < b_i$ となる場合である。

本ライブラリでは、以下のように $\Psi_i( x )$ を構成する。

$$
\Psi_i( x ) =
\begin{cases}
\dfrac{1}{2} ( \Phi_i( x ) + b_i ) & \textrm{where} \quad \Phi_i( x ) < a_{i+1}, \quad \Phi_{i+1}( x ) < b_i \\
\dfrac{1}{2} ( a_{i+1} + b_i ) & \textrm{where} \quad \Phi_i( x ) \geq a_{i+1}, \quad \Phi_{i+1}( x ) < b_i \\
\dfrac{1}{2} ( \Phi_i( x ) + \Phi_{i+1}( x ) ) & \textrm{where} \quad \Phi_i( x ) < a_{i+1}, \quad \Phi_{i+1}( x ) \geq b_i \\
\dfrac{1}{2} ( a_{i+1} + \Phi_{i+1}( x ) ) & \textrm{where} \quad \Phi_i( x ) \geq a_{i+1}, \quad \Phi_{i+1}( x ) \geq b_i
\end{cases}
$$

このとき、確率密度関数 $f( x )$ は

$$
f( x ) =
\begin{cases}
\dfrac{1}{2} f_i( x ) & \textrm{where} \quad \Phi_i( x ) < a_{i+1}, \quad \Phi_{i+1}( x ) < b_i \\
0 & \textrm{where} \quad \Phi_i( x ) \geq a_{i+1}, \quad \Phi_{i+1}( x ) < b_i \\
\dfrac{1}{2} ( f_i( x ) + f_{i+1}( x ) ) & \textrm{where} \quad \Phi_i( x ) < a_{i+1}, \quad \Phi_{i+1}( x ) \geq b_i \\
\dfrac{1}{2} f_{i+1}( x ) & \textrm{where} \quad \Phi_i( x ) \geq a_{i+1}, \quad \Phi_{i+1}( x ) \geq b_i
\end{cases}
$$

となる。このとき、 $f( x )$ は多くの点で不連続になる。

#### Type 3a - 接続区間 $Q_i = ( b_i, a_{i + 1} )$ が平均値 $\mu$ を含む場合 その1
+ 接続区間 $Q_i$ の範囲に平均値 $\mu$ が含まれ、標準偏差が $\sigma_i < \sigma_{i + 1}$ の場合

この場合は $x \in (\beta_i, \alpha_{i+1})$ に対して、 $\Phi_{i+1}( x )$ は接続区間 $Q_i$ の範囲内に常に収まるが、
$\Phi_i( x )$ は $x > \mu$ の範囲の後半部分で、 $\Phi_i( x ) > a_{i+1}$ となり、 $Q_i$ の範囲外に出ることがある。
そのため、接続関数 $\Psi_i( x )$ は、その値が $Q_i$ の範囲から出ないように注意して、以下のように構成する。

$$
\Psi_i( x ) =
\begin{cases}
\Phi_i( x ) & \textrm{where} \quad x \leq \mu \\
\dfrac{1}{2} ( \Phi_i( x ) + \Phi_{i+1}( x ) ) & \textrm{where} \quad x > \mu, \quad \Phi_i( x ) < a_{i+1} \\
\dfrac{1}{2} ( a_{i+1} + \Phi_{i+1}( x ) ) & \textrm{where} \quad x > \mu, \quad \Phi_i( x ) \geq a_{i+1}
\end{cases}
$$

このとき、確率密度関数 $f( x )$ は

$$
f( x ) =
\begin{cases}
f_i( x ) & \textrm{where} \quad x \leq \mu \\
\dfrac{1}{2} ( f_i( x ) + f_{i+1}( x ) ) & \textrm{where} \quad x > \mu, \quad \Phi_i( x ) < a_{i+1} \\
\dfrac{1}{2} f_{i+1}( x ) & \textrm{where} \quad x > \mu, \quad \Phi_i( x ) \geq a_{i+1}
\end{cases}
$$

となる。

#### Type 3b - 接続区間 $Q_i = ( b_i, a_{i + 1} )$ が平均値 $\mu$ を含む場合 その2
+ 接続区間 $Q_i$ の範囲に平均値 $\mu$ が含まれ、標準偏差が $\sigma_i > \sigma_{i + 1}$ の場合

この場合は type 3a と対称的である。
すなわち、 $x \in (\beta_i, \alpha_{i+1})$ に対して、 $\Phi_i( x )$ は接続区間 $Q_i$ の範囲内に常に収まるが、
$\Phi_{i+1}( x )$ は $x < \mu$ の範囲の前半部分で、 $\Phi_{i+1}( x ) < b_i$ となり、 $Q_i$ の範囲外に出ることがある。
そのため、接続関数 $\Psi_i( x )$ は、その値が $Q_i$ の範囲から出ないように、以下のように構成する。

$$
\Psi_i( x ) =
\begin{cases}
\dfrac{1}{2} ( \Phi_i( x ) + b_i ) & \textrm{where} \quad x \leq \mu, \quad \Phi_{i+1}( x ) < b_i \\
\dfrac{1}{2} ( \Phi_i( x ) + \Phi_{i+1}( x ) ) & \textrm{where} \quad x \leq \mu, \quad \Phi_{i+1}( x ) \geq b_i \\
\Phi_{i+1}( x ) & \textrm{where} \quad x > \mu
\end{cases}
$$

このとき、確率密度関数 $f( x )$ は

$$
f( x ) =
\begin{cases}
\dfrac{1}{2} f_i( x ) & \textrm{where} \quad x \leq \mu, \quad \Phi_{i+1}( x ) < b_i \\
\dfrac{1}{2} ( f_i( x ) + f_{i+1}( x ) ) & \textrm{where} \quad x \leq \mu, \quad \Phi_{i+1}( x ) \geq b_i \\
f_{i+1}( x ) & \textrm{where} \quad x > \mu
\end{cases}
$$

となる。


## 参考資料 - References

### 連結ガウス分布 (Connected Gaussian Distribution) に関する資料

著作者が思いつきで考えて作った確率分布なので、特に外部資料はありません。

ある程度統計をやってたら誰でも思いつきそうな確率分布だし、どっかにあるんじゃね？という気がするので、見つけたら教えてください。


## ライセンス - License

[MIT](https://github.com/Kimitsuna-Goblin/extClark/blob/master/LICENSE)


## 著作者 - Author

[Kimitsuna-Goblin](https://github.com/Kimitsuna-Goblin) (浦 公統; Ura Kimitsuna)
