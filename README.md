# 連結ガウス分布 - Connected Gaussian Distribution

連結ガウス分布とは、
正規分布 (ガウス分布) を区間で切断し、隙間を持たせてX軸方向に並べ、累積分布関数のグラフを徐々に変化させて、連続になるように繋げた確率分布です。
あるいは、それを拡張した、正規分布の確率密度関数のグラフが、横方向や縦方向にグラデーション的に徐々に変化するような確率分布です。

連結ガウス分布を使えば、どんなクォンタイルを指定しても、それを極めて小さい誤差で再現する分布を構成できます。

平均値が等しく、分散が異なる正規分布を連結させることによって、
どんな経路 (X座標 (クォンタイル) とその点における確率) を指定しても、
累積分布関数がその経路上のすべての点を極めて小さい誤差 (理論的にはゼロ誤差) で通過する確率分布を構成できるからです。

……と書くと、なんとなく凄そうな気がするかも知れませんが、
連結ガウス分布の確率密度関数は、一般に不連続で、いびつで、自然界には絶対に存在しなさそうな分布になります。
連結する隙間の部分に、凄いひずみを作るからです。

ただし、クォンタイルが8点以下の場合は (たいていの場合は3点か5点、せいぜい7点でしょう)、確率密度関数が連続で滑らかな分布を構成することもできます。
たとえば設定 (type1.type, continuous / symmetric / v.grad) によって、

1. 2つの正規分布の平均 (type1.type = 1, continuous)、
2. 横方向 (X軸方向) にグラデーション的に確率密度関数が変化する分布 (type1.type = 2, continuous)、
3. 確率密度関数の中央が鋭利に尖ったり、逆に凹んだりしている、左右対称な分布 (type1.type = 2, symmetric)、
4. 縦方向 (Y軸方向) にグラデーション的に確率密度関数が変化する分布 (type1.type = 3, v.grad)
5. 縦横両方向にグラデーション的に確率密度関数が変化する分布 (type1.type = 4)

のような分布モデルが作れます。
これらの分布モデルは、正規分布よりも歪んだ分布や、裾野の広い分布のモデルなどに使えると思われます。

ともかく、これはそういった分布を構成する、R言語のソースファイルです。

## 注意 - Remark

連結ガウス分布は混合ガウス分布とは少し違います。
混合ガウス分布は、複雑な分布のデータを複数の正規分布の混合と考え、1つのデータを複数の正規分布に従うデータに分離することを目的とした、クラスタリングの手法と考えられます。
それに対して、連結ガウス分布は、複雑な分布のデータをあるがままの一団のデータとして捉えて、解析したり、他のデータと比較したりすることを目的と考えています。

## これは何に使える？ - What can the CGD model be used for?

連結ガウス分布は以下のような目的に使うために、このライブラリの作者が考案しました。

+ 不連続な連結ガウス分布

    正規分布に従わない、分布モデルが不明なデータのクォンタイルが与えられているときに、リサンプルしたランダムデータが欲しい場合

+ 連続な連結ガウス分布

    有限個の正規分布の混合ではなく、 (平均や) 分散が連続的に変動している、無限個の正規分布の混合モデルを考える場合

このライブラリの作者は、とある医療・人体関係のデータを扱っていて、上記のようなデータに遭遇したので、このようなモデルを考案しました。

## インストール - Installation

<pre>
# Install devtools from CRAN
install.packages( "devtools" )

# Then use devtools::install_github( "user/repository" ) to install cgd package from GitHub
devtools::install_github( "Kimitsuna-Goblin/cgd" )
</pre>

## ライブラリの使い方 - How to use library

<pre>
> library( cgd )    # ライブラリを読み込みます
> a <- CGD$new()    # 連結ガウス分布クラスのオブジェクトを生成します
>
>    # 連結分布の構成が既知の場合は (ほとんど無いと思いますが)
>    # new() の引数に
>    #       mean = 平均値
>    #       intervals = 連結分布の構成 (CGDInterval オブジェクトの list)
>    #       type1.type = 接続区間が type 1 の場合の計算方法
>    # を与えてください
>
> # set.waypoints() : 経路 (X座標 (あるいはクォンタイル) とその点における確率) を指定し、
> #                   指定されたすべての点を通過する累積分布関数を持つ連結ガウス分布を構成します
> #                   p = 0.5 (平均値) の点は必ず指定してください
> a$set.waypoints(
+   data.frame(
+     p = c( 0.1, 0.3, 0.5, 0.6, 0.7 ),
+     q = c( qnorm( c( 0.1, 0.3, 0.5, 0.6 ), 0, 1 ), 0.5 ) ) )
> NULL
>
> # d() : X座標を指定して、確率密度を返します
> dev.new(); plot.new()
> plot( seq( -3, 3, 0.01 ), a$d( seq( -3, 3, 0.01 ) ), type = "l" )
>
> # p() : X座標を指定して、確率を返します
> dev.new(); plot.new()
> plot( seq( -3, 3, 0.01 ), a$p( seq( -3, 3, 0.01 ) ), type = "l" )
>
> # q() : 確率を指定して、X座標 (クォンタイル) を返します
> #       確率が同一となるX座標が、ある区間内に無限に存在し、
> #       一意に定まらない場合は、該当区間の中点の座標を返します
> dev.new(); plot.new()
> plot( seq( 0, 1, 0.01 ), a$q( seq( 0, 1, 0.01 ) ), type = "l" )
>
> # r() : ランダムサンプルを生成します (高速化は全然やってません)
> #       でも、もしかしたらこれが一番役に立つのかも？
> dev.new(); plot.new()
> sample <- a$r( 1000 )
> hist( sample )
>
> a    # オブジェクトの内部構造を表示します

Reference class object of class "CGD"
Field "mean":
[1] 0
Field "intervals":
[[1]]
Reference class object of class "CGDInterval"
Field "mean":
[1] 0
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
Field "mean":
[1] 0
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

Field "type1.type":
[1] 1
Field "m.sd":
[1] -Inf
Field "m.lsd":
[1] -Inf
Field "m.usd":
[1] -Inf
> # 各要素の意味はマニュアルまたはソースファイルのコメントを参照してください
> #
> # m.sd, m.lsd, m.usd はこの連結ガウス分布の標準偏差 (全体, 下側, 上側) です
> # 計算に時間がかかるので、計算結果をフィールドに持たせています
> # これらのフィールドは sd() 等のメソッドを呼び出すと、計算結果と同じ値が設定されます
> # 次に sd() 等を呼んだときは、再計算せずに、フィールドの値を返します
> # 標準偏差を得るには sd() 等のメソッドを使い、フィールドの値は直接参照しないでください
</pre>


## ソースファイル - Source files

このソフトウェア (ライブラリ) は以下のファイルから成り立ちます。
<br>
なお、以下のソースファイルはすべて4文字タブで整形しています。

[CGD.R](https://github.com/Kimitsuna-Goblin/CGD/blob/master/R/CGD.R) - メインのR言語のソースファイルです。

[test.R](https://github.com/Kimitsuna-Goblin/CGD/blob/master/test.R) - いくつかのテストケースを書いた、テスト用ソースフィルです。


## 連結ガウス分布の構成方法 (一般の場合)<BR> - How to construct a Connected Gaussian Distribution (generally)

一般に、 (不連続な) 連結ガウス分布は次のようにして構成することができる。

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

以上、任意の経路の点を通過する連結ガウス分布の構成方法を述べたが、通過すべき経路の点が少なく3点程度の場合は、
非線形連立方程式を解くことによって、独立区間を $[0, 0]$ や $[1, 1]$ といった特別な点に置き、その間を結ぶ接続関数に経路の点を通過させることもできる。
その場合は、確率密度関数を全区間で連続あるいは $C^\infty$ 級にすることも可能である。

## 本ライブラリにおける連結ガウス分布の構成方法<BR> - The way to construct a Connected Gaussian Distribution in this library

### 独立区間と正規分布の構成 - Constructing indipendent intervals and normal distributions

不連続な連結ガウス分布の場合、本ライブラリでは、以下のように独立区間を構成する。

+ 累積分布関数の経路が与えられたとき (通常は与えられる)、原則として、経路上の点 $(x_i, a_i)$ の確率 $a_i$ に対し、1点のみからなる区間 $[a_i, a_i]$ を独立区間とする。
ただし、以下の2つの場合を例外とする。

    + 確率が 0 または 1 でない、最初の経路の点 $( x_1, b_1 )$ および 最後の点 $( x_n, a_n )$ $( 0 < b_1, a_n < 1 )$ の確率 $b_1, a_n$ に対しては、確率 0 および 1 を含む区間 $[0, b_1], [a_n, 1]$ を独立区間とする。

    + 2つの連続する経路の点が、同じ正規分布の累積分布関数上の点となり得る場合は、
      それら2つの点の確率を1つの独立区間に含める。ただし、独立区間の始点と終点は、いずれかの経路の確率 (または 0 か 1) とする。

独立区間を負担する正規分布は、以下のように構成する。

+ すべての正規分布の平均値は、経路で与えられた点 $( x, 0.5 )$ の $x$ の値とする。

+ 最初の独立区間 $[0, b_1]$ に対しては、累積分布関数が経路上の点 $( \beta_1, b_1 )$ を通るような正規分布を採用し、
それ以外の独立区間 $[a_i, b_i]$ に対しては、 累積分布関数が経路上の点 $( \alpha_i, a_i )$ を通るような正規分布を採用する (その累積分布関数は必ず点 $( \beta_i, b_i )$ も通る)。

ここで、ある平均値 $\mu$ の正規分布があって、点 $x = q$ で確率 $p$ を取るときに、その分布の標準偏差 $\sigma$ を $\mu, q, p$ を使って表すことを考える。
点 $x = q$ における、正規分布 $N( \mu, \sigma^2 )$ の確率 $p$ は

$$
p = \dfrac{1}{ \sqrt{ 2 \pi \sigma^2 } } \int_{-\infty}^{q} \exp( -\dfrac{ ( x - \mu )^2 }{ 2 \sigma^2 } ) dx
$$

で表される。この式の右辺を $t = \dfrac{ x - \mu }{ \sigma }$ と置いて変換すると、

$$
\begin{eqnarray}
p &=& \dfrac{1}{ \sqrt{ 2 \pi } } \int_{-\infty}^{ \dfrac{ q - \mu }{ \sigma } } \exp( -\dfrac{ t^2 }{ 2 } ) dt \\
  &=& \Phi( \dfrac{ q - \mu }{ \sigma } )
\end{eqnarray}
$$

となる。ただし、 $\Phi( x )$ は標準正規分布 $N( 0, 1 )$ の累積分布関数である。
$\Phi( x )$ の逆関数 $\Phi^{-1}( x )$ を使って、この式を変形すると、

$$
\sigma = \dfrac{ q - \mu }{ \Phi^{-1}( p ) } \quad ( q \neq \mu, \  0  < p < 1 \  \land \  p \neq 0.5 )
$$

という式が得られる。ここに、標準偏差 $\sigma$ を $\mu, q, p$ を使って表すことができた。
この式を使えば、正規分布の平均値と、その累積分布関数が通る任意の1点を与えれば、それらの条件を満たす正規分布の標準偏差が得られる。
したがって、上に述べたような正規分布は容易に見つけることができる。
本ライブラリでは、この式を使って正規分布を構成し、連結ガウス分布の累積分布関数 $\Phi_{CGD}(x)$ の独立区間の部分を得る。

なお、連続な連結ガウス分布の場合、独立区間は $[0, 0]$ や $[1, 1]$ など、特殊な1点のみからなる区間となり、それら以外の独立区間を作ることはできない。

### 接続区間の構成 - Constructing connecting intervals

連結ガウス分布の累積分布関数 $\Phi_{CGD}(x)$ の接続区間の部分は、以下のように、type 1, type 2, type 3a, type 3b の4つの場合に分けて構成する。

以下、独立区間 $P_i = [a_i, b_i]$ を負担する正規分布 $N_i$ の標準偏差を $\sigma_i$ と書き、平均値は $\mu_i$ と書く。
$N_i$ の累積分布関数を $\Phi_i( x )$ 、確率密度関数を $f_i( x )$ と書く。
なお、不連続な連結ガウス分布の場合、すべての $N_i$ の平均値は等しい値に取る。そして、その値はそのまま連結ガウス分布の平均値になる。その値を添字のない $\mu$ と書く。

また、接続区間 $Q_i = ( b_i, a_{i + 1} )$ について、
両端の確率に対するX座標を $\beta_i, \alpha_{i+1}$ とする (すなわち、 $\Phi_i( \beta_i ) = b_i, \Phi_{i + 1}( \alpha_{i+1} ) = a_{i + 1}$ とする)。

#### Type 1 - 接続区間 $Q_i = ( b_i, a_{i + 1} )$ が平均値 $\mu$ を含まない場合 その1
+ 接続区間 $Q_i$ の範囲が平均値 $\mu$ よりも小さく、標準偏差が $\sigma_i < \sigma_{i + 1}$ の場合
+ 接続区間 $Q_i$ の範囲が平均値 $\mu$ よりも大きく、標準偏差が $\sigma_i > \sigma_{i + 1}$ の場合

この場合はすべての $x \in (\beta_i, \alpha_{i+1})$ に対して、
累積分布関数 $\Phi_i( x ), \Phi_{i + 1}( x )$ の値が両方とも接続区間 $Q_i$ の範囲内に収まり、範囲外に出ることがない。
そのため、4つの場合の中で、最も自由に接続関数を構成することできる。

本ライブラリでは、各オプションに応じて、以下の表のように接続関数を構成するよう試みる。
不連続な連続ガウス分布は構成に失敗することはないが、連続な分布は構成に失敗することがある。


表中の式で、 $\Phi_i( x ), \Phi_{i+1}( x )$ は正規分布 $N_i,N_{i+1}$ の累積分布関数、 $f_i( x ), f_{i+1}( x )$ は同じく正規分布 $N_i,N_{i+1}$ の確率密度関数、 $\bar \Phi_i( x ) = ( \Phi_i( x ) + \Phi_{i+1}( x ) ) / 2$ 、 $\Phi^\ast_i(x)$ は正規分布 $N( \mu, ( \dfrac{ \sigma_i }{ \sqrt2 } )^2 )$ の累積分布関数 である。
添字のない $\mu$ は連結ガウス分布の平均値を表し、添字のある $\mu_i$ は構成要素の正規分布 $N_i( \mu_i, \sigma_i^2 )$ の平均値を表す。
なお、連続な分布の場合、接続関数 $\Psi( x )$ はそのまま累積分布関数となるので、表の項目にカッコ書きしておいた。

| オプション  | 接続関数 (累積分布関数) $\Psi_i(x)$ ・確率密度関数 $g_i(x)$ | 適用可能な経路の点の個数 | 独立区間 | 確率密度関数の連続性 | 実装ver |
| :--------: | :--------------------------------------- | :------: | :----: | :----------------: | :-----: |
| type1.type = 1 | $\Psi_i( x ) = \dfrac{ \alpha_{i+1} - x }{ \alpha_{i+1} - \beta_i } \Phi_i( x ) + \dfrac{ x - \beta_i }{ \alpha_{i+1} - \beta_i } \Phi_{i+1}( x )$ <br> $g_i( x ) = \dfrac{ \alpha_{i+1} - x }{ \alpha_{i+1} - \beta_i } f_i( x ) + \dfrac{ x - \beta_i }{ \alpha_{i+1} - \beta_i } f_{i+1}( x ) + \dfrac{ \Phi_{i+1}( x ) - \Phi_i( x ) }{ \alpha_{i+1} - \beta_i }$ | 任意 | 任意 | 不連続 | 1.0.0 |
| type1.type = 2  | $\Psi_i( x ) = \dfrac{ \bar \Phi_i( \alpha_{i+1} ) - \bar \Phi_i( x ) }{ \bar \Phi_i( \alpha_{i+1} ) - \bar \Phi_i( \beta_i ) } \Phi_i( x ) + \dfrac{ \bar \Phi_i( x ) - \bar \Phi_i( \beta_i ) }{ \bar \Phi_i( \alpha_{i+1} ) - \bar \Phi_i( \beta_i ) } \Phi_{i+1}( x )$ <br> $g_i( x ) = \dfrac{ \bar \Phi_i( \alpha_{i+1} ) -\Phi_i( x ) }{ \bar \Phi_i( \alpha_{i+1} ) - \bar \Phi_i( \beta_i ) } f_i( x ) + \dfrac{ \Phi_{i+1}( x ) - \bar \Phi_i( \beta_i ) }{ \bar \Phi_i( \alpha_{i+1} ) - \bar \Phi_i( \beta_i ) } f_{i+1}( x )$ | 任意 | 任意 | 不連続 | 1.1.0 |
| type1.type = 3 | $\Psi( x ) = \Psi_1( x ) + \Psi_2( x ) + \Psi_3( x )$ <br> $\qquad \Psi_1( x ) = \mathrm{ min }( \Phi_1( x ) - \dfrac{1}{ \sqrt{2} } \Phi^\ast_1( x ), \  \dfrac{2 - \sqrt{2}}{4} )$ <br> $\qquad \Psi_2( x ) = \dfrac{1}{ \sqrt{2} } \Phi^\ast_2( x )$ <br> $\qquad \Psi_3( x ) = \mathrm{ max }( 0, \  \Phi_3( x ) - \dfrac{1}{ \sqrt{2} } \Phi^\ast_3( x ) - \dfrac{2 - \sqrt{2}}{4} )$ <br> $g( x ) = g_1( x ) + g_2( x ) + g_3( x )$ <br> $\qquad g_1( x ) = ( 1 - \dfrac{ f_1( x ) }{ f_1( \mu_1 ) } ) f_1( x ) \ \  ( x \leq \mu_1 ), \quad 0 \ \  ( x > \mu_1 )$ <br> $\qquad g_2( x ) = \dfrac{ f_2( x ) }{ f_2( \mu_2 ) } f_2( x )$ <br> $\qquad g_3( x ) = 0 \ \  ( x < \mu_3 ), \quad ( 1 - \dfrac{ f_3( x ) }{ f_3( \mu_3 ) } ) f_3( x ) \ \  ( x \geq \mu_3 )$ | $( \mu, 0.5 )$ を含む3～5点<br>または $( \mu, 0.5 )$ を含まない4点または6点 | $[0, 0]$, $[0.5, 0.5]$, $[1, 1]$, $[0, 0.5]$, $[0.5, 1]$, $[0, 1]$ のいずれか | 連続<br>( $C^1$ 級) | 1.3.5 |
| type1.type = 1 <br> continuous = TRUE or symmetric = TRUE | $\Psi( x ) = \dfrac{1}{2} ( \Phi_1( x ) + \Phi_2( x ) )$ <br> $g( x ) = \dfrac{1}{2} ( f_1( x ) + f_2( x ) )$ | $( \mu, 0.5 )$ を含む3点 | $[0, 0]$, $[1, 1]$ の2点 | 連続<br>( $C^\infty$ 級) | 1.2.0 |
| type1.type = 2 <br> continuous = TRUE <br> (横方向グラデーション) | $\Psi( x ) = \Phi_1( x ) - \dfrac{1}{2} \Phi_1( x )^2 + \dfrac{1}{2} \Phi_2( x )^2$ <br> $g( x ) = ( 1 - \Phi_1( x ) )f_1( x ) + \Phi_2( x ) f_2( x )$ | $( \mu, 0.5 )$ を含む3点<br>または $( \mu, 0.5 )$ を含まない4点 | $[0, 0]$, $[1, 1]$ の2点 | 連続<br>( $C^\infty$ 級) | 1.3.0 |
| type1.type = 2 <br> symmetric = TRUE | $\Psi_1( x ) = \Phi_1( x ) - \Phi_1( x )^2 + \Phi_2( x )^2 \qquad \qquad ( x \leq \mu )$ <br> $\Psi_2( x ) = 1 - \Psi_1( 2\mu - x ) \qquad \qquad \qquad \qquad \  ( x > \mu )$ <br> $g_1( x ) = ( 1 - 2\Phi_1( x ) ) f_1( x ) + 2\Phi_2( x ) f_2( x ) \quad ( x \leq \mu )$ <br> $g_2( x ) = g_1( 2\mu - x ) \qquad \qquad \qquad \qquad \qquad \  \  \  ( x > \mu )$ | $( \mu, 0.5 )$ を含む3点 | $[0, 0]$, $[0.5, 0.5]$, $[1, 1]$ の3点 | 連続 | 1.2.0 |
| type1.type = 3 <br> symmetric = TRUE <br> (縦方向グラデーション) <br> (廃止 → v.grad に変更) |  $\Psi( x ) = \Phi_1( x ) - \dfrac{1}{ \sqrt{2} } \Phi^\ast_1( x ) + \dfrac{1}{ \sqrt{2} } \Phi^\ast_2( x )$ <br> $g( x ) = ( 1 - \dfrac{ f_1( x ) }{ f_1( \mu ) } ) f_1( x ) + \dfrac{ f_2( x ) }{ f_2( \mu ) } f_2( x )$ | $( \mu, 0.5 )$ を含む3点 | $[0, 0]$, $[0.5, 0.5]$, $[1, 1]$ の3点 | 連続<br>( $C^\infty$ 級) | 1.2.0 (1.3.8 にて廃止) |
| type1.type = 3 <br> v.grad = TRUE <br> (縦方向グラデーション) |  $\Psi( x ) = \Phi_1( x ) - \dfrac{1}{ \sqrt{2} } \Phi^\ast_1( x ) + \dfrac{1}{ \sqrt{2} } \Phi^\ast_2( x )$ <br> $g( x ) = ( 1 - \dfrac{ f_1( x ) }{ f_1( \mu_1 ) } ) f_1( x ) + \dfrac{ f_2( x ) }{ f_2( \mu_2 ) } f_2( x )$ | 任意の3点<br>または $( \mu, 0.5 )$ を含まない4点 | $[0, 0]$, $[1, 1]$ の2点<br>あるいは $[0.5, 0.5]$ を加えた3点 | 連続<br>( $C^\infty$ 級) | 1.3.8 |
| type1.type = 4 <br> (縦横グラデーション) |  $\Psi( x ) = \Psi_1( x ) - \dfrac{1}{2} \Psi_1( x )^2 + \dfrac{1}{2} \Psi_2( x )^2$ <br> $\qquad \Psi_1( x ) = \Phi_{1, 1}( x ) - \dfrac{1}{ \sqrt{2} } \Phi^\ast_{1, 1}( x ) + \dfrac{1}{ \sqrt{2} } \Phi^\ast_{1, 2}( x )$ <br> $\qquad \Psi_2( x ) = \Phi_{2, 1}( x ) - \dfrac{1}{ \sqrt{2}  } \Phi^\ast_{2, 1}( x ) + \dfrac{1}{ \sqrt{2}  } \Phi^\ast_{2, 2}( x )$ <br> $g( x ) = ( 1 - \Psi_1( x ) ) g_1( x ) + \Psi_2( x ) g_2( x )$ <br> $\qquad g_1( x ) = ( 1 - \dfrac{ f_{1, 1}( x ) }{ f_{1, 1}( \mu_{1, 1} ) } ) f_{1, 1}( x ) + \dfrac{ f_{1, 2}( x ) }{ f_{1, 2}( \mu_{1, 2} ) } f_{1, 2}( x )$ <br> $\qquad g_2 ( x ) = ( 1 - \dfrac{ f_{2, 1}( x ) }{ f_{2, 1}( \mu_{2, 1} ) } ) f_{2, 1}( x ) + \dfrac{ f_{2, 2}( x ) }{ f_{2, 2}( \mu_{2, 2} ) } f_{2, 2}( x )$ | $( \mu, 0.5 )$ を含む5点または7点<br>または $( \mu, 0.5 )$ を含まない6点または8点 | $[0, 0]$, $[1, 1]$ の2点 | 連続<br>( $C^\infty$ 級) | 1.4.0 |

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

このとき、確率密度関数 $g_i( x )$ は

$$
g_i( x ) =
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

このとき、確率密度関数 $g_i( x )$ は

$$
g_i( x ) =
\begin{cases}
f_i( x ) & \textrm{where} \quad x \leq \mu \\
\dfrac{1}{2} ( f_i( x ) + f_{i+1}( x ) ) & \textrm{where} \quad x > \mu, \quad \Phi_i( x ) < a_{i+1} \\
\dfrac{1}{2} f_{i+1}( x ) & \textrm{where} \quad x > \mu, \quad \Phi_i( x ) \geq a_{i+1}
\end{cases}
$$

となる。

type1.type = 1, continuous = TRUE の設定のとき、確率分布が2つの正規分布の平均になるのは、この type 3 の計算を用いるためである。

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

このとき、確率密度関数 $g_i( x )$ は

$$
g_i( x ) =
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
