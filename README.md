# 連結ガウス分布 - Connected Gaussian Distribution

連結ガウス分布とは、
直感的には、複数の正規分布 (ガウス分布) を区間で切断し、隙間を持たせてX軸方向に並べ、
累積分布関数が連続になるように連結させた確率分布です。
<p>
平均値が等しく分散が異なる正規分布を連結させることによって、
どんな経路 (X座標とその点における確率。あるいはクォンタイル) を指定しても、
その経路上のすべての点をゼロ誤差で通過するような累積分布関数を持つ確率分布を生成することができます。
つまり、どんなクォンタイルを満たす分布でも、すぐに生成できます。
<p>
……と書くと、なんとなく凄そうな気がするかも知れませんが、
生成される分布の確率密度関数は、一般に不連続で、いびつで、自然界には絶対に存在しなさそうな分布になります。
連結する隙間の部分に、凄いひずみを作るからです。
隙間の幅はいくらでも小さくすることができますが、幅を小さくすればするほど、ひずみは大きくなります。
<p>
本ライブラリでは、分布を生成する際に、できる限りひずみが小さくなるようにしていますが、それでも凄いひずみになります。
<p>
とはいえ、生成される分布は、確率分布としての要件を満たしますし、(局所的な) 正規分布の混合だけで実現していますので、
もしかしたら、何かの役に立つかも知れません。
<p>
確率密度関数がいびつな形でも、ランダムサンプルを取得してヒストグラムを描けば、かなり滑らかになるので、
たとえば、クォンタイルが与えられていて、それを近似する確率分布のグラフが欲しいときなどは、
ランダムサンプルを使ってヒストグラムを描けば、わりと良い感じのグラフが描けるだろうと思います。
<p>
ともかく、これはそういった分布を生成する、R言語のソースファイルです。


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


## 説明 - Description

### ファイル - Files

このソフトウェア (ライブラリ) は以下のファイルから成り立ちます。
<br>
なお、以下のソースファイルはすべて4文字タブで整形しています。

[CGD.stand-alone.R](https://github.com/Kimitsuna-Goblin/CGD/blob/master/CGD.stand-alone.R) - メインのR言語のソースファイルです。単独で動きます。

[common.R](https://github.com/Kimitsuna-Goblin/CGD/blob/master/common.R) - おまけの自作ライブラリ (抜粋公開版) です。
<br>
著作者のローカルPC環境では、CGD.stand-alone.R の元になった GCD.R は、この common.R を参照していました。
ですが、common.R の中にある GCD.R に必要だった関数は、すべて CGD.stand-alone.R にコピーしました。
<br>
なので、このファイルは別に無くても動きます。
<br>
ということで、別に common.R は無くてもいいのですが、誰かの役に立つ関数があるかも知れないということもあって、おまけで公開しました。


### 連結ガウス分布の構成方法 (一般の場合) - How to construct a Connected Gaussian Distribution (generally)

連結ガウス分布は次のようにして構成されます。

1. まず、区間 $[0, 1]$ の部分閉区間の有限集合 $I$ を、その要素の閉区間同士が互いに重なり合うことがないように用意する。
ただし、$I$ の要素のどれか1つに必ず 0 が含まれ、どれか1つに必ず 1 が含まれるようにする。

つまり、

$$
I = \left\lbrace I_i \subset [0, 1] | 1 \leq i \leq n, I_i=[a_i, b_i] ( 0 \leq a_i \leq b_i \leq 1 ), a_1=0, b_n=1,
      i \neq j \Rightarrow I_i \cap I_j = \varnothing \right\rbrace
$$

を満たすような、閉区間の集合 $I$ を用意する (便宜上、$I$ の先頭の要素 $I_1$ に 0 が含まれ、最後の要素 $I_n$ に 1 が含まれるものとする)。
その要素 $I_i$ は、1点のみからなる区間 $[a_i, a_i] ( 0 \leq a_i \leq 1 )$ でもよい。
また、それぞれの閉区間 $I_i$ は昇順に並んでいることが望ましい。
すなわち、$1 \leq i \leq n - 1$ に対して、$b_i ＜ a_{i+1}$ となるように構成しておくことが望ましい
 (以下、$I$ はそのように構成されているものとする)。

ここで、累積分布関数の経路 (クォンタイル。以下、経路と言う) $W = \left\lbrace ( x_j, p_j ) \in ( \mathbb{R}, [0, 1] ) | 1 \leq j \leq m \right\rbrace$ が与えられているときは、
その経路にあるすべての確率 $p_j ( 1 \leq j \leq m )$ の値が、いずれかの $I_i$ に必ず含まれるように取る
 (便宜上、確率 $\left\lbrace p_j \right\rbrace$ は昇順に並んでいるものとする)。
このとき、$n = m$ として、1つの閉区間 $I_i ( 1 \leq i \leq n )$ に確率 $p_i$ が1つだけ含まれるように $I$ を構成すれば、後の工程も容易になる。
ただし、後の工程が実現可能であれば、1つの閉区間 $I_i$ に複数の確率 $p_j, p_{j+1}, p_{j+2}, \cdots$ が含まれるように構成してもよい。


2. 閉区間の集合 $I$ の要素数と等しい、$n$個の正規分布からなる集合 $N = \left\lbrace N_1, N_2, \cdots, N_n \right\rbrace$ を用意して、
その要素の正規分布 $N_i ( 1 \leq i \leq n )$ に、
閉区間 $I_i$ をそれぞれ1つずつ割り当てる。

このとき、集合 $N$ の要素には、以下の2つの条件を課す。

+ 任意の2つの $N_i, N_j ( i \neq j )$ について、$N_i, N_j$ に割り当てられた閉区間 $I_i, I_j$ の範囲においては、
$N_i, N_j$ の累積分布関数 $\Phi_i( x ), \Phi_j( x )$ の定義域が互いに重ならないこと。
すなわち、
$\left\lbrace \Phi_i^{-1}( p \in I_i ) \right\rbrace ∩ \left\lbrace \Phi_j^{-1}( p \in I_j ) \right\rbrace = \varnothing$
であること。
+ 経路 $W = \left\lbrace ( x_j, p_j ) \right\rbrace$ が与えられており、
経路の確率 $p_j$ が閉区間 $I_i$ に含まれているときは、
その区間が割り当てられている確率分布 $N_i$ の累積分布関数 $\Phi_i$ が、必ずその経路上の点を通過すること。
すなわち、$p_j \in I_i ⇒ \Phi_i( x_j ) = p_j$ が成り立つこと。

なお、集合 $N$ の要素の中には、$N_i = N_j ( i \neq j )$ なる同一の分布が存在してもよい。

閉区間 $I_i$ がある程度の幅を持ち、$I_i$ の中に複数個の経路上の確率 $p_j, p_{j+1}, p_{j+2}, ...$ を含んでいる場合は、
特別な場合を除いて、一般に、上の2つの条件を満たす正規分布の集合 $N$ を構成することはできない。
しかし、すべての閉区間 $I_i$ が高々1個の経路上の確率 $p_j$ しか含まない場合、特に、すべての閉区間 $I_i$ が1点のみからなる場合は、
2つの条件を満たす正規分布の集合 $N$ を必ず構成することができる (自明であろう)。

なお、正規分布の平均値 $\mu$ を1つの値に固定して、ある点 $x_j (\in \mathbb{R})$ における確率 $p_j (\in [0, 1])$ が与えられたとき、
累積分布関数が $\Phi( x_j ) = p_j, \Phi( \mu ) = 0.5$ を満たすような正規分布は一意に定まるので、
特に、すべての閉区間 $I_i$ が1点のみからなるような場合は、
そのような正規分布を探して集めることによって、平均値が等しい正規分布のみで集合 $N$ を構成することができる。

さて、これまでの工程によって、与えられた経路上の点をすべて通過する、累積分布関数の不連続な断片が構成できたので、あとは、それらの断片を連続になるように接続すればよい。
そこで、最終的な工程として、

3. 隣り合う2つの閉区間 $I_i = [a_i, b_i], I_{i+1} = [a_{i+1}, b_{i+1}]$ に対し、
2つの累積分布関数 $\Phi_i( x )$ と $\Phi_{i+1}( x )$ を用いて、
$\Psi_i( b_i ) = \Phi_i( b_i ), \Psi_i( a_{i+1} ) = \Phi_{i+1}( a_{i+1} )$ を満たすような、
区間 $[b_i, a_{i+1}]$ において連続で単調増加する関数 $\Psi_i(x) = f_i( x ) \Phi_i( x ) + g_i( x ) \Phi_{i+1}( x )$ を適当に定める $( 1 \leq i \leq n - 1 )$。
このとき、連続関数 $f_i( x ), g_i( x )$ はできるだけシンプルな関数であることが望ましい。

以上のように関数 $\Phi_i( x ), \Psi_i( x )$ を定めたとき、
関数

$$
\Phi_{CGD}(x)=
\begin{cases}
\Phi_i(x) & \textrm{where } x \in [a_i, b_i], \quad i = 1, \cdots, n \\
\Psi_i(x) & \textrm{where } x \in (b_i, a_{i+1}), \quad i = 1, \cdots, n - 1
\end{cases}
$$

は確率分布の累積分布関数としての要件を満たす。
以上のように構成した累積分布関数を持つ確率分布を「連結ガウス分布」と呼ぶ。

上の式に出てくる閉区間と開区間の呼称について、
閉区間 $I_i =  [a_i, b_i]$ は、1つの正規分布の累積分布関数 $\Phi_i(x)$ が単独で、
連結ガウス分布の累積分布関数 $\Phi_{CGD}(x)$ を独立的に負担している区間であることから、これを「独立区間」と呼ぶ。
それに対して、関数 $\Psi_i(x) ( 1 \leq i \leq n - 1 )$ が累積分布関数 $\Phi_{CGD}(x)$ を負担している開区間 $(b_i, a_{i+1})$ は、
2つの独立区間を接続していることから、これを「接続区間」と呼ぶ。


### 本ライブラリにおける連結ガウス分布の構成方法 - The way to construct a Connected Gaussian Distribution in this library

#### 独立区間と正規分布の構成 - Constructing indipendent intervals and normal distributions



#### 接続区間の構成 - Constructing connecting intervals


## 参考資料 - References

### 連結ガウス分布 (Connected Gaussian Distribution) に関する資料

著作者が思いつきで考えて作った確率分布なので、特に外部資料はありません。


## ライセンス - License

[MIT](https://github.com/Kimitsuna-Goblin/extClark/blob/master/LICENSE)


## 著作者 - Author

[Kimitsuna-Goblin](https://github.com/Kimitsuna-Goblin) (浦 公統; Ura Kimitsuna)
