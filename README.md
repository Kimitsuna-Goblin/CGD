# 連結ガウス分布 - Connected Gaussian Distribution

連結ガウス分布とは、
直感的には、複数の正規分布 (ガウス分布) を区間で切断し、隙間を持たせてX軸方向に並べ、
累積分布関数が連続になるように連結させた確率分布です。
<p>
平均値が等しく分散が異なる正規分布を連結させることによって、
どんな経路 (離散的なX座標とその点における確率。あるいはクォンタイル) を指定しても、
その経路上のすべての点をゼロ誤差で通過するような累積分布関数を持つ確率分布を生成することができます。
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
とにかく、これはそういった分布を生成する、R言語のソースファイルです。


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
ということで、別にこのファイルは無くてもいいのですが、誰かの役に立つ関数があるかも知れないということもあって、おまけで公開しました。


### 連結ガウス分布の構成 - How to construct a connected Gaussian distribution

連結ガウス分布は次のようにして構成されます。

1. ある有限個の正規分布の集合 S<sub>N</sub> = { N<sub>1</sub>, N<sub>2</sub>, ..., N<sub>n</sub> } の要素である各正規分布 N<sub>i</sub> ( 1 ≦ i ≦ n ) に対して、
区間 [0, 1] の部分閉区間 I<sub>1</sub>, I<sub>2</sub>, ..., I<sub>n</sub> ⊂ [0, 1] をそれぞれ割り当てる。各部分閉区間は互いに重複しないようにする。
割り当てる区間は1点からなる区間 [x, x] (x ∈ [0, 1]) も許される。
なお、集合 S の要素には、N<sub>i</sub> = N<sub>j</sub> ( i ≠ j ) なる同一の分布があってもよい。

ただし、区間を割り当てる条件として、それらの区間同士がすべて互いに重複しないことだけでなく、
割り当てた区間 [a, b] に対する累積分布関数の根の区間
[φ<sup>-1</sup>(a), φ<sup>-1</sup>(b)] ( ⊂ [-∞, ∞]。ただし、φ は割り当てられる分布の累積分布関数。φ<sup>-1</sup> はその逆関数) も、
すべて互いに重複しないことを条件とする。
<br>
これらの、1つの正規分布に対して1つずつ独立的に割り当てられた閉区間を「独立区間」と呼ぶ。
そして、独立区間における連結ガウス分布の累積分布関数は、当該区間を割り当てられた正規分布の累積分布関数に一致するように定める。

2. 1.でそれぞれの正規分布に割り当てた独立区間に対して、隣接する2つの独立区間の間の区間を


### 証明 - Proof

この分布が確率分布であり、その累積分布関数が、与えられた有限個の経路上の点をすべて通過できることを示します。

#### この分布が確率分布であること

この分布は
+ 有限な n 個の正規分布の局所的な混合からなる
+ 最初の区間すなわち ( -∞, x<sub>1,1</sub> ) を負担する正規分布と、最後の区間 ( x<sub>n,1</sub>, ∞ ) を負担する正規分布はそれぞれ唯一つ ( N<sub>1</sub> と N<sub>n</sub> ) とする
+ 1個の正規分布が分布を負担する区間 (独立区間) および、2つの独立区間の間に存在する、2個以下の正規分布が分布を負担する区間 (接続区間) の和集合によって、
 (-∞, ∞) の範囲を包含する
+ 連続するいかなる独立区間と接続区間の境界点においても、累積分布関数は連続し、全領域で単調増加する
となるように構成した分布である。

そのように構成された累積分布関数 P は、定義域 -∞ < x < ∞ において、0 < P < 1 となり、単調増加で連続となるので、
もし、このような累積分布関数 P の構成が可能であれば、その分布は確率分布である。分布の構成可能性については、後の論述で示す。

#### 分布が構成可能であること

与えられた経路上の点の集合は W = { ( x<sub>i</sub>, p<sub>i</sub> ) ∈ R^2 | i ∈ { 1, 2, ..., n }, -∞ < x<sub>i</sub> < ∞, 0 < p<sub>i</sub> < 1 } と表されるが、
W を i < j ⇒ x<sub>i</sub> < x_j を満たす全順序集合としても一般性は失わないので、以後、W はそのように与えられたものとする。

ところで、正規分布の平均値 μ∈R を固定して、累積分布関数が任意の1点 ( x<sub>i</sub>, p<sub>i</sub> ) 上を通るような正規分布を構成するためには、
任意の正規分布 N(μ,σ^2) の累積分布関数の値は x - μ = nσ (n∈R) を満たす点 x において、μ,σの値によらず、
n に対して一定の値になることから、Φ を標準正規分布 N(0,1) の累積分布関数とすると、
標準偏差 σ<sub>i</sub> を σ<sub>i</sub> = ( x<sub>i</sub> - μ ) / Φ<sup>-1</sup>( p<sub>i</sub> ) と取ることによって構成できる。
なお、Φ<sup>-1</sup> は Φ の逆関数である。


## 参考資料 - References

### 連結ガウス分布 (Connected Gaussian Distribution) に関する資料

著作者が思いつきで考えて作った確率分布なので、特に外部資料はありません。


## ライセンス - License

[MIT](https://github.com/Kimitsuna-Goblin/extClark/blob/master/LICENSE)


## 著作者 - Author

[Kimitsuna-Goblin](https://github.com/Kimitsuna-Goblin) (浦 公統; Ura Kimitsuna)
