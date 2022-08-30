source( "CGD.need-nleqslv.R" )	 # ソースファイルを読み込みます
a1 <- CGD$new( type1.type = 2 )	  # 連結ガウス分布クラスのオブジェクトを生成します

a1$set.waypoints(
  data.frame(
	p = c( 0.1, 0.5, 0.6 ),
	q = c( qnorm( 0.1, 0, 1 ), 0, qnorm( 0.6, 0, 1.2 ) ) ), continuous = TRUE )
# continuous = TRUE は確率密度関数が全区間で連続で、左右非対称な分布を作成するよう試みます
# 経路は p = 0.5 の点を含め、3点指定する必要があります

dev.new(); plot.new()
plot( seq( -3, 3, 0.01 ), a1$d( seq( -3, 3, 0.01 ) ), type = "l" )
# d() : X座標を指定して、確率密度を返します

a1$p( qnorm( 0.1, 0, 1 ) )
a1$p( qnorm( 0.6, 0, 1.2 ) )
dev.new(); plot.new()
plot( seq( -3, 3, 0.01 ), a1$p( seq( -3, 3, 0.01 ) ), type = "l" )
# p() : X座標を指定して、確率を返します

a1$q( 0.1 )
a1$q( 0.6 )
dev.new(); plot.new()
plot( seq( 0, 1, 0.01 ), a1$q( seq( 0, 1, 0.01 ) ), type = "l" )
# q() : 確率を指定して、X座標 (クォンタイル) を返します
#		確率が同一となるX座標が、ある区間内に無限に存在し、
#		一意に定まらない場合は、該当区間の中点の座標を返します

dev.new(); plot.new()
sample <- a1$r( 1000 )
hist( sample )
# r() : ランダムサンプルを生成します (高速化は全然やってません)
#		でも、もしかしたらこれが一番役に立つのかも？

a1	 # オブジェクトの内部構造を表示します
