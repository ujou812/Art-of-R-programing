#収入と効用度を定義する
i <- 150
a <- 1
b <- 1
c <- 1

#最大値を初期化
max <- 0
Max <- 0
Max_k <- 0

x_max <- i/180

#以下計算プログラム

for(i in 0:x_max){
  y_max <- (1000-i*180)/150
  for(j in 0:y_max){
    for( k in 0:z_max){
      max <- a*(0.9**i) + b*(0.9**j) + c*(0.9**k)
      if(max >= Max_k) {
        Max_k <- max
        }
    }
  }
  print(Max_k)
}

