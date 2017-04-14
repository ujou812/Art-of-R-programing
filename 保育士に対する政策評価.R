#保育士の給与労働者数についての実証実験

#データはH23とH26の給与と労働者数に関する全職種についてのデータの差
#したがって、実証方法はDID

#職種名を含んだデータフレームの作成

names <- names$names

num <- c(1:129)
work <- data.frame(num,names,research,dummy0,dummy1,dummy2)

#保育士ダミー
dummy0 <- NULL
for(i in 1:129){
  if(i == 21) dummy0 <- c(dummy0,1)
  else dummy0 <- c(dummy0,0)
}
dummy0

#医療関係ダミー
dummy1 <- NULL
for(i in 1:129){
  if(i >= 9 && i <= 20) dummy1 <- c(dummy1,1)
  else dummy1 <- c(dummy1,0)
}
dummy1

dummy2 <- NULL
for(i in 1:129){
  if(i >= 22 && i <= 24) dummy2 <- c(dummy2,1)
  else dummy2 <- c(dummy2,0)
}
dummy2


works0 <- data.frame(works0)
works <- data.frame(wokrs,lnum_dif)


head(work)
work[22,]

show(work)

summary(lm(y_dif ~ dummy0 + dummy1 + dummy2,data = work))
summary(lm(lnum_dif ~ dummy0 + dummy1 + dummy2,data = work))

