#9章

#シュミレーション

x <- rbinom(100000,5,0.5)
mean(x >= 4)

sum <- 0
nreps <- 10000
for (i in 1:nreps){
  xy <- rnorm(2) #2つのN(0,1)を生成する
  sum <- sum + max(xy)
}
print(sum/nreps)

emax <- function(nreps){
  x <- rnorm(2*nreps)
  maxxxy <- pmax(x[1:nreps],x[(nreps+1):(2*nreps)])
  return(mean(maxxxy))
}

#組み合わせシュミレーション

sim <- functinon(nreps) {
  commdata <- list() #３つの委員会に関するすべての情報を格納する
  commdata$countabsamecomm <- 0
  for ( rep in 1:nreps) {
    commdata$whosleft <- 1:20 #残りの選出対象
    commdata$numabchosen <- 0 #これまでに選ばれたAとBの数
    #委員会１を選び、AとBが一緒に勤めているかを調べる
    commdata<- choosecomm(commdata,5)
    #AかBがすでに選ばれている場合、べつの委員会を調べる必要はない
    if(commdata$numabchosen > 0) next
    #委員会2を選んで調べる
    commdata <- choosecomm(commdata,4)
    if(commdata$numabchosen > 0) next
    #委員会3を選んで調べる
    commdata <- choosecomm(commdata,3)
    
  }
  print(commdata$coutabsamecomm/nreps)
}
chosecomm <- function(comdata,comsize){
  #委員を選ぶ
  committee <- sample(comdat$whosleft,comsize)
  #AとBが何人選ばれているかを数える
  comdata$numabchosen <- length(intersect(1:2,committee))
  if(comdat$numabchosen ==2)
    comdat$countabsamecom <- comdat$countabsamecom +1
  #現在の選出対象から選ばれた委員を削除する
  comdat$whosleft <- setdiff(comdat$whosleft,committee)

  return(comdat)
  }