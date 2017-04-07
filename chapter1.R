#1章 Rを始める

demo() 
help()
help.start()
q()

#Y=|X| ; X ~N(0,1) の平均を求める
mean(abs(rnorm(100)))

rnorm(10)
source("test.R")

pdf("xh.pdf")　#グラフ出力ファイルの設定
hist(rnorm(100)) #1oo個のN(0,1)変量を生成してヒストグラムを描画する
dev.off() #グラフ出力ファイルを閉じる

#1.2最初のRセッション

x<-c(1,2,4)
q <-c(x,x,8)

x[3]
x[2:3]

mean(x)
sd(x)

y<-mean(x)

data(Nile)
mean(Nile)
sd(Nile)
hist(Nile)

q()

#1.3　関数の概要

oddcount <- function(x){
  k<-0 #Kに0を割りあてる
  for(n in x){
    if( n %% 2 ==1) k <- k+1 #%%は剰余演算子
  }
  return(k)
}

oddcount(c(1,3,5))
oddcount(c(1,2,3,7,9))

f<-function(x) return(x+y)
y<-3
f(5)

#g<- function(x,y=2,z=T){}

#1.4　重要なRデータ構造の概説

x<-c(5,12,13)
length(x)
mode(x)
y<-"abc"
length(y)
mode(y)
z<-c("abc","29 88")
length(z)
mode(z)

u<-paste("abc","de","f") #文字列を連結する
u
v<-strsplit(u," ") #空白で文字列を分割する
v

m<- rbind(c(1,4),c(2,2))
m
m %*% c(1,1) #行列の乗算演算子

m[1,2]
m[2,2]
m[1,]
m[,2]

x<-list(u=2,v="abc")
$u
$v
x$u

hn <- hist(Nile)
print(hn)
str(hn)

d<-data.frame(list=c("Jack","Jill"),ages=c(12,10))
d
d$ages

print(hn)
options(editor = "/Users/Ryunosuke/Desktop/R")

#1.6 起動と終了
#options(editor = "/Users/Ryunosuke/Desktop/R")
getwd()
setwd("/Users/Ryunosuke/Desktop/R")
setwd("../../")

?seq #help関数


