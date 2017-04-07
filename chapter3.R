#3章　行列と配列
#3.1 行列の作成

y<-matrix(c(1,2,3,4),nrow=2,ncol=2)
y

#y<-matrix(nrow=2,ncol=2)

#3.2 一般的な行列操作

y %*% y
3*y
y+y

y[,2]

#y[c(1,3),] <- matrix(c(1,1,8,12),nrow = 2)

#3.2　応用例：画像操作

library(pixmap)
mtrush1 <- read.pnm("mtrush1.pgm")
mtrush1
plot(mtrush1)
str(mtrush1)
mtrush1@grey[200,200]
locator()
mtrush2 <- mtrush1
mtrush2@grey[120:208,84:124] <-1
plot(mtrush2)

#imgのrowsとcolsの範囲にランダムノイズを追加する
#imgと戻り値はどちらもpixmapクラスのオブジェクトである
#パラメータqはノイズの重みを制御し、結果は元の画像の1-q倍とランダムノイズのq倍を加えたものになる
blurpart <- function(img,rows,cols,q){
  lrows <- length(rows)
  lcols <- length(cols)
  newimg <- img
  randomnoise <- matrix(nrow=lrows, ncol = lcols, runif(lrows*lcols))
  newimg@grey[rows,cols] <- (1-q) * img@grey[rows,cols] + q * randomnoise
  return(newimg)
}

#行列サイズが違うから足し算できない?(テキストの間違いp63)

mtrush3 <- blurpart(mtrush1,120:208,84:124,0.65)
plot(mtrush3)

x <- matrix(c(1:10),nrow = 5)
x

x[x[,2] >= 7,]

maekcov <- function(rho,n){
  m <- matrix(nrow = n,ncol = n)
  m <- ifelse(row(m) == col(m),1,rho)
  return(m)
}

#3.3 行列の行と列への関数の適用

#apply(m,dimcode,f,fargs)

z <- matrix(c(1:6),nrow = 3)
z

apply(z,2,mean)
apply(z,1,mean)

f <-function(x) x/c(2,8)
y <-apply(z, 1, f)
y
t(apply(z,1,f))

cbind(1,z)
rbind(1,z)

#正方対称行列dに対し、d[i,j](i!=j)の最小値とその最小値を実現する行/列を返す
#同順位に対する特別な方針はない

mind<- function(d){
  n <- nrow(d)
  #apply()のために行番号を特定する列を指定する
  dd <- cbind(d,1:n)
  wmins <- apply(dd[-n,], 1, imin)
  #winsは2x(n-1)になり、最初の行がインデックス、２番目の行が値になる
  i <- which.min(wmins[2,])
  j <- wmins[1,i]
  return(c(d[i,j],i,j))
}

#行xの最小値とその位置を見つける
imin <- function(x){
  lx <- length(x)
  i <- x[lx]　#元の行番号
  j <- which.min(x[(i+1):(lx-1)])
  k<- i+j
  return(c(k,x[k]))
} 
mind(z)
z

#その他のアルゴリズム

n <- nrow(z)
dd<-NULL
for(k in 1:n){
  a<-z[k,-k]
  dd<-c(dd,a)
}
dd

ddd<-matrix(dd,nrow=n)
ddd
x<-which.min(ddd)
(z[z == x])

#3.5　ベクトルと行列の違いに関する詳細

z<- matrix(1:8,nrow = 4)
z

length(z)
class(z)
attributes(z)
dim(z)

#3.6 予期せぬ次元削減の回避

r<-z[2,, drop=FALSE]
r

u<-c(1:3)
v<-as.matrix(u)
attributes(u)
attributes(v)

#3.7 行列の行と列の名前つけ

colnames(v)
colnames(v)<-c("a")
v[,"a"]

#高次元配列

firsttest <- matrix(c(51:60),nrow=5)
secondtest <- matrix(c(61:70),nrow = 5)

tests<-array(data=c(firsttest,secondtest),dim=c(5,2,2))
tests
attributes(tests)

