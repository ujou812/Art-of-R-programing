#2章　ベクトル

#2.1 ベクトル要素の追加と削除

x<-c(88,5,12,13)
x<-c(x[1:3],168,x[4])
x
length(x)

#2.2 宣言

y <- vector(length=2)
y[1] <-5
y[2] <-12

#y <- c(5,12) も同様

y <- "abc"


#2.3 リサイクル

c(1,2,4) + c(6,0,9,20,22)

#行列は列ごとに格納された長いベクトル

#2.4 一般的なベクトル操作

y<-c(1.2,3.9,0.4,0.12)
y[c(1,3)]
y[2:3]

z <- c(5,12,13)
z[-1]
z[-1:-2]

seq(from=12, to=30, by=3)
seq(from=1.1, to=2, length=10)

x<- c(5,12,14)
seq(x)
x<- NULL
seq(x)

findruns <- function(x,k){
  n<-length(x)
  runs <- NULL
  for(i in 1:(n-k+1)){
    if (all(x[i:(i+k-1)]==1)) runs <- c(runs,i)
  }
  return(runs)
}

findruns1 <- function(x,k){
  n<-length(x)
  runs <- vector(length=n)
  count<-0
  for(i in 1:(n-k+1)){
    if(all(x[i:(i+k-1)]==1)){
    count<- count+1
    runs[count]<-i
    }
  }
  if(count >0){
    runs <- runs[1:count]
    
  }else runs <- NULL
  return(runs)
}

y<-c(1,0,0,1,1,1,0,1,1)

findruns(y,2)
findruns1(y,2)

preda <- function(x,k){
  n<- length(x)
  k2 <- k/2
  #ベクトルpredには予測値が入っている。
  pred<- vector(length = n-k)
  for(i in 1:(n-k)){
    if(sum(x[i:(i+(k-1))]) >= k2) pred[i] <-1
    else pred[i] <-0
  }
  return(mean(abs(pred-x[(k+1):n])))
}

predb <- function(x,k){
  n <- length(x)
  k2 <- k/2
  pred <- vector(lenth= n-k)
  sm <- sum(x[1:k])
  if(sm >= k2)pred[1] <- 1 
  else pred[1] <- 0
  if (n-k >= 2){
    for(i in 2:(n-k)){
      sm <- sm + x[i+k-1] - x[i-1]　#合計値に新しい値を足して更新する
      if(sm >= k2) pred[i] <- 1 
      else pred[i] <- 0
    }
  }
  return(mean(abs(pred-x[(k+1):n])))
}

#cumsum関数
y <- c(5,2,-3,8)
cumsum(y)

predc <- function(x,k){
  n <- length(x)
  k2 <- k/2
  #ベクトルpredには予測値が入っている。
  pred <- vector(length = n-k){
    csx <- c(0,cumsum(x))
    for(i in 1:(n-k)){
      if(csx[i+k] - csx[i] >= k2) pred[i] <- 1
      else pred[i] <- 0
      
    }
    return(mean(abs(pred-x[k+1:n])))
  }
}

#2.6 ベクトルを入力し、ベクトルを出力する

#引数をベクトルにすればベクトルのまま計算するから早く計算することができる
u<-c(5,2,8)
v<-c(1,3,9)
u>v

w <- function(x) return(x+1)
w(u)

y<- c(1.2,3.9, 0.4)
z <- round(y)
z

f <- function(x,c ) return((x+c)^2)
f(1:3,0)
f(1:3,1)

#引数cにスカラ（厳密には長さ1のベクトル)に限定したい時
f0 <- function(x,c){
  if(length(c) != 1) stop("vector c not allowed ")
  return((x+c)^2)
}

z12 <- function(z) return(c(z,z^2))
z12(4)
c<-1:8
z12(c)
matrix(z12(c),ncol = 2)

#sapply関数
z12 <- function(z) return(c(z,z^2))
sapply(1:8,z12)

# 2.7　NA値とNULL値

x<-c(88,NA,12,168,13,NULL)
mean(x)
mean(x,na.rm=T)
mode(x[2])
mode(x[6])

z<-NULL
for(i in 1:10)if(i %% 2 ==0) z <- c(z,i)
z

u<-NULL
v<-NA
length(u)
length(v)

#2.8　フィルタリング

z<- c(5,2,-3,8)
w <- z[z*z >8]
w

j<-z*z >8
j

y<-c(1,2,30,5)
y[j]

#subset関数
 x<-c(6,1:3,NA,12)
 x[x>5]
 subse(x,x>5)
 
 z<-c(5,2,-3,8)
 which(z*z >8)
 
 first1a <-function(x) return(which(x==1)[1])
 
#2.10 ベクトル等価性の検査

x<-1:3
y<-c(1,3,4)
x==y

identical(x,y)

#2.11 ベクトル要素名

x<-c(1,2,4)
names(x)
names(x) <-c("a","b","ab")
x
x["a"]

