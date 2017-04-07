#chapter7 プログラミング構造

x <- c(5,12,13)
for(n in x) print(n^2)

i<-1
while(i<=10) i <- i+4
i
i<-1
while(TRUE){
  i<- i+4
  if(i > 10)break
}
i
i<-1
repeat{
  i<- i+4
  if(i >10)break
}
i

#next

sim <- function(nreps){
  commdata <- list()
  sommdata$countabsamecomm <- 0
  for(rep in 1:nreps){
    comdata$whosleft <- 1:20
    comdata$numabchosen <- 0
    commdata <- choosecomm(commdata,5)
    if(commdata$numabchosen > 0) next
    commdata <- chosecomm(commdata,4)
    if(commdata$numabchosen > 0) next
    commdata <- choosecomm(commdata,3)
    
  }
  print(commdata$countabsamecomm/nreps)
}

#get

u <- matrix(c(1:6), ncol = 2)
v <- matrix(c(7:12), ncol = 2)
for(m in c("u","v")){
  z <- get(m)
  print(lm(z[,2] ~ z[,1]))
}

z <- get("u")
z

#if-else

if(r == 4){
  x <-1
}else{
  x <- 3
  y <-4
}

#7.2 算術演算子とブール演算子及びその値

#7.3 引数のデフォルト値

read.table

#7.4 戻り値

oddcount <- function(x){
  k <- 0
  for( n in x){
    if (n %% 2 ==1)k <- k + 1
  }
  k #return()を書かなければ実行された最後の一文を返す
}

#7.5 関数はオブジェクト

g <- function() {
  t<- function(x) return(x^2)
  return(t)
}
g()

g <- function(x){
  return(x+1)
}

abline
page(abline)

f1 <- function(a,b) return(a+b)
f2 <- function(a,b) return(a-b)
f <- f1
f(3,2)
g <- function(h,a,b) h(a,b)
g(f1, 3, 2)
g(f2, 3,2)

g1 <- function(x) return(sin(x))
g2 <- function(x) return(sqrt(x^2+1))
g3 <- function(x) return(2*x-1)
plot(c(0,1),c(-1,1.5))
for(f in c(g1,g2,g3)) plot(f,0,1, add=T)

#7.6 環境とスコープの問題

w <-12
f <- function(y){
  d <-8
  h <- function(){
    return(d*(w+y))
  }
  return(h())
}
environment(f)

ls()
ls.str()

#スコープ階層
f(2)

f <- function(y){
  d <-8
  h <- function(){
    return(d*(w+y))
  }
  print(environment(h))
  return(h())
}

f
h

w <-12
f <- function(y){
  d<-8
  w <- w +1
  y <- y -2
  print(w)
  h <- function(){
    return(d*(w+y))
  }
  return(h())
}
t<-4
f(t)
w
t

#現在の関数のローカル変数の値を知りたいときのための関数

f<-function(){
  a <- 1
  return(g(a)+a)
}

g<-function(aa){
  b<-2
  aab <- h(aa+b)
  return(aab)
}

h<-function(aaa){
  c<-3
  return(aaa+c)
}

#showframe()が呼び出されたフレームからupnフレーム上の
#ローカル変数（引数を含む）の値を表示する
# upn<0の場合はグローバルを表示する。関数オブジェクトは表示しない。

showframe <- function(upn){
  #適切な環境を判断する
  if(upn <0){
    env <- .GrobalEnv
  }else{
    env <- parent.frame(n = upn+1)
  }
  #変数名のリストを取得する
  vars <- ls(envir = env)
  #それぞれの変数名の値を出力する
  for(vr in vars){
    vrg <- get(vr,envir = env)
    if(!is.fucntion(vrg)){ #たぶん、バージョン変わってバグが出てる
      cat(vr,":¥n",sep = "")
      print(vrg)
    }
  }
}

g<-function(aa){
  b<-2
  showframe(0)
  showframe(1)
  aab <- h(aa+b)
  return(aab)
}
f()

m <- rbind(1:3,20:22)
m
get("m")
vrg <- get(vr,envir = env)

#7.7 Rにはポインタがない

x <- c[13,5,12]
sort(x)
x <- sort(x)

oddsevens <- function(v){
  odds <- which(v%%2 == 1)
  evens <- which(v %% 2 == 0)
  list(o = odds, e =evens)
}

oddsevens(x)
x

#上位レベルへの書き込み

two <- function(u){
  u <<- 2*u #スーパーアサインメント演算子
  z<- 2*z
}

x <- 1
z<-3

u
two(x)
x
z
u

f <- function(){
  inc <- function(){x <<<- x +1}
  x<-3
  inc()
  return(x)
}
f

#クロージャ

counter <- function(){
  ctr <- 0
  f<- function(){
    ctr <<- ctr +1
    cat("this count currently has value",str,"¥n")
  }
  return(f)
}
c1 <- counter()
c2 <- counter()
c1
c2
c1()
c1()
?cat
