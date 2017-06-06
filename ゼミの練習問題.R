#線形計画法

#solveLP(cvec,bvec,Amat,maximum = FALSE, const.dir = rep( "<=", length(bvec) ))

#cvec　ベクトルb
#bvec    ベクトルc
#Amat   行列A
#maximum  最大化問題の場合はTRUE
#const.dir

library(boot)
library(linprog)
library(lpSolve)
library(lpSolveAPI)
library(glpkAPI)
library(Rglpk)

cvec <- c(pi.a, pi.b)
bvec <- c(m1,m2,m3)
Amat <- rbind(A,B,C)
A <- c(a,d)
B <- c(b,e)
C <- c(c,f)
a<-1
b<-2
c<-3
d<-4
e<-5
f<-6
pi.a <- 10
pi.b <- 20
m1 <- 100
m2 <- 150
m3 <- 300

#solveLP中身

solveLP

res <- solveLP(cvec,bvec,Amat,maximum = FALSE, const.dir = rep( ">=", length(bvec) ))
#最適値
res$opt
#最適解
res$solution
#反復回数第一段階
res$iter1
#反復回数第二段階
res$iter2

#クロージャ@fibonacci数列

fibonacci <- function(n){
  if(n==0)
    return(0)
  else if (n == 1)
    return(1)
  else
    return(fibonacci(n-1)  + fibonacci(n-2))
}

n_fibonacci <- function(){
  n <- 0
  f <- function(){
    n <<- n + 1
    fibonacci(n)
  }
}
a <- n_fibonacci()
a()

fib <- NULL
for(i in 1:20){
      fib <- c(fib,a())
}
    
fib