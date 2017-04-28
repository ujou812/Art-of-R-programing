#勉強会用のRファイル

head(test_body)
str(test_body)

#欠損値を取り除いたデータフレームの作成

test <- test_body[complete.cases(test_body),]
str(test)

#性別ダミーの作成
str(test$gender)
gender <- NULL
for(i in 1:length(test$ID)){
  if(test$gender[i] == "M") gender <- c(gender,1)
  else gender <- c(gender,0)
}
gender

#年齢グループのダミー変数の作成
str(test$old_group)
old_group <- NULL
for(i in 1:length(test$ID)){
  if(test$old_group[i] =="Y") old_group <- c(old_group,1)
  else old_group <- c(old_group, 0)
}
old_group

#作成したダミー変数を含んだデータフレームの作成
dataframe <- data.frame(test,gender,old_group)
head(dataframe)

#回帰分析

regression <- lm(A15 ~ okd + gender.1 + old_group.1 + A1, data =dataframe)
summary(regression)

#Bostonデータ
library(MASS)
head(Boston)

y <- as.vector(Boston$medv)
x <- as.matrix(Boston[,-14])

#S4クラス

x <- matrix(c(1:9),ncol = 3,nrow = 3)
y <- matrix(100:102)
lm(y ~ x)

setClass(
  "MultipleRegression", 
  representation(
    y ="vector",
    x ="matrix",
    n ="numeric",
    k ="numeric",
    beta="matrix"
  )
)

specimen.MultipleRegression <- function(object){
  object@n <- length(object@y)
  object@k <- ncol(object@x)
  return(c(object@n,object@k))
}
sample <- new("MultipleRegression", x = x, y = y)
sample

beta.MultipleRegression <- function(object){
  x <- object@x
  y <- object@y
  X <- cbind(rep(1, nrow(x)), x)
  beta <- solve(t(X)%*%X) %*% t(X) %*% y
  return(beta)
}

v_error.MultipleRegression <- function(object){
  X <- cbind(rep(1, nrow(object@x)), object@x)
  beta <- beta.MultipleRegression(object)
  n <- specimen.MultipleRegression(object)[1]
  k <- specimen.MultipleRegression(object)[2]
  s <- X %*% beta
  v_e <- 0
  for(i in 1:n){
    e <- (object@y[i] - s[i] )**2
    v_e <- v_e + e
  }
  return(v_e/(n-k))
}

v_beta.MultipleRegression <- function(object){
  X <- cbind(rep(1, nrow(object@x)), object@x)
  y <- object@y
  v_e <- v_error.MultipleRegression(object)
  v_beta <- v_e * solve(t(X) %*% X)
  return(v_beta)
}

t_value.MultipleRegression <- function(object){
  X <- cbind(rep(1, nrow(object@x)), object@x)
  k <- specimen.MultipleRegression(object)[2] + 1 
  beta <- beta.MultipleRegression(object)
  y <- object@y
  v_e <- v_error.MultipleRegression(object)
  Q <- solve(t(X) %*% X)
  t <- NULL
  for(i in 1:k){
    S <- (v_e)**2 * Q[i,i]
    Sbi <- sqrt(S)
    t0 <- beta[i] / Sbi
    t <- c(t,t0)
  }
  return(t)
}

squaredR.MultipleRegression <- function(object){
  X <- cbind(rep(1, nrow(object@x)), object@x)
  y <- object@y
  Y_mean <- mean(y)
  beta <- beta.MultipleRegression(object)
  n <- specimen.MultipleRegression(object)[1]
  k <- specimen.MultipleRegression(object)[2]
  TSS <- 0
  ESS <- 0
  for(i in 1:n){
    y.error <- (y[i]-Y_mean)**2
    TSS <- TSS + y.error
    zansa <- X[i,] %*% beta
    ESS <- ESS + zansa
  }
  squaredR <- ESS / TSS
  RSS <- TSS - ESS
  adjusted.squaredR <- 1- (RSS/TSS)*(n-1)/(n-k)
  return(c(squaredR,adjusted.squaredR))
}

F_value.MultipleRegression <- function(object){
  R <- squaredR.MultipleRegression(object)[1]
  n <- specimen.MultipleRegression(object)[1]
  k <- specimen.MultipleRegression(object)[2]
  F0 <- (R/(1-R))*((n-k)/(k-1))
  return(F0)
}

specimen.MultipleRegression(sample)
beta.MultipleRegression(sample)
v_e <- v_error.MultipleRegression(sample)
v_beta.MultipleRegression(sample)
t_value.MultipleRegression(sample)
squaredR.MultipleRegression(sample)
F_value.MultipleRegression(sample)

#S3クラスで書いた
X <- x
X <- scale(X) #基準化
X <- cbind(rep(1, nrow(X)), X)
beta <- solve(t(X)%*%X) %*% t(X) %*% y
print(beta)
