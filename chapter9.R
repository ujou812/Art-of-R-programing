#9章　オブジェクト指向プログラミング

x <- c(1,2,3)
y <- c(1,3,8)
lmout <- lm(y~x)
class(lmout)
lmout

print
print.lm
unclass(lmout)

methods(print)
getAnywhere(print.lm)

j <- list(name="Joe",salary=55000,union=T)
class(j) <- "employee"
attributes(j)
j

print.employee <- function(wrkr){
  cat(wrkr$name,"")
  cat("salary",wrkr$salary,"")
  cat("union member",wrkr$union,"")
}
j

k <- list(name="Kate",salary = 68000, union=F, hrsthismonth = 2)
class(k) <- c("hrlyemployee","employee")
k

#9.2 S4クラスの記述

setClass("employee",
         representation(
           name="character",
           salary="numeric",
           union="logical"
         ))

joe <- new("employee",name="Joe",salary=55000,union=TRUE)
joe
joe@salary
slot(joe,"salary") <- 88000
joe

show(joe)

setMethod("show","employee",
          function(object) { 
            inorout <- ifelse(object@union,"is","is not")
            cat(object@name,"has a salary of",object@salary,
                "and",inorout,"in the union","")
            })
joe

#9.4 ls関数を使ったオブジェクトの列挙
ls(pattern = "ut")
rm(list = ls(pattern="ut"))

z <- rnorm(10000000)
hz <- hist(z)
save(hz,"hzfile") #ファイル名無効
ls()
rm(hz)
load("hzfile")
