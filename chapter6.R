#6章　ファクタと表

#6.1 ファクタと水準

x<- c(5,12,13,12)
xf <- factor(x) 
xf
str(xf)
unclass(xf)
length(xf)

xff <- factor(x,levels = c(5,12,13,88))
xff
xff[2]<- 88
xff

xff[2]<- 28

#6.2 ファクタで使う一般的関数

ages <- c(25,26,55,37,21,42)
affils <- c("R","D","D","R","U","D")
tapply(ages,affils,mean)

d <- data.frame(list(gender=c("M","M","F","M","F","F"),ages=c(47,59,21,32,33,24),income=c(55000,88000,32450,76500,123000,45650)))
d
d$over25 <- ifelse(d$age >25,1,0)
d
tapply(d$income,list(d$gender,d$over25),mean)

split(d$income,list(d$gender,d$over25))

#by()
#by(aba,aba$gender,function(m) lm(m[2,]~m[,3]))

#6.3 表の扱い

u <- c(22,8,33,6,8,29,-2)
fl <- list(c(5,12,13,12,13,5,13),c("a","bc","a","a","bc","a","a"))
tapply(u,fl,length)
table(fl)
