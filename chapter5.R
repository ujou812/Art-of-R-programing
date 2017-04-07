#Chapter5 　データフレーム

kids <- c("Jack","Jill")
ages <- c(12,10)
d <- data.frame(kids,ages,stringsAsFactors = FALSE)
d

d[[1]]
d$kids
d[,1]
str(d)

#5.2 サブデータフレームの抽出

testdata <- data.frame(c(runif(10)),c(runif(10)),c(runif(10)))
testdata[2:5,2]
testdata[2:5,2,drop = FALSE]
testdata[2,2] <- NA
testdata

subset(testdata,testdata[[2]] >= 0.5)
complete.cases(testdata)
testdata2<-testdata[complete.cases(testdata),]
testdata2

rbind(d,list("Laura", 19))

eq <- cbind(testdata,testdata[[2]]-testdata[[1]])
class(eq)
head(eq)

testdata$testDif <- testdata[[2]]-testdata[[1]]
head(testdata)

apply(testdata,1,max)
apply(testdata,2,min)

ages <- c(10,7,12)
kids <- c("Jill","Lillian","Jack")
d2 <- data.frame(ages,kids,stringsAsFactors = FALSE)

kids <-c("Jill","Lillian","Jack","John")
states <- c("CA","MA","MA","HI")
d1 <- data.frame(kids,states,stringsAsFactors = FALSE)
d1
d <- merge(d1,d2)
d

#5.4　データフレームへの関数の適用

dl <- lapply(d,sort)
dl
as.data.frame(dl)
