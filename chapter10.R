#10章入出力

scan("z1.txt")
scan("z2.txt")
scan("z3.txt")
scan("z4.txt")

v <- scan("z1.txt")
x1 <-scan("z3.txt", what = "")
x2<-scan("z3.txt",what = "",sep="n")
x1
x1[2]
x2[2]

v<- scan("")
12
3 4 5
8
v

w <- readline()
abc de f
inits <- readline("type your initials:")
RY

x <- 1:3
print(x^2)
print("abx")

cat("abc n")

cat(x,"abcn","de",sep="n")

z <- read.table("z.txt",header = T)
z

read.matrix <- function(filename){
  as.matrix(read.matrix(filename))
}

?connection

c<- file("z.txt","r")

readLines(c,n=1)


while(TRUE){
  rl <- readLines(c,n=1)
  if(length(rl) == 0){
    print("reached the end")
    break
  }else print(rl)
}

seek(con=c,where = 0)

uci <- "http://archive.ics.uci.edu/ml/machine-learning-databases/"
uci <- paste(uci,"echocardiogram/echocardiogram.data",sep = "")
ecc <- read.csv(uci)
head(ecc)

kids <- c("Jack","Jill")
ages<- c(12,10)
d <- data.frame(kids,ages,stringsAsFactors = F)
d
write.table(d,"kds")
write.table(xc,"xcnew",row.names = F,col.names = F)

file.info()
dir()
file.exists("kds")
