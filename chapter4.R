#４章リスト

#4.1 リストの作成

j <- list(name="joe",salary=55000,union=T)
j

#4.2 一般的なリスト操作

j$salary
j[["salary"]]
j[[2]]

j[1:2]
j2 <-j[2]
class(j2)
str(j2)

z <- list(a="abc",b=12)
z

z$c <- "sailing"
z
z[[4]] <-28
z[5:7]<-c(FALSE,TRUE,TRUE)
z

z$b <- NULL
z

c(list("JOE",55000,T),list(5))
length(j)

#テキスト索引

#findwords

findwords <- function(tf){
  #ファイルから単語を読み込み、モード文字にベクトルに入れる
  txt <- scan(tf,"")
  wl<-list()
  for(i in 1:length(txt)){
    wrd <- txt[i] # 入力ファイル内のi番目の単語
    wl[[wrd]] <- c(wl[[wrd]],i)
    
  }
  return(wl)
}
findwords(tf = "testconcorda.txt")

#4.3 リストの成分と値へのアクセス

names(j)
ulj <- unlist(j)
ulj
class(ulj)

#4.4 リストへの関数の適用
list(1:3,25:29)
lapply(list(1:3,25:29), median)
sapply(list(1:3,25:29), median)

alphawl <- function(tf){
  txt <- scan(tf,"")
  wl<-list()
  for(i in 1:length(txt)){
    wrd <- txt[i] # 入力ファイル内のi番目の単語
    wl[[wrd]] <- c(wl[[wrd]],i)
  }
  
  nms <- names(wl) #単語
  sn <- sort(nms)#アルファベット順の同じ単語
  return (wl[sn])#再配置したバージョンを返す
}

freqwl <- function(tf){
  txt <- scan(tf,"")
  wl<-list()
  for(i in 1:length(txt)){
    wrd <- txt[i] # 入力ファイル内のi番目の単語
    wl[[wrd]] <- c(wl[[wrd]],i)
  }
  freqs <- sapply(wl,length)
  return(wl[order(freqs)])
}

findwords(tf = "testconcorda.txt")
alphawl(tf = "testconcorda.txt")
freqwl(tf = "testconcorda.txt")

#4.5 再帰的リスト

b<- list(u=5, v=12)
c<- list(w = 13)
a<- list(b,c)
a

length(a)

c(list(a=1,b=2,c=list(d=5,e=9)))
c(list(a=1,b=2,c=list(d=5,e=9)),recursive=T)
