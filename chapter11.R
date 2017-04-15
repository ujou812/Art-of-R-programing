#11章　文字列操作

#grep()

grep("Pole",c("Equater","North Pole","South Pole"))
grep("pole",c("Equater","North Pole","South Pole"))

#nchar()

nchar("South Pole")

#paste()

paste("North","Pole")
paste("North","Pole",sep = ".")
paste("North","and","South","Poles")

#sprintf()

i <- 8
s <- sprintf("the square of %d is %d",i,i^2)
s

#substr(),substring()

substring("Equator",3,5)
strsplit("6-16-2011",split="-")

#gregexpr()
gregexpr("iss","Mississippi")

#11.2 正規表現

grep("[au]",c("Equator","North Pole","South Pole"))
grep("o.e",c("Equator","North Pole","South Pole"))
grep("N..t",c("Equator","North Pole","South Pole"))

grep(".",c("abc","de","f.g"))

grep("\\.",c("abc","de","f.g"))

#拡張子htmlを探すプログラム

testsuffix <- function(fn,suff){
  parts <- strsplit(fn,".",fixed = T)
  nparts <- length(parts[[1]])
  return(parts[[1]][nparts] == suff)
}
testsuffix("x.abc","abc")

#ファイル名を作成する

for(i in 1:5){
  fname <- paste("q",i,".pdf",sep = "")
  pdf(fname)
  hist(rnorm(100,sd=i))
  dev.off
}

#ファイルが壊れているエラーが出たのであとでデバグ

#11.3 edtdbgデバッグツールでの文字列ユーテリティの使用

#コマンドをエディタに送信する
dbgsendeditcmd <- function(cmd){
  syscmd <- paste("vim --remote-send",cmd," --servername ",vimserver,sep="")
  system(syscmd)
}

#Vimエディタに関して知識がないので学習してあとでフォロー
