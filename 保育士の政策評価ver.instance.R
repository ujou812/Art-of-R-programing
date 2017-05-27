#==================================================================
#[女] せめてインスタンス化を
#S3クラスの関数
#==================================================================

#女性のデータベースをインポートし初期化

shokika <- { 
  #データのインポート
  josei_h28 <- read.csv("~/Desktop/R/josei_csv/josei_h28.csv", fileEncoding="CP932", header=TRUE, quote="\"")
  josei_h27 <- read.csv("~/Desktop/R/josei_csv/josei_h27.csv", fileEncoding="CP932", header=TRUE, quote="\"")
  josei_h26 <- read.csv("~/Desktop/R/josei_csv/josei_h26.csv", fileEncoding="CP932", header=TRUE, quote="\"")
  josei_h25 <- read.csv("~/Desktop/R/josei_csv/josei_h25.csv", fileEncoding="CP932", header=TRUE, quote="\"")
  josei_h24 <- read.csv("~/Desktop/R/josei_csv/josei_h24.csv", fileEncoding="CP932", header=TRUE, quote="\"")
  josei_h23 <- read.csv("~/Desktop/R/josei_csv/josei_h23.csv", fileEncoding="CP932", header=TRUE, quote="\"")
  josei_h22 <- read.csv("~/Desktop/R/josei_csv/josei_h22.csv", fileEncoding="CP932", header=TRUE, quote="\"")
  josei_h21 <- read.csv("~/Desktop/R/josei_csv/josei_h21.csv", fileEncoding="CP932", header=TRUE, quote="\"")
  #View(josei_h22)
  #head(josei_h28,3)
  
  #CPI(消費者物価指数)[2015年基準]
  
  cpi_h21 <- 97.2 /100
  cpi_h22 <- 96.5 /100
  cpi_h23 <- 96.3 /100
  cpi_h24 <- 96.2 /100
  cpi_h25 <- 96.6 /100
  cpi_h26 <- 99.2 /100
  cpi_h27 <- 100 / 100
  cpi_h28 <- 99.9 /100
  
  #抜け値にNA値を代入
  #before
  josei_h21[c(118,120),] <- NA  
  josei_h22[c(28,112,125),] <- NA
  josei_h23[c(112,125),] <- NA
  josei_h24[64,] <- NA
  #after
  josei_h25[c(122,125), ] <- NA
  josei_h26[c(64,112,122),] <- NA
  josei_h28[c(118,125),] <- NA
  
  #胸が痛い労働者数操作[労働者数が少なすぎる値を省くことでlogをとることができる]
  #df$labor[df$labor ==0] <- NA
  
  #給与実質化 [実質]　= [名目] / CPI
  
  josei_h21[,c(6,7,8)] <- josei_h21[,c(6,7,8)] / cpi_h21
  josei_h22[,c(6,7,8)] <- josei_h22[,c(6,7,8)] / cpi_h22
  josei_h23[,c(6,7,8)] <- josei_h23[,c(6,7,8)] / cpi_h23
  josei_h24[,c(6,7,8)] <- josei_h24[,c(6,7,8)] / cpi_h24
  
  josei_h25[,c(6,7,8)] <- josei_h25[,c(6,7,8)] / cpi_h25
  josei_h26[,c(6,7,8)] <- josei_h26[,c(6,7,8)] / cpi_h26
  josei_h27[,c(6,7,8)] <- josei_h27[,c(6,7,8)] / cpi_h27
  josei_h28[,c(6,7,8)] <- josei_h28[,c(6,7,8)] / cpi_h28　
  
  database <- list(josei_h21[,-1],josei_h22[,-1],josei_h23[,-1],josei_h24[,-1],josei_h25[,-1],josei_h26[,-1],josei_h27[,-1],josei_h28[,-1])
}

#befor/afrer期間をベクトルで定義[H21~28]
start <- c(21:23)
end <- c(26:28)

#みたい年度を変数に入れてインスタンス化

instance <- function(start,end){
  df <- NULL
  s <- start-20
  e <- end-20
  for(i in c(s) ) df <- rbind(df,database[[i]])
  for(i in c(e) ) df <- rbind(df,database[[i]])
  #str(df)
  #時間ダミー
  n <- length(s) + length(e)
  d_n <- 129 *n
  dummy_t <- NULL
  for(i in 1:d_n){
    if(i <= length(s)*129 ) dummy_t <- c(dummy_t,0)
    else dummy_t <- c(dummy_t,1)
  }
  #str(dummy_t)
  
  #保育士ダミー
  d_hoiku <- NULL
  for(i in 1:129){
    if(i == 21) d_hoiku <- c(d_hoiku,1)
    else d_hoiku <- c(d_hoiku,0)
  }
  #d_hoiku
  
  #介護職ダミー
  d_kaigo <- NULL
  for(i in 1:129){
    if(i >= 22 && i <= 24) d_kaigo <- c(d_kaigo,1)
    else d_kaigo <- c(d_kaigo,0)
  }
  #d_kaigo
  
  d_hoikushi <- NULL
  for(i in 1:n){ d_hoikushi <- c(d_hoikushi,d_hoiku)}
  #str(d_hoikushi)
  d_kaigoshi <- NULL
  for(i in 1:n){ d_kaigoshi <- c(d_kaigoshi,d_kaigo)}
  #str(d_kaigoshi)
  
  #交差項
  cross1 <- d_hoikushi * dummy_t
  cross2 <- d_kaigoshi * dummy_t
  #str(cross2)
  
  #トレンド変数
  
  d_trend <- NULL
  for(i in 1:d_n){
    if(i %% 129 ==0)d_trend <- c(d_trend,i %/% 129 - 1)
    else d_trend <- c(d_trend,i %/% 129)
  }
  #str(d_trend)
  d_trend_2 <- d_trend ** 2
  #str(d_trend_2)
  
  #列名
  names <- c("age","year","labor.time","labor.over.time","salary.zangyo","salary","bonus","labor","time","trend","trend2","hoiku","kaigo","cross1","cross2")
  
  df <- cbind(df,dummy_t,d_trend,d_trend_2,d_hoikushi,d_kaigo,cross1,cross2)
  df <- as.data.frame(df)
  names(df) <- names
  df <- df[complete.cases(df),]
  
  #結果参照
  
  res_salary1 <- lm(log(salary.zangyo) ~ year + time + trend + trend2 + hoiku + kaigo + cross1 + cross2, data=df)
  res_salary2 <- lm(log(salary) ~  year + time +trend + trend2 + hoiku + kaigo + cross1 + cross2, data=df)
  res_bonus <- lm(bonus ~   year + time + trend + trend2 + hoiku + kaigo + cross1 + cross2, data=df)
  res_labor <-  lm(labor ~  year + time + trend + trend2 + hoiku + kaigo + cross1 + cross2, data=df)
  result <- list(summary(res_salary1),summary(res_salary2),summary(res_bonus),summary(res_labor))
  #res_labor <- lm(log(labor) ~  year + time + trend + trend2 + hoiku + kaigo + cross1 + cross2, data=df)
  #summary(res_labor)
  return(result)
}

#実行
instance(start,end)
