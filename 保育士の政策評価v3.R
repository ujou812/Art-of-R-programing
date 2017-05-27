#==================================================================
#[男女] 再検討
#==================================================================

danjo_h28 <- read.csv("~/Desktop/R/danjo_csv/danjo_h28.csv", fileEncoding="CP932", header=TRUE, quote="\"")
danjo_h27 <- read.csv("~/Desktop/R/danjo_csv/danjo_h27.csv", fileEncoding="CP932", header=TRUE, quote="\"")
danjo_h26 <- read.csv("~/Desktop/R/danjo_csv/danjo_h26.csv", fileEncoding="CP932", header=TRUE, quote="\"")
danjo_h25 <- read.csv("~/Desktop/R/danjo_csv/danjo_h25.csv", fileEncoding="CP932", header=TRUE, quote="\"")
danjo_h24 <- read.csv("~/Desktop/R/danjo_csv/danjo_h24.csv", fileEncoding="CP932", header=TRUE, quote="\"")
danjo_h23 <- read.csv("~/Desktop/R/danjo_csv/danjo_h23.csv", fileEncoding="CP932", header=TRUE, quote="\"")
danjo_h22 <- read.csv("~/Desktop/R/danjo_csv/danjo_h22.csv", fileEncoding="CP932", header=TRUE, quote="\"")
danjo_h21 <- read.csv("~/Desktop/R/danjo_csv/danjo_h21.csv", fileEncoding="CP932", header=TRUE, quote="\"")
#View(danjo_h28)

df <- NULL
df <- rbind(danjo_h21[,-1],danjo_h22[,-1],danjo_h23[,-1],danjo_h24[,-1],danjo_h25[,-1],danjo_h26[,-1],danjo_h27[,-1],danjo_h28[,-1])
str(df)
head(df)

#==================================================================
#[男女] データをプールしてDD分析　before[21:23],after[26:28]
#==================================================================

df <- NULL
df <- rbind(danjo_h21[,-1],danjo_h22[,-1],danjo_h23[,-1],danjo_h26[,-1],danjo_h27[,-1],danjo_h28[,-1])
str(df)

#時間ダミー
dummy_t <- NULL
for(i in 1:774){
  if(i <= 387) dummy_t <- c(dummy_t,0)
  else dummy_t <- c(dummy_t,1)
}
str(dummy_t)

d_hoikushi <- c(d_hoiku,d_hoiku,d_hoiku,d_hoiku,d_hoiku,d_hoiku)
str(d_hoikushi)
d_kaigoshi <- c(d_kaigo,d_kaigo,d_kaigo,d_kaigo,d_kaigo,d_kaigo)
str(d_kaigo)

#交差項
cross1 <- d_hoikushi * dummy_t
cross2 <- d_kaigo * dummy_t
str(cross2)

#列名
names <- c("age","year","labor.time","labor.over.time","salary.zangyo","salary","bonus","labor","time","hoiku","kaigo","cross1","cross2")

df <- cbind(df,dummy_t,d_hoikushi,d_kaigo,cross1,cross2)
df <- as.data.frame(df)
head(df)
names(df) <- names
head(df)

res_salary1 <- lm(salary.zangyo ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_salary2 <- lm(salary ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_bonus <- lm(bonus ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_labor <- lm(labor ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
summary(res_labor)

#==================================================================
#[男女] データをプールしてDD分析　before[21:24],after[25:28]
#==================================================================

df <- NULL
df <- rbind(danjo_h21[,-1],danjo_h22[,-1],danjo_h23[,-1],danjo_h24[,-1],danjo_h25[,-1],danjo_h26[,-1],danjo_h27[,-1],danjo_h28[,-1])
str(df)

#時間ダミー
dummy_t <- NULL
for(i in 1:1032){
  if(i <= 516) dummy_t <- c(dummy_t,0)
  else dummy_t <- c(dummy_t,1)
}
str(dummy_t)

d_hoikushi <- c(d_hoiku,d_hoiku,d_hoiku,d_hoiku,d_hoiku,d_hoiku,d_hoiku,d_hoiku)
str(d_hoikushi)
d_kaigoshi <- c(d_kaigo,d_kaigo,d_kaigo,d_kaigo,d_kaigo,d_kaigo,d_kaigo,d_kaigo)
str(d_kaigoshi)

#交差項
cross1 <- d_hoikushi * dummy_t
cross2 <- d_kaigoshi * dummy_t
str(cross2)

#列名
names <- c("age","year","labor.time","labor.over.time","salary.zangyo","salary","bonus","labor","time","hoiku","kaigo","cross1","cross2")

df <- cbind(df,dummy_t,d_hoikushi,d_kaigo,cross1,cross2)
df <- as.data.frame(df)
head(df)
names(df) <- names
head(df)

res_salary1 <- lm(salary.zangyo ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_salary2 <- lm(salary ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_bonus <- lm(bonus ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_labor <- lm(labor ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
summary(res_labor)

#==================================================================
#[男女] プールせずにH22とH28で回帰
#==================================================================

df <- NULL
df <- rbind(danjo_h22[,-1],danjo_h28[,-1])
str(df)

dummy_t <- NULL
for(i in 1:258){
  if(i <= 129) dummy_t <- c(dummy_t,0)
  else dummy_t <- c(dummy_t,1)
}
str(dummy_t)

d_hoikushi <- NULL
d_hoikushi <- c(d_hoiku,d_hoiku)
str(d_hoikushi)
d_kaigoshi <- NULL
d_kaigoshi <- c(d_kaigo,d_kaigo)
str(d_kaigoshi)

cross1 <- d_hoikushi * dummy_t
cross2 <- d_kaigoshi * dummy_t
str(cross2)

#列名
names <- c("age","year","labor.time","labor.over.time","salary.zangyo","salary","bonus","labor","time","hoiku","kaigo","cross1","cross2")

df <- cbind(df,dummy_t,d_hoikushi,d_kaigo,cross1,cross2)
df <- as.data.frame(df)
head(df)
names(df) <- names
head(df)

res_salary1 <- lm(salary.zangyo ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_salary2 <- lm(salary ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_bonus <- lm(bonus ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_labor <- lm(labor ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
summary(res_salary1)
summary(res_salary2)
summary(res_bonus)
summary(res_labor)

#==================================================================
#[男女] プールせずにH21とH28で回帰
#==================================================================

df <- NULL
df <- rbind(danjo_h21[,-1],danjo_h28[,-1])
str(df)

dummy_t <- NULL
for(i in 1:258){
  if(i <= 129) dummy_t <- c(dummy_t,0)
  else dummy_t <- c(dummy_t,1)
}
str(dummy_t)

d_hoikushi <- NULL
d_hoikushi <- c(d_hoiku,d_hoiku)
str(d_hoikushi)
d_kaigoshi <- NULL
d_kaigoshi <- c(d_kaigo,d_kaigo)
str(d_kaigoshi)

cross1 <- d_hoikushi * dummy_t
cross2 <- d_kaigoshi * dummy_t
str(cross2)

#列名
names <- c("age","year","labor.time","labor.over.time","salary.zangyo","salary","bonus","labor","time","hoiku","kaigo","cross1","cross2")

df <- cbind(df,dummy_t,d_hoikushi,d_kaigo,cross1,cross2)
df <- as.data.frame(df)
head(df)
names(df) <- names
head(df)

res_salary1 <- lm(salary.zangyo ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_salary2 <- lm(salary ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_bonus <- lm(bonus ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_labor <- lm(labor ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
summary(res_salary1)
summary(res_salary2)
summary(res_bonus)
summary(res_labor)

#==================================================================
#[女] 再検討
#==================================================================

josei_h28 <- read.csv("~/Desktop/R/josei_csv/josei_h28.csv", fileEncoding="CP932", header=TRUE, quote="\"")
josei_h27 <- read.csv("~/Desktop/R/josei_csv/josei_h27.csv", fileEncoding="CP932", header=TRUE, quote="\"")
josei_h26 <- read.csv("~/Desktop/R/josei_csv/josei_h26.csv", fileEncoding="CP932", header=TRUE, quote="\"")
josei_h25 <- read.csv("~/Desktop/R/josei_csv/josei_h25.csv", fileEncoding="CP932", header=TRUE, quote="\"")
josei_h24 <- read.csv("~/Desktop/R/josei_csv/josei_h24.csv", fileEncoding="CP932", header=TRUE, quote="\"")
josei_h23 <- read.csv("~/Desktop/R/josei_csv/josei_h23.csv", fileEncoding="CP932", header=TRUE, quote="\"")
josei_h22 <- read.csv("~/Desktop/R/josei_csv/josei_h22.csv", fileEncoding="CP932", header=TRUE, quote="\"")
josei_h21 <- read.csv("~/Desktop/R/josei_csv/josei_h21.csv", fileEncoding="CP932", header=TRUE, quote="\"")
View(josei_h22)
head(josei_h28,3)

#CPI(消費者物価指数)[2015年基準]

cpi_h21 <- 97.2 /100
cpi_h22 <- 96.5 /100
cpi_h23 <- 96.3 /100
cpi_h24 <- 96.2 /100
cpi_h25 <- 96.6 /100
cpi_h26 <- 99.2 /100
cpi_h27 <- 100 / 100
cpi_h28 <- 99.9 /100

#==================================================================
#[女] データをプールしてDD分析　before[21:24],after[25:28]
#==================================================================

#before
josei_h21[c(118,120),] <- NA  
josei_h22[c(28,112,125),] <- NA
josei_h23[c(112,125),] <- NA
josei_h24[64,] <- NA

#after
josei_h25[c(122,125), ] <- NA
josei_h26[c(64,112,122),] <- NA
josei_h28[c(118,125),] <- NA

#給与実質化 [実質]　= [名目] / CPI

josei_h21[,c(6,7,8)] <- josei_h21[,c(6,7,8)] / cpi_h21
josei_h22[,c(6,7,8)] <- josei_h22[,c(6,7,8)] / cpi_h22
josei_h23[,c(6,7,8)] <- josei_h23[,c(6,7,8)] / cpi_h23
josei_h24[,c(6,7,8)] <- josei_h24[,c(6,7,8)] / cpi_h24

josei_h25[,c(6,7,8)] <- josei_h25[,c(6,7,8)] / cpi_h25
josei_h26[,c(6,7,8)] <- josei_h26[,c(6,7,8)] / cpi_h26
josei_h27[,c(6,7,8)] <- josei_h27[,c(6,7,8)] / cpi_h27
josei_h28[,c(6,7,8)] <- josei_h28[,c(6,7,8)] / cpi_h28　
#View(josei_h27)

#==================================================================
#[女] データをプールしてDD分析　before[21:24],after[25:28]
#==================================================================

df <- NULL
df <- rbind(josei_h21[,-1],josei_h22[,-1],josei_h23[,-1],josei_h24[,-1],josei_h25[,-1],josei_h26[,-1],josei_h27[,-1],josei_h28[,-1])

str(df)
head(df)


#==================================================================
#[女]データサンプリング
#==================================================================

df_v <- df
#df_v[is.na(danjo23_v)] <- 0
str(df_v)
df_v <- df_v[complete.cases(df_v),]

names_v <- c("age","year","labor.time","labor.over.time","salary.zangyo","salary","bonus","labor")
names(df_v) <- names_v

head(df_v)
plot(df_v$salary)
hist(df_v$salary)
hist(log(df_v$salary))
hist(df_v$salary.zangyo)
hist(log(df_v$salary.zangyo))
hist(df_v$labor)
hist(log(df_v$labor))


#==================================================================
#[女]ダミー作成
#==================================================================

#時間ダミー
dummy_t <- NULL
for(i in 1:1032){
  if(i <= 516) dummy_t <- c(dummy_t,0)
  else dummy_t <- c(dummy_t,1)
}
str(dummy_t)

d_hoikushi <- c(d_hoiku,d_hoiku,d_hoiku,d_hoiku,d_hoiku,d_hoiku,d_hoiku,d_hoiku)
str(d_hoikushi)
d_kaigoshi <- c(d_kaigo,d_kaigo,d_kaigo,d_kaigo,d_kaigo,d_kaigo,d_kaigo,d_kaigo)
str(d_kaigoshi)

#交差項
cross1 <- d_hoikushi * dummy_t
cross2 <- d_kaigoshi * dummy_t
str(cross2)

#トレンド変数

d_trend <- NULL
for(i in 1:1032){
  if(i %% 129 ==0)d_trend <- c(d_trend,i %/% 129 - 1)
  else d_trend <- c(d_trend,i %/% 129)
}
str(d_trend)
d_trend_2 <- d_trend ** 2
str(d_trend_2)

#列名
names <- c("age","year","labor.time","labor.over.time","salary.zangyo","salary","bonus","labor","time","trend","trend2","hoiku","kaigo","cross1","cross2")

df <- cbind(df,dummy_t,d_trend,d_trend_2,d_hoikushi,d_kaigo,cross1,cross2)
df <- as.data.frame(df)
head(df)
names(df) <- names
df <- df[complete.cases(df),]
str(df)
head(df)

#結果参照

res_salary1 <- lm(salary.zangyo ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_salary2 <- lm(salary ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_bonus <- lm(bonus ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_labor <- lm(labor ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
summary(res_salary1)
summary(res_salary2)
summary(res_bonus)
summary(res_labor)

#変数テスト
res_salary1 <- lm(salary.zangyo ~ year + time + trend + trend2 + hoiku + kaigo + cross1 + cross2, data=df)
res_salary2 <- lm(salary ~  year + time +trend + trend2 + hoiku + kaigo + cross1 + cross2, data=df)
res_bonus <- lm(bonus ~   year + time + trend + trend2 + hoiku + kaigo + cross1 + cross2, data=df)
res_labor <- lm(labor ~  year + time + trend + trend2 + hoiku + kaigo + cross1 + cross2, data=df)
summary(res_salary1)
summary(res_salary2)
summary(res_bonus)
summary(res_labor)


#日説明変数の散布図を意識してログをとる
res_salary3 <- lm(log(salary.zangyo) ~  year + time + trend + trend2 +hoiku + kaigo + cross1 + cross2, data=df)
res_salary4 <- lm(log(salary) ~ year + time +trend + trend2 + hoiku + kaigo + cross1 + cross2, data=df)
summary(res_salary1)
summary(res_salary2)
summary(res_salary3)
summary(res_salary4)

log(df$salary.zangyo)

#マイナス無限大が値に入る
#res_bonus_log <- lm(log(bonus) ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
#res_labor_log <- lm(log(labor) ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
#summary(res_bonus_log)
#summary(res_labor_log)

#==================================================================
#[女] データをプールしてDD分析　before[21:23],after[26:28]
#==================================================================

df <- NULL
df <- rbind(josei_h21[,-1],josei_h22[,-1],josei_h23[,-1],josei_h26[,-1],josei_h27[,-1],josei_h28[,-1])

str(df)
head(df)

#時間ダミー
dummy_t <- NULL
for(i in 1:774){
  if(i <= 387) dummy_t <- c(dummy_t,0)
  else dummy_t <- c(dummy_t,1)
}
str(dummy_t)

d_hoikushi <- c(d_hoiku,d_hoiku,d_hoiku,d_hoiku,d_hoiku,d_hoiku)
str(d_hoikushi)
d_kaigoshi <- c(d_kaigo,d_kaigo,d_kaigo,d_kaigo,d_kaigo,d_kaigo)
str(d_kaigoshi)

#交差項
cross1 <- d_hoikushi * dummy_t
cross2 <- d_kaigoshi * dummy_t
str(cross2)

#列名
names <- c("age","year","labor.time","labor.over.time","salary.zangyo","salary","bonus","labor","time","hoiku","kaigo","cross1","cross2")

df <- cbind(df,dummy_t,d_hoikushi,d_kaigo,cross1,cross2)
df <- as.data.frame(df)
head(df)
names(df) <- names
df <- df[complete.cases(df),]
str(df)
head(df)

res_salary1 <- lm(salary.zangyo ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_salary2 <- lm(salary ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_bonus <- lm(bonus ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_labor <- lm(labor ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
summary(res_salary1)
summary(res_salary2)
summary(res_bonus)
summary(res_labor)

#日説明変数の散布図を意識してログをとる
res_salary3 <- lm(log(salary.zangyo) ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_salary4 <- lm(log(salary) ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
summary(res_salary1)
summary(res_salary2)
summary(res_salary3)
summary(res_salary4)

#==================================================================
#[女] データをプールしてDD分析　before[22:24],after[26:28]
#==================================================================

df <- NULL
df <- rbind(josei_h22[,-1],josei_h23[,-1],josei_h24[,-1],josei_h26[,-1],josei_h27[,-1],josei_h28[,-1])

str(df)
head(df)

#時間ダミー
dummy_t <- NULL
for(i in 1:774){
  if(i <= 387) dummy_t <- c(dummy_t,0)
  else dummy_t <- c(dummy_t,1)
}
str(dummy_t)

d_hoikushi <- c(d_hoiku,d_hoiku,d_hoiku,d_hoiku,d_hoiku,d_hoiku)
str(d_hoikushi)
d_kaigoshi <- c(d_kaigo,d_kaigo,d_kaigo,d_kaigo,d_kaigo,d_kaigo)
str(d_kaigoshi)

#交差項
cross1 <- d_hoikushi * dummy_t
cross2 <- d_kaigoshi * dummy_t
str(cross2)

#列名
names <- c("age","year","labor.time","labor.over.time","salary.zangyo","salary","bonus","labor","time","hoiku","kaigo","cross1","cross2")

df <- cbind(df,dummy_t,d_hoikushi,d_kaigo,cross1,cross2)
df <- as.data.frame(df)
head(df)
names(df) <- names
df <- df[complete.cases(df),]
str(df)
head(df)
hist(df$salary.zangyo)

res_salary1 <- lm(salary.zangyo ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_salary2 <- lm(salary ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_bonus <- lm(bonus ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_labor <- lm(labor ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
summary(res_salary1)
summary(res_salary2)
summary(res_bonus)
summary(res_labor)

#日説明変数の散布図を意識してログをとる
res_salary3 <- lm(log(salary.zangyo) ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_salary4 <- lm(log(salary) ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
summary(res_salary1)
summary(res_salary2)
summary(res_salary3)
summary(res_salary4)

#==================================================================
#[女] データをプールしてDD分析　before[21:22],after[27:28]
#==================================================================

df <- NULL
df <- rbind(josei_h21[,-1],josei_h22[,-1],josei_h27[,-1],josei_h28[,-1])

str(df)
head(df)

#時間ダミー
dummy_t <- NULL
for(i in 1:516){
  if(i <= 258) dummy_t <- c(dummy_t,0)
  else dummy_t <- c(dummy_t,1)
}
str(dummy_t)

d_hoikushi <- c(d_hoiku,d_hoiku,d_hoiku,d_hoiku)
str(d_hoikushi)
d_kaigoshi <- c(d_kaigo,d_kaigo,d_kaigo,d_kaigo)
str(d_kaigoshi)

#交差項
cross1 <- d_hoikushi * dummy_t
cross2 <- d_kaigo * dummy_t
str(cross2)

#列名
names <- c("age","year","labor.time","labor.over.time","salary.zangyo","salary","bonus","labor","time","hoiku","kaigo","cross1","cross2")

df <- cbind(df,dummy_t,d_hoikushi,d_kaigo,cross1,cross2)
df <- as.data.frame(df)
head(df)
names(df) <- names
df <- df[complete.cases(df),]
str(df)
head(df)

res_salary1 <- lm(salary.zangyo ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_salary2 <- lm(salary ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_bonus <- lm(bonus ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_labor <- lm(labor ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
summary(res_salary1)
summary(res_salary2)
summary(res_bonus)
summary(res_labor)

#日説明変数の散布図を意識してログをとる
res_salary3 <- lm(log(salary.zangyo) ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_salary4 <- lm(log(salary) ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
summary(res_salary1)
summary(res_salary2)
summary(res_salary3)
summary(res_salary4)

#==================================================================
#[女] プールせずにH22とH28で回帰
#==================================================================

df <- NULL
df <- rbind(josei_h24[,-1],josei_h28[,-1])
str(df)

dummy_t <- NULL
for(i in 1:258){
  if(i <= 129) dummy_t <- c(dummy_t,0)
  else dummy_t <- c(dummy_t,1)
}
str(dummy_t)

d_hoikushi <- NULL
d_hoikushi <- c(d_hoiku,d_hoiku)
str(d_hoikushi)
d_kaigoshi <- NULL
d_kaigoshi <- c(d_kaigo,d_kaigo)
str(d_kaigoshi)

cross1 <- d_hoikushi * dummy_t
cross2 <- d_kaigoshi * dummy_t
str(cross2)

#列名
names <- c("age","year","labor.time","labor.over.time","salary.zangyo","salary","bonus","labor","time","hoiku","kaigo","cross1","cross2")

df <- cbind(df,dummy_t,d_hoikushi,d_kaigo,cross1,cross2)
df <- as.data.frame(df)
head(df)
names(df) <- names
head(df)

res_salary1 <- lm(salary.zangyo ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_salary2 <- lm(salary ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_bonus <- lm(bonus ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
res_labor <- lm(labor ~ age + year + time + hoiku + kaigo + cross1 + cross2, data=df)
summary(res_salary1)
summary(res_salary2)
summary(res_bonus)
summary(res_labor)

#==================================================================
#[女] せめてインスタンス化を
#==================================================================

#女性のデータベースをインポートし初期化

shokika <- { 
  josei_h28 <- read.csv("~/Desktop/R/josei_csv/josei_h28.csv", fileEncoding="CP932", header=TRUE, quote="\"")
  josei_h27 <- read.csv("~/Desktop/R/josei_csv/josei_h27.csv", fileEncoding="CP932", header=TRUE, quote="\"")
  josei_h26 <- read.csv("~/Desktop/R/josei_csv/josei_h26.csv", fileEncoding="CP932", header=TRUE, quote="\"")
  josei_h25 <- read.csv("~/Desktop/R/josei_csv/josei_h25.csv", fileEncoding="CP932", header=TRUE, quote="\"")
  josei_h24 <- read.csv("~/Desktop/R/josei_csv/josei_h24.csv", fileEncoding="CP932", header=TRUE, quote="\"")
  josei_h23 <- read.csv("~/Desktop/R/josei_csv/josei_h23.csv", fileEncoding="CP932", header=TRUE, quote="\"")
  josei_h22 <- read.csv("~/Desktop/R/josei_csv/josei_h22.csv", fileEncoding="CP932", header=TRUE, quote="\"")
  josei_h21 <- read.csv("~/Desktop/R/josei_csv/josei_h21.csv", fileEncoding="CP932", header=TRUE, quote="\"")
  View(josei_h22)
  head(josei_h28,3)
  
  #CPI(消費者物価指数)[2015年基準]
  
  cpi_h21 <- 97.2 /100
  cpi_h22 <- 96.5 /100
  cpi_h23 <- 96.3 /100
  cpi_h24 <- 96.2 /100
  cpi_h25 <- 96.6 /100
  cpi_h26 <- 99.2 /100
  cpi_h27 <- 100 / 100
  cpi_h28 <- 99.9 /100
  
  #==================================================================
  #[女] データをプールしてDD分析　before[21:24],after[25:28]
  #==================================================================
  
  #before
  josei_h21[c(118,120),] <- NA  
  josei_h22[c(28,112,125),] <- NA
  josei_h23[c(112,125),] <- NA
  josei_h24[64,] <- NA
  
  #after
  josei_h25[c(122,125), ] <- NA
  josei_h26[c(64,112,122),] <- NA
  josei_h28[c(118,125),] <- NA
  
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
end <- c(24:28)

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
  res_labor <- lm(labor ~  year + time + trend + trend2 + hoiku + kaigo + cross1 + cross2, data=df)
  result <- list(summary(res_salary1),summary(res_salary2),summary(res_bonus),summary(res_labor))
  
  return(result)

}

instance(start,end)

