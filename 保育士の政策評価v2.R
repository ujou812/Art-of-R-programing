todoufuken_danjo_H23 <- read.csv("~/Desktop/R/todoufuken_danjo_H23_v2.csv", header=FALSE)
todoufuken_danjo_H28 <- read.csv("~/Desktop/R/todoufuken_danjo_H28.csv", header=FALSE)
danjo23 <- todoufuken_danjo_H23
danjo28 <- todoufuken_danjo_H28
todoufuken_danjo_names <- read.table("~/Desktop/R/todoufuken_danjo_names.csv", fileEncoding="CP932", quote="\"")
names_danjo <- todoufuken_danjo_names

str(danjo23)
str(danjo28)

#danjo23_v <- danjo23
#danjo23_v[is.na(danjo23_v)] <- 0
#danjo23_v[danjo23_v == 0] <- NA

danjo23[is.na(danjo23)] <- 0
danjo28[is.na(danjo28)] <- 0

a <- append(danjo23[,1],danjo23[,5])
str(danjo23[,4])

str(as.vector(danjo23[,1]))

#H2３年度のデータ結合
H23.age <- NULL
H23.year <- NULL
H23.kyuyo <- NULL
H23.labor <- NULL

i <- 1
DF_23 <- NULL
for(i in 1:47){
  k<- 4*(i-1) + 1
  H23.age <- c(H23.age,danjo23[,k])
  l<- 4*(i-1) + 2
  H23.year <- c(H23.year,danjo23[,l])
  m <- 4*(i-1) + 3
  H23.kyuyo <- c(H23.kyuyo,danjo23[,m])
  n <- 4*(i-1) + 4
  H23.labor <- c(H23.labor,danjo23[,n])
}
DF_23 <- data.frame(H23.kyuyo,H23.labor,H23.age,H23.year)
head(DF_23,10)
length(H23.age)

#H28年度のデータ結合
H28.age <- NULL
H28.year <- NULL
H28.kyuyo <- NULL
H28.labor <- NULL

DF_28 <- NULL
for(i in 1:47){
  k<- 4*(i-1) + 1
  H28.age <- c(H28.age,danjo28[,k])
  l<- 4*(i-1) + 2
  H28.year <- c(H28.year,danjo28[,l])
  m <- 4*(i-1) + 3
  H28.kyuyo <- c(H28.kyuyo,danjo28[,m])
  n <- 4*(i-1) + 4
  H28.labor <- c(H28.labor,danjo28[,n])
}
DF_28 <- data.frame(H28.kyuyo,H28.labor,H28.age,H28.year)
head(DF_28,10)
length(H28.age)

#H23とH28を結合
age <- c(H23.age,H28.age)
year <- c(H23.year,H28.year)
kyuyo <- c(H23.kyuyo,H28.kyuyo)
labor <- c(H23.labor,H23.labor)

DF_2328 <- data.frame(age,year,kyuyo,labor)
nrow(DF_2328)
ncol(DF_2328)
n <- 47 * 91 * 2

#ダミー変数作成
#保育士[16,]

#保育士ダミー
D_hoiku <- NULL
D_hoiku <- vector(mode = "numeric",length=n)
for(i in 1:93){
  D_hoiku[(i-1)*91 + 16] <- 1
}

length(D_hoiku)
n/47

#看護師ダミー
D_nurse <- NULL
D_nurse <- vector(mode = "numeric",length=n)
for(i in 1:93){
  D_nurse[(i-1)*91 + 10] <- 1
  D_nurse[(i-1)*91 + 11] <- 1
  D_nurse[(i-1)*91 + 12] <- 1
}
str(D_nurse)

#時間ダミー
t <- n/2
D_time <- NULL
for(i in 1:n){
  if(i <= t){
    D_time <- c(D_time,0)
  }
  else{
    D_time <- c(D_time,1)
  }
}
length(D_time)
length(D_hoiku)

#都市ダミー
D_city <- NULL
D_city <- vector(mode = "numeric",length=n)

city <- c(11,13,14,23,26,27,28,34,40)
city2 <- city + 47

#11埼玉
#13東京
#14神奈川
#23愛知
#26京都
#27大阪
#28兵庫
#34広島
#40福岡

for(i in 1:9){
  k <- city[i]
  s <- k*91 + 1
  e <- k*91 +91
  D_city[s : e] <-1
}
for(i in 1:9){
  k <- city2[i]
  s <- k*91 + 1
  e <- k*91 +91
  D_city[s : e] <-1
}
str(D_city)

#交差項
D_cross <- D_hoiku * D_time
D_cross_nurse <- D_nurse * D_time
D_city_hoiku <- D_hoiku * D_city
D_city_nurse <- D_nurse * D_city
D_city_hoiku_time <- D_hoiku * D_time * D_city
D_city_nurse_time <- D_cross_nurse * D_city

length(D_city_hoiku_time)
nrow(DF_2328)

DF <- NULL
DF_2328[DF_2328 == 0] <- NA
head(DF_2328)

DF <- data.frame(DF_2328,D_hoiku,D_nurse,D_time,D_city,D_cross,D_cross_nurse,D_city_hoiku,D_city_nurse,D_city_hoiku_time,D_city_nurse_time)
DF <- DF[complete.cases(DF),]
head(DF,16)
length(kyuyo)

res <- lm(kyuyo ~ age + year + D_hoiku +D_time + D_cross ,data =DF)
summary(res)
res1 <- lm(labor ~  age + year + D_hoiku +D_time + D_cross ,data =DF)
summary(res1)


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
res_salary1 <- lm(salary.zangyo ~ age + year + time + trend + trend2 + hoiku + kaigo + cross1 + cross2, data=df)
res_salary2 <- lm(salary ~ age + year + time +trend + trend2 + hoiku + kaigo + cross1 + cross2, data=df)
res_bonus <- lm(bonus ~ age + year + time + trend + trend2 + hoiku + kaigo + cross1 + cross2, data=df)
res_labor <- lm(labor ~ age + year + time + trend + trend2 + hoiku + kaigo + cross1 + cross2, data=df)
summary(res_salary1)
summary(res_salary2)
summary(res_bonus)
summary(res_labor)


#日説明変数の散布図を意識してログをとる
res_salary3 <- lm(log(salary.zangyo) ~ age + year + time + trend + trend2 +hoiku + kaigo + cross1 + cross2, data=df)
res_salary4 <- lm(log(salary) ~ age + year + time +trend + trend2 + hoiku + kaigo + cross1 + cross2, data=df)
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
#[女] データをプールしてDD分析　before[23:24],after[27:28]
#==================================================================

df <- NULL
df <- rbind(josei_h23[,-1],josei_h24[,-1],josei_h27[,-1],josei_h28[,-1])

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

