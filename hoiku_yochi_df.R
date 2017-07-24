setwd("/Users/Ryunosuke/desktop/R")

hoiku_df<- NULL

hoiku_df <- read.csv("/Users/Ryunosuke/desktop/R/hoikushi_panel.csv", fileEncoding="CP932", header=TRUE, quote="\"")
yochi_df <- read.csv("/Users/Ryunosuke/desktop/R/youchien_panel.csv", fileEncoding="CP932", header=TRUE, quote="\"")

hoiku_df$Labor <- hoiku_df$Labor * 10

head(hoiku_df)
head(yochi_df)

#as.data.

#hoiku_df_list[] <- NULL
#for ( i in 1:47){
sample_df <- as.data.frame(hoiku_df[hoiku_df$Prefecture == i,])
list[] <- sample_df
}

#===============================================================================
#ビジュアル化
#===============================================================================

sample <- hoiku_df[hoiku_df$Prefecture == 1,]$Salary1
y <- sample$Salary1
x <- c(2009,2010,2011,2012,2013,2014,2015,2016)

i <- 1
for(i in 2:47){
  y <- hoiku_df[hoiku_df$Prefecture == i,]$Salary1_s
  par(new=T)
  plot(x ,y, type="l",ylim = c(150,350),main = "Salary1_s")
}

i <- 1
for(i in 2:47){
  y <- hoiku_df[hoiku_df$Prefecture == i,]$Bonus_s
  par(new=T)
  plot(x ,y, type="l",ylim = c(150,350),main = "Bonus_s")
}

i <- 1
for(i in 2:47){
  y <- hoiku_df[hoiku_df$Prefecture == i,]$Labor
  par(new=T)
  plot(x ,y, type="l",ylim = c(0,40000),main = "Labor")
  }
hist(hoiku_df$Labor)

i <- 1
for(i in 2:47){
  y <- hoiku_df[hoiku_df$Prefecture == i,]$ln_Labor
  par(new=T)
  plot(x ,y, type="l",ylim = c(0,12),main = "ln_Labor")
}

i <- 1
for(i in 2:47){
  y <- hoiku_df[hoiku_df$Prefecture == i,]$children_total
  ln_y <- log(y)
  par(new=T)
  plot(x ,ln_y, type="l",ylim = c(9,13),main = "children")
}

i <- 1
for(i in 2:47){
  y <- hoiku_df[hoiku_df$Prefecture == i,]$Bounus_s
  par(new=T)
  plot(x ,y, type="l",ylim = c(0,1100),main = "Bonus_s")
}
hist(hoiku_df$Bounus_s)


i <- 1
for(i in 2:47){
  y <- hoiku_df[hoiku_df$Prefecture == i,]$children_total
  par(new=T)
  plot(x ,y, type="l",ylim = c(0,210000), main = "children")
}

hist(hoiku_df$Salary1_s)
a <- log(hoiku_df$Salary1_s)
a2 <- log(hoiku_df$Salary1_s)
b <- log(hoiku_df$Labor)
hist(a)
hist(hoiku_df$Salary2_s)
hist(hoiku_df$Labor)
hist(b)
hist(hoiku_df$children_total)
c <- log(hoiku_df$children_total)
hist(c)
hist(hoiku_df$chil_lab)
hist(zzz)
#===============================================================================
#Regression Discontinuity Design
#===============================================================================

#Dumy_policy

D_policy <- NULL
hoiku_df$D_policy <- NULL
str(hoiku_df)
47*4
for(i in 1:376){
  if(i >=189) D_policy[i] <- 1
  else D_policy[i] <- 0
}

chil_lab <-hoiku_df$children_total / hoiku_df$Labor
pub_all <- hoiku_df$facilities_public /hoiku_df$facilities_total
chil_facil <- hoiku_df$children_total/hoiku_df$facilities_total

hoiku_df$chil_lab <- NULL
hoiku_df$D_policy <- D_policy
hoiku_df$ln_Sal1_s <- a
hoiku_df$ln_Sal2_s <- a2
hoiku_df$ln_Labor <- b
hoiku_df$ln_child <- c
head(hoiku_df)

#===============================================================================
#Interrupted Time-Series Analysis
#===============================================================================
time <- NULL
post_time <- NULL
for(i in 1:376){
  post_time[i] <- hoiku_df$Year[i] - 2012
}

time[48]
post_time
hoiku_df$post_time <- post_time
head(hoiku_df)
str(post_time)

Model_ITA <- lm(Salary1_s ~ Duration + Labor + children_total + facilities_total + Year + D_policy + post_time, data = hoiku_df)
summary(Model_ITA)

Model_ITA2 <- lm(Salary2_s ~ Duration + Labor + children_total + facilities_total + Year + D_policy + post_time, data = hoiku_df)
summary(Model_ITA2)

Model_ITA3 <- lm(Bounus_s ~ Duration + Labor + children_total + facilities_total  + D_policy + post_time, data = hoiku_df)
summary(Model_ITA3)

Model_ITA4 <- lm(ln_Labor ~ Duration + children_total + facilities_total  + time + D_policy + post_time, data = hoiku_df)
summary(Model_ITA4)

#===============================================================================
#Simple OLS
#===============================================================================
Model1 <- lm(Salary1_s ~ Duration + Labor + children_total + chil_facil + facilities_total + pub_all + Year,data = hoiku_df)
summary(Model1)

#===============================================================================
#Simple RDD
#===============================================================================

Model2 <- lm(Salary1_s ~ Duration + Labor + children_total +  facilities_total +  D_policy,data = hoiku_df)
Model3 <- lm(Salary2_s ~ Duration + Labor + children_total +  facilities_total +  D_policy,data = hoiku_df)
Model4 <- lm(Bounus_s ~ Duration + Labor + children_total +  facilities_total +  D_policy ,data = hoiku_df)
Model4_l <- lm(ln_Labor ~ Duration + children_total +  facilities_total +  D_policy ,data = hoiku_df)

summary(Model2)
summary(Model3)
summary(Model4)
summary(Model4_l)

#===============================================================================
#Panel Model
#===============================================================================

Model5 <- plm(Salary1_s ~ Duration + Labor + children_total +  facilities_total +  D_policy,data = df,model = "pooling")
Model6 <- plm(Salary1_s ~ Duration + Labor + children_total +  facilities_total +  D_policy,data = df,model = "within")
Model7 <- plm(Salary1_s ~ Duration + Labor + children_total +  facilities_total +  D_policy,data = df,model = "random")
summary(Model5)
summary(Model6)
summary(Model7)

Model8 <- plm(ln_Labor ~ Duration + children_total + facilities_total + D_policy,data = df,model = "pooling")
Model9 <- plm(ln_Labor ~ Duration + children_total + facilities_total + D_policy,data = df,model = "within")
Model10 <- plm(ln_Labor ~ Duration + children_total + facilities_total + D_policy,data = df,model = "random")
summary(Model8)
summary(Model9)
summary(Model10)

#===============================================================================
#Spatial Econometrics
#===============================================================================
df <- NULL
head(hoiku_df)
order(hoiku_df$Prefecture)
df <- hoiku_df[order(hoiku_df$Prefecture),]
df_c <- df
head(df,10)
df[,2] <- df_c[,1]
df[,1] <- df_c[,2]

#空間ラグ型ランダム効果モデル
Model11 <- spml(Salary1_s ~ Duration + Labor + children_total + facilities_total + D_policy,
                listw = W,
                data = df, model = "random",spatial.error = "n",lag = TRUE)
summary(Model11)

Model12 <- spml(ln_Labor ~ Duration + children_total + facilities_total + D_policy,
                listw = W,
                data = df, model = "random",spatial.error = "n",lag = TRUE)
summary(Model12)

#残差
Model4$residuals


#空間誤差ランダム効果モデル
Model13 <- spml(Salary1_s ~ Duration + Labor + children_total + facilities_total + D_policy,
                listw = W,
                data = df, model = "random",spatial.error = "b",lag = FALSE)
summary(Model13)

Model14 <- spml(ln_Labor ~ Duration + children_total + facilities_total + D_policy,
                listw = W,
                data = df, model = "random",spatial.error = "b",lag = FALSE)
summary(Model14)

#KKPModel(ML推定)
Model15 <- spml(Salary1_s ~ Duration + Labor + children_total + facilities_total + D_policy,
               listw = W,
               data = df, model = "random",spatial.error = "kkp",lag = FALSE)
summary(Model15)

Model16 <- spml(ln_Labor ~ Duration  + children_total + facilities_total + D_policy,
                listw = W,
                data = df, model = "random",spatial.error = "kkp",lag = FALSE)
summary(Model16)

#空間ラグ型固定効果モデル
Model17 <- spml(Salary1_s ~ Duration + Labor + children_total + facilities_total + D_policy,
                listw = W,
                data = df, model = "random",spatial.error = "none",lag = TRUE)
summary(Model17)

Model18 <- spml(ln_Labor ~ Duration  + children_total + facilities_total + D_policy,
                listw = W,
                data = df, model = "random",spatial.error = "none",lag = TRUE)
summary(Model18)

#空間誤差型固定効果モデル
Model19 <- spml(Salary1_s ~ Duration + Labor + children_total + facilities_total + D_policy,
                listw = W,data = df, model = "random",spatial.error = "b",lag = TRUE)
summary(Model19)

Model20 <- spml(ln_Labor ~ Duration  + children_total + facilities_total + D_policy,
                listw = W,
                data = df, model = "random",spatial.error = "b",lag = TRUE)

summary(Model20)
