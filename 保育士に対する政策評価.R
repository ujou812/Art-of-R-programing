#保育士の給与労働者数についての実証実験

#データはH23とH26の給与と労働者数に関する全職種についてのデータの差
#したがって、実証方法はDID

#職種名を含んだデータフレームの作成

names <- names$names

num <- c(1:129)
work <- data.frame(num,names,research,dummy0,dummy1,dummy2)
head(work)

#research <- research[,c(-3,-4)]
research <- research[complete.cases(research),]
head(research)

#NA値かNull値かについて対応する必要がある
research.female[c(112,118,125),] <- list(NA,NA,NA,NA)

work_male <- data.frame(num,names,research.male,dummy0,dummy1,dummy2)
work_female <- data.frame(num,names,research.female,dummy0,dummy1,dummy2) 


work_male <- work_male[complete.cases(work_male),]
work_female <- work_female[complete.cases(work_female),]

#保育士ダミー
dummy0 <- NULL
for(i in 1:129){
  if(i == 21) dummy0 <- c(dummy0,1)
  else dummy0 <- c(dummy0,0)
}
dummy0

#医療関係ダミー
dummy1 <- NULL
for(i in 1:129){
  if(i >= 9 && i <= 20) dummy1 <- c(dummy1,1)
  else dummy1 <- c(dummy1,0)
}
dummy1

#介護職ダミー
dummy2 <- NULL
for(i in 1:129){
  if(i >= 22 && i <= 24) dummy2 <- c(dummy2,1)
  else dummy2 <- c(dummy2,0)
}
dummy2

#交差項の作成
#保育士ダミー* age_dif / year_dif

research$cross_age0 <- dummy0*research$age_dif
research$cross_year0 <- dummy0*research$year_dif
str(research$cross_age0)

cross_age <- research$cross_age0
cross_year <- research$cross_year0

str(research$cross_year2)

#介護職ダミー* age_dif / year_dif
research$cross_age2 <- dummy2*research$age_dif
research$cross_year2 <- dummy2*research$year_dif

head(research)

head(work)
work[22,]

head(work)

result1 <- summary(lm(y_dif ~ dummy0 + dummy1 + dummy2 + age_dif + year_dif,data = work))
result2 <-summary(lm(lnum_dif ~ dummy0 + dummy1 + dummy2 + age_dif + year_dif,data = work))

#ダミー変数との交差項を導入
result1_cross <- summary(lm(y_dif ~ dummy0 + dummy1 + dummy2 + age_dif + year_dif + cross_age2 + cross_year2,data = work))
result1_cross
result1_cross_y <- summary(lm(lnum_dif ~ dummy0 + dummy1 + dummy2 + age_dif + year_dif  + cross_age2 + cross_year2,data = work))
result1_cross_y

result3 <- summary(lm(y_dif ~ dummy0 + dummy1 + dummy2 + age_dif + year_dif,data = work_male))
result4 <-summary(lm(lnum_dif ~ dummy0 + dummy1 + dummy2 + age_dif + year_dif,data = work_male))

result5 <- summary(lm(y_dif ~ dummy0 + dummy1 + dummy2 + age_dif + year_dif,data = work_female))
result6 <-summary(lm(lnum_dif ~ dummy0 + dummy1 + dummy2 + age_dif + year_dif,data = work_female))

work_female

result1
result2
result3
result4
result5
result6

#=====================================================================
#ちゃんとしたDD分析?
#=====================================================================

head(hoikushi_danjo)

kyuuyos <-  c(hoikushi_danjo$kyuuyo_H23,hoikushi_danjo$kyuuyo_H28)
labors <- c(hoikushi_danjo$labor_H23,hoikushi_danjo$labor_H28)
ages <- c(hoikushi_danjo$age_H23,hoikushi_danjo$age_H28)
years <- c(hoikushi_danjo$year_H23,hoikushi_danjo$year_H28)

head(names)

DF <- NULL
DF <- data.frame(names,hoikushi_danjo,d_hoikushi)
head(DF)

#保育士ダミー
d_hoiku <- NULL
for(i in 1:129){
  if(i == 21) d_hoiku <- c(d_hoiku,1)
  else d_hoiku <- c(d_hoiku,0)
}
d_hoiku

dummy_hoikushi <- c(d_hoikushi,d_hoikushi)

#医療関係ダミー
d_medical <- NULL
for(i in 1:129){
  if(i >= 8 && i <= 20) d_medical <- c(d_medical,1)
  else d_medical <- c(d_medical,0)
}
d_medical

dummy_medical <- c(d_medical,d_medical)

#介護職ダミー
d_kaigo <- NULL
for(i in 1:129){
  if(i >= 22 && i <= 24) d_kaigo <- c(d_kaigo,1)
  else d_kaigo <- c(d_kaigo,0)
}
d_kaigo

dummy_kaigo <- c(d_kaigo,d_kaigo)

#医療介護福祉ダミー(医者除く)
d_social <- NULL
for(i in 1:129){
  if(i >= 9 && i <= 24) d_social <- c(d_social,1)
  else d_social <- c(d_social,0)
}
d_social

dummy_social <- c(d_social,d_social)

#時間ダミー
dummy_t <- NULL
for(i in 1:258){
  if(i <= 129) dummy_t <- c(dummy_t,0)
  else dummy_t <- c(dummy_t,1)
}
dummy_t



#データフレーム作成

DF_danjo <- NULL
DF_danjo <- data.frame(kyuuyos,labors,ages,years,dummy_social,dummy_hoikushi,dummy_kaigo,dummy_t,d_hoiku_t,d_kango_t,d_social_t)
head(DF_danjo,10)

#交差項のダミー
d_hoiku_t <- dummy_hoikushi * dummy_t
d_hoiku_t

d_kango_t <- dummy_kaigo * dummy_t
d_kango_t

d_social_t <- dummy_social * dummy_t

result1 <- summary(lm(kyuuyos ~ ages + years + dummy_hoikushi + dummy_kaigo + dummy_t + d_hoiku_t + d_kango_t,data = DF_danjo))
result2 <-summary(lm(labors ~ ages + years + dummy_hoikushi + dummy_kaigo + dummy_t + d_hoiku_t + d_kango_t,data = DF_danjo))

#医療介護福祉
res1 <- summary(lm(kyuuyos ~ ages + years + dummy_social + dummy_t + d_social_t ,data = DF_danjo))
res2 <- summary(lm(labors ~ ages + years + dummy_social + dummy_t + d_social_t ,data = DF_danjo))

#=====================================================
#女性バージョン
#=====================================================

head(hoikushi_josei)

kyuuyos_f <-  c(hoikushi_josei$kyuuyo_H23,hoikushi_josei$kyuuyo_H28)
labors_f <- c(hoikushi_josei$labor_H23,hoikushi_josei$labor_H28)
ages_f <- c(hoikushi_josei$age_H23,hoikushi_josei$age_H28)
years_f <- c(hoikushi_josei$year_H23,hoikushi_josei$year_H28)

head(names)

DF_josei <- NULL
DF_josei <- data.frame(kyuuyos,labors,ages,years,dummy_hoikushi,dummy_kaigo,dummy_social,dummy_t,d_hoiku_t,d_kango_t,d_social_t)
DF_josei <- DF_josei[complete.cases(DF_josei),]
head(DF_josei,10)

result3 <- summary(lm(kyuuyos_f ~ ages_f + years_f + dummy_hoikushi + dummy_kaigo + dummy_t + d_hoiku_t + d_kango_t,data = DF_josei))
result4 <-summary(lm(labors_f ~ ages_f + years_f + dummy_hoikushi + dummy_kaigo + dummy_t + d_hoiku_t + d_kango_t,data = DF_josei))

res3 <- summary(lm(kyuuyos_f ~ ages_f + years_f + dummy_social + dummy_t + d_social_t ,data = DF_josei))
res4 <- summary(lm(labors_f ~ ages_f + years_f + dummy_social + dummy_t + d_social_t ,data = DF_josei))
