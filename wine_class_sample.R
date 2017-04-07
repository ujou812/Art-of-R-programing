#例 ワインのクラス分類

library(httr) #URL先からデータをDLするライブラリをインポート

#Web上からデータをインポートして、変数に格納
geturl <- GET("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data")
dat <- read.csv(textConnection(content(geturl)), header=F)

#データの先頭10コを表示
head(dat)

#データに各列に名前をつける/データフレーム化（エクセル式の形）に形成
names(dat) <- c("class","Alchool","Malic_acid","Ash","Alcalinity_of_ash", 
                "Magnesium","Total_phenols","Flavanoids","Nonflavanoid_phenols",
                "Proanthocyanins","Color_intensity","Hue","OD280_OD315","Proline")

dat <- data.frame(dat)

str(dat)#データフレームの概要

#ワインのランク付けに関して要因分析のための回帰分析を行う
result <- lm (class ~ Alchool + Malic_acid +Ash + Alcalinity_of_ash +
                Magnesium +Total_phenols +Flavanoids +Nonflavanoid_phenols 
              +Proanthocyanins +Color_intensity + Hue +OD280_OD315 + Proline,data =dat)

result #結果を表示
summary(result)　#結果の要約を表示

#データをプロットする
plot(dat$class)


