getwd()
setwd("/Users/Ryunosuke/Desktop/R")
zaimu <- read.csv("~/Desktop/R/stocks.csv", fileEncoding="CP932", header=TRUE, quote="\"")
View(zaimu)
#zaimu ：データベース
#meigara :　銘柄番号 スカラ/ベクトル
#year : 年数vector

meigara <- 3
mei <- (zaimu$日経会社コード == meigara)
zaimu[mei,]
str(mei)
#i <- 1
year <- c(2007:2016)


keiei <- function(zaimu,meigara,year){
  #銘柄で検索
  mei <- (zaimu$日経会社コード == meigara)
  data_stockcoad <- zaimu[mei,]
  data_stockcoad <- data_stockcoad[complete.cases(data_stockcoad),]
  #分析項目名称
  name1 <- c("自己資本比率","固定比率","固定長期適合率","流動比率","当座比率")
  name2 <- c("売上総利益率","売上高営業利益率","売上高経常利益率","売上高当期利益率")
  name3 <- c("棚卸資産回転率","売上債権回転率","有形固定資産回転率","純資本回転率")
  name4 <- c("収益力指数","支払能力指数","活力指数","持久力指数","成長力指数","企業力指数")
  #name5 <- c("労働生産性","従業員一人当たり売上高","付加価値率","労働装備率","設備生産性","労働分配率","一人当たり人件費")
  
  #i<-1
  #年数で選択
  n <- length(year)
  for(i in 1:n){
    ye <- (data_stockcoad$決算期 == year[i])
    data <- data_stockcoad[ye,] 
    
    junshisan <- data$純資産合計.資本合計
    fusai <- data$負債合計
    koteishisan <- data$固定資産.非流動資産
    koteifusai <- data$負債合計 - data$流動負債
    ryuudoushisan <- data$流動資産
    ryuudoufusai <- data$流動負債
    touzashisan <- data$現金.預金.現金及び現金同等物 + data$X.ＩＦＲＳ..うち受取手形.売掛金. + data$X.受取手形.
    uriagesourieki <- data$売上総利益.累計.
    uriagedaka <- data$売上高.営業収益.累計.
    eigyourieki <- data$営業利益.累計.
    keijourieki <- data$経常利益.税金等調整前当期純利益.累計.
    toukijunrieki <- data$当期純利益.連結..累計.
    tanaoroshishisan <- data$棚卸資産
    uriagesaiken <- data$X.ＩＦＲＳ..うち受取手形.売掛金. + data$X.受取手形.
    yuukeikoteishisan <- data$有形固定資産
    #jinkenhi <- data$X.販.人件費.福利厚生費 + data$X.製.労務費.福利厚生費
    #chintairyou <- data$X.販.賃借料 + data$X.製..うち賃借料.
    #sozeikouka <- data$X.販.租税公課 + data$X.製..うち租税公課.
    genkasyoukyakuhi <- data$減価償却費.累計.
    juugyouinsuu <- data$従業員数.単位.人.
    jikoshihon <- data$株主資本
    shisan <- data$資産合計
    junshihon <- junshisan #純資本の勘定項目チェック
    
    #自己資本比率 = 純資産/(負債+純資産)
    jikoshihonhiritsu <- junshisan / (fusai + junshisan)
    #固定比率 = 固定資産/純資産
    koteihiritsu <- koteishisan / junshisan
    #固定長期適合率 = 固定資産 / (純資産 + 固定負債)
    koteityoukiteigouritsu <- koteishisan / (junshisan + koteifusai)
    #流動比率 = 流動資産/流動負債
    ryuudouhiritsu <- ryuudoushisan / ryuudoufusai
    #当座比率 = 当座資産/流動負債
    touzahiritsu <- touzashisan / ryuudoufusai
    
    #売上総利益率 = 売上総利益/売上高
    uriagesouriekiritsu <- uriagesourieki / uriagedaka
    #売上高営業利益率 = 営業利益/売上高
    uriagedakaeigyouriekiritsu <- eigyourieki / uriagedaka
    #売上高経常利益率 = 経常利益/売上高
    uriagedakakeijourieki <- keijourieki/uriagedaka
    #売上高当期利益率 = 当期純利益/売上高 
    uriagedakatoukiriekiritsu <- toukijunrieki / uriagedaka
    
    #貸借対照表と損益計算書による資本運用効率の分析
    
    #棚卸資産回転率 = 売上高 / 棚卸資産
    tanaoroshishisankaitenritsu <- uriagedaka / tanaoroshishisan
    #売上債権回転率 = 売上高 / 売上債権
    uriagesaikenkaitenritsu <- uriagedaka / uriagesaiken
    #有形固定資産回転率 = 売上高 / 有形固定資産
    yuukeikoteishisankaitenritsu <- uriagedaka/yuukeikoteishisan
    #純資本回転率 = 売上高 / 純資本
    junshihonkaitenritsu <- uriagedaka / junshihon
    
    #企業力指数による伝統的な分析手法の総合
    
    #収益力指数 = 売上高/(売上高-経常利益)
    syuuekiryokushisuu <- uriagedaka/(uriagedaka-keijourieki)
    #支払能力指数 = 流動資産/負債
    shiharainouryokusisuu <- ryuudoushisan / fusai
    #活力指数 = 売上高/資産
    katsuryokushisuu <- uriagedaka / shisan
    #持久力指数 = 自己資本/負債
    jikyuuryokushisuu <- jikoshihon / fusai
    #成長力指数 = 資産/(資産-当期純利益)
    seityouryokushisuu <- shisan /(shisan-toukijunrieki)
    #企業力指数 = (収益力指数 + 支払能力指数 + 活力指数 + 持久力指数 + 成長力指数)/ 5
    kigyouryokushisuu <- (syuuekiryokushisuu + shiharainouryokusisuu + katsuryokushisuu + jikyuuryokushisuu + seityouryokushisuu)/5
    
    #付加価値分析
    
    #付加価値(控除法) = 売上高 - 前給付費用
    #付加価値(加算法) = 営業利益 + 人件費 + 賃貸料 + 租税公課 + 減価償却費
    #fukakachi <- eigyourieki + jinkenhi + chintairyou + sozeikouka + genkasyoukyakuhi
    #労働生産性 ＝付加価値 /　従業員数
    #roudouseisansei <- fukakachi / juugyouinsuu
    #従業員一人当たり売上高 = 売上高/従業員数
    #hitoriatariuriagedaka <- uriagedaka/juugyouinsuu
    #付加価値率 = 付加価値/売上高
    #fukakachiritsu <- fukakachi / uriagedaka
    #労働装備率 = 有形固定資産額 / 従業員数
    #roudousoubiritsu <- yuukeikoteishisan / juugyouinsuu
    #設備生産性 = 付加価値額/有形固定資産額
    #setsubiseisansei <- fukakachi / yuukeikoteishisan
    #労働分配率 = 人件費/付加価値額 
    #roudoubunpairitsu <- jinkenhi / fukakachi
    #一人当たり人件費 = 労働生産性 * 労働分配率
    #hitoriatarijinkenhi <- roudouseisansei * roudoubunpairitsu
    
    if(i == 1){
      taishaku <- data.frame(name1, c(jikoshihonhiritsu,koteihiritsu,koteityoukiteigouritsu,ryuudouhiritsu,touzahiritsu))
      soneki <- data.frame(name2,c(uriagesouriekiritsu,uriagedakaeigyouriekiritsu,uriagedakakeijourieki,uriagedakatoukiriekiritsu))
      kouritsu <- data.frame(name3,c(tanaoroshishisankaitenritsu,uriagesaikenkaitenritsu,yuukeikoteishisankaitenritsu,junshihonkaitenritsu))
      kigyouryoku <- data.frame(name4,c(syuuekiryokushisuu,shiharainouryokusisuu,katsuryokushisuu,jikyuuryokushisuu,seityouryokushisuu,kigyouryokushisuu))
      #fukaka <- data.frame(name5,c(roudouseisansei,hitoriatariuriagedaka,fukakachiritsu,roudousoubiritsu,setsubiseisansei,roudoubunpairitsu,
      #                                                        hitoriatarijinkenhi))
    }else{
      taishaku <- data.frame(taishaku,c(jikoshihonhiritsu,koteihiritsu,koteityoukiteigouritsu,ryuudouhiritsu,touzahiritsu))
      soneki <- data.frame(soneki,c(uriagesouriekiritsu,uriagedakaeigyouriekiritsu,uriagedakakeijourieki,uriagedakatoukiriekiritsu))
      kouritsu <- data.frame(kouritsu,c(tanaoroshishisankaitenritsu,uriagesaikenkaitenritsu,yuukeikoteishisankaitenritsu,junshihonkaitenritsu))
      kigyouryoku <- data.frame(kigyouryoku,c(syuuekiryokushisuu,shiharainouryokusisuu,katsuryokushisuu,jikyuuryokushisuu,seityouryokushisuu,kigyouryokushisuu))
      #fukaka <- data.frame(fukaka,c(roudouseisansei,hitoriatariuriagedaka,fukakachiritsu,roudousoubiritsu,setsubiseisansei,roudoubunpairitsu,
      #                                hitoriatarijinkenhi))
      
    }
  }
  
  tais <- c("name")
  sone <- c("name")
  kour <- c("name")
  kigy <- c("name")
  #fuka <- c("name")
  
  for(i in 1:n){
    y[i] <- paste(year[i],"年",seq = "")
    tais <- c(tais,y[i])
    sone <- c(sone,y[i])
    kour <- c(kour,y[i])
    kigy <- c(kigy,y[i])
    #fuka <- c(fuka,y[i])
  }
  
  colnames(taishaku) <- c(tais)
  colnames(soneki) <- c(sone)
  colnames(kouritsu) <- c(kour)
  colnames(kigyouryoku) <- c(kigy)
  #colnames(fukaka) <- c(fuka)
  
  
  #result <- list(taishaku,soneki,kouritsu,kigyouryoku,fukaka)
  #resul <- rbind(taishaku,soneki,kouritsu,kigyouryoku,fukaka)
  
  result <- rbind(taishaku,soneki,kouritsu,kigyouryoku)
  resul <- rbind(taishaku,soneki,kouritsu,kigyouryoku)
  
  assign(paste("analysis_", meigara, sep=""),result)
  
  filename <- paste(data_stockcoad$企業名[1],".csv",collapse = "")
  setwd("/Users/Ryunosuke/Desktop/R/zaimubunseki")
  write.csv(fileEncoding="CP932",resul,file =filename ,row.names=F)
  
  return(result)
}

year <- c(1985:2008)

fac <- factor(zaimu$日経会社コード)
length(levels(fac))
#k<-1
results <- NULL
for(k in 1:3627){
  mei <- (zaimu$日経会社コード == levels(fac)[k])
  data_stockcoad <- zaimu[mei,]
  meigara <- levels(fac)[k]
  year <- data_stockcoad$決算期
  keiei(zaimu,meigara,year)
}

