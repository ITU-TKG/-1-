wash <- read.csv("./洗濯に関するアンケート.csv/洗濯に関するアンケート.csv")
View(wash)

# 行列処理 ---------------------------------------------------------------------

#na値処理
wash <- na.omit(wash)

#行を削除
wash <- wash[wash$MBTIの4つ目のアルファベットはなんですか != "わかんない",]
print(nrow(wash))#行数の確認

#列の処理
wash <- wash[,-1:-5]
print(ncol(wash))#自分の担当分に分けた



#カテゴリ値処理
#mbti
#wash$MBTIの4つ目のアルファベットはなんですか <- as.numeric(factor(wash$MBTIの4つ目のアルファベットはなんですか))




# label作成 -----------------------------------------------------------------


#labelを作る
wash_mbti4 <- wash[,1]#目的変数
wash_data <- wash[,-1] #wash_dataは一時的においておく場所


# 数値とカテゴリに分ける -------------------------------------------------------------


#数値とカテゴリに分ける
wash_data_text <- wash_data[,c(1,7,8,12,14,17)]
View(wash_data_text)#カテゴリ

wash_data_num <- wash_data[,c(2,3,4,5,6,9,10,11,13,15,16,18)]
View(wash_data_num)#num 
#num 1~5のデータを0~1データにしたい 
scalenum <- scale(wash_data_num)#標準化



# dummy変数変換 ---------------------------------------------------------------
#dummy変数変換
install.packages("fastDummies")
devtools::install_github("jacobkap/fastDummies")
library(fastDummies)

dummy_type <- dummy_cols(wash_data_text)
dummy_type <- dummy_type[,-1:-6] #dummy作成

wash_mbti4_2 <- dummy_cols(wash_mbti4)

dummy_num <- dummy_cols(wash_data_num)



# dummy_type1 -------------------------------------------------------------


#居住、選択のめんどくさいところ
dummy_type1 <- dummy_type[,c(-1,-3,-7,-20,-21,-22,-23,-24,-25,-26,-27,-28)]
#相関関係なし
dummy_type1 <- dummy_type1[,c(
  -3,-4,-7,-8,-9,-15
  )]

#標準化
sca1 <- scale(dummy_type1)

# dummy_type2 -------------------------------------------------------------


#他人と洗濯の課題点
dummy_type2 <- dummy_type[,c(20:28)]
#相関関係なし
dummy_type2 <- dummy_type2[,c(
  -1,-3,-4,-6,-9
)]

gabage <- dummy_type1[,1:6] #dummy以外を格納

#標準化
sca2 <- scale(dummy_type2)

# 使う変数（仮） -----------------------------------------------------------------

#標準化 標準化dummy+標準化washdatanum    ####使わん
new_data1 <- cbind(sca1,scalenum[,1:7])
new_data2 <- cbind(sca2,scalenum[,8:11])

#dummyデータ+washNum_deleted
new_data3 <- cbind(dummy_type1,washNum_deleted[,1:35])   ######使う！！！
new_data3 <- new_data3[,c(-14:-23)]
new_data3 <- new_data3[,-29]
new_data3 <- new_data3[,-2]



new_data3 <- data.frame(lapply(new_data3, as.factor))  # 全列を因子型に変換

new_data4 <- cbind(dummy_type2,washNum_deleted[,36:60])
new_data4 <- new_data4[,c(-25:-29)]


new_data4 <- data.frame(lapply(new_data4, as.factor))  # 全列を因子型に変換


#標準化mbti4
wash_mbti4_2 <- dummy_cols(wash_mbti4)
wash_mbti4_2 <- wash_mbti4_2[,-1]

#一旦他ソフトで編集　###無視
write.csv(new_data3,file="./new_data3.csv")
write.csv(new_data4,file="./new_data4.csv")
write.csv(wash_mbti4_2,file="./mbti.csv")
write.csv(wash,file="./wash.csv")
write.csv(wash_data_num,file="./washnum.csv")

library(openxlsx)
#5段階評価データ2値変換 ###済み
washNum <- read.xlsx("./washnum.xlsx")
washNum_deleted <- washNum[,c(-1,-2,-8,-14,-20,-26,-32,-38,-44,-50,-56,-62,-68)]

# 多重対応分析 --------------------------------------------------------------------
install.packages("FactoMineR")
install.packages("factoextra")
library(FactoMineR)
library(factoextra)

taju1 <- MCA(new_data3,graph = FALSE)
taju2 <- MCA(new_data4,graph = FALSE)
summary(taju1)
summary(taju2)

library(ggplot2)
fviz_screeplot(taju1,addlabels=TRUE)
fviz_screeplot(taju2,addlabels=TRUE)





# ロジスティック回帰 ---------------------------------------------------------------
#サービスを利用したい人の特徴分析
log1 <- glm(wash_mbti4_2~new_data1,family=binomial)
log2 <- glm(wash_data_num[,12]~new_data2,family=binomial)
log3 <- glm(wash_data_num[,12]~new_data3,family=binomial)

# 決定木・ランダムフォレスト ---------------------------------------------------------------
library(rpart)
model1 = rpart(wash_data_num[,12]~new_data1,control = rpart.control(maxdepth = 4)) 
#可視化
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(model1)

# なんとなく分析 -----------------------------------------------------------------


model1 <- chisq.test(
  wash_data_num$洗濯をするのがめんどくさいと思いますか,
  wash_data_num$洗濯代行サービスがあるとしたら使いたいと思いますか,
  simulate.p.value = TRUE
)
model2 <- chisq.test(
  wash_mbti4,
  wash_data_num$洗濯をするのがめんどくさいと思いますか,
  simulate.p.value=TRUE
)
model3 <- chisq.test(
  wash_data_num$洗濯物が乾かず困ったことはありますか,
  wash_data_num$洗濯代行サービスがあるとしたら使いたいと思いますか,
  simulate.p.value = TRUE
)
model4 <- chisq.test(
  wash_data_num$洗濯をする時間がないと思ったことはありますか,
  wash_data_num$洗濯代行サービスがあるとしたら使いたいと思いますか,
  simulate.p.value = TRUE
)

model5<- chisq.test(
  new_data3,new_data4,
  simulate.p.value = TRUE
)


# 主成分分析 -------------------------------------------------------------------

install.packages("remotes") 
remotes::install_github("vqv/ggbiplot") 
library(ggbiplot)

library(devtools) #実行不可
install.github("vqv/ggbiplot")

#相関関係確認
cor_mat1 <- cor(dummy_type1)
cor_mat2 <- cor(dummy_type2)#処理後は前処理に飛ぶ
#標準化
sca1 <- scale(dummy_type1)
sca2 <- scale(dummy_type2)

#データ結合
new_data1 <- cbind(sca1,scalenum[,1:7])
new_data2 <- cbind(sca2,scalenum[,8:11])


result4 <- prcomp(sca1,center=TRUE,scale.=TRUE)
result5 <- prcomp(sca2,center=TRUE,scale.=TRUE)

result6 <- prcomp(new_data1,center=TRUE,scale.=TRUE)
result7 <- prcomp(new_data2,center=TRUE,scale.=TRUE)


result8 <- prcomp(new_data3,center=TRUE)
result9 <- prcomp(new_data4,center=TRUE)

ggbiplot(result8)
ggbiplot(result9)
