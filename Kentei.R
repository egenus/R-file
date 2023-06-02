library(tidyverse)

setwd("C:/R/random")

rawdata<-read_csv("0417_shoot.csv") %>% 
  na.omit()

#データを横持ちに
data<-rawdata %>% 
  pivot_wider(names_from = accession,values_from = long)

#for文でつかうベクトルの取得
list=colnames(data[3:length(data)])

for (i in list) {
  
  #selectで名前を必要な列を取得するのと列名を変更
  dx<-data %>% 
    dplyr::select(2,"length"=i) 
  
  dx <- dplyr::select(dx, "fx" = 1,"vx"=2, dplyr::everything())
  
  #fxの列をfactor型，vxの列をnumeric型にする
  fx=as.factor(dx$fx)
  vx=as.numeric(dx$vx)
  #Tukey検定する
  #「パッケージ名::関数」でパッケージを指定して関数を動かす
  #tidyverse とmultcompでselect関数が被ってバクるのを防ぐ
  #これでパッケージ指定で関数動かすとlibraryで読み込む必要もなくなる
  #有意水準は0.05
  tukey_result=multcomp::cld(multcomp::glht(aov(vx~fx),linfct=multcomp::mcp(fx="Tukey")),level=0.05,decreasing=T)
  print(tukey_result) 
  
}


#別パターン
for (i in list) {
  
  dx<-rawdata %>% 
    filter(accession==i) %>% 
    dplyr::select(3,4)
  
  dx <- dplyr::select(dx, "fx" = 1,"vx"=2, dplyr::everything())
  
  #fxの列をfactor型，vxの列をnumeric型にする
  fx=as.factor(dx$fx)
  vx=as.numeric(dx$vx)
  #Tukey検定する
  #「パッケージ名::関数」でパッケージを指定して関数を動かす
  #tidyverse とmultcompでselect関数が被ってバクるのを防ぐ
  #これでパッケージ指定で関数動かすとlibraryで読み込む必要もなくなる
  #有意水準は0.05
  tukey_result=multcomp::cld(multcomp::glht(aov(vx~fx),linfct=multcomp::mcp(fx="Tukey")),level=0.05,decreasing=T)
  print(tukey_result) 
  