library(tidyverse)

setwd("C:/R/2022/field")
fs::dir_create("Fig_2023_1_23")

rawdata<-read_csv("syuryou.csv")
cdata<-read_csv("Fujieda7_28.csv")

C<-filter(cdata,treat=="Control") %>% 
  select(1:4) %>% 
  na.omit() %>% 
  group_by(ID) %>% #スペースあるとセミコロン的なのでくくらないとだめ
  summarise(mean=mean(`Stem length`),sd=sd(`Stem length`))

#NW15を削除[行，列]
C<-C[-106,]

A<-filter(cdata,treat=="AOH") %>% 
  select(1:4) %>% 
  na.omit() %>% 
  group_by(ID) %>% 
  summarise(mean=mean(`Stem length`),sd=sd(`Stem length`))

#NJ52を削除
A<-A[-50,]

CA<-C %>% 
  select(1) %>% 
  mutate(Relative = A$mean/C$mean)%>% 
  arrange(Relative)

list<-colnames(rawdata[4:9])
#r<-list()
#r<-c(r,list(cor(dRc$Relative.x,dRc$Relative.y))) 繰り返してリストを作成できる 今回は使用しない

#7月28日の主茎長との相関----------------------------------
for (z in list) {
  
  data<-rawdata %>% 
    select(1:3,"s"=z,10) %>% 
    na.omit()
  
  dC<-data %>% 
    filter(treat=="Control") %>% 
    group_by(ID) %>% 
    summarise(mean=mean(s),sd=sd(s)) %>% 
    na.omit() 
  
  dC<-dC %>% 
    mutate(error=dC$sd/dC$mean) %>% 
    filter(error<0.5)
  
  dA<-data %>% 
    filter(treat=="AOH") %>% 
    group_by(ID) %>% 
    summarise(mean=mean(s),sd=sd(s)) %>% 
    na.omit()
  
  dA<-dA %>% 
    mutate(error=dA$sd/dA$mean) %>% 
    filter(error<0.5)
  
  dCA<-inner_join(dC,dA,by="ID")
  
  dR<-dCA[,1] %>% 
    mutate(Relative=dCA$mean.y/dCA$mean.x) %>% 
    arrange(Relative)
  
  dRc<-inner_join(dR,CA,by="ID") 
  
  title=paste(z)
  
  #桁数調整
  r<-formatC(cor(dRc$Relative.x,dRc$Relative.y),digits = 2)
  r<-data.frame(r)
  
  xlim<-max(dRc$Relative.x)
  
  png_name=paste("Fig_2023_1_23/相関図",z,".png")
  #正規表現削除して使う
  png(gsub(" ","",png_name))
  
  g<-ggplot(dRc,aes(x=Relative.x,y=Relative.y))+
    geom_point()+
    geom_smooth(method="lm",formula = y ~ x,se = T, colour = "black", size = 0.8)+
    geom_text(data=r, mapping = aes(x = xlim*0.9, y = 0.75, label = paste("r =",r)),size=5)+
    theme_bw()+
    labs(title = title)
  
  print(g)
  
  dev.off()
}

#9月29日主茎長との相関----------------------
cdata<-read_csv("Fujieda9_28.csv")
rawdata<-read_csv("syuryou.csv")
cdata<-read_csv("Fujieda7_28.csv")

C<-cdata %>% 
  na.omit() %>% 
  filter(treat=="Control") %>% 
  group_by(ID) %>% 
  summarise(mean=mean(Stem_length)) %>% 
  ungroup()

A<-cdata %>% 
  na.omit() %>% 
  filter(treat=="AOH") %>% 
  group_by(ID) %>% 
  summarise(mean=mean(Stem_length))

CA<-inner_join(C,A,by="ID")


R<-CA %>% 
  select(1) %>% 
  mutate(Relative=CA$mean.y/CA$mean.x) %>% 
  arrange(Relative)

for (z in list) {
  
  data<-rawdata %>% 
    select(1:3,"s"=z,10) %>% 
    na.omit()
  
  dC<-data %>% 
    filter(treat=="Control") %>% 
    group_by(ID) %>% 
    summarise(mean=mean(s),sd=sd(s)) %>% 
    na.omit() 
  
  dC<-dC %>% 
    mutate(error=dC$sd/dC$mean) %>% 
    filter(error<0.5)
  
  dA<-data %>% 
    filter(treat=="AOH") %>% 
    group_by(ID) %>% 
    summarise(mean=mean(s),sd=sd(s)) %>% 
    na.omit()
  
  dA<-dA %>% 
    mutate(error=dA$sd/dA$mean) %>% 
    filter(error<0.5)
  
  dCA<-inner_join(dC,dA,by="ID")
  
  dR<-dCA[,1] %>% 
    mutate(Relative=dCA$mean.y/dCA$mean.x) %>% 
    arrange(Relative)
  
  dRc<-inner_join(dR,R,by="ID") 
  
  title=paste(z)
  
  #桁数調整
  r<-formatC(cor(dRc$Relative.x,dRc$Relative.y),digits = 2)
  r<-data.frame(r)
  
  xlim<-max(dRc$Relative.x)
  
  png_name=paste("Fig_2023_1_23/9_29相関図",z,".png")
  #正規表現削除して使う
  png(gsub(" ","",png_name))
  
  g<-ggplot(dRc,aes(x=Relative.x,y=Relative.y))+
    geom_point()+
    geom_smooth(method="lm",formula = y ~ x,se = T, colour = "black", size = 0.8)+
    geom_text(data=r, mapping = aes(x = xlim*0.9, y = 0.5, label = paste("r =",r)),size=5)+
    theme_bw()+
    labs(title = title)
  
  print(g)
  
  dev.off()
}


#収量形質ごとの相関------------------
rawdata<-read_csv("syuryou.csv")
list<-colnames(rawdata[4:9])

for (z in list) {
  
  data<-rawdata %>% 
    select(1:3,"s"=z,10) %>% 
    na.omit()
  
  dC<-data %>% 
    filter(treat=="Control") %>% 
    group_by(ID) %>% 
    summarise(mean=mean(s),sd=sd(s)) %>% 
    na.omit() 
  
  dC<-dC %>% 
    mutate(error=dC$sd/dC$mean) %>% 
    filter(error<0.5)
  
  dA<-data %>% 
    filter(treat=="AOH") %>% 
    group_by(ID) %>% 
    summarise(mean=mean(s),sd=sd(s)) %>% 
    na.omit()
  
  dA<-dA %>% 
    mutate(error=dA$sd/dA$mean) %>% 
    filter(error<0.5)
  
  dCA<-inner_join(dC,dA,by="ID")
  
  dR<-dCA[,1] %>% 
    mutate(Relative=dCA$mean.y/dCA$mean.x) %>% 
    arrange(Relative)
  
  #可変的なオブジェクト名を作成
  R_name <- paste('R', z, sep = '') #変数名作成
  assign(R_name, dR) #作成した変数名(v1,v2,v3)にnを格納

}
#max関数の中にapplyファミリーのsapplyでリスト内に一括演算を行い，結果をベクトル型で変換する
#max_l <- max(sapply(r, length))
#??
#f<-function(x){length(x) <- max_l
#x}
#r<-data.frame(lapply(r,f))
#library(openxlsx)
#write.xlsx(r,"Relative.xlsx")
#R<-read_csv("Relative.csv")
#list<-colnames(R[1:5])

R<-full_join(Rbiomass,Rgrain_number,by="ID")
R<-full_join(R,Rgrain_weight,by="ID")
R<-full_join(R,Rpod_grains,by="ID")
R<-full_join(R,Rpod_number,by="ID")
R<-full_join(R,R100grains,by="ID")

R<-R %>% 
  rename("バイオマス"=Relative.x) %>% 
  rename("子実数"=Relative.y) %>% 
  rename("子実重"=Relative.x.x) %>% 
  rename("一莢粒数"=Relative.y.y) %>% 
  rename("着莢数"=Relative.x.x.x) %>% 
  rename("百粒重"=Relative.y.y.y)

Rlist<-colnames(R[3:7])         

for (i in Rlist) {
  
  Rd<-R %>% 
    select(1,2,"R"=i) %>% 
    na.omit()
  
  r<-formatC(cor(Rd$バイオマス,Rd$R),digits = 2)
  r<-data.frame(r)
  
  title=paste("バイオマスvs",i)
  
  ylim<-min(Rd$R)
  
  png_name=paste("Fig_2023_1_23/バイオマスvs",i,".png")
  #正規表現削除して使う
  png(gsub(" ","",png_name))
  
  g<-ggplot(Rd,aes(x=バイオマス,y=R))+
    geom_point()+
    geom_smooth(method="lm",formula = y ~ x,se = T, colour = "black", size = 0.8)+
    geom_text(data=r, mapping = aes(x = 1.7, y = ylim*1.1, label = paste("r =",r)),size=5)+
    theme_bw()+
    theme(axis.text.x = element_text(size = 12,color = "black"), axis.text.y = element_text(size = 12,color = "black"),aspect.ratio=1)+
    labs(title = title)
  
  print(g)
  
  dev.off()
}
