library(corrplot)
library(tidyverse)
setwd("C:/R/2022/field")

rawdata<-read.csv("syuryou.csv")

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
    filter(error<0.5)#変動係数50%以下残す
  
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

R<-left_join(Rbiomass,Rgrain_number,by="ID")
R<-left_join(R,Rgrain_weight,by="ID")
R<-left_join(R,Rpod_grains,by="ID")
R<-left_join(R,Rpod_number,by="ID")
R<-left_join(R,RX100grains,by="ID")

R<-R %>% 
  rename("バイオマス"=Relative.x) %>% 
  rename("子実数"=Relative.y) %>% 
  rename("子実重"=Relative.x.x) %>% 
  rename("一莢粒数"=Relative.y.y) %>% 
  rename("着莢数"=Relative.x.x.x) %>% 
  rename("百粒重"=Relative.y.y.y)
#--------------------------------------------
d1<-read.csv("Fujieda7_12.csv")
C<-d1 %>% 
  na.omit() %>% 
  filter(treat=="Control") %>% 
  group_by(ID) %>% 
  summarise(mean=mean(Stem.length)) %>% 
  ungroup()

A<-d1 %>% 
  na.omit() %>% 
  filter(treat=="AOH") %>% 
  group_by(ID) %>% 
  summarise(mean=mean(Stem.length))

CA<-inner_join(C,A,by="ID")
R1<-CA %>% 
  select(1) %>% 
  mutate(Relative=CA$mean.y/CA$mean.x) 
#----------------------------------------------------
d2<-read.csv("Fujieda7_28.csv")
C<-d2 %>% 
  na.omit() %>% 
  filter(treat=="Control") %>% 
  group_by(ID) %>% 
  summarise(mean=mean(Stem.length)) %>% 
  ungroup()

A<-d2 %>% 
  na.omit() %>% 
  filter(treat=="AOH") %>% 
  group_by(ID) %>% 
  summarise(mean=mean(Stem.length))

CA<-inner_join(C,A,by="ID")
R2<-CA %>% 
  select(1) %>% 
  mutate(Relative=CA$mean.y/CA$mean.x)
#------------------------------------------------------------
d3<-read.csv("Fujieda9_28.csv")
C<-d3 %>% 
  na.omit() %>% 
  filter(treat=="Control") %>% 
  group_by(ID) %>% 
  summarise(mean=mean(Stem_length)) %>% 
  ungroup()

A<-d3 %>% 
  na.omit() %>% 
  filter(treat=="AOH") %>% 
  group_by(ID) %>% 
  summarise(mean=mean(Stem_length))

CA<-inner_join(C,A,by="ID")
R3<-CA %>% 
  select(1) %>% 
  mutate(Relative=CA$mean.y/CA$mean.x)
#-----------------------------------------------------
C<-d1 %>% 
  na.omit() %>% 
  filter(treat=="Control") %>% 
  group_by(ID) %>% 
  summarise(mean=mean(Stem.node)) %>% 
  ungroup()

A<-d1 %>% 
  na.omit() %>% 
  filter(treat=="AOH") %>% 
  group_by(ID) %>% 
  summarise(mean=mean(Stem.node))

CA<-inner_join(C,A,by="ID")
Rn1<-CA %>% 
  select(1) %>% 
  mutate(Relative=CA$mean.y/CA$mean.x)
#-------------------------------------------------------
C<-d2 %>% 
  na.omit() %>% 
  filter(treat=="Control") %>% 
  group_by(ID) %>% 
  summarise(mean=mean(Stem.node)) %>% 
  ungroup()

A<-d2 %>% 
  na.omit() %>% 
  filter(treat=="AOH") %>% 
  group_by(ID) %>% 
  summarise(mean=mean(Stem.node))

CA<-inner_join(C,A,by="ID")
Rn2<-CA %>% 
  select(1) %>% 
  mutate(Relative=CA$mean.y/CA$mean.x)
#-----------------------------------------------------
D<-d2 %>% 
  na.omit() 
D$SPAD<-as.numeric(D$SPAD)
C<-D %>% 
  filter(treat=="Control") %>% 
  group_by(ID) %>% 
  summarise(mean=mean(SPAD)) %>% 
  ungroup()

A<-D %>% 
  na.omit() %>% 
  filter(treat=="AOH") %>% 
  group_by(ID) %>% 
  summarise(mean=mean(SPAD))

CA<-inner_join(C,A,by="ID")
RS<-CA %>% 
  select(1) %>% 
  mutate(Relative=CA$mean.y/CA$mean.x)
#------------------------------------------------------
R<-left_join(R,R1,by="ID")
R<-left_join(R,R2,by="ID")
R<-left_join(R,R3,by="ID")
R<-left_join(R,Rn1,by="ID")
R<-left_join(R,Rn2,by="ID")
R<-left_join(R,RS,by="ID")

Rc<-R %>% 
  rename("主茎長 (V2-V4)"=Relative.x) %>% 
  rename("主茎長 (V6-V8)"=Relative.y) %>% 
  rename("主茎長 (R6-R8)"=Relative.x.x) %>% 
  rename("主茎節数 (V2-V4)"=Relative.y.y) %>% 
  rename("主茎節数 (V6-V8)"=Relative.x.x.x) %>% 
  rename("SPAD (V6-V8)"=Relative.y.y.y)

Rc<-Rc[,2:13]

cor<-  cor(Rc,use = "pairwise.complete.obs")

p<-cor.mtest(Rc)

png("corrplot.png")
corrplot(cor, method="color",tl.col="black",type = "upper",addCoef.col = "black",
         diag = FALSE,p.mat=p$p,sig.level=0.05,insig = "blank",tl.srt=45)
dev.off()

pdf("corrplot.pdf")
corrplot(cor, method="color",tl.col="black",type = "upper",addCoef.col = "black",
         diag = FALSE,p.mat=p$p,sig.level=0.05,insig = "blank",tl.srt=45)$corrPos -> p1
text(p1$x, p1$y, round(p1$corr, 2))
dev.off()
