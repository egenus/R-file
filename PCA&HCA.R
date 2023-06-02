library(tidyverse)
library(ggrepel)

setwd("C:/R/Experiment_TA")

rawdata<-read_csv("komatsuna_ionome_2.csv") %>%   #header=Tで1行目をカラム名に設定,row.names=1で1列目をrownameに設定
  na.omit() 

data<-rawdata %>% 
  select(2:14)


pca <- prcomp(data, scale=TRUE)

#大量のデータの先頭部分を取り出す関数，tail()末尾
head(pca$x)

#rbind()縦につなげる，cbind()横につなげる
pca.data <- cbind(pca$x,rawdata)

pdf("pca_plot.pdf")

ggplot(pca.data, aes(x=PC1, y=PC2,color = Exp))+
  geom_point(size=0.5)+
  geom_text_repel(aes(label = Exp), size = 2.5)+
  theme(text=element_text(size=20),legend.position = "none")

dev.off()


png("pca_biplot.png")

biplot(pca)

dev.off()


rownames(data) <- rawdata$Exp
#クラスター解析
d <- dist(scale(data), method="euclidean")

hca <- hclust(d, method="ward.D2")

hca$labels<-rawdata$Exp

options(repr.plot.width = 15, repr.plot.height = 10)


png("Dendrogram.png")
#デンドログラムの作成

d
#rawname()関数で変える
plot(hca)


dev.off()
#cutree関数を用いて、各サンプルが3つのクラスターのどこに分類されているのかを調べる
cl <- cutree(hca, k=3)
#table関数でクロス集計
table(cl,rawdata$treat)

library(ggdendro)


d <- as.dendrogram(hca)
d.hca <- dendro_data(d, type = "rectangle")


options(repr.plot.width = 20, repr.plot.height = 10)

d.hca$labels<-rawdata$Exp
d.hca


row <- rawdata %>% mutate(label=rawdata$Exp)
d.hca.labels <- d.hca$labels %>% left_join(row, by="label")

#ggplotでデンドログラムを作成
ggplot(segment(d.hca))+
  geom_segment(aes(x = x,y = y, xend = xend, yend = yend))+
  geom_text(data= d.hca.labels, aes(x,y,label = label), size=5, hjust = 1, angle = 90)+
  theme(text=element_text(size=20))


