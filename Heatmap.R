library(tidyverse)
library(superheat)
setwd("C:/R/random")

geno <- read.csv("geno_Analysis_only_a_list_pheno.csv",header = FALSE) 
geno.score <- geno %>% select(4:ncol(geno))

geno.score.t<-data.frame(t(geno.score)) 
geno.score.t<-rename(geno.score.t,"ID"=X1)

namelist <- read.csv("namelist2.csv")

data<-geno.score.t[,-1]

data<-data.frame(lapply(data[1:11340],as.numeric))
str(data)

row.names(data)<-namelist$品種

d <- dist(scale(data), method="euclidean")

hca <- hclust(d, method="ward.D2")

plot(hca,hang = - 1)

name<-read_csv("name_bind.csv")
data.s<-read_csv("strawberries.csv")

name<-select(name,"品種"=3)

d.heat<-full_join(name,data.s,by="品種")
d.heat<-d.heat[,-(1:4)]

rownames(d.heat)<-name$品種

d.heat<-scale(d.heat)

png("heatmap.png")
superheat(d.heat,bottom.label.size=0.05,bottom.label.text.size = 2)
dev.off()
