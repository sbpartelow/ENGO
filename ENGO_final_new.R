######
ENGO FINAL R Script - New data set
######
getwd()

setwd("D:/spa/Documents/Project files/ENGO/New final data")

########################

####
NEW SPI calculations with October 18 data
####

data_2<-read.csv("New_data_full_oct18.csv",header=T,sep=",")


library(som)

names(data_2)
dim(data)

spi_data<- matrix(, nrow = 662, ncol = 5)        ###create empty matrix
rownames(spi_data)<-data_2[,1]               ###match row/ col names to FINAL DATA
colnames(spi_data)<-c("ENGO","Origin_country","Continent","Norm_budget","Norm_emp")

spi_data[,2]<-data_2[,2]
spi_data[,3]<-data_2[,3]
spi_data
names(spi_data)
spi_data[,4]<-normalize(data_2[,9], byrow=F)
summary(Norm_budget)

spi_data[,5]<-normalize(data_2[,11], byrow=F)
summary(norm_emp)
spi_data

write.csv(spi_data, file = "SPI_index_oct18.csv", sep=";")
?write.csv

##########################################

#####
Structural power by region
#####

SPI_reg<-read.csv("SPI_new_only_with_data_oct18.csv",header=T,sep=",")

names(SPI_reg)

table(SPI_reg[,9])

spi_africa <- subset(SPI_reg, Continent == "Africa")
table(spi_africa[,9])

spi_asia <- subset(SPI_reg, Continent == "Asia")
table(spi_asia[,9])

spi_oceania <- subset(SPI_reg, Continent == "Australia")
table(spi_oceania[,9])

spi_europe <- subset(SPI_reg, Continent == "Europe")
table(spi_europe[,9])

spi_na <- subset(SPI_reg, Continent == "North America")
table(spi_na[,9])

spi_sa <- subset(SPI_reg, Continent == "South America")
table(spi_sa[,9])

table(SPI_reg[,9])

table(SPI_reg[,3])


#####################################

###
Word clouds
###
library(wordcloud)
?wordcloud2
library(wordcloud2)

#Single words total
Single_cd<-read.csv("Single_word_for_cloud.csv",header=T,sep=",")
wordcloud2(Single_cd, size = 0.8, minSize=1.5,rotateRatio = 0,color = "black",backgroundColor = "white") #size might exclude words
all_single

#Combo words total
Combo_cd<-read.csv("Combo_words_for_cloud.csv",header=T,sep=",")
wordcloud2(Combo_cd, rotateRatio = 0, size = 0.8,minSize=1.5,color = "black",backgroundColor = "white") #size might exclude words
all_combo


####################################
####
typlogy plot with simplified data set
####
typ_all<-read.csv("Typology_plot_all_new_oct18_data.csv",header=T,sep=",")
names(typ_all)
par(pty="s")

plot(jitter(typ_all$PC1.score.NEW,2),jitter(typ_all$PC2.score.NEW,2),xlim=c(-15,15),ylim=c(-15,15),pch=c(1,19)[as.numeric(typ_all$SPI_binary)],col=c("grey60","red")[as.numeric(typ_all$SPI_binary)])
abline(h=0,v=0)

table(typ_all[,8])

#######
Regional typology plots
#######
par(mfrow=c(2,3))

par(pty="s")
africa <- subset(typ_all, Document.group == "Africa")
africa
plot(jitter(jitter(africa$PC1.score)),jitter(jitter(africa$PC2.score)),xlim=c(-15,15),ylim=c(-15,15),xlab="",ylab="",main="Africa (N=76)",pch=c(1,19)[as.numeric(africa$SPI_binary)],col=c("grey60","red")[as.numeric(africa$SPI_binary)])
abline(h=0,v=0)

par(pty="s")
asia <- subset(typ_all, Document.group == "Asia")
plot(jitter(jitter(asia$PC1.score)),jitter(jitter(asia$PC2.score)),xlim=c(-15,15),ylim=c(-15,15),xlab="",ylab="",main="Asia (N=143)",pch=c(1,19)[as.numeric(asia$SPI_binary)],col=c("grey60","red")[as.numeric(asia$SPI_binary)])
abline(h=0,v=0)

par(pty="s")
aus  <- subset(typ_all, Document.group == "Australia")
aus
plot(jitter(jitter(aus$PC1.score)),jitter(jitter(aus$PC2.score)),xlim=c(-15,15),ylim=c(-15,15),xlab="",ylab="",main="Oceania (N=13)",pch=c(1,19)[as.numeric(aus$SPI_binary)],col=c("grey60","red")[as.numeric(aus$SPI_binary)])
abline(h=0,v=0)

par(pty="s")
eu  <- subset(typ_all, Document.group == "Europe")
eu
plot(jitter(jitter(eu$PC1.score)),jitter(jitter(eu$PC2.score)),xlim=c(-15,15),ylim=c(-15,15),xlab="",ylab="",main="Europe (N=229)",pch=c(1,19)[as.numeric(eu$SPI_binary)],col=c("grey60","red")[as.numeric(eu$SPI_binary)])
abline(h=0,v=0)

par(pty="s")
noam  <- subset(typ_all, Document.group == "North America")
plot(jitter(jitter(noam$PC1.score)),jitter(jitter(noam$PC2.score)),xlim=c(-15,15),ylim=c(-15,15),xlab="",ylab="",main="North America (N=165)",pch=c(1,19)[as.numeric(noam$SPI_binary)],col=c("grey60","red")[as.numeric(noam$SPI_binary)])
abline(h=0,v=0)

par(pty="s")
soam  <- subset(typ_all, Document.group == "South America")
plot(jitter(jitter(soam$PC1.score)),jitter(jitter(soam$PC2.score)),xlim=c(-15,15),ylim=c(-15,15),xlab="",ylab="",main="South America (N=37)",pch=c(1,19)[as.numeric(soam$SPI_binary)],col=c("grey60","red")[as.numeric(soam$SPI_binary)])
abline(h=0,v=0)

table(typ_all[,1])


############################
PC scores barplot distribution
############################

data_pcbar<-read.csv("PC_scores_barplot_no_names.csv",header=T,sep=",")

names(data_pcbar)
dim(data_pcbar)

?boxplot
?par
par(mar=c(8,15,10,15))
boxplot(data_pcbar,names = c("Environmental management","Climate politics","Environmental Justice","Ecological moderization"))

library(beanplot)
beanplot(data_pcbar, beanlines = "median")
?beanplot

############################
Quadrant table
############################
quad<-read.csv("Quadrant_table.csv",header=T,sep=",")

names(quad)

table(quad[,7])

high_quad<-subset(quad, SPI_binary == 2)

dim(high_quad)

table(high_quad[,7])


##########
ENGOs by region with UN Conventions
#########
data_2<-read.csv("New_data_full_oct18.csv",header=T,sep=",")

names(data_2)
?par
par(mfrow=c(1,3),mar=c(15,7,7,7))

engoUNFCCC<-table(data_2[,16],data_2[,3])

engoUNCBD<-table(data_2[,17],data_2[,3])

engoUNCCD<-table(data_2[,18],data_2[,3])

prop1 = prop.table(engoUNFCCC,margin=2)
prop1
prop2 = prop.table(engoUNCBD,margin=2)
prop3 = prop.table(engoUNCCD,margin=2)


?barplot
barplot(prop1[2,],las=2,main="UNFCCC",cex.names=2,cex.lab=2,cex.axis=1,ylab='Proportion of ENGOs affiliated',col=c("black"))
barplot(prop2[2,],las=2,main="UNCBD",cex.names=2,cex.lab=2,cex.axis=1,ylab='Proportion of ENGOs affiliated',col=c("black"))
barplot(prop3[2,],las=2,main="UNCCD",cex.names=2,cex.lab=2,cex.axis=1,ylab='Proportion of ENGOs affiliated',col=c("black"))



##########
Comparison of words lists frequency
##########

##
single words
##

wsc_tp<-read.csv("Word_single_comp_totalper.csv",header=T,sep=",")
wsc_op<-read.csv("Word_single_comp_onceper.csv",header=T,sep=",")

m<-cbind(wsc_tp,wsc_op)
m


sT <- as.numeric(factor(m[,1]))
sT
sO<- as.numeric(factor(m[,2]))
sO

dd <- cbind(sT, sO)
dd

cor(dd, method="kendall", use="pairwise") 
cor.test(sT, sO, method="kendall") 

##
Combo words
##

wcc<-read.csv("Word_combo_comp.csv",header=T,sep=",")

cT <- as.numeric(factor(wcc[,1]))
cT
cO<- as.numeric(factor(wcc[,2]))
cO
n <- cbind(cT, cO) 
n
cor(n, method="kendall", use="pairwise") 
cor.test(cT, cO, method="kendall")

##
mixed list
##

wmc<-read.csv("Word_mixed_comp.csv",header=T,sep=",")

mT <- as.numeric(factor(wmc[,1]))
mT
mO<- as.numeric(factor(wmc[,2]))
mO
t <- cbind(mT, mO) 
t
cor(t, method="kendall", use="pairwise") 
cor.test(mT, mO, method="kendall")


