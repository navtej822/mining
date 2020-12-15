#TEAM 15
#Navtejinder Singh Brar 
#Vamsi Krishna Pentakota
install.packages("plyr")
install.packages("arules")
install.packages("arulesViz")
install.packages("tidyverse")
install.packages("readxml")
install.packages("knitr")
install.packages("patchwork")
install.packages("lubridate")
library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)  
library(ggfortify)
library(patchwork)
library(lubridate)
library(plyr)
library(dplyr)
theme_set(theme_pubr())
retail <- read_excel("~/Desktop/data_mining/online-retail_2.xlsx")



######################################PRE-PROCESSING###########################################################
retail <- retail[complete.cases(retail), ]
retail %>% mutate(Description = as.factor(Description))

retail %>% mutate(Country = as.factor(Country))
retail$Date <- as.Date(retail$InvoiceDate)
TransTime<- format(retail$InvoiceDate,"%H:%M:%S")
InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
cbind(retail,TransTime)
cbind(retail,InvoiceNo)
retail<-retail %>%
  filter(!str_detect(InvoiceNo, "C"))
retail<-subset(retail, Description!="\\?")
retail<-subset(retail, Description!="WRONG")
retail<-subset(retail, Description!="LOST")
retail<-subset(retail, Description!="CRUSHED")
retail<-subset(retail, Description!="SMASHED")
retail<-subset(retail, Description!="DAMAGED")
retail<-subset(retail, Description!="FOUND")
retail<-subset(retail, Description!="THROWN")
retail<-subset(retail, Description!="MISSING")
retail<-subset(retail, Description!="AWAY")
retail<-subset(retail, Description!="?")
retail<-subset(retail, Description!="CHECK")
retail<-subset(retail, Description!="POSTAGE")
retail<-subset(retail, Description!="MANUAL")
retail<-subset(retail, Description!="CHARGES")
retail<-subset(retail, Description!="AMAZON")
retail<-subset(retail, Description!="FEE")
retail<-subset(retail, Description!="FAULT")
retail<-subset(retail, Description!="SALES")
retail<-subset(retail, Description!="ADJUST")
retail<-subset(retail, Description!="COUNTED")
retail<-subset(retail, Description!="LABEL")
retail<-subset(retail, Description!="INCORRECT")
retail<-subset(retail, Description!="SOLD")
retail<-subset(retail, Description!="BROKEN")
retail<-subset(retail, Description!="BARCODE")
retail<-subset(retail, Description!="CRACKED")
retail<-subset(retail, Description!="RETURNED")
retail<-subset(retail, Description!="MAILOUT")
retail<-subset(retail, Description!="DELIVERY")
retail<-subset(retail, Description!="MIX UP")
retail<-subset(retail, Description!="MOULDLY")
retail<-subset(retail, Description!="PUT ASIDE")
retail<-subset(retail, Description!="ERROR")
retail<-subset(retail, Description!="DESTROYED")
retail<-subset(retail, Description!="RUSTY")
glimpse(retail)
View(retail)
library(plyr)

transactionData <- ddply(retail,c("InvoiceNo","Date"),
                         function(df1)paste(df1$Description,
                                            collapse = ","))


######transactionData

 
transactionData$InvoiceNo <- NULL

transactionData$Date <- NULL

colnames(transactionData) <- c("items")

#####transactionData

write.csv(transactionData,"~/Desktop/data_mining/Project3-team15/dataset/march_transactions.csv", quote = FALSE, row.names = FALSE)

tr <- read.transactions('~/Desktop/data_mining/Project3-team15/dataset/march_transactions.csv', format = 'basket', sep=',')


tr

summary(tr)

#######################################################CANDIDATE ITEMSETS########################################

#support is 0.0001
itemsets <- apriori(tr, parameter = list(minlen=1, maxlen=4, support=0.0001, target="frequent itemsets"))
summary(itemsets)
#inspect(sort(itemsets, by="support"))
#print top-5 1-itemsets in descending order of support
inspect(head(sort(itemsets, by="support"), 5))
## S3 method for class 'itemsets'
plot(itemsets, method = "scatterplot", measure = "support", shading = NA,
     interactive=TRUE, data = NULL, control = NULL)

#################################################################################################################
#######################################################FREQUENT ITEMSETS#########################################
#support is 0.008
itemsets1 <- apriori(tr, parameter = list(minlen=1, maxlen=3, support=0.008, target="frequent itemsets"))
summary(itemsets1)
#inspect(sort(itemsets, by="support"))
#print top-5 1-itemsets in descending order of support
inspect(head(sort(itemsets1, by="support"), 5))
## S3 method for class 'itemsets'
plot(itemsets1, method = "scatterplot", measure = "support", shading = NA,
     interactive=TRUE, data = NULL, control = NULL)


#support is 0.011
itemsets2 <- apriori(tr, parameter = list(minlen=1, maxlen=3, support=0.011, target="frequent itemsets"))
summary(itemsets2)
#inspect(sort(itemsets, by="support"))
#print top-5 1-itemsets in descending order of support
inspect(head(sort(itemsets2, by="support"), 5))
## S3 method for class 'itemsets'
plot(itemsets2, method = "scatterplot", measure = "support", shading = NA,
     interactive=TRUE, data = NULL, control = NULL)

#support is 0.015
itemsets3 <- apriori(tr, parameter = list(minlen=1, maxlen=3, support=0.015, target="frequent itemsets"))
summary(itemsets3)
#inspect(sort(itemsets, by="support"))
#print top-5 1-itemsets in descending order of support
inspect(head(sort(itemsets3, by="support"), 5))
## S3 method for class 'itemsets'
plot(itemsets3, method = "scatterplot", measure = "support", shading = NA,
     interactive=TRUE, data = NULL, control = NULL)
####################################################################################################


##############################################APRIORI#################################################

# Min Support as 0.008, confidence as 0.5.
association.rules2 <- apriori(tr, parameter = list(supp=0.008, conf=0.5,maxlen=5))
inspect(association.rules2[1:20])
summary(association.rules2)
subRules<-association.rules2[quality(association.rules2)$confidence>0.5]
plotly_arules(subRules,jitter=0)
plot(subRules)
plot(association.rules2)
top.confidence2 <- sort(association.rules2, decreasing = TRUE, na.last = NA, by = "confidence")
#inspect(head(top.confidence2, 100))
n1<-head(top.confidence2, 100)
plot(n1)

# Min Support as 0.011, confidence as 0.5.
association.rules3 <- apriori(tr, parameter = list(supp=0.011, conf=0.5,maxlen=5))
inspect(association.rules3[1:20])
summary(association.rules3)
subRules<-association.rules3[quality(association.rules3)$confidence>0.5]
plotly_arules(subRules,jitter=0)
plot(subRules)
top.confidence3 <- sort(association.rules3, decreasing = TRUE, na.last = NA, by = "confidence")
#inspect(head(top.confidence3, 100))
n2<-head(top.confidence3, 100)
plot(n2)

# Min Support as 0.015, confidence as 0.5.
association.rules4 <- apriori(tr, parameter = list(supp=0.015, conf=0.5,maxlen=5))
inspect(association.rules4[1:20])
summary(association.rules4)
subRules<-association.rules4[quality(association.rules4)$confidence>0.5]
plotly_arules(subRules,jitter=0)
plot(subRules)
top.confidence4 <- sort(association.rules4, decreasing = TRUE, na.last = NA, by = "confidence")
#inspect(head(top.confidence4, 100))
n3<-head(top.confidence4, 100)
plot(n3)

# Min Support as 0.008, confidence as 0.7.
association.rules5 <- apriori(tr, parameter = list(supp=0.008, conf=0.7,maxlen=5))
inspect(association.rules5[1:20])
summary(association.rules5)
subRules<-association.rules5[quality(association.rules5)$confidence>0.7]
plotly_arules(subRules,jitter=0)
plot(subRules)
top.confidence5 <- sort(association.rules5, decreasing = TRUE, na.last = NA, by = "confidence")
#inspect(head(top.confidence5, 100))
n4<-head(top.confidence5, 100)
plot(n4)

# Min Support as 0.011, confidence as 0.7.
association.rules6 <- apriori(tr, parameter = list(supp=0.011, conf=0.7,maxlen=5))
inspect(association.rules6[1:20])
summary(association.rules6)
subRules<-association.rules6[quality(association.rules6)$confidence>0.7]
plotly_arules(subRules,jitter=0)
plot(subRules)
top.confidence6 <- sort(association.rules6, decreasing = TRUE, na.last = NA, by = "confidence")
#inspect(head(top.confidence6, 100))
n5<-head(top.confidence6, 100)
plot(n5)

# Min Support as 0.015, confidence as 0.7.
association.rules7 <- apriori(tr, parameter = list(supp=0.015, conf=0.7,maxlen=5) )
inspect(association.rules7[1:20])
summary(association.rules7)
subRules<-association.rules7[quality(association.rules7)$confidence>0.7]
plotly_arules(subRules,jitter=0)
plot(subRules)
top.confidence7 <- sort(association.rules7, decreasing = TRUE, na.last = NA, by = "confidence")
#inspect(head(top.confidence7, 100))
n6<-head(top.confidence7, 100)
plot(n6)


# Min Support as 0.008, confidence as 0.8.
association.rules8 <- apriori(tr, parameter = list(supp=0.008, conf=0.8,maxlen=5))
inspect(association.rules8[1:20])
summary(association.rules8)
subRules<-association.rules8[quality(association.rules8)$confidence>0.8]
plotly_arules(subRules,jitter=0)
plot(subRules)
top.confidence8 <- sort(association.rules8, decreasing = TRUE, na.last = NA, by = "confidence")
#inspect(head(top.confidence8, 100))
n7<-head(top.confidence8, 100)
plot(n7)


# Min Support as 0.011, confidence as 0.8.
association.rules9 <- apriori(tr, parameter = list(supp=0.011, conf=0.8,maxlen=5))
inspect(association.rules9[1:20])
summary(association.rules9)
subRules<-association.rules9[quality(association.rules9)$confidence>0.8]
plotly_arules(subRules,jitter=0)
plot(subRules)
top.confidence9 <- sort(association.rules9, decreasing = TRUE, na.last = NA, by = "confidence")
#inspect(head(top.confidence9, 100))
n8<-head(top.confidence9, 100)
plot(n8)

# Min Support as 0.015, confidence as 0.8.
association.rules10 <- apriori(tr, parameter = list(supp=0.015, conf=0.8,maxlen=5))
inspect(association.rules10[1:20])
summary(association.rules10)
subRules<-association.rules10[quality(association.rules10)$confidence>0.8]
plotly_arules(subRules,jitter=0)
plot(subRules)
top.confidence10 <- sort(association.rules10, decreasing = TRUE, na.last = NA, by = "confidence")
#inspect(head(top.confidence10, 100))
n9<-head(top.confidence10, 100)
plot(n9)
########################################################################################################################
# figure <- ggarrange(association.rules2,association.rules3 ,association.rules4, association.rules5, association.rules6,association.rules7,
#                     association.rules8, association.rules9, association.rules10,
#                     labels = c("Supp = 0.008 Conf = 0.5","Supp = 0.011 Conf = 0.5","Supp = 0.015 Conf = 0.5","Supp = 0.008 Conf = 0.7","Supp = 0.011 Conf = 0.7",
#                                "Supp = 0.015 Conf = 0.7","Supp = 0.008 Conf = 0.8","Supp = 0.011 Conf = 0.8","Supp = 0.015 Conf = 0.8"),ncol = 3, nrow = 3)
# figure
# 
# # Create a new page
# grid.newpage()
# # Next push the vissible area with a layout of 2 columns and 2 row using pushViewport()
# pushViewport(viewport(layout = grid.layout(3,3)))
# 
# # Put the plot on the the area by row and column position
# print(association.rules2, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
# print(association.rules3, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
# print(association.rules4, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
# print(association.rules5, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
# print(association.rules6, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
# print(association.rules7, vp = viewport(layout.pos.row = 2, layout.pos.col = 3))
# print(association.rules8, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
# print(association.rules9, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
# print(association.rules10, vp = viewport(layout.pos.row = 3, layout.pos.col = 3))
# grid.arrange(association.rules2,association.rules3 ,association.rules4, association.rules5, association.rules6,association.rules7,
#              association.rules8, association.rules9, association.rules10,nrow=3,ncol=3)
# plots<-grid.newpage()
# grid.arrange(association.rules2@plots[[1]], association.rules3@plots[[1]], nrow = 2)
##########################################################################################

####################################LIFT<10###############################################
lift.subset1<-subset(association.rules2, subset=lift<10)
summary(lift.subset1)
inspect(lift.subset1[1:10])
plot(lift.subset1)

lift.subset2<-subset(association.rules3, subset=lift<10)
summary(lift.subset2)
inspect(lift.subset2[1:10])
plot(lift.subset2)

lift.subset3<-subset(association.rules4, subset=lift<10)
summary(lift.subset3)
inspect(lift.subset3[1:10])
plot(lift.subset3)

lift.subset4<-subset(association.rules5, subset=lift<10)
summary(lift.subset4)
inspect(lift.subset4)
plot(lift.subset4)

lift.subset5<-subset(association.rules6, subset=lift<10)
summary(lift.subset5)
inspect(lift.subset5)
plot(lift.subset5)

lift.subset6<-subset(association.rules7, subset=lift<10)
summary(lift.subset6)
inspect(lift.subset6)
plot(lift.subset6)

lift.subset7<-subset(association.rules8, subset=lift<10)
summary(lift.subset7)
inspect(lift.subset7)
plot(lift.subset7)

lift.subset8<-subset(association.rules9, subset=lift<10)
summary(lift.subset8)
inspect(lift.subset8)
plot(lift.subset8)

lift.subset9<-subset(association.rules10, subset=lift<10)
summary(lift.subset9)
inspect(lift.subset9)
plot(lift.subset9)


##########################################################################################

####################################LIFT>10###############################################
lift.subset11<-subset(association.rules2, subset=lift>10)
summary(lift.subset11)
inspect(lift.subset11[1:10])
plot(lift.subset11)

lift.subset21<-subset(association.rules3, subset=lift>10)
summary(lift.subset21)
inspect(lift.subset21[1:10])
plot(lift.subset21)

lift.subset31<-subset(association.rules4, subset=lift>10)
summary(lift.subset31)
inspect(lift.subset31[1:10])
plot(lift.subset31)

lift.subset41<-subset(association.rules5, subset=lift>10)
summary(lift.subset41)
inspect(lift.subset41[1:10])
plot(lift.subset41)

lift.subset51<-subset(association.rules6, subset=lift>10)
summary(lift.subset51)
inspect(lift.subset51[1:10])
plot(lift.subset51)

lift.subset61<-subset(association.rules7, subset=lift>10)
summary(lift.subset61)
inspect(lift.subset61)
plot(lift.subset61)

lift.subset71<-subset(association.rules8, subset=lift>10)
summary(lift.subset71)
inspect(lift.subset71[1:10])
plot(lift.subset71)

lift.subset81<-subset(association.rules9, subset=lift>10)
summary(lift.subset81)
inspect(lift.subset81[1:10])
plot(lift.subset81)

lift.subset91<-subset(association.rules10, subset=lift>10)
summary(lift.subset91)
inspect(lift.subset91)
plot(lift.subset91)
###########################################################################################




