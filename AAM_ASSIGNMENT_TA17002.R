####################################
# RAJIB MANDAL (TA17002)

###################################
library(cluster)
library(ggplot2)
library(dplyr)
library(zoo)

#library(ape)

#################################################
# set  working directory

##################################################
setwd("C:\\Software\\xlri\\Advanced Analytics\\r assignment\\Source")


####################################################
### Reading the csv file
####################################################
rfmdata<-read.csv("retailrfm.csv")

summary(rfmdata)

sapply(rfmdata, function(x) length(unique(x)))
####################################################
### Separate the variable InvoiceDate into
# two variables Date and Time 
####################################################

rfmdata$Date <- as.Date(rfmdata$InvoiceDate , format="%m/%d/%Y")
rfmdata$Time <- format(as.POSIXct(strptime(rfmdata$InvoiceDate,"%m/%d/%Y %H:%M",tz="")) ,format = "%H:%M")


####################################################
##Calcultate amount for each transaction 
####################################################
rfmdata$monetery<-as.numeric(rfmdata$Quantity * rfmdata$UnitPrice)


####################################################
### calculate recency ,frequency & monetary 
####################################################
retailtranformdata <- rfmdata %>%
  group_by(CustomerID) %>%
  summarise(recency=12 * as.numeric((as.yearmon("2011-12-09") - as.yearmon(max(Date)))),
            frequency=n_distinct(InvoiceNo), monetery= sum(monetery))

summary(retailtranformdata)
####################################################
### Exclude all records whose monetary value <0
####################################################
rfm<-retailtranformdata
rfm <- rfm[rfm$monetery>0,]
summary(rfm)
####################################################
###Remove Customer Id from dataset
####################################################

rfm$CustomerID<-NULL
write.csv(rfm,"rfm.csv")

####################################################
###Normalize data using Min-Max
####################################################
minmaxstandard <- function(x)
{
  (x-min(x))/(max(x)-min(x))
}

rfm$recency<-as.numeric(minmaxstandard(rfm$recency)) 
rfm$monetery <-as.numeric(minmaxstandard(rfm$monetery))
rfm$frequency <-as.numeric(minmaxstandard(rfm$frequency))


boxplot(rfm)
set.seed(1234)
ak<-kmeans(rfm,3,nstart = 40)
wss<-(nrow(rfm))*sum(apply(rfm,2,var))

####################################################
###Calculatw Within Sum of Square
####################################################

for ( i in 2:25)
{
  wss[i]<-kmeans(rfm,i,iter.max =40,nstart=60)$tot.withinss
  
}

####################################################
###Finding the elbow
####################################################
plot(1:25,wss,type="b",xlab="Number of Clusters", ylab="Within  Sum of Squares")


####################################################
###K-mean clustering with 5 clusters
####################################################
set.seed(1234)
km<-kmeans(rfm,5,iter.max =40,nstart = 50)
km$size

####################################################
###Get cluster detail
####################################################
rfmcluster<-data.frame(rfm,km$cluster)

###Get cluster detail for cluster-1

clusterdetail<-subset(rfmcluster,rfmcluster$km.cluster==1)
kable(head(clusterdetail))
hist(clusterdetail$recency)
summary(clusterdetail)


###Get cluster detail for cluster-2

clusterdetail<-subset(rfmcluster,rfmcluster$km.cluster==2)
hist(clusterdetail$recency)
summary(clusterdetail)


###Get cluster detail for cluster-3

clusterdetail<-subset(rfmcluster,rfmcluster$km.cluster==3)
hist(clusterdetail$recency)
summary(clusterdetail)

###Get cluster detail for cluster-4

clusterdetail<-subset(rfmcluster,rfmcluster$km.cluster==4)
hist(clusterdetail$recency)
summary(clusterdetail)

###Get cluster detail for cluster-5

clusterdetail<-subset(rfmcluster,rfmcluster$km.cluster==5)
hist(clusterdetail$recency)
summary(clusterdetail)





####################################################
###Dendogram- complete linkage clustering
####################################################
eudist <- dist(as.matrix(rfm), method = "euclidean")
hcluster <- hclust(eudist, method = "complete" )


plot(hcluster, cex = 0.1,xlab="Index of data points", ylab="Steps",main = "Cluster Dendrogram")
rect.hclust(hcluster, k = 5, border = 1:5)

# Cut tree into 5 groups
set.seed(1234)
subgrp<- cutree(hcluster, k =5)

# Number of members in each cluster
table(subgrp)

####################################################
###Convert to dataframe
####################################################
dfhclust<-data.frame(rfm,subgrp)

###Get cluster detail for cluster-1
hclustsummary<-subset(dfhclust,dfhclust$subgrp==1)
hist(hclustsummary$recency)
summary(hclustsummary)

###Get cluster detail for cluster-2
hclustsummary<-subset(dfhclust,dfhclust$subgrp==2)
hist(hclustsummary$recency)
summary(hclustsummary)
###Get cluster detail for cluster-3
hclustsummary<-subset(dfhclust,dfhclust$subgrp==3)
hist(hclustsummary$recency)
summary(hclustsummary)

###Get cluster detail for cluster-4
hclustsummary<-subset(dfhclust,dfhclust$subgrp==4)
hist(hclustsummary$recency)
summary(hclustsummary)
###Get cluster detail for cluster-5
hclustsummary<-subset(dfhclust,dfhclust$subgrp==5)
hist(hclustsummary$recency)
summary(hclustsummary)

