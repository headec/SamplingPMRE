setwd("D:/1_YongKuk/1_SFU/2020 Laptop/STAT 410/Project")

data <- read.csv("property data.csv")

id1 <- data$strata
id2<-1:50

#Property value
value <- data$land+data$building

#Let's make the data simple now
data <- data.frame(id1,id2,value)

##CLUSTER SAMPLING## (2,7,9,10,11,13 만 했을때, 15개중 6개)

library(survey)

#Randomly choose clusters to be sampled
sample(1:14,4)

#Total number of clusters, N
n1<-14

#Number of ssus in psu_i where i = 2,7,9,10,11,13
n2<-c(rep(147,50),rep(65,50),rep(2998,50),rep(1398,50),rep(770,50),rep(1217,50))

#Indice of data of each cluster.
i2<- c(51:100)
i7<- c(301:350)
i9<- c(401:450)
i10 <- c(451:500)
i11<- c(501:550)
i13<- c(601:650)

#Save indice of sampled data
samps <- c(i2,i7,i9,i10,i11,i13)

#Save the selected clusters to a new dataset called 'prop'
prop <- data[samps,]

#Combine info altogether
prop <- cbind(prop,n1,n2 )

#Two-stage cluster sampling
prop.des <- svydesign(data=prop, id=~id1+id2,fpc=~n1+n2)

#Estimatd total value
est.total=svytotal(~value,design=prop.des)
est.total

#95% confidence interval
confint(est.total)

#Variance
vcov(est.total)

#Side-by-side box-plots for the value by cluster
#install.packages("ggplot2")
library('ggplot2')
ggplot(prop, aes(x=id1,y=value,group=id1,colors(id1)))+
  geom_boxplot(outlier.color = 'blue') +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))




##Pilot Study
#Randomly select the clusters to be sampled from the samples we are going to use.
sample(c(2,7,9,10,11,13),3)

#Number of ssus in psu_i where i=2,7,9; M_i. 
n2<-c(rep(147,50),rep(2998,50),rep(1217,50))

#Indice of data of each cluster.
i2<- c(51:100)
i9<- c(401:450)
i13<- c(601:650)

#Save indice of sampled data
samps <- c(i2,i9,i13)

#Save the selected clusters to a new dataset called 'prop'
prop <- data[samps,]

#Combine info altogether
prop <- cbind(prop,n1,n2 )

#Two-stage cluster sampling
prop.des <- svydesign(data=prop, id=~id1+id2,fpc=~n1+n2)

#Estimatd total value
est.total=svytotal(~value,design=prop.des)
est.total

#95% confidence interval
confint(est.total)

#Variance
vcov(est.total)

confint(est.total)

v<-data$value
mean(v[i2])
mean(v[i9])
mean(v[i13])
mean(prop$value[-(1:50)])
sd(v[i2])
sd(v[i9])
sd(v[i13])
