##########################################
###    1: Import attached text file    ###
##########################################

setwd("~/Documents/code/marketing-analysis")
data=read.table("data/Test.txt", header=T, sep='\t')

###########################################
###  2: Create indicator age variables  ###
###########################################
#AG1= 18-24, AG2= 25-44, AG3=45-64, AG4= 65+

AG1=ifelse((data$age>17 & data$age< 25), 1, 0)
AG2=ifelse((data$age>24 & data$age< 45), 1, 0)
AG3=ifelse((data$age>44 & data$age< 65), 1, 0)
AG4=ifelse((data$age>64), 1, 0)

data=(cbind(data, AG1, AG2, AG3, AG4))
head(data)

###########################################
###  3. Create top 3 category variable  ###
###########################################

T3B=(apply(data[,c(2:19,22:24)], 2, function(q) ifelse((q>7 & q<= 10), 1, 0)))
colnames(T3B)=(paste("T3B", colnames(T3B),sep=""))
head(T3B)

#######################################################
## 4. Are there significant differences in the means ##
#######################################################

#assume independence between male and female
T3B= as.data.frame(cbind(T3B, gender=data$gender))
male=subset(T3B,T3B$gender==1)
n1=nrow(male) #9973 males
female=subset(T3B,T3B$gender==2)
n2=nrow(female) #11741 females
p1=apply(male[,1:20], 2, mean)
p2=apply(female[,1:20], 2, mean)

#2 sided
# H0:Mu_males (p1) = Mu_females (p2)
# H1:Mu_males (p1)!= Mu_females (p2)

tstar=function(x){
  (p1[x]-p2[x])/
    (sqrt ( (p1[x]*(1-p1[x])/n1) + (p2[x]*(1-p2[x])/n2) ) )
}

first=tstar(1:19)

#case for NAs
male=na.omit(subset(T3B,T3B$gender==1))
n1=nrow(male) #9851 males
female=na.omit(subset(T3B,T3B$gender==2))
n2=nrow(female) #11621 females
p1=(apply(male[,20:21], 2, mean))
p2=(apply(female[,20:21], 2, mean))

second=tstar(1:2)
testStat=c(first,second)

#.01 level of significance
cv = abs(qnorm(.005))
test=ifelse((abs(testStat)> cv), "reject null", "cannot reject null")
test
table(test)

#one-sided test could be more informative 

###############################################################################
#####  Case 1. test hypothesis that improving brand image and attitudes   #####
#####  among the young audience will have better impact on Consideration  #####
###############################################################################

data=na.omit(data)
T3B=(apply(data[,c(2:19,22:24)], 2, function(q) ifelse((q>7 & q<= 10), 1, 0)))
colnames(T3B)=(paste("T3B", colnames(T3B),sep=""))
AG=ifelse((data$age>17 & data$age< 45), 1, 0)
T3B=as.data.frame(cbind(T3B, AG))
T3B=subset(T3B, AG==1)

mdl=glm(T3Bconsideration ~ . -T3Bimageattr4 -T3Bimageattr7 -T3Bimageattr10 -T3Bimageattr20 
        -T3Bimageattr15 -T3Bimageattr22 -T3Bimageattr23 -T3Bimageattr24 -T3Bimageattr2 
        -T3Bimageattr5 -AG -T3Bimageattr21, 
        family=binomial, data = T3B)
summary(mdl)
