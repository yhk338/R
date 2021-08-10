#clear workspace (this will drop all data frames from R)
rm(list=ls()) 

#Install libraries (only ever need to do this once -- once libraries are installed you just need to load them thereafter)
install.packages("wooldridge")
install.packages("sandwich")
install.packages("lmtest")
install.packages("plm")
install.packages("dplyr")
install.packages("ggplot2")

#Load libraries
library(wooldridge)
library(sandwich)
library(lmtest)
library(plm)
library(dplyr)
library(ggplot2)

data1 = read.csv("C:/Users/yangh/Downloads/stat_assign1_data.csv")
#This is a panel of individuals from 2 states with data on wage income, # of children, race, and others.
#Source: https://thetarzan.wordpress.com/2011/06/20/differences-in-differences-estimation-in-r-and-stata/
#We'll assume there is a policy change in 1994 which applies to individuals with children.
#We are interested in the effect of the policy on labor market participation (the probability of working).
#We need to create a treatment group, post-treatment period and interaction dummies

#First lets "look" at the data by using the summary command:
summary(data1)

data1$miami = as.numeric(data1$metarea ==5000)
data.means <- data1 %>% group_by(year, miami) %>% summarize(wagemean = mean(wage))
ggplot(data.means, aes(x = year, y = wagemean, linetype=factor(miami))) + geom_line()

data1$drop  = as.numeric(data1$metarea ==5000 & data1$edcode==1)
data.means2 <- data1 %>% group_by(year,drop) %>% summarize(wagemean2 = mean(wage))
ggplot(data.means2, aes(x = year, y = wagemean2, linetype=factor(drop))) + geom_line()+ scale_linetype_discrete(labels=c("Miami", "Rest of US"))


data1$notdrop  = as.numeric(data1$metarea ==5000 & data1$edcode!=1)
data.means3 <- data1 %>% group_by(year,notdrop) %>% summarize(wagemean3 = mean(wage))
ggplot(data.means3, aes(x = year, y = wagemean3, linetype=factor(notdrop))) + geom_line()+ scale_linetype_discrete(labels=c("Miami", "Rest of US"))

#Q4-1
stat_assign1_data$treat = as.numeric(stat_assign1_data$metarea ==5000)
stat_assign1_data$newOrlans = as.numeric(stat_assign1_data$metarea ==5560)
stat_assign1_data$atlanta = as.numeric(stat_assign1_data$metarea ==520)
stat_assign1_data$seattle = as.numeric(stat_assign1_data$metarea ==7600)
stat_assign1_data$bergen= as.numeric(stat_assign1_data$metarea ==5602)
stat_assign1_data$treat[stat_assign1_data$newOrlans==0 & stat_assign1_data$atlanta ==0 & stat_assign1_data$seattle ==0 & stat_assign1_data$bergen ==0  & stat_assign1_data$treat ==0 ] = NA

data.means2 <- stat_assign1_data %>% group_by(year, treat) %>% summarize(earnmean = mean(wage))

data.means2 <- na.omit(data.means2)

ggplot(data.means2, aes(x = year, y = earnmean, linetype=factor(treat))) + geom_line(aes(linetype=factor(treat))) + geom_point() + geom_vline(xintercept=1982) + scale_linetype_discrete(labels=c("Miami", "Control"))

#Q4-2
#First let's creat our "post" variable:
data1$post  = as.numeric(data1$year>=1982)
data1$pre  = as.numeric(data1$year<1982)
#Next, our "treatment" variable:
data1$treat = as.numeric(data1$miami) 

#Finally, we interact post and treatment.  This is our coefficient of interest in DD models:
data1$DD    = data1$treat*data1$post

#Can look at the basic diff-in-diff result by looking at means of outcome by year and treatment status:
data1 %>% group_by(year,treat) %>%summarise(mean(wage))

#Simples DD model:
m1<-lm(wage ~ DD + treat + post, data=data1)
summary(m1)
#To add fixed effects, use "factor()".  Here we want to use FEs for "year" (i.e., time FEs) and state
m1<-lm(wage ~ DD + treat + post + factor(year)+ factor(metarea), data=data1)
summary(m1)
#Note one "year" dummy is omitted? That is because with year FE we do not actually need to include our post dummy as it is collinear with them.  To prove it:
#m1<-lm(wage ~ DD + treat + factor(year) + factor(state), data=data1)
#summary(m1)
#Also likely want to throw some controls into regression:
#m1<-lm(wage ~ DD + treat + age + nonwhite + factor(ed) + factor(year) + factor(state), data=data1)
#summary(m1)



#Say I wanted to look at average wages for individuals with and without kids:
mean(subset(data1)$earn)
#Without kids. Note "==" means "equals to" in R:
mean(subset(data1,children==0)$earn)
#With kids (two possibilities). (i) #"!=" means "not equal", while ">" means greater than
mean(subset(data1,children!=0)$earn)
mean(subset(data1,children>0)$earn)

#Other common summ stats:
#Variance:
var(subset(data1)$earn)
#Length (i.e.,number of observations)
length(subset(data1)$earn)

# Another useful command to check groups: group_by()
install.packages("dplyr")
library(dplyr)

# Get sample size, mean and standard deviation of wages, separately for by ethnicity (2 groups):
data1 %>% group_by(nonwhite) %>%summarise(length(earn))
data1 %>% group_by(nonwhite) %>%summarise(mean(earn))
data1 %>% group_by(nonwhite) %>%summarise(sd(earn))

#To visualize data, often useful to graph histogram
hist(data1$ed, 
     main="Histogram of Education",
     xlab = "Years of Education", 
     ylab = "Number of observations",
     border="blue",
     col="green",
     xlim=c(0,12))

#Basic regression:
reg1=lm(earn~nonwhite, data=data1)
summary(reg1)
#Can add more explanatory variables:
reg1=lm(earn~nonwhite + age + children + ed, data=data1)
summary(reg1)

#Okay that is most of the summary commands that you will use.  Now let's think of what we want to do for a difference-in-differences.  With this dataset,
#there if a policy change in 1994. We therefore define "post" as an after 1994 dummy.  The change affected the with kids much more than those without kids,
#so we will define "treatment" as those with kids and "control" as those without.

#First let's creat our "post" variable:
data1$post  = as.numeric(data1$year>=1994)

#Next, our "treatment" variable:
data1$treat = as.numeric(data1$children>0) 

#Finally, we interact post and treatment.  This is our coefficient of interest in DD models:
data1$DD    = data1$treat*data1$post 

#Can look at the basic diff-in-diff result by looking at means of outcome by year and treatment status:
data1 %>% group_by(year,treat) %>%summarise(mean(earn))

#Simples DD model:
m1<-lm(earn ~ DD + treat + post, data=data1)
summary(m1)
#To add fixed effects, use "factor()".  Here we want to use FEs for "year" (i.e., time FEs) and state
m1<-lm(earn ~ DD + treat + post + factor(year) + factor(state), data=data1)
summary(m1)
#Note one "year" dummy is omitted? That is because with year FE we do not actually need to include our post dummy as it is collinear with them.  To prove it:
m1<-lm(earn ~ DD + treat + factor(year) + factor(state), data=data1)
summary(m1)
#Also likely want to throw some controls into regression:
m1<-lm(earn ~ DD + treat + age + nonwhite + factor(ed) + factor(year) + factor(state), data=data1)
summary(m1)

###Sidenote: For standard error, should likely cluster them at state level (in your assignment you should cluster at city level). Do this with coeftest command:
coeftest(m1,vcovHC(m1, type = "HC1",cluster="state"))


### Diff-in-diff graph:
#First calculate mean of earning to create the data you will graph
data.means <- data1 %>% group_by(year, treat) %>% summarize(earnmean = mean(earn))
#Then graph!
ggplot(data.means, aes(x = year, y = earnmean, linetype=factor(treat))) + geom_line()
#Now let's add some bells and whistles. First, vertical line to denote when policy changes using "geom_vline()"
ggplot(data.means, aes(x = year, y = earnmean, linetype=factor(treat))) + geom_line() + geom_vline(xintercept=1993)
#Finally let's label our lines using "scale_linetype_discrete()"
ggplot(data.means, aes(x = year, y = earnmean, linetype=factor(treat))) + geom_line(aes(linetype=factor(treat))) + geom_point() + geom_vline(xintercept=1993) + scale_linetype_discrete(labels=c("No Kids", "With Kids"))



#Suppose that instead of doing a generic treat if has children vs. not children for all individuals, you want to do this for only those with 11 years of education
#To do so, you can create a variable that takes value =1 if children>0 & ed==11 and =0 if children==0 & ed==11 with missing values assigned to all other individuals
#that do not have 11 years of education
data1$treat2 = ifelse(data1$ed == 11, as.numeric(data1$children > 0), NA)

#Another way to accomplish the same thing is to create variable =1 if children>0 & ed==11 and  otherwise, then replace the variable with missing values ("NA")
#if the ed group is not equal to 11
data1$treat3 = as.numeric(data1$children>0 & data1$ed==11) 



data1$treat3[data1$ed != 11] = NA

#Then graph as before, using our new treatment group definition "treat2" and relabelling "data.means" and "earnmean" to some other name
data.means2 <- data1 %>% group_by(year, treat2) %>% summarize(earnmean2 = mean(earn))
#Now let's drop obsevations labelled "NA" (i.e., missing).  R will think they are a treatment group otherwise in next line of graphing code
data.means2 <- na.omit(data.means2)
#Now graph:
ggplot(data.means2, aes(x = year, y = earnmean2, linetype=factor(treat2))) + geom_line(aes(linetype=factor(treat2))) + geom_point() + geom_vline(xintercept=1993) + scale_linetype_discrete(labels=c("No Kids", "With Kids"))






