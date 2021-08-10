library(sandwich)
library(plm)
library(dplyr)
library(ggplot2)

data1 = read.csv("C:/Users/yangh/Downloads/R_class_example.csv")

#comment to myself: this summarizes the data
summary(R_class_example)

mean(subset(R_class_example)$earn)

# number of observations
length(subset(R_class_example)$earn)

#with children, treated group
mean(subset(R_class_example, children>0)$earn)
# same thing - mean(subset(R_class_example, children!=0)$earn)

#without children, control group
mean(subset(R_class_example, children==0)$earn)

#outcome = b0 +b1 inter + b2treat + b3post + e
#inter = post*treat/// treat: 1 if treated, 0 if control /// post: 1 if after, 0 if before

reg1 = lm(earn ~ nonwhite, data=R_class_example)
reg1
summary(reg1)

R_class_example$post = as.numeric(R_class_example$year>=1994)

R_class_example$treat = as.numeric(data1$children>=0)

R_class_example$DD = R_class_example$post * R_class_example$treat

#difference in difference curve
reg1 = 1m(earn ~ DD + treat + post, data=R_class_example)
summary(reg1)

reg1 = 1m(earn ~ DD + treat+ post + age + factor(ed) +factor(year)+ factor(state), data=R_class_example)
summary(reg1)

data.means <- R_class_example %>% group_by(year, treat) %>% summary(earnmean = mean(earn))

#drawing graph
ggplot(data.means, aes(x= year, y = earnmean, linetype= factor(treat))) + geom_line()