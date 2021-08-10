install.packages(“AER”)
library(AER)

data1 = read.csv("C:/Users/yangh/Downloads/data_assign2.csv")

data1$X = as.numeric(data1$thirdkid)
data1$Z = as.numeric((data1$boy1st==1 & data1$boy2nd==1) |(data1$boy1st==0 & data1$boy2nd==0))

mean(subset(data1,Z==1)$thirdkid)

m1 = lm(X∼Z, data=data1)
summary(m1)

data1$X = as.numeric(data1$thirdkid)
data1$Z = as.numeric((data1$boy1st==1 & data1$boy2nd==0) | (data1$boy1st==0 & data1$boy2nd==1))



data1$Y = as.numeric(data1$workedm)
data1$Z = as.numeric((data1$boy1st==1 & data1$boy2nd==1) |(data1$boy1st==0 & data1$boy2nd==0))

mean(subset(data1,Z==1)$thirdkid)

m4 = ivreg(Y ~ X + agem1 + I(agem1^2) + black +hispan +othrace + agefstm + I(agefstm^2) | Z+ agem1 + I(agem1^2) + black +hispan +othrace + agefstm + I(agefstm^2), data=data1 )
coeftest(m4, vcov=sandwich)
---------------------------------------------------------------------------------------------------
data1$X = as.numeric(data1$thirdkid==1 & data1$boy1st==0 & data1$boy2nd==0 )
data1$Z = as.numeric(data1$boy1st==0 & data1$boy2nd==0 )

m1 = lm(X∼Z, data=data1)
summary(m1)


data1$Y = as.numeric(data1$workedm==1 & data1$boy1st==0 & data1$boy2nd==0 )
data1$Z = as.numeric(data1$boy1st==0 & data1$boy2nd==0 )

m2 = lm(Y∼Z, data=data1)
summary(m2)
---------------------------------------------------
data1$X = as.numeric(data1$thirdkid)
data1$Z = as.numeric(data1$boy1st==1 & data1$boy2nd==1)


data1$Y = as.numeric(data1$thirdkid)
data1$Z = as.numeric(data1$boy1st==0 & data1$boy2nd==0 )

mean(subset(data1,Z==1)$thirdkid)
data1$example -> na.omit(data.means2)

