install.packages('readxl')
library(readxl)

PBL <- read_excel("C:/Users/이정현/Desktop/R실습/DataScience/data.xlsx")

Director = PBL$director
PBL$director=as.numeric(factor(Director))

Distributor = PBL$distributor
PBL$distributor=as.numeric(factor(Distributor))

Name = PBL$name
PBL$name=as.numeric(factor(Name))


Thousand=PBL$thousand



names(PBL)

dim(PBL)
attach(PBL)

glm.fit=glm(thousand~.-name-thousand-TT, family=binomial, data=PBL)
summary(glm.fit)

coef(glm.fit)
summary(glm.fit)$coef
paisummary(glm.fit)$coef[,4]

glm.probs=predict(glm.fit, type="response")
glm.probs[1:10]
contrasts(thousand)

glm.pred=rep(0,503)
glm.pred[glm.probs>.5]=1
table(glm.pred,thousand)

(473+14)/503

TT=PBL$TT
train=(TT<1000)
PBL.1000=PBL[!train,]
thousand.1000 = thousand[!train]

glm.fit=glm(thousand~.-name-thousand-TT, family=binomial, data=PBL, subset=train)
glm.probs=predict(glm.fit, PBL.1000, type="response")
glm.pred=rep(0,101)
glm.pred[glm.probs>.5]=1
table(glm.pred, thousand.1000)
mean(glm.pred==thousand.1000)

glm.fit=glm(thousand~screen+seat+cumulative_spectator, family=binomial, data=PBL, subset=train)

