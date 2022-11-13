#validation set 기법
library(ISLR)
set.seed(1)#난수발생 시드설정
train=sample(392,196)
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)#훈련셋에 없는 관측치만
#mse=23.26

#Poly를통한 다항식(2차)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
#Poly를통한 다항식(3차)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

set.seed(2)#난수발생 시드변경
train=sample(392,196)#validation set 기법
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)#훈련셋에 없는 관측치만
#mse=23.26

#Poly를통한 다항식(2차)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
#Poly를통한 다항식(3차)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)



########
#Loocv기법
library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta

#for문으로 5차식까지 구현
cv.error=rep(0,5)
for(i in 1:5)
{
glm.fit=glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error
plot(cv.error,type='o')


############
#K-fold 검증
set.seed(17)
cv.error.10=rep(0,10)
for(i in 1:10){
  glm.fit=glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10
plot(cv.error.10,type='o')
################

#bootstrap 리니어리그레션에 응용해보기
alpha.fn=function(data,index)
{
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y))))
}  
alpha.fn(Portfolio,1:100)
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace = T))
boot(Portfolio,alpha.fn,R=1000)

#######정확도추정
boot.fn=function(data,index)
return(coef(lm(mpg~horsepower,data=data,subset=index)))  
boot.fn(Auto,1:392)
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))
#r에서 자동 지원
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data=Auto))$coef
boot.fn=function(data,index)
coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))  
set.seed(1)
boot(Auto,boot.fn,1000)

summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef
