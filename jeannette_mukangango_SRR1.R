death_rates<-read.csv("~/Downloads/STATISTICAL REGRESSION with R/Stats Regr - Homework1/death_rates.csv")
View(death_rates)
summary(death_rates)
n<-length(death_rates$Sex)
colnames(death_rates)
x<-death_rates$Age
y<-death_rates$DeathRate
sex<-death_rates$Sex
sort(x)
sort(y)
x_bar<-sum(x)/n
y_bar<-sum(y)/n
var_x<-sum((x-x_bar)^2)/(n-1)
var_y<-sum((y-y_bar)^2)/(n-1)
sd_x<-sqrt(var_x)
sd_y<-sqrt(var_y)
cov_x_y<-sum((x-x_bar)*(y-y_bar))/(n-1)
cor_x_y<-cov_x_y/(sd_x*sd_y)
plot(x,y)

#############################################################################
#############################################################################
summary(y[1:31])
summary(y[32:62])
par(mfrow=c(1,2))
boxplot(y~sex,main="Male and Female Death rate",xlab="sex", ylab="Death rate")
hist(x,main ="Histogram of Age", xlab = "Age",ylab = "Frequency",col = "grey")

######################################################################################
######################################################################################
##2)the association between Death rates and Age, separately for males and females.
#calculation of correlation coefficient of male

female_data<-death_rates[death_rates$Sex=="F",]
summary(female_data)
x1<-female_data$Age
y1<-female_data$DeathRate
n1<-length(x1)
x1_bar<-sum(x1)/n1
y1_bar<-sum(y1)/n1
var_x1<-sum((x1-x1_bar)^2)/(n1-1)
sd_x1<-sqrt(var_x1)
var_y1<-sum((y1-y1_bar)^2)/(n1-1)
sd_y1<-sqrt(var_y1)
cov_x1_y1<-sum((x1-x1_bar)*(y1-y1_bar))/(n1-1);cov_x1_y1

#correlation coefficient between death_rates anf Age for male
cor_x1_y1<-cov_x1_y1/(sd_x1*sd_y1);cor_x1_y1

cor(y1,x1)
###################################################
male_data<-death_rates[death_rates$Sex=="M",]
summary(male_data)
x2<-male_data$Age
y2<-male_data$DeathRate
n2<-length(x2)
x2_bar<-sum(x2)/n2
y2_bar<-sum(y2)/n2
var_x2<-sum((x2-x2_bar)^2)/(n2-1)
sd_x2<-sqrt(var_x2)
var_y2<-sum((y2-y2_bar)^2)/(n2-1)
sd_y2<-sqrt(var_y2)
cov_x2_y2<-sum((x2-x2_bar)*(y2-y2_bar))/(n2-1);cov_x2_y2

#correlation coefficient between death_rates anf Age for male
cor_x2_y2<-cov_x2_y2/(sd_x2*sd_y2);cor_x2_y2

cor(y2,x2)
#########################################################################################

##################################################################################
##################################################################################
##3)

beta1_hat<-sum((x1-x1_bar)*y1)/sum((x1-x1_bar)^2)
beta0_hat<-y1_bar-(beta1_hat*x1_bar)
y1_hat<-beta0_hat+beta1_hat*x1   #we are predicting y1 values that correspond to x1
#par(mfrow=c(1,2))
plot(x1,y1,main = "Female death rate by age",xlab="Age",ylab = "Death rate")
lines(x1,y1_hat,lty=2,lwd=2,col="green")
plot(x2,y2,main = "Male death rate by age",xlab="Age",ylab = "Death rate")
#########################################################################################
#########################################################################################
#prediction equation
#4)
x3<-51
y_hat<-beta0_hat+beta1_hat*x3;y_hat     #we are answering question 3 by predicting the death rate that corresponds to age 51 years
##5)
sigma_hat<-sum((y1-beta0_hat-beta1_hat*x1)^2)/(n1-2)
var_x1<-sum((x1-x1_bar)^2)/(n-1)
var_beta1_hat<-var_x1/sum((x1-x1_bar)^2)
var_beta1_hat_hat<-sigma_hat/sum((x1-x1_bar)^2)
var_beta0_hat<-(sum((x1)^2)/(n*sum((x1-x1_bar)^2)))*sigma_hat
cov_beta0beta1_hat<-(-x1_bar/(sum((x1-x1_bar)^2)))*sigma_hat
var_y_hat_new<-var_beta0_hat+x3^2*var_beta1_hat+2*x3*cov_beta0beta1_hat
sd_y_hat_new<-sqrt(var_y_hat_new)
#############################################################################################
#############################################################################################
#5)
r_squared<-1-(sum((y1-y1_hat)^2)/(sum((y1-y1_bar)^2)));r_squared # cx



