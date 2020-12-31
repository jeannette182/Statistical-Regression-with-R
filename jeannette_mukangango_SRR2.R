#IMPORTING DATASET
data1<-read.csv((file.choose()))
data1
Purchase=data1$Purchase
Gender=data1[,3]
Gender
Age_num=data1[,7]
Age_num
Marital_Status=data1[,6]
Marital_Status
View(data1)
b=summary(data1)
b

x=data1$Marital_Status
x
table(x)
dim(data1)
names(data1)
head(data1)
#print(xtable(b),"latex")
boxplot(Purchase~Gender,main="Male and Female customers",xlab="Gender", ylab="Purchase")

hist(Age_num,main ="Histogram of Age", xlab = "Age_num",ylab = "Frequency",col = "grey")
count<- c(93654,65346)
lbs<- c("Unmarried","Married")
pie(count,labels=lbs)



#QUESTION 2
#a)first model all variables are included
model1=lm(Purchase~Gender+City_Category+Stay_In_Current_City_Years+Marital_Status+Age_num,data = data1)
c=summary(model1)
c
print(xtable(c),"latex")
#model 2 Marital_Status and Stay_In_Current_City_Years removed because they have no significant in model1


#QUESTION3/BEST MODEL
model2=lm(Purchase~Gender+City_Category+Age_num,data = data1)
summary(model2)
d=summary(model2)
d
print(xtable(d),"latex")


#QUESTION4
par(mfrow=c(2,2))
#1.linearity
plot(model2,which =1) 
#2.normality
plot(model2,which =2)
#3.homoskedastic
plot(model2,which =3)
#2.potential outliers
plot(model2,which =4)
#QUESTION 7
model3=lm(Purchase~Gender+Age_num,data = data1)
e=summary(model3)
e
#COMPARE model1 and model2
anova(model2,model3) #model3 with gender and age has significant while model 2 has no significant 
                     #which means model with gender and age is better than model with gender,
                     #city category and age.

