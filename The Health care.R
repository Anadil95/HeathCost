#import library
library(readxl)
#import dataset
hospitalcosts <- read_excel("Downloads/hospitalcosts.xlsx")
#View the dataset
View(hospitalcosts)
#see the first 5 rows 
head(hospitalcosts)
#Summary of the dataset which show staitcs 
summary(hospitalcosts)
#show the first six rows of Age column
head(hospitalcosts$AGE)
#Show summary of Age column
summary(hospitalcosts$AGE)
#Frequency table for Age 
table(hospitalcosts$AGE)
#Show a Histogram of Age 
hist(hospitalcosts$AGE,breaks =30,col="blue", xlab="Age", main="The Age distrbution ")
hist(hospitalcosts$AGE,col="blue", xlab="Age", main="The Age distrbution ")
##Show the frequency for Age "will give the same output as table() function 
summary(as.factor(hospitalcosts$AGE))
#The max frequency in Age for both ways
max(table(hospitalcosts$AGE))
max(summary(as.factor(hospitalcosts$AGE)))
#The max value in Age for both ways
which.max(table(hospitalcosts$AGE))
which.max(summary(as.factor(hospitalcosts$AGE)))

age <- aggregate(TOTCHG ~ AGE, data = hospitalcosts, sum)
max(age)


#In order of severity of the diagnosis and treatments and to find out the expensive treatments, the agency wants to find the diagnosis related group that has maximum hospitalization and expenditure.
t <- table(hospitalcosts$APRDRG)
d <- as.data.frame(t)
names(d)[1] = 'Diagnosis Group'
d
which.max(t)
res <- aggregate(TOTCHG ~ APRDRG, data = hospitalcosts, sum)
res
which.max(res$TOTCHG)
res[which.max(res$TOTCHG),]


#To make sure that there is no malpractice, the agency needs to analyze if the race of the patient is related to the hospitalization costs
table(hospitalcosts$RACE)
hospitalcosts$RACE <- as.factor(hospitalcosts$RACE)
fit <- lm(TOTCHG ~ RACE,data=hospitalcosts)
fit
summary(fit)
fit1 <- aov(TOTCHG ~ RACE,data=hospitalcosts)
summary(fit1)
hospitalcosts <- na.omit(hospitalcosts)

#To properly utilize the costs, the agency has to analyze the severity of the hospital costs by age and gender for proper allocation of resources.
table(hospitalcosts$FEMALE)
a <- aov(TOTCHG ~ AGE+FEMALE,data=hospitalcosts)
summary(a)
b <- lm(TOTCHG ~ AGE+FEMALE,data=hospitalcosts)
summary(b)

#Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of stay can be predicted from age, gender, and race.
table(hospitalcosts$LOS)
cat <- aov(LOS ~ AGE+FEMALE+RACE,data=hospitalcosts)
summary(cat)
cat <- lm(LOS ~ AGE+FEMALE+RACE,data=hospitalcosts)
summary(cat)

#To perform a complete analysis, the agency wants to find the variable that mainly affects the hospital costs.
aov(TOTCHG ~.,data=hospitalcosts)
mod <- lm(TOTCHG ~ .,data=hospitalcosts)
summary(mod)
