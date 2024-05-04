#CFA and SEM 
rm(list=ls())
library(readxl)
library(openxlsx)
library(dplyr)
library(lavaan)
library(lavaanPlot)
library(energy)
library(psych)
library(semTools)

estimator<-"DWLS"
data<-read.csv() #enter path of the dataset
country_code<-read_excel() #enter path of country codes dataset


colnames(country_code)<-c(c("country","count","continent"))
df <- merge(data, country_code, by = "country", all.x = TRUE) #assigning the countries their respective continent


#taking a subset of the whole dataset
questions <- paste0("Q", 1:42, "A")
subdf<-df[,c(questions,"education","urban","gender","engnat","age","religion","hand","orientation","race","voted","married","familysize","major","country","continent")]


subdf<-subdf[subdf$country %in% c("GB","DE","FR","PL"), ]
subdf <- na.omit(subdf) #removing empty rows
subdf<-subdf[subdf$gender %in% c(1:2), ]
subdf<-subdf[subdf$orientation %in% c(1:5), ]

subdf$gender <- ifelse(subdf$gender == 1, 0, 1)
subdf$orientation <- ifelse(subdf$orientation == 1, 0, 1)

cf<-subdf[,questions]

#calculating polychoric correlations
stress<-cf[,c(1,6,8,11,12,14,18,22,27,29,32,33,35,39)]
anxiety<-cf[,c(2,4,7,9,15,19,20,23,25,28,30,36,40,41)]
depression<-cf[,c(3,5,10,13,16,17,21,24,26,31,34,37,38,42)]

stresscor<-lavCor(stress,estimator="two.step",ordered=TRUE)
anxietycor<-lavCor(anxiety,estimator="two.step",ordered=TRUE)
depressioncor<-lavCor(depression,estimator="two.step",ordered=TRUE)

stresscor
anxietycor
depressioncor

##CFA
#Model Specification
#Model 1
model.cfa<-'Stress=~Q1A+Q6A+Q8A+Q11A+Q12A+Q14A+Q18A+Q22A+Q27A+Q29A+Q32A+Q33A+Q35A+Q39A
        Anxiety=~Q2A+Q4A+Q7A+Q9A+Q15A+Q19A+Q20A+Q23A+Q25A+Q28A+Q30A+Q36A+Q40A+Q41A
        Depression=~Q3A+Q5A+Q10A+Q13A+Q16A+Q17A+Q21A+Q24A+Q26A+Q31A+Q34A+Q37A+Q38A+Q42A
'

fit.cfa<- cfa(model.cfa, data = subdf, estimator = estimator, control = list(optimizer = "nlminb", iter.max = 10000),ordered = TRUE)
summary.cfa<-summary(fit.cfa,standardized=TRUE,fit.measures=TRUE,rsq=TRUE)


sink("cfa_DWLS.txt")
summary.cfa
sink()

##SEM
#Model Specification
#Model 2
model.sem <- '
  #measurement model
        Stress=~Q1A+Q6A+Q8A+Q11A+Q12A+Q14A+Q18A+Q22A+Q27A+Q29A+Q32A+Q33A+Q35A+Q39A
        Anxiety=~Q2A+Q4A+Q7A+Q9A+Q15A+Q19A+Q20A+Q23A+Q25A+Q28A+Q30A+Q36A+Q40A+Q41A
        Depression=~Q3A+Q5A+Q10A+Q13A+Q16A+Q17A+Q21A+Q24A+Q26A+Q31A+Q34A+Q37A+Q38A+Q42A
        
        #regression
        Stress~Anxiety
        Anxiety~gender
        Depression~Stress+orientation
'

fit.sem <- sem(model.sem, data = subdf, estimator = estimator,control = list(optimizer = "nlminb", iter.max = 10000),ordered=TRUE)
summary.sem<-summary(fit.sem,standardized=TRUE,fit.measures=TRUE,rsq=TRUE)

sink("sem_DWLS.txt")
summary.sem
sink()

sink("polychoric.txt")
stresscor
anxietycor
depressioncor
sink()

