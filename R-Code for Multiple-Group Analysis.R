#CFA-MGA and MG-SEM
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
group<-"country"
data<-read.csv() #enter path of the dataset
country_code<-read_excel() #enter path of country codes dataset


colnames(country_code)<-c(c("country","count","continent"))
df <- merge(data, country_code, by = "country", all.x = TRUE) #assigning the countries their respective continent


#taking a subset of the whole dataset
questions <- paste0("Q", 1:42, "A")
subdf<-df[,c(questions,"education","urban","gender","engnat","age","religion","hand","orientation","race","voted","married","familysize","major","country","continent")]



subdf<-subdf[subdf$country %in% c("GB","DE","FR","PL"), ]
subdf <-na.omit(subdf) #removing empty rows
subdf<-subdf[subdf$gender %in% c(1:2), ]
subdf<-subdf[subdf$orientation %in% c(1:5), ]

subdf$gender <- ifelse(subdf$gender == 1, 0, 1)
subdf$orientation <- ifelse(subdf$orientation == 1, 0, 1)

all.results<-matrix(NA, nrow = 3, ncol = 6)

#multigroups
#cfa
mg.cfa<-'Stress=~Q1A+Q6A+Q8A+Q11A+Q12A+Q14A+Q18A+Q22A+Q27A+Q29A+Q32A+Q33A+Q35A+Q39A
        Anxiety=~Q2A+Q4A+Q7A+Q9A+Q15A+Q19A+Q20A+Q23A+Q25A+Q28A+Q30A+Q36A+Q40A+Q41A
        Depression=~Q3A+Q5A+Q10A+Q13A+Q16A+Q17A+Q21A+Q24A+Q26A+Q31A+Q34A+Q37A+Q38A+Q42A
'

#baseline model (Model 3)
baseline.cfa.model <- measEq.syntax(configural.model = mg.cfa,
                                    data = subdf,
                                    ordered = questions,
                                    parameterization = "delta",
                                    ID.fac = "std.lv",
                                    ID.cat = "Wu.Estabrook.2016",
                                    group = group,
                                    group.equal = "configural" )

baseline.cfa.model<-as.character(baseline.cfa.model)
baseline.cfa<-cfa(baseline.cfa.model,data=subdf,group=group,ordered = TRUE,estimator=estimator)


summary.cfa.mg<-summary(baseline.cfa,standardized=TRUE,fit.measures=TRUE,rsq=TRUE)


sink("g.cfa_mg_baseline.txt")
summary.cfa.mg
sink()


#thresholds equal (Model 4)
thresholds.cfa.model <- measEq.syntax(configural.model = mg.cfa,
                                      data = subdf,
                                      ordered = questions,
                                      parameterization = "delta",
                                      ID.fac = "std.lv",
                                      ID.cat = "Wu.Estabrook.2016",
                                      group = group,
                                      group.equal = "thresholds" )

thresholds.cfa.model<-as.character(thresholds.cfa.model)
thresholds.cfa<-cfa(thresholds.cfa.model,data=subdf,group=group,ordered = TRUE,estimator=estimator)


summary.cfa.mg.t<-summary(thresholds.cfa,standardized=TRUE,fit.measures=TRUE,rsq=TRUE)


sink("g.cfa_mg_thresholds.txt")
summary.cfa.mg.t
sink()


#thresholds and loadings equal (Model 5)
loadings.cfa.model <- measEq.syntax(configural.model = mg.cfa,
                                    data = subdf,
                                    ordered = questions,
                                    parameterization = "delta",
                                    ID.fac = "std.lv",
                                    ID.cat = "Wu.Estabrook.2016",
                                    group = group,
                                    group.equal = c("thresholds","loadings") )

loadings.cfa.model<-as.character(loadings.cfa.model)
loadings.cfa<-cfa(loadings.cfa.model,data=subdf,group=group,ordered = TRUE,estimator=estimator)


summary.cfa.mg.l<-summary(loadings.cfa,standardized=TRUE,fit.measures=TRUE,rsq=TRUE)


sink("g.cfa_mg_loadings.txt")
summary.cfa.mg.l
sink()



all.results[1,]<-round(data.matrix(fitmeasures(baseline.cfa,
                                               fit.measures = c("chisq","df","pvalue",
                                                                "cfi", "tli", "rmsea"))),
                       digits=3)

all.results[2,]<-round(data.matrix(fitmeasures(thresholds.cfa,
                                               fit.measures = c("chisq","df","pvalue",
                                                                "cfi", "tli", "rmsea"))),
                       digits=3)

all.results[3,]<-round(data.matrix(fitmeasures(loadings.cfa,
                                               fit.measures = c("chisq","df","pvalue",
                                                                "cfi", "tli", "rmsea"))),
                       digits=3)

print(all.results)


summary_sem <- cbind(c("Baseline", "Equal Tresholds", "Equal Tresholds and Loadings"), all.results) %>%
  as.data.frame() %>% 
  rename(Model = V1,
         DWLS_based_chi2 = V2,
         df = V3,
         p_value = V4,
         CFI = V5,
         TLI = V6,
         RMSEA = V7
  ) 

summary_sem %>% 
  knitr::kable()


summary_2 <- rbind(
  (lavTestLRT(baseline.cfa,thresholds.cfa)),
  (lavTestLRT(thresholds.cfa,loadings.cfa))
)[c(1,2,4), c(5,7,8)] %>%
  as_tibble() %>%
  round(., 3)

summary_2 %>% 
  knitr::kable()


#table of fit indices
options(knitr.kable.NA = '-') 
cbind(summary_sem, summary_2) %>% 
  knitr::kable(., "simple",
               col.names = c("Model", "DWLS-based$\\chi^2$", "$df$", "$p$", "CFI", "TLI", "RMSEA",
                             "$\\Delta\\chi^2$", "$\\Delta df$", "$p$"),
               align = c("l", "c", "c", "c", "c", "c", "c", "c", "c", "c"))


##SEM MGA
#Model Specification
#Model 6
modelSEM <- '
  #measurement model
        Stress=~Q1A+Q6A+Q8A+Q11A+Q12A+Q14A+Q18A+Q22A+Q27A+Q29A+Q32A+Q33A+Q35A+Q39A
        Anxiety=~Q2A+Q4A+Q7A+Q9A+Q15A+Q19A+Q20A+Q23A+Q25A+Q28A+Q30A+Q36A+Q40A+Q41A
        Depression=~Q3A+Q5A+Q10A+Q13A+Q16A+Q17A+Q21A+Q24A+Q26A+Q31A+Q34A+Q37A+Q38A+Q42A
        
        #regression
        Stress~Anxiety
        Anxiety~gender
        Depression~Stress+orientation'

fitsem.mg<-fitsem <- sem(modelSEM, data = subdf, estimator = estimator,group="country",control = list(optimizer = "nlminb", iter.max = 10000),ordered=TRUE)
summarysem.mg<-summary(fitsem.mg,standardized=TRUE,fit.measures=TRUE,rsq=TRUE)

sink("sem_DWLS_mg.txt")
summarysem.mg
sink()



