### Healthcare_cost_analysis###
HospitalCosts=read.csv("hospital_costs.csv", header=TRUE)
head(HospitalCosts)
#columns of data#
names(HospitalCosts)
#############1. To record the patient statistics, the agency wants to find the age category of people who frequently visit the hospital and has the maximum expenditure.
#The agency wants to find the age category of people who frequently visit the hospital and has the maximum expenditure.
#Age: Age of the patient discharged
#Totchg: Hospital discharge costs
summary(HospitalCosts)
#Get number of hospital visits based on age
summary(as.factor(HospitalCosts$AGE))
#Total number of hospital for 0-1 age group is 307
hist(HospitalCosts$AGE, main="Histogram of Age Group and their hospital visits",
     xlab="Age group", border="black", col=c("light green", "dark green"), xlim=c(0,20), ylim=c(0,350))
#observation point:As can be seen here, the maximum number of hospital visits are for age group is 0-1 years
#Summarize expenditure based on age group
ExpenseBasedOnAge = aggregate(TOTCHG ~ AGE, FUN=sum, data=HospitalCosts)
which.max(tapply(ExpenseBasedOnAge$TOTCHG, ExpenseBasedOnAge$TOTCHG, FUN=sum))
barplot(tapply(ExpenseBasedOnAge$TOTCHG, ExpenseBasedOnAge$AGE, FUN=sum))
#observation:Maximum expenditure for 0-1 yr is 678118
#################2. In order of severity of the diagnosis and treatments and to find out the expensive treatments, the agency wants to find the diagnosis-related group that has maximum hospitalization and expenditure.
#In order of severity of the diagnosis and treatments and to find out the expensive treatments, the agency wants to find the diagnosis-related group that has maximum hospitalization and expenditure.
#i)Aprdrg: All Patient Refined Diagnosis Related Groups
#ii)Totchg: Hospital discharge costs
summary(as.factor(HospitalCosts$APRDRG))
##Get the diagnosis-related group and its hospitalization expenditure
DiagnosisCost = aggregate(TOTCHG ~ APRDRG, FUN = sum, data = HospitalCosts)
DiagnosisCost[which.max(DiagnosisCost$TOTCHG), ]
#observation:As can be seen here 640 diagnosis related group had a max cost of 437978
################3. Race vs Hospitalization costs#####################################################
#To make sure that there is no malpractice, the agency needs to analyze if the race of the patient is related to the hospitalization costs.
#Ho (Null hypothesis):Independent variable (RACE) is not influencing dependent variable (COSTS) #H0:there is no no correlation among residuals, # p-value = 0.7394 <== this is > 0.5 #i.e. they are independent #in case of regression, we need high p value so that we cannot reject the null
summary(as.factor(HospitalCosts$RACE))
#obs:There is one null value. This needs to be removed
HospitalCosts = na.omit(HospitalCosts)
summary(as.factor(HospitalCosts$RACE))
### As can be seen 484 patients out of 499 fall under group 1, showing that the number of observations for 1 category is way higher than others - hence data is skewed. This will only affect the results from linear regression or ANOVA analysis
raceInfluence=lm(TOTCHG~ RACE, data=HospitalCosts)
summary(raceInfluence)
#observation
#pValue is 0.69 it is much higher than 0.5
#We can say that race doesn’t affect the hospitalization costs
####Anaysis using ANOVA
#We can also use anova statistical test for estimating how dependent variable, in this case RACE, affects the independent variable, the hospitalization cost
raceInfluenceAOV <- aov(TOTCHG ~ RACE, data=HospitalCosts)
raceInfluenceAOV
summary(raceInfluenceAOV)
#The residual variance (deviation from original) (of all other variables) is very high. This implies that there is very little influence from RACE on hospitalization costs
#As can be seen, the degree of freedom (Df) for RACE is 1 and that of residuals is 497 observations
#The F-Value, the test statistic is 0.16 which is much less than 0.5 showing that RACE doesn’t affect teh hospitalization cost.
#The Pr(>F), the p_value of 0.69 is high confirming that RACE does not affect hospitalization cost.
###########4. To properly utilize the costs, the agency has to analyze the severity of the hospital costs by age and 
##########gender for the proper allocation of resources.##########################################################
summary(HospitalCosts$FEMALE)
summary(lm(formula = TOTCHG ~ AGE + FEMALE, data = HospitalCosts))
#Since the pValues of AGE is much lesser than 0.05, the ideal statistical significance level, and it also has three stars (***) next to it, it means AGE has the most statitical significance
#Similarly, gender is also less than 0.05.
#Hence, we can conclude that the model is statistically significant
#############5. Since the length of stay is the crucial factor for inpatients, the agency wants to
####find if the length of stay can be predicted from age, gender, and race.###########################################
summary(lm(formula = LOS ~ AGE + FEMALE + RACE, data = HospitalCosts))
##The p-value is higher than 0.05 for age, gender and race, indicating there is no linear relationship between these variables and length of stay.
#Hence, age, gender and race cannot be used to predict the length of stay of inpatients.
#######################6. Complete analysis##############################################################
#The agency wants to find the variable that mainly affects hospital costs.
#Significance method - build a model using all independent variables vs dependent variable
summary(lm(formula = TOTCHG ~ ., data = HospitalCosts))
summary(lm(formula = TOTCHG ~ AGE + FEMALE + LOS + APRDRG, data = HospitalCosts))
summary(lm(formula = TOTCHG ~ AGE + LOS + APRDRG, data = HospitalCosts))
###########Since APRDRG has -ve t-value, dropping it.
summary(lm(formula = TOTCHG ~ AGE + LOS, data = HospitalCosts))
##########################Analysis Conclusion:###########################################################
#As is evident in the multiple models above, health care costs is dependent on age, length of stay and the diagnosis type.
#Healthcare cost is the most for patients in the 0-1 yrs age group category
#Maximum expenditure for 0-1 yr is 678118
#Length of Stay increases the hospital cost

#All Patient Refined Diagnosis Related Groups also affects healthcare costs

#640 diagnosis related group had a max cost of 437978
#Race or gender doesn’t have that much impact on hospital cost









