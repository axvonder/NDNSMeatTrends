library(dplyr)
library(tidyverse)
library(srvyr)
library(survey)
library(effects)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(gridExtra)
library(cowplot)
library(MASS)
#set wd
setwd("/Users/alexandervonderschmidt/Library/CloudStorage/OneDrive-SharedLibraries-UniversityofEdinburgh/NDNS Meat Trends - General/Data")
#upload datasets
dat <- read.csv('omega.csv')

#########################EDITS AND CREATIONS#######################

#remove participants with only 3 diary days (removed n = 323; n = 15,332)
dat <- dat[!(dat$DiaryDaysCompleted == 3),]

#define age brackets; <10 = 1; 11-17 = 2; 18-40 = 3; 41-59 = 4; >= 60 = 5
dat <- dat %>%
  mutate(AgeG = case_when(
    Age <= 10 ~ 1,
    Age >= 11 & Age <= 17 ~ 2,
    Age >= 18 & Age <= 40 ~ 3,
    Age >= 41 & Age <= 59 ~ 4,
    Age >= 60 ~ 5,
    TRUE ~ 99
  ))
#set reference group for age (18-40)
dat$AgeG <- as.factor(dat$AgeG)
dat$AgeG <- relevel(dat$AgeG, ref = 3)
#eqv as factor
dat$eqv <- as.factor(dat$eqv)
#sex as factor
dat$Sex <- as.factor(dat$Sex)
dat$Sex <- factor(dat$Sex, levels = c(1, 2), labels = c("M", "F"))#had some problems, just makes things easier to change to letters
#create variable for '% of SMTs that contain meat'
#breakfast
dat$BMeatokajperc <- dat$BMeatokaj/dat$Btotokaj
dat$BProcessedokajperc <- dat$BProcessedokaj/dat$Btotokaj
dat$BRedokajperc <- dat$BRedokaj/dat$Btotokaj
dat$BWhiteokajperc <- dat$BWhiteokaj/dat$Btotokaj
#lunch
dat$LMeatokajperc <- dat$LMeatokaj/dat$Ltotokaj
dat$LProcessedokajperc <- dat$LProcessedokaj/dat$Ltotokaj
dat$LRedokajperc <- dat$LRedokaj/dat$Ltotokaj
dat$LWhiteokajperc <- dat$LWhiteokaj/dat$Ltotokaj
#dinner
dat$DMeatokajperc <- dat$DMeatokaj/dat$Dtotokaj
dat$DProcessedokajperc <- dat$DProcessedokaj/dat$Dtotokaj
dat$DRedokajperc <- dat$DRedokaj/dat$Dtotokaj
dat$DWhiteokajperc <- dat$DWhiteokaj/dat$Dtotokaj

#set survey designs
#make survey year factor or numeric, depending on analyses intended to be completed
#make sure to re-set the survey design after you've changed surveyyear's variable type
dat$SurveyYear <- as.factor(dat$SurveyYear)#run for regression analyses
dat$SurveyYear <- as.numeric(dat$SurveyYear)#run for plots and also for calculating p value trends across all years (not just y1 & y11)

#RERUN SURVEY DESIGN IF YOU'VE CHANGE ANY VARIABLES TO/FROM FACTOR/NUMERIC
#specify survey weighting structure for GLM
dat$fpc <- 15332
dat.design <-
  svydesign(
    id = ~area,
    strata = ~astrata5,
    data = dat,
    weights = ~wti,
    fpc = ~fpc
  )

#create the survey design object (for descriptive)
survey_design <- svydesign(id = ~area, strata = ~astrata5, weights = ~wti, data = dat)


#####################TABLE 1 - DEMOGRAPHICS#######################

#subset population into years 1 and 11
dat1 <- dat[dat$SurveyYear == 1, ]
dat11 <- dat[dat$SurveyYear == 11, ]

#specify survey weighting structure for descriptive analysis
survey_design1 <- dat1 %>%
  as_survey_design(ids = area, # cluster ids
                   weights = wti, # weight variable created above
                   strata = astrata5 # sampling was stratified by district
  )
survey_design11 <- dat11 %>%
  as_survey_design(ids = area, # cluster ids
                   weights = wti, # weight variable created above
                   strata = astrata5 # sampling was stratified by district
  )

survey_design1 %>%
  summarise(meatx = survey_mean(sumMeatg))
415/4
survey_design11 %>%
  summarise(meatx = survey_mean(sumMeatg))
345/4

(415-345)/4

#count age groups 
#unweighted Ns
table(dat1$AgeG)
table(dat11$AgeG)
#weighted%s
survey_design1 %>%
  group_by(AgeG) %>%
  summarise(pct = survey_mean())
survey_design11 %>%
  group_by(AgeG) %>%
  summarise(pct = survey_mean())

#sex
#unweighted Ns
table(dat1$Sex)
table(dat11$Sex)
#weighted %s
survey_design1 %>%
  group_by(Sex) %>%
  summarise(pct = survey_mean())
survey_design11 %>%
  group_by(Sex) %>%
  summarise(pct = survey_mean())

#income tertiles
#unweighted Ns
table(dat1$eqv)
table(dat11$eqv)
#missing
1629-(506+448+470)
1076-(315+307+312)
#percentages of income tertiles
survey_design1 %>%
  group_by(eqv) %>%
  summarise(pct = survey_mean())
survey_design11 %>%
  group_by(eqv) %>%
  summarise(pct = survey_mean())


#count % of meat consumers
survey_design1 <- mutate(survey_design1, meat_gt_0 = as.numeric(sumMeatg > 0))
survey_design1 %>%
  group_by(meat_gt_0) %>%
  summarise(pct = survey_mean()) #96.4%

# Create a new variable indicating whether sumMeatg > 0
survey_design11 <- mutate(survey_design11, meat_gt_0 = as.numeric(sumMeatg > 0))
survey_design11 %>%
  group_by(meat_gt_0) %>%
  summarise(pct = survey_mean()) #93.4%


#############################TABLE 2 - MAIN ANALYSIS ###################

# Creates a custom summary function for exponentiated coefficients
# as well as printing out the 95% confidence interval values
# (and retains significance indicators)
exp_summary <- function(model) {
  # Exponentiate the sum between the intercept and SurveyYearX coefficients
  exp_diff <- exp(coef(model)["(Intercept)"] + coef(model)[-1])
  
  # Exponentiate the confidence intervals
  conf_int <- confint(model)
  exp_conf_int <- exp(conf_int)
  
  # Calculate the confidence intervals for the differences
  exp_diff_conf_int <- exp(conf_int["(Intercept)",] + conf_int[-1,])
  
  # Create a new summary object
  exp_summary_obj <- summary(model)
  
  # Calculate the z-values and p-values
  z_values <- coef(model) / exp_summary_obj$coefficients[, "Std. Error"]
  p_values <- 2 * pnorm(-abs(z_values))
  
  # Replace the coefficients and confidence intervals with the exponentiated values
  exp_summary_obj$coefficients <- rbind(c(exp(coef(model)["(Intercept)"]), exp_summary_obj$coefficients[1, "Std. Error"], exp_conf_int[1,], p_values[1]),
                                        cbind(exp_diff,
                                              exp_summary_obj$coefficients[-1, "Std. Error"],
                                              exp_diff_conf_int,
                                              p_values[-1]))
  
  # Update the column names
  colnames(exp_summary_obj$coefficients) <- c("Exp(Coef)", "Std. Error", "2.5 %", "97.5 %", "Pr(>|z|)")
  
  # Include the significance stars
  signif.stars <- options("show.signif.stars")
  if (is.logical(signif.stars) && signif.stars) {
    exp_summary_obj$coefficients <- cbind(exp_summary_obj$coefficients, 
                                          exp_summary_obj$coefficients[, "Pr(>|z|)"])
    colnames(exp_summary_obj$coefficients)[ncol(exp_summary_obj$coefficients)] <- " "
    exp_summary_obj$coefficients[, " "] <- symnum(exp_summary_obj$coefficients[, "Pr(>|z|)"], 
                                                  corr = FALSE, na = FALSE,
                                                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                                  symbols = c("***", "**", "*", ".", " "))
  }
  
  return(exp_summary_obj)
}

lm_summary <- function(model) {
  # Calculate the sum between the intercept and SurveyYearX coefficients
  diff <- coef(model)["(Intercept)"] + coef(model)[-1]
  
  # Calculate the confidence intervals
  conf_int <- confint(model)
  
  # Calculate the confidence intervals for the differences
  diff_conf_int <- conf_int["(Intercept)",] + conf_int[-1,]
  
  # Create a new summary object
  summary_obj <- summary(model)
  
  # Calculate the t-values and p-values
  t_values <- coef(model) / summary_obj$coefficients[, "Std. Error"]
  p_values <- 2 * pt(-abs(t_values), df.residual(model))
  
  # Replace the coefficients and confidence intervals with the calculated values
  summary_obj$coefficients <- rbind(c(coef(model)["(Intercept)"], summary_obj$coefficients[1, "Std. Error"], conf_int[1,], p_values[1]),
                                    cbind(diff,
                                          summary_obj$coefficients[-1, "Std. Error"],
                                          diff_conf_int,
                                          p_values[-1]))
  
  # Update the column names
  colnames(summary_obj$coefficients) <- c("Coef", "Std. Error", "2.5 %", "97.5 %", "Pr(>|t|)")
  
  # Include the significance stars
  signif.stars <- options("show.signif.stars")
  if (is.logical(signif.stars) && signif.stars) {
    summary_obj$coefficients <- cbind(summary_obj$coefficients, 
                                      summary_obj$coefficients[, "Pr(>|t|)"])
    colnames(summary_obj$coefficients)[ncol(summary_obj$coefficients)] <- " "
    summary_obj$coefficients[, " "] <- symnum(summary_obj$coefficients[, "Pr(>|t|)"], 
                                              corr = FALSE, na = FALSE,
                                              cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                              symbols = c("***", "**", "*", ".", " "))
  }
  
  return(summary_obj)
}



##MEAT DAYS##
m1 <- svyglm(MeatDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)
m1 <- svyglm(ProcessedDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)
m1 <- svyglm(RedDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)
m1 <- svyglm(WhiteDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)
m1 <- svyglm(NoMeatDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)


##Meat occasions##
m1 <- svyglm(avgMeatokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)
m1 <- svyglm(avgProcessedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)
m1 <- svyglm(avgRedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)
m1 <- svyglm(avgWhiteokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)


##g per occasion##
m1 <- svyglm(gperokajMeat ~ SurveyYear, dat.design)
lm_summary(m1)
summary(m1) #p for trend value (use only after setting SurveyYear to numeric)
m1 <- svyglm(gperokajProcessed ~ SurveyYear, dat.design)
lm_summary(m1)
summary(m1) #p for trend value (use only after setting SurveyYear to numeric)
m1 <- svyglm(gperokajRed ~ SurveyYear, dat.design)
lm_summary(m1)
summary(m1) #p for trend value (use only after setting SurveyYear to numeric)
m1 <- svyglm(gperokajWhite ~ SurveyYear, dat.design)
lm_summary(m1)
summary(m1) #p for trend value (use only after setting SurveyYear to numeric)
m1 <- svyglm(okajTotalGrams ~ SurveyYear, dat.design)
lm_summary(m1)
summary(m1) #p for trend value (use only after setting SurveyYear to numeric)





y1$fpc <- 1629
dat.design.y1 <-
  svydesign(
    id = ~area,
    strata = ~astrata5,
    data = y1,
    weights = ~wti,
    fpc = ~fpc
  )
y11$fpc <- 1076
dat.design.y11 <-
  svydesign(
    id = ~area,
    strata = ~astrata5,
    data = y11,
    weights = ~wti,
    fpc = ~fpc
  )

#count #of participants in each survey year
table(dat$SurveyYear)


#########################SI TABLE 1 - STM ANALYSIS########################
#BREAKFAST
#overall n values
sum(complete.cases(dat.design$variables$BMeatokaj[dat.design$variables$SurveyYear == 1])) #n values year 1
sum(complete.cases(dat.design$variables$BMeatokaj[dat.design$variables$SurveyYear == 11])) #n values year 11
##Meat occasions##
m1 <- svyglm(BMeatokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)
m1 <- svyglm(BProcessedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)
m1 <- svyglm(BRedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)
m1 <- svyglm(BWhiteokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)


##g per occasion##
m1 <- svyglm(BgperokajMeat ~ SurveyYear, dat.design)
lm_summary(m1)
summary(m1) #p for trend value (use only after setting SurveyYear to numeric)
sum(complete.cases(dat.design$variables$BgperokajMeat[dat.design$variables$SurveyYear == 1])) #n values year 1
sum(complete.cases(dat.design$variables$BgperokajMeat[dat.design$variables$SurveyYear == 11])) #n values year 11
m1 <- svyglm(BgperokajProcessed ~ SurveyYear, dat.design)
lm_summary(m1)
summary(m1) #p for trend value (use only after setting SurveyYear to numeric)
sum(complete.cases(dat.design$variables$BgperokajProcessed[dat.design$variables$SurveyYear == 1])) #n values year 1
sum(complete.cases(dat.design$variables$BgperokajProcessed[dat.design$variables$SurveyYear == 11])) #n values year 11
m1 <- svyglm(BgperokajRed ~ SurveyYear, dat.design)
lm_summary(m1)
summary(m1) #p for trend value (use only after setting SurveyYear to numeric)
sum(complete.cases(dat.design$variables$BgperokajRed[dat.design$variables$SurveyYear == 1])) #n values year 1
sum(complete.cases(dat.design$variables$BgperokajRed[dat.design$variables$SurveyYear == 11])) #n values year 11
m1 <- svyglm(BgperokajWhite ~ SurveyYear, dat.design)
lm_summary(m1)
summary(m1) #p for trend value (use only after setting SurveyYear to numeric)
sum(complete.cases(dat.design$variables$BgperokajWhite[dat.design$variables$SurveyYear == 1])) #n values year 1
sum(complete.cases(dat.design$variables$BgperokajWhite[dat.design$variables$SurveyYear == 11])) #n values year 11
m1 <- svyglm(BokajGrams ~ SurveyYear, dat.design)
lm_summary(m1)
summary(m1) #p for trend value (use only after setting SurveyYear to numeric)
sum(complete.cases(dat.design$variables$BokajGrams[dat.design$variables$SurveyYear == 1])) #n values year 1
sum(complete.cases(dat.design$variables$BokajGrams[dat.design$variables$SurveyYear == 11])) #n values year 11


#LUNCH
#overall n values
sum(complete.cases(dat.design$variables$LMeatokaj[dat.design$variables$SurveyYear == 1])) #n values year 1
sum(complete.cases(dat.design$variables$LMeatokaj[dat.design$variables$SurveyYear == 11])) #n values year 11
##Meat occasions##
m1 <- svyglm(LMeatokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)
m1 <- svyglm(LProcessedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)
m1 <- svyglm(LRedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)
m1 <- svyglm(LWhiteokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)


##g per occasion##
m1 <- svyglm(LgperokajMeat ~ SurveyYear, dat.design)
lm_summary(m1)
summary(m1) #p for trend value (use only after setting SurveyYear to numeric)
sum(complete.cases(dat.design$variables$LgperokajMeat[dat.design$variables$SurveyYear == 1])) #n values year 1
sum(complete.cases(dat.design$variables$LgperokajMeat[dat.design$variables$SurveyYear == 11])) #n values year 11
m1 <- svyglm(LgperokajProcessed ~ SurveyYear, dat.design)
lm_summary(m1)
summary(m1) #p for trend value (use only after setting SurveyYear to numeric)
sum(complete.cases(dat.design$variables$LgperokajProcessed[dat.design$variables$SurveyYear == 1])) #n values year 1
sum(complete.cases(dat.design$variables$LgperokajProcessed[dat.design$variables$SurveyYear == 11])) #n values year 11
m1 <- svyglm(LgperokajRed ~ SurveyYear, dat.design)
lm_summary(m1)
summary(m1) #p for trend value (use only after setting SurveyYear to numeric)
sum(complete.cases(dat.design$variables$LgperokajRed[dat.design$variables$SurveyYear == 1])) #n values year 1
sum(complete.cases(dat.design$variables$LgperokajRed[dat.design$variables$SurveyYear == 11])) #n values year 11
m1 <- svyglm(LgperokajWhite ~ SurveyYear, dat.design)
lm_summary(m1)
summary(m1) #p for trend value (use only after setting SurveyYear to numeric)
sum(complete.cases(dat.design$variables$LgperokajWhite[dat.design$variables$SurveyYear == 1])) #n values year 1
sum(complete.cases(dat.design$variables$LgperokajWhite[dat.design$variables$SurveyYear == 11])) #n values year 11
m1 <- svyglm(LokajGrams ~ SurveyYear, dat.design)
lm_summary(m1)
summary(m1) #p for trend value (use only after setting SurveyYear to numeric)
sum(complete.cases(dat.design$variables$LokajGrams[dat.design$variables$SurveyYear == 1])) #n values year 1
sum(complete.cases(dat.design$variables$LokajGrams[dat.design$variables$SurveyYear == 11])) #n values year 11




#DINNER
#overall n values
sum(complete.cases(dat.design$variables$DMeatokaj[dat.design$variables$SurveyYear == 1])) #n values year 1
sum(complete.cases(dat.design$variables$DMeatokaj[dat.design$variables$SurveyYear == 11])) #n values year 11
##Meat occasions##
m1 <- svyglm(DMeatokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)
m1 <- svyglm(DProcessedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)
m1 <- svyglm(DRedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)
m1 <- svyglm(DWhiteokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
exp_summary(m1)


##g per occasion##
m1 <- svyglm(DgperokajMeat ~ SurveyYear, dat.design)
lm_summary(m1)
summary(m1) #p for trend value (use only after setting SurveyYear to numeric)
sum(complete.cases(dat.design$variables$DgperokajMeat[dat.design$variables$SurveyYear == 1])) #n values year 1
sum(complete.cases(dat.design$variables$DgperokajMeat[dat.design$variables$SurveyYear == 11])) #n values year 11
m1 <- svyglm(DgperokajProcessed ~ SurveyYear, dat.design)
lm_summary(m1)
summary(m1) #p for trend value (use only after setting SurveyYear to numeric)
sum(complete.cases(dat.design$variables$DgperokajProcessed[dat.design$variables$SurveyYear == 1])) #n values year 1
sum(complete.cases(dat.design$variables$DgperokajProcessed[dat.design$variables$SurveyYear == 11])) #n values year 11
m1 <- svyglm(DgperokajRed ~ SurveyYear, dat.design)
lm_summary(m1)
summary(m1) #p for trend value (use only after setting SurveyYear to numeric)
sum(complete.cases(dat.design$variables$DgperokajRed[dat.design$variables$SurveyYear == 1])) #n values year 1
sum(complete.cases(dat.design$variables$DgperokajRed[dat.design$variables$SurveyYear == 11])) #n values year 11
m1 <- svyglm(DgperokajWhite ~ SurveyYear, dat.design)
lm_summary(m1)
summary(m1) #p for trend value (use only after setting SurveyYear to numeric)
sum(complete.cases(dat.design$variables$DgperokajWhite[dat.design$variables$SurveyYear == 1])) #n values year 1
sum(complete.cases(dat.design$variables$DgperokajWhite[dat.design$variables$SurveyYear == 11])) #n values year 11
m1 <- svyglm(DokajGrams ~ SurveyYear, dat.design)
lm_summary(m1)
summary(m1) #p for trend value (use only after setting SurveyYear to numeric)
sum(complete.cases(dat.design$variables$DokajGrams[dat.design$variables$SurveyYear == 1])) #n values year 1
sum(complete.cases(dat.design$variables$DokajGrams[dat.design$variables$SurveyYear == 11])) #n values year 11



##########################SI TABLE 2 - analysis by covariates########################

#use this general structure for the following functions to extract beta coefficient
#estimates in their exponentiated form (leaving this information in for reasoning).
#I give age as an example because that's the most complex. Sex & eqv are simpler
#b0 #intercept
#b1 #survey year 11
#b2.1 #ageG1
#b2.2 #ageG2
#b2.3 #ageG4
#b2.4 #ageG5
#b3.1 #ageG1 @ y11
#b3.2 #ageG2 @y11
#b3.3 #ageG4 @y11
#b3.4 #ageG5 @y11
#18-40
#exp(b0)
#exp(b0+b1)
#<10
#exp(b0+b2.1)
#exp(b0+b2.1+b1+b3.1)
#11-17
#exp(b0+b2.2)
#exp(b0+b2.2+b1+b3.2)
#41-59
#exp(b0+b2.3)
#exp(b0+b2.3+b1+b3.3)
#>=60
#exp(b0+b2.4)
#exp(b0+b2.4+b1+b3.4)

#SEX
exp_interaction_CI_sex <- function(response_var, design) {
  model_formula <- as.formula(paste(response_var, "~ SurveyYear + Sex + SurveyYear * Sex"))
  model <- svyglm(model_formula, family = poisson(link = "log"), design = design)
  model_summary <- summary(model)
  betas <- coef(model)
  se <- coef(model_summary)[, "Std. Error"]
  #combinations of coefficients
  b_combinations <- c(
    betas["(Intercept)"],
    betas["(Intercept)"] + betas["SurveyYear11"],
    betas["(Intercept)"] + betas["SexM"],
    betas["(Intercept)"] + betas["SexM"] + betas["SurveyYear11"] + betas["SurveyYear11:SexM"]
  )
  #standard errors for combinations
  se_combinations <- c(
    se["(Intercept)"],
    sqrt(se["(Intercept)"]^2 + se["SurveyYear11"]^2),
    sqrt(se["(Intercept)"]^2 + se["SexM"]^2),
    sqrt(se["(Intercept)"]^2 + se["SexM"]^2 + se["SurveyYear11"]^2 + se["SurveyYear11:SexM"]^2)
  )
  #exponentiated coefficients and CI for combinations
  exp_coef <- round(exp(b_combinations), 2)
  lower_bound <- round(exp(b_combinations - 1.96 * se_combinations), 2)
  upper_bound <- round(exp(b_combinations + 1.96 * se_combinations), 2)
  #data frame to present the results
  result_table <- data.frame(
    Group = c("M_Y1", "M_Y11", "F_Y1", "F_Y11"),
    Beta = exp_coef,
    Lower = lower_bound,
    Upper = upper_bound
  )
  return(result_table)
}
glm_interaction_CI_sex <- function(response_var, design) {
  model_formula <- as.formula(paste(response_var, "~ SurveyYear + Sex + SurveyYear * Sex"))
  model <- svyglm(model_formula, design = design)
  model_summary <- summary(model)
  betas <- coef(model)
  se <- coef(model_summary)[, "Std. Error"]
  #combinations of coefficients
  b_combinations <- c(
    betas["(Intercept)"],
    betas["(Intercept)"] + betas["SurveyYear11"],
    betas["(Intercept)"] + betas["SexF"],
    betas["(Intercept)"] + betas["SexF"] + betas["SurveyYear11"] + betas["SurveyYear11:SexF"]
  )
  #standard errors for combinations
  se_combinations <- c(
    se["(Intercept)"],
    sqrt(se["(Intercept)"]^2 + se["SurveyYear11"]^2),
    sqrt(se["(Intercept)"]^2 + se["SexF"]^2),
    sqrt(se["(Intercept)"]^2 + se["SexF"]^2 + se["SurveyYear11"]^2 + se["SurveyYear11:SexF"]^2)
  )
  # CI for combinations
  lower_bound <- round(b_combinations - 1.96 * se_combinations, 2)
  upper_bound <- round(b_combinations + 1.96 * se_combinations, 2)
  #data frame to present the results
  result_table <- data.frame(
    Group = c("M_Y1", "M_Y11", "F_Y1", "F_Y11"),
    Beta = b_combinations,
    Lower = lower_bound,
    Upper = upper_bound
  )
  return(result_table)
}
#days
exp_interaction_CI_sex(response_var = "MeatDays", design = dat.design)
exp_interaction_CI_sex(response_var = "ProcessedDays", design = dat.design)
exp_interaction_CI_sex(response_var = "RedDays", design = dat.design)
exp_interaction_CI_sex(response_var = "WhiteDays", design = dat.design)
exp_interaction_CI_sex(response_var = "NoMeatDays", design = dat.design)
#occasions
exp_interaction_CI_sex(response_var = "avgMeatokaj", design = dat.design)
exp_interaction_CI_sex(response_var = "avgProcessedokaj", design = dat.design)
exp_interaction_CI_sex(response_var = "avgRedokaj", design = dat.design)
exp_interaction_CI_sex(response_var = "avgWhiteokaj", design = dat.design)
#portion size
glm_interaction_CI_sex(response_var = "gperokajMeat", design = dat.design)
glm_interaction_CI_sex(response_var = "gperokajProcessed", design = dat.design)
glm_interaction_CI_sex(response_var = "gperokajRed", design = dat.design)
glm_interaction_CI_sex(response_var = "gperokajWhite", design = dat.design)

#AGE
exp_interaction_CI_age <- function(response_var, design) {
  model_formula <- as.formula(paste(response_var, "~ SurveyYear + AgeG + SurveyYear * AgeG"))
  model <- svyglm(model_formula, family = poisson(link = "log"), design = design)
  model_summary <- summary(model)
  betas <- coef(model)
  se <- coef(model_summary)[, "Std. Error"]
  #combinations of coefficients
  b_combinations <- c(
    betas["(Intercept)"],
    betas["(Intercept)"] + betas["SurveyYear11"],
    betas["(Intercept)"] + betas["AgeG1"],
    betas["(Intercept)"] + betas["AgeG1"] + betas["SurveyYear11"] + betas["SurveyYear11:AgeG1"],
    betas["(Intercept)"] + betas["AgeG2"],
    betas["(Intercept)"] + betas["AgeG2"] + betas["SurveyYear11"] + betas["SurveyYear11:AgeG2"],
    betas["(Intercept)"] + betas["AgeG4"],
    betas["(Intercept)"] + betas["AgeG4"] + betas["SurveyYear11"] + betas["SurveyYear11:AgeG4"],
    betas["(Intercept)"] + betas["AgeG5"],
    betas["(Intercept)"] + betas["AgeG5"] + betas["SurveyYear11"] + betas["SurveyYear11:AgeG5"]
  )
  #standard errors for combinations
  se_combinations <- c(
    se["(Intercept)"],
    sqrt(se["(Intercept)"]^2 + se["SurveyYear11"]^2),
    se["AgeG1"],
    sqrt(se["AgeG1"]^2 + se["SurveyYear11"]^2 + se["SurveyYear11:AgeG1"]^2),
    se["AgeG2"],
    sqrt(se["AgeG2"]^2 + se["SurveyYear11"]^2 + se["SurveyYear11:AgeG2"]^2),
    se["AgeG4"],
    sqrt(se["AgeG4"]^2 + se["SurveyYear11"]^2 + se["SurveyYear11:AgeG4"]^2),
    se["AgeG5"],
    sqrt(se["AgeG5"]^2 + se["SurveyYear11"]^2 + se["SurveyYear11:AgeG5"]^2)
  )
  #exponentiated coefficients and CI for combinations
  exp_coef <- round(exp(b_combinations), 2)
  lower_bound <- round(exp(b_combinations - 1.96 * se_combinations), 2)
  upper_bound <- round(exp(b_combinations + 1.96 * se_combinations), 2)
  #data frame to present the results
  result_table <- data.frame(
    Group = c("18-40_Y1", "18-40_Y11", "<10_Y1", "<10_Y11", "11-17_Y1", "11-17_Y11", "41-59_Y1", "41-59_Y11", ">=60_Y1", ">=60_Y11"),
    Beta = exp_coef,
    Lower = lower_bound,
    Upper = upper_bound
  )
  return(result_table)
}
glm_interaction_CI_age <- function(response_var, design) {
  model_formula <- as.formula(paste(response_var, "~ SurveyYear + AgeG + SurveyYear * AgeG"))
  model <- svyglm(model_formula, design = design)
  model_summary <- summary(model)
  betas <- coef(model)
  se <- coef(model_summary)[, "Std. Error"]
  #combinations of coefficients
  b_combinations <- c(
    betas["(Intercept)"],
    betas["(Intercept)"] + betas["SurveyYear11"],
    betas["(Intercept)"] + betas["AgeG1"],
    betas["(Intercept)"] + betas["AgeG1"] + betas["SurveyYear11"] + betas["SurveyYear11:AgeG1"],
    betas["(Intercept)"] + betas["AgeG2"],
    betas["(Intercept)"] + betas["AgeG2"] + betas["SurveyYear11"] + betas["SurveyYear11:AgeG2"],
    betas["(Intercept)"] + betas["AgeG4"],
    betas["(Intercept)"] + betas["AgeG4"] + betas["SurveyYear11"] + betas["SurveyYear11:AgeG4"],
    betas["(Intercept)"] + betas["AgeG5"],
    betas["(Intercept)"] + betas["AgeG5"] + betas["SurveyYear11"] + betas["SurveyYear11:AgeG5"]
  )
  #standard errors for combinations
  se_combinations <- c(
    se["(Intercept)"],
    sqrt(se["(Intercept)"]^2 + se["SurveyYear11"]^2),
    se["AgeG1"],
    sqrt(se["AgeG1"]^2 + se["SurveyYear11"]^2 + se["SurveyYear11:AgeG1"]^2),
    se["AgeG2"],
    sqrt(se["AgeG2"]^2 + se["SurveyYear11"]^2 + se["SurveyYear11:AgeG2"]^2),
    se["AgeG4"],
    sqrt(se["AgeG4"]^2 + se["SurveyYear11"]^2 + se["SurveyYear11:AgeG4"]^2),
    se["AgeG5"],
    sqrt(se["AgeG5"]^2 + se["SurveyYear11"]^2 + se["SurveyYear11:AgeG5"]^2)
  )
  # CI for combinations
  lower_bound <- round(b_combinations - 1.96 * se_combinations, 2)
  upper_bound <- round(b_combinations + 1.96 * se_combinations, 2)
  #data frame to present the results
  result_table <- data.frame(
    Group = c("18-40_Y1", "18-40_Y11", "<10_Y1", "<10_Y11", "11-17_Y1", "11-17_Y11", "41-59_Y1", "41-59_Y11", ">=60_Y1", ">=60_Y11"),
    Beta = b_combinations,
    Lower = lower_bound,
    Upper = upper_bound
  )
  return(result_table)
}
#days
exp_interaction_CI_age(response_var = "MeatDays", design = dat.design)
exp_interaction_CI_age(response_var = "ProcessedDays", design = dat.design)
exp_interaction_CI_age(response_var = "RedDays", design = dat.design)
exp_interaction_CI_age(response_var = "WhiteDays", design = dat.design)
exp_interaction_CI_age(response_var = "NoMeatDays", design = dat.design)
#occasions
exp_interaction_CI_age(response_var = "avgMeatokaj", design = dat.design)
exp_interaction_CI_age(response_var = "avgProcessedokaj", design = dat.design)
exp_interaction_CI_age(response_var = "avgRedokaj", design = dat.design)
exp_interaction_CI_age(response_var = "avgWhiteokaj", design = dat.design)
#portion size
glm_interaction_CI_age(response_var = "gperokajMeat", design = dat.design)
glm_interaction_CI_age(response_var = "gperokajProcessed", design = dat.design)
glm_interaction_CI_age(response_var = "gperokajRed", design = dat.design)
glm_interaction_CI_age(response_var = "gperokajWhite", design = dat.design)

#EQV
exp_interaction_CI_eqv <- function(response_var, design) {
  model_formula <- as.formula(paste(response_var, "~ SurveyYear + eqv + SurveyYear * eqv"))
  model <- svyglm(model_formula, family = poisson(link = "log"), design = design)
  model_summary <- summary(model)
  betas <- coef(model)
  se <- coef(model_summary)[, "Std. Error"]
  #combinations of coefficients
  b_combinations <- c(
    betas["(Intercept)"],
    betas["(Intercept)"] + betas["eqv2"],
    betas["(Intercept)"] + betas["eqv3"],
    betas["(Intercept)"] + betas["SurveyYear11"],
    betas["(Intercept)"] + betas["SurveyYear11"] + betas["eqv2"] + betas["SurveyYear11:eqv2"],
    betas["(Intercept)"] + betas["SurveyYear11"] + betas["eqv3"] + betas["SurveyYear11:eqv3"]
  )
  #standard errors for combinations
  se_combinations <- c(
    se["(Intercept)"],
    sqrt(se["(Intercept)"]^2 + se["eqv2"]^2),
    sqrt(se["(Intercept)"]^2 + se["eqv3"]^2),
    sqrt(se["(Intercept)"]^2 + se["SurveyYear11"]^2),
    sqrt(se["(Intercept)"]^2 + se["SurveyYear11"]^2 + se["eqv2"]^2 + se["SurveyYear11:eqv2"]^2),
    sqrt(se["(Intercept)"]^2 + se["SurveyYear11"]^2 + se["eqv3"]^2 + se["SurveyYear11:eqv3"]^2)
  )
  #exponentiated coefficients and CI for combinations
  exp_coef <- round(exp(b_combinations), 2)
  lower_bound <- round(exp(b_combinations - 1.96 * se_combinations), 2)
  upper_bound <- round(exp(b_combinations + 1.96 * se_combinations), 2)
  #data frame to present the results
  result_table <- data.frame(
    Group = c("eqv1_Y1", "eqv2_Y1", "eqv3_Y1", "eqv1_Y11", "eqv2_Y11", "eqv3_Y11"),
    Beta = exp_coef,
    Lower = lower_bound,
    Upper = upper_bound
  )
  return(result_table)
}
glm_interaction_CI_eqv <- function(response_var, design) {
  model_formula <- as.formula(paste(response_var, "~ SurveyYear + eqv + SurveyYear * eqv"))
  model <- svyglm(model_formula, design = design)
  model_summary <- summary(model)
  betas <- coef(model)
  se <- coef(model_summary)[, "Std. Error"]
  #combinations of coefficients
  b_combinations <- c(
    betas["(Intercept)"],
    betas["(Intercept)"] + betas["eqv2"],
    betas["(Intercept)"] + betas["eqv3"],
    betas["(Intercept)"] + betas["SurveyYear11"],
    betas["(Intercept)"] + betas["SurveyYear11"] + betas["eqv2"] + betas["SurveyYear11:eqv2"],
    betas["(Intercept)"] + betas["SurveyYear11"] + betas["eqv3"] + betas["SurveyYear11:eqv3"]
  )
  #standard errors for combinations
  se_combinations <- c(
    se["(Intercept)"],
    sqrt(se["(Intercept)"]^2 + se["eqv2"]^2),
    sqrt(se["(Intercept)"]^2 + se["eqv3"]^2),
    sqrt(se["(Intercept)"]^2 + se["SurveyYear11"]^2),
    sqrt(se["(Intercept)"]^2 + se["SurveyYear11"]^2 + se["eqv2"]^2 + se["SurveyYear11:eqv2"]^2),
    sqrt(se["(Intercept)"]^2 + se["SurveyYear11"]^2 + se["eqv3"]^2 + se["SurveyYear11:eqv3"]^2)
  )
  # CI for combinations
  lower_bound <- round(b_combinations - 1.96 * se_combinations, 2)
  upper_bound <- round(b_combinations + 1.96 * se_combinations, 2)
  #data frame to present the results
  result_table <- data.frame(
    Group = c("eqv1_Y1", "eqv2_Y1", "eqv3_Y1", "eqv1_Y11", "eqv2_Y11", "eqv3_Y11"),
    Beta = b_combinations,
    Lower = lower_bound,
    Upper = upper_bound
  )
  return(result_table)
}
#days
exp_interaction_CI_eqv(response_var = "MeatDays", design = dat.design)
exp_interaction_CI_eqv(response_var = "ProcessedDays", design = dat.design)
exp_interaction_CI_eqv(response_var = "RedDays", design = dat.design)
exp_interaction_CI_eqv(response_var = "WhiteDays", design = dat.design)
exp_interaction_CI_eqv(response_var = "NoMeatDays", design = dat.design)
#occasions
exp_interaction_CI_eqv(response_var = "avgMeatokaj", design = dat.design)
exp_interaction_CI_eqv(response_var = "avgProcessedokaj", design = dat.design)
exp_interaction_CI_eqv(response_var = "avgRedokaj", design = dat.design)
exp_interaction_CI_eqv(response_var = "avgWhiteokaj", design = dat.design)
#portion size
glm_interaction_CI_eqv(response_var = "gperokajMeat", design = dat.design)
glm_interaction_CI_eqv(response_var = "gperokajProcessed", design = dat.design)
glm_interaction_CI_eqv(response_var = "gperokajRed", design = dat.design)
glm_interaction_CI_eqv(response_var = "gperokajWhite", design = dat.design)


#######################DECOMPOSITION ANALYSIS###############

c1/4
c2/4
#total meat
#convert occasions per day variable to occasions per MEAT day (to add up for decomp)
zo1 <- 1.238239
zo2 <- 1.13248
#total meat decomp
d1 <- 3.270036 #meat days y1
d2 <- 3.026991 #meat days y11
o1 <- (zo1*4)/d1 #meat occasions per meat day y1
o2 <- (zo2*4)/d2 #meat occasions per meat day y11
p1 <- 85.758 #meat portion size y1
p2 <- 76.120 #meat portion size y11
#total consumption
c1 <- p1*o1*d1
c2 <- p2*o2*d2
#calculate change in consumption
cdelta <- c2 - c1
#run models for days, occasions, and portion size
dm <- ((c2-c1)/(log(c2)-(log(c1))))*(log(d2/d1))
om <- ((c2-c1)/(log(c2)-(log(c1))))*(log(o2/o1))
pm <- ((c2-c1)/(log(c2)-(log(c1))))*(log(p2/p1))
#sum of models should equal change in consumption
dm+om+pm
#percent attributable to each reduction type
dm1 <- dm/cdelta
om1 <- om/cdelta
pm1 <- pm/cdelta
dm1
om1
pm1
#check that the separate percents add up to 100%
dm1+om1+pm1


#processed meat
#convert occasions per day variable to occasions per MEAT day (to add up for decomp)
zo1 <- 0.5396168
zo2 <- 0.4951703
#processed meat decomp
d1 <- 1.768601 #meat days y1
d2 <- 1.592737 #meat days y11
o1 <- (zo1*4)/d1 #meat occasions per meat day y1
o2 <- (zo2*4)/d2 #meat occasions per meat day y11
p1 <- 63.214 #meat portion size y1
p2 <- 52.642 #meat portion size y11
#total consumption
c1 <- p1*o1*d1 
c2 <- p2*o2*d2
#calculate change in consumption
cdelta <- c2 - c1 
#run models for days, occasions, and portion size
dm <- ((c2-c1)/(log(c2)-(log(c1))))*(log(d2/d1))
om <- ((c2-c1)/(log(c2)-(log(c1))))*(log(o2/o1))
pm <- ((c2-c1)/(log(c2)-(log(c1))))*(log(p2/p1))
#sum of models should equal change in consumption
dm+om+pm
#percent attributable to each reduction type
dm1 <- dm/cdelta
om1 <- om/cdelta
pm1 <- pm/cdelta
dm1
om1
pm1
#check that the separate percents add up to 100%
dm1+om1+pm1




#Red meat
#convert occasions per day variable to occasions per MEAT day (to add up for decomp)
zo1 <- 0.4397187
zo2 <- 0.3344058
#processed meat decomp
d1 <- 1.559663 #meat days y1
d2 <- 1.227206 #meat days y11
o1 <- (zo1*4)/d1 #meat occasions per meat day y1
o2 <- (zo2*4)/d2 #meat occasions per meat day y11
p1 <- 89.731 #meat portion size y1
p2 <- 69.686 #meat portion size y11
#total consumption
c1 <- p1*o1*d1 
c2 <- p2*o2*d2
#calculate change in consumption
cdelta <- c2 - c1 
#run models for days, occasions, and portion size
dm <- ((c2-c1)/(log(c2)-(log(c1))))*(log(d2/d1))
om <- ((c2-c1)/(log(c2)-(log(c1))))*(log(o2/o1))
pm <- ((c2-c1)/(log(c2)-(log(c1))))*(log(p2/p1))
#sum of models should equal change in consumption
dm+om+pm
#percent attributable to each reduction type
dm1 <- dm/cdelta
om1 <- om/cdelta
pm1 <- pm/cdelta
dm1
om1
pm1
#check that the separate percents add up to 100%
dm1+om1+pm1






#white meat
#convert occasions per day variable to occasions per MEAT day (to add up for decomp)
zo1 <- 0.4043073
zo2 <- 0.4514052
#processed meat decomp
d1 <- 1.420405 #meat days y1
d2 <- 1.568424 #meat days y11
o1 <- (zo1*4)/d1 #meat occasions per meat day y1
o2 <- (zo2*4)/d2 #meat occasions per meat day y11
p1 <- 84.650 #meat portion size y1
p2 <- 79.741 #meat portion size y11
#total consumption
c1 <- p1*o1*d1 
c2 <- p2*o2*d2
#calculate change in consumption
cdelta <- c2 - c1 
#run models for days, occasions, and portion size
dm <- ((c2-c1)/(log(c2)-(log(c1))))*(log(d2/d1))
om <- ((c2-c1)/(log(c2)-(log(c1))))*(log(o2/o1))
pm <- ((c2-c1)/(log(c2)-(log(c1))))*(log(p2/p1))
#sum of models should equal change in consumption
dm+om+pm
#percent attributable to each reduction type
dm1 <- dm/cdelta
om1 <- om/cdelta
pm1 <- pm/cdelta
dm1
om1
pm1
#check that the separate percents add up to 100%
dm1+om1+pm1




#######################COLORBLIND PALETTE##################
# Define the color palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Create a data frame with the colors and their corresponding labels
palette_df <- data.frame(color = cbPalette, label = 1:length(cbPalette))

# Generate the bar plot
ggplot(palette_df, aes(x = factor(label), y = 1, fill = color)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_identity() +
  theme_void() +
  theme(legend.position = "none") +
  coord_fixed(ratio = 0.2)


#######################FIGURE 1###########################
#function for Survey Year x axis labels
custom_x_labels <- function(x) {
  labels <- ifelse(x == 1, "2008", sprintf("'%02d", x + 7))
  return(labels)
}
#line plot of meat trends
#Days
m2 <- svyglm(ProcessedDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
m3 <- svyglm(RedDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
m4 <- svyglm(WhiteDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
m5 <- svyglm(NoMeatDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
#fitted values for each model
survey_years <- unique(dat.design$variables$SurveyYear)
predictions <- data.frame(
  SurveyYear = rep(survey_years, 4),
  Category = factor(rep(c("ProcessedDays", "RedDays", "WhiteDays", "NoMeatDays"), each = length(survey_years))),
  PredictedDays = c(predict(m2, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m3, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m4, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m5, newdata = data.frame(SurveyYear = survey_years), type = "response"))
)
#create a custom color palette using colorblind friendly colors
color_palette <- c("#E69F00", "#0072B2", "#D55E00", "#009E73") #order: processed (orange), white (blue), red (red), no meat (green)
#correct order
predictions$Category <- factor(predictions$Category, levels = c("ProcessedDays", "WhiteDays", "RedDays", "NoMeatDays"))
#category names
levels(predictions$Category) <- c("Processed", "White", "Red", "No Meat")
#convert SurveyYear to numeric
predictions$SurveyYear <- as.numeric(as.character(predictions$SurveyYear))
#create plot
plot1 <- ggplot(predictions, aes(x = SurveyYear, y = PredictedDays, color = Category, group = Category)) +
  geom_point(size = 1) +
  geom_line(aes(linetype = "solid")) +
  geom_smooth(method = "glm", formula = 'y ~ x', se = FALSE, aes(linetype = "dotted", group = Category)) +
  scale_color_manual(values = color_palette) +
  scale_linetype_manual(name = "Line Type",
                        values = c("solid" = "solid", "dotted" = "dotted"),
                        labels = c("solid" = "Actual data", "dotted" = "2008-2019 trend")) +
  labs(x = "Survey Year", y = "Number of days (avg. across 4-day period)", color = "Meat category") +
  scale_x_continuous(breaks = predictions$SurveyYear, labels = custom_x_labels) +
  theme_classic() +
  theme(text = element_text(family = "Avenir", size = 12)) +
  guides(linetype = guide_legend(override.aes = list(color = "black")))
print(plot1)


#Occasions
m2 <- svyglm(avgProcessedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
m3 <- svyglm(avgRedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
m4 <- svyglm(avgWhiteokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
#fitted values for each model
survey_years <- unique(dat.design$variables$SurveyYear)
predictions <- data.frame(
  SurveyYear = rep(survey_years, 3),
  Category = factor(rep(c("avgProcessedokaj", "avgRedokaj", "avgWhiteokaj"), each = length(survey_years))),
  PredictedOccasions = c(predict(m2, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                         predict(m3, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                         predict(m4, newdata = data.frame(SurveyYear = survey_years), type = "response"))
)
#colorblind friendly colors
color_palette <- c("#E69F00", "#0072B2", "#D55E00") #order: processed (orange), white (blue), red (red)
#correct order
predictions$Category <- factor(predictions$Category, levels = c("avgProcessedokaj", "avgWhiteokaj", "avgRedokaj"))
#names for xlab
levels(predictions$Category) <- c("Processed", "White", "Red")
#convert SurveyYear to numeric
predictions$SurveyYear <- as.numeric(as.character(predictions$SurveyYear))
#create plot
plot2 <- ggplot(predictions, aes(x = SurveyYear, y = PredictedOccasions, color = Category, group = Category)) +
  geom_point(size = 1) +
  geom_line() +
  geom_smooth(method = "glm", se = FALSE, linetype = "dotted", aes(group = Category)) + #this adds the fitted line
  scale_color_manual(values = color_palette) +
  labs(x = "Survey Year", y = "Number of meat-containing occasions/day", color = "Meat category") +
  scale_x_continuous(breaks = predictions$SurveyYear, labels = custom_x_labels) +
  theme_classic() +
  theme(text = element_text(family = "Avenir", size = 12))
print(plot2)


#portion size
m2 <- svyglm(gperokajProcessed ~ SurveyYear, family=poisson(link = "log"), dat.design)
m3 <- svyglm(gperokajRed ~ SurveyYear, family=poisson(link = "log"), dat.design)
m4 <- svyglm(gperokajWhite ~ SurveyYear, family=poisson(link = "log"), dat.design)
#fitted values for each model
survey_years <- unique(dat.design$variables$SurveyYear)
predictions <- data.frame(
  SurveyYear = rep(survey_years, 3),
  Category = factor(rep(c("gperokajProcessed", "gperokajRed", "gperokajWhite"), each = length(survey_years))),
  PredictedPortion = c(predict(m2, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                       predict(m3, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                       predict(m4, newdata = data.frame(SurveyYear = survey_years), type = "response"))
)
#colorblind friendly
color_palette <- c("#0072B2", "#D55E00", "#E69F00") #order: white (blue), red (red), processed (orange)
#correct order
predictions$Category <- factor(predictions$Category, levels = c("gperokajWhite", "gperokajRed", "gperokajProcessed"))
#names for xlab
levels(predictions$Category) <- c("White", "Red", "Processed")
#convert SurveyYear to numeric
predictions$SurveyYear <- as.numeric(as.character(predictions$SurveyYear))
#create plot
plot3 <- ggplot(predictions, aes(x = SurveyYear, y = PredictedPortion, color = Category, group = Category)) +
  geom_point(size = 1) +
  geom_line() +
  geom_smooth(method = "glm", se = FALSE, linetype = "dotted", aes(group = Category)) + #this adds the fitted line
  scale_color_manual(values = color_palette) +
  labs(x = "Survey Year", y = "Portion size (g)/meat-containing occasion", color = "Meat category") +
  scale_x_continuous(breaks = predictions$SurveyYear, labels = custom_x_labels) +
  theme_classic() +
  theme(text = element_text(family = "Avenir", size = 12))
print(plot3)


#combine all into 1 figure
#Remove the legend from plot2 and plot3
plot2 <- plot2 + theme(legend.position = "none")
plot3 <- plot3 + theme(legend.position = "none")
#extract the legend from plot1
legend_grob <- cowplot::get_legend(plot1)
#remove the legend from plot1
plot1 <- plot1 + theme(legend.position = "none")
#combine the plots and legend into a single plot
top_row <- cowplot::plot_grid(plot1, plot2, nrow = 1)
bottom_row <- cowplot::plot_grid(plot3, legend_grob, nrow = 1, rel_widths = c(1, 1))
combined_plot <- cowplot::plot_grid(top_row, bottom_row, ncol = 1, rel_heights = c(1, 1))
print(combined_plot)
ggsave("~/University of Edinburgh/NDNS Meat Trends - General/Results/Figure 1.png", combined_plot, width = 8, height = 8, dpi = 600)















################FIGURE 2#########################
#decomposition analysis plot
#make data (values pulled from decomposition analysis section of this code)
meat_data <- data.frame(
  Meat = factor(c("Total Meat", "Processed Meat", "Red Meat", "White Meat"),
                levels = c("Total Meat", "Processed Meat", "Red Meat", "White Meat")),
  Total_Delta = c(-79.94, -32.18, -64.61, 7.08),
  Days_Delta = c(-29.61, -12.53, -29.41, 13.92),
  Occasions_Delta = c(-4.62, 2.25, -4.18, 1.55),
  Portion_Size_Delta = c(-45.71, -21.90, -31.02, -8.39)
)

meat_data_divided <- meat_data
numeric_columns <- sapply(meat_data, is.numeric)
#divide numeric columns by 4
meat_data_divided[, numeric_columns] <- meat_data[, numeric_columns] / 4
#rename back to original
meat_data <- meat_data_divided
print(meat_data)
#transform
melted_data <- reshape2::melt(meat_data, id.vars = "Meat")
#make plot
bar_plot <- ggplot(melted_data, aes(x = Meat, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  scale_fill_manual(values = c("Total_Delta" = "black", "Days_Delta" = "#CB181D", "Occasions_Delta" = "#FB6A4A", "Portion_Size_Delta" = "#FCBBA1"),
                    labels = c("Total_Delta" = "Total ∆",
                               "Days_Delta" = "∆ days",
                               "Occasions_Delta" = "∆ occasions",
                               "Portion_Size_Delta" = "∆ portion size")) +
  labs(x = "Meat categories", y = "Change in meat consumption (g/capita/day)", fill = "Consumption categories") +
  theme_classic() +
  theme(text = element_text(family = "Avenir", size = 12)) 
#define the y-axis limits (didn't like the cuts it was giving me)
y_limits <- c(-22, 5)
#update plot with modified y-axis limits/breaks and a dashed line at y=0
bar_plot <- bar_plot +
  scale_y_continuous(limits = y_limits, expand = c(0, 0), 
                     breaks = seq(-20, 5, by = 5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(label = sprintf("%.1f", value), y = value, group = variable, vjust = ifelse(value >= 0, -0.5, 1.5)), 
            position = position_dodge(width = 0.5), size = 2.5)
print(bar_plot)
file_path <- "~/University of Edinburgh/NDNS Meat Trends - General/Results/Figure 2.png"
ggsave(file_path, bar_plot, width = 10, height = 8, dpi = 600)

###############FIGURE 3#################################

custom_x_labels <- function(x) {
  labels <- ifelse(x == 1, "2008", sprintf("'%02d", x + 7))
  return(labels)
}

#total meat days
#set the weighting structure a srvyr object with the survey design
dat_svy <- as_survey(survey_design)
#create categorical variable for each level of MeatDays by SurveyYear
meat_days_prop <- dat_svy %>% 
  group_by(SurveyYear) %>% 
  summarize(prop_0 = survey_mean(MeatDays == 0),
            prop_1 = survey_mean(MeatDays == 1),
            prop_2 = survey_mean(MeatDays == 2),
            prop_3 = survey_mean(MeatDays == 3),
            prop_4 = survey_mean(MeatDays == 4))
meat_days_prop
#subset the columns that end in "_se"
se_cols <- grep("_se$", names(meat_days_prop))
#Remove the columns that end in "_se"
meat_days_prop_no_se <- meat_days_prop[, -se_cols]
#transform  data from wide to long format
meat_days_prop_long <- pivot_longer(meat_days_prop_no_se, cols = -SurveyYear, names_to = "MeatDays", values_to = "proportion")
#create stacked bar plot
plot1 <- ggplot(meat_days_prop_long, aes(x = SurveyYear, y = proportion, fill = factor(str_remove(MeatDays, "prop_"), levels = c("4", "3", "2", "1", "0")))) + 
  geom_col() +
  scale_fill_brewer(palette = "Reds", direction = -1) +
  labs(x = "Survey Year", y = "Proportion of participants", fill = "Meat Days") +
  scale_x_continuous(breaks = meat_days_prop$SurveyYear, labels = custom_x_labels) +
  geom_text(aes(label = paste0(round(proportion*100),"%")), 
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(text = element_text(family = "Avenir", size = 12)) +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, by = 0.25))
plot1

#processed meat days
#create categorical variable for each level of Processed by SurveyYear
Processed_days_prop <- dat_svy %>% 
  group_by(SurveyYear) %>% 
  summarize(prop_0 = survey_mean(ProcessedDays == 0),
            prop_1 = survey_mean(ProcessedDays == 1),
            prop_2 = survey_mean(ProcessedDays == 2),
            prop_3 = survey_mean(ProcessedDays == 3),
            prop_4 = survey_mean(ProcessedDays == 4))
Processed_days_prop
#subset the columns that end in "_se"
se_cols <- grep("_se$", names(Processed_days_prop))
#remove the columns that end in "_se"
Processed_days_prop_no_se <- Processed_days_prop[, -se_cols]
#transform data from wide to long format
Processed_days_prop_long <- pivot_longer(Processed_days_prop_no_se, cols = -SurveyYear, names_to = "ProcessedDays", values_to = "proportion")
#stacked bar plot
plot2 <- ggplot(Processed_days_prop_long, aes(x = SurveyYear, y = proportion, fill = factor(str_remove(ProcessedDays, "prop_"), levels = c("4", "3", "2", "1", "0")))) + 
  geom_col() +
  scale_fill_brewer(palette = "Reds", direction = -1) +
  labs(x = "Survey Year", y = "Proportion of participants", fill = "Processed Days") +
  scale_x_continuous(breaks = Processed_days_prop$SurveyYear, labels = custom_x_labels) +
  geom_text(aes(label = paste0(round(proportion*100),"%")), 
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(text = element_text(family = "Avenir", size = 12)) +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, by = 0.25))
plot2

#red meat days
#create categorical variable for each level of RedDays by SurveyYear
Red_days_prop <- dat_svy %>% 
  group_by(SurveyYear) %>% 
  summarize(prop_0 = survey_mean(RedDays == 0),
            prop_1 = survey_mean(RedDays == 1),
            prop_2 = survey_mean(RedDays == 2),
            prop_3 = survey_mean(RedDays == 3),
            prop_4 = survey_mean(RedDays == 4))
Red_days_prop
#subset the columns that end in "_se"
se_cols <- grep("_se$", names(Red_days_prop))
#remove the columns that end in "_se"
Red_days_prop_no_se <- Red_days_prop[, -se_cols]
#transform the data from wide to long format
Red_days_prop_long <- pivot_longer(Red_days_prop_no_se, cols = -SurveyYear, names_to = "RedDays", values_to = "proportion")
#stacked bar plot
plot3 <- ggplot(Red_days_prop_long, aes(x = SurveyYear, y = proportion, fill = factor(str_remove(RedDays, "prop_"), levels = c("4", "3", "2", "1", "0")))) +
  geom_col() +
  scale_fill_brewer(palette = "Reds", direction = -1) +
  labs(x = "Survey Year", y = "Proportion of participants", fill = "Red Days") +
  scale_x_continuous(breaks = Red_days_prop$SurveyYear, labels = custom_x_labels) +
  geom_text(aes(label = paste0(round(proportion*100),"%")), 
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(text = element_text(family = "Avenir", size = 12)) +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, by = 0.25))
plot3

#white meat days
#create categorical variable for each level of WhiteDays by SurveyYear
white_days_prop <- dat_svy %>% 
  group_by(SurveyYear) %>% 
  summarize(prop_0 = survey_mean(WhiteDays == 0),
            prop_1 = survey_mean(WhiteDays == 1),
            prop_2 = survey_mean(WhiteDays == 2),
            prop_3 = survey_mean(WhiteDays == 3),
            prop_4 = survey_mean(WhiteDays == 4))
white_days_prop
#subset the columns that end in "_se"
se_cols <- grep("_se$", names(white_days_prop))
#remove the columns that end in "_se"
white_days_prop_no_se <- white_days_prop[, -se_cols]
#transform the data from wide to long format
white_days_prop_long <- pivot_longer(white_days_prop_no_se, cols = -SurveyYear, names_to = "WhiteDays", values_to = "proportion")
#stacked bar plot
plot4 <- ggplot(white_days_prop_long, aes(x = SurveyYear, y = proportion, fill = factor(str_remove(WhiteDays, "prop_"), levels = c("4", "3", "2", "1", "0")))) +
  geom_col() +
  scale_fill_brewer(palette = "Reds", direction = -1) +
  labs(x = "Survey Year", y = "Proportion of participants", fill = "White Days") +
  scale_x_continuous(breaks = white_days_prop$SurveyYear, labels = custom_x_labels) +
  geom_text(aes(label = paste0(round(proportion*100),"%")), 
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(text = element_text(family = "Avenir", size = 12)) +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, by = 0.25))
plot4

#plot titles
plot1 <- plot1 + ggtitle("Total meat") + theme(plot.title = element_text(hjust = 0.5))
plot2 <- plot2 + ggtitle("Processed meat") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
plot3 <- plot3 + ggtitle("Red meat") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
plot4 <- plot4 + ggtitle("White meat") + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
#remove the x-axis label from plot1 and plot2
plot1 <- plot1 + xlab(NULL)
plot2 <- plot2 + xlab(NULL)
#remove the y-axis label from plot2 and plot4
plot2 <- plot2 + ylab(NULL)
plot4 <- plot4 + ylab(NULL)
#take legend from plot1
plot1_legend <- cowplot::get_legend(plot1)
#remove the legend from the original plot1
plot1 <- plot1 + theme(legend.position = "none")
#combine the plots into a single 4-pane plot without the legend (I'll add it later)
combined_plot <- grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)
#add the legend to the right of the combined plot
combined_plot <- grid.arrange(combined_plot, plot1_legend, ncol = 2, widths = c(8, 1))
combined_plot
file_path <- "~/University of Edinburgh/NDNS Meat Trends - General/Results/Figure 3.png"
ggsave(file_path, combined_plot, width = 16, height = 12, dpi = 600)











###############SI FIGURE 1###########################

#function for survey year x axis
custom_x_labels <- function(x) {
  labels <- ifelse(x == 1, "2008", sprintf("'%02d", x + 7))
  return(labels)
}

#assign survey to dataset
dat_svy <- as_survey(survey_design)
#create categorical variable for BMeatokajperc
dat_svy <- dat_svy %>%
  mutate(BMeatokajperc_cat = case_when(
    BMeatokajperc == 0 ~ "0%",
    BMeatokajperc > 0 & BMeatokajperc < 0.5 ~ "1-49%",
    BMeatokajperc >= 0.5 & BMeatokajperc < 1 ~ "50-99%",
    BMeatokajperc == 1 ~ "100%"
  )) %>%
  filter(!is.na(BMeatokajperc_cat))
#summarize data for plotting
summary_df <- dat_svy %>%
  group_by(SurveyYear, BMeatokajperc_cat) %>%
  summarise(count = survey_total(weights = weights)) %>%
  ungroup()
#calculate the percentage for each category within each SurveyYear
summary_df <- summary_df %>%
  group_by(SurveyYear) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()
#reorder the categories
summary_df$BMeatokajperc_cat <- factor(summary_df$BMeatokajperc_cat, levels = c("100%", "50-99%", "1-49%", "0%"))
#plot
plot1 <- ggplot(summary_df, aes(x = SurveyYear, y = percentage, fill = BMeatokajperc_cat)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.5), size = 3.5) +
  scale_x_continuous(breaks = summary_df$SurveyYear, labels = custom_x_labels) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_brewer(palette = "Reds", direction = -1) +
  theme_classic() +
  theme(text = element_text(family = "Avenir", size = 12)) +
  labs(x = "Survey Year", y = "Percentage of breakfasts containing meat") +
  guides(fill = guide_legend(title = "Meat consumption"))
print(plot1)

#create categorical variable for LMeatokajperc
dat_svy <- dat_svy %>%
  mutate(LMeatokajperc_cat = case_when(
    LMeatokajperc == 0 ~ "0%",
    LMeatokajperc > 0 & LMeatokajperc < 0.5 ~ "1-49%",
    LMeatokajperc >= 0.5 & LMeatokajperc < 1 ~ "50-99%",
    LMeatokajperc == 1 ~ "100%"
  )) %>%
  filter(!is.na(LMeatokajperc_cat))
#summarize data for plotting
summary_df <- dat_svy %>%
  group_by(SurveyYear, LMeatokajperc_cat) %>%
  summarise(count = survey_total(weights = weights)) %>%
  ungroup()
#calculate the percentage for each category within each SurveyYear
summary_df <- summary_df %>%
  group_by(SurveyYear) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()
#reorder the categories
summary_df$LMeatokajperc_cat <- factor(summary_df$LMeatokajperc_cat, levels = c("100%", "50-99%", "1-49%", "0%"))
#plot
plot2 <- ggplot(summary_df, aes(x = SurveyYear, y = percentage, fill = LMeatokajperc_cat)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.5), size = 3.5) +
  scale_x_continuous(breaks = summary_df$SurveyYear, labels = custom_x_labels) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_brewer(palette = "Reds", direction = -1) +
  theme_classic() +
  theme(text = element_text(family = "Avenir", size = 12)) +
  labs(x = "Survey Year", y = "Percentage of lunches containing meat") +
  guides(fill = guide_legend(title = "Meat consumption"))
print(plot2)

#create categorical variable for DMeatokajperc
dat_svy <- dat_svy %>%
  mutate(DMeatokajperc_cat = case_when(
    DMeatokajperc == 0 ~ "0%",
    DMeatokajperc > 0 & DMeatokajperc < 0.5 ~ "1-49%",
    DMeatokajperc >= 0.5 & DMeatokajperc < 1 ~ "50-99%",
    DMeatokajperc == 1 ~ "100%"
  )) %>%
  filter(!is.na(DMeatokajperc_cat))
#ummarize data for plotting
summary_df <- dat_svy %>%
  group_by(SurveyYear, DMeatokajperc_cat) %>%
  summarise(count = survey_total(weights = weights)) %>%
  ungroup()
#calculate the percentage for each category within each SurveyYear
summary_df <- summary_df %>%
  group_by(SurveyYear) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()
#reorder the categories
summary_df$DMeatokajperc_cat <- factor(summary_df$DMeatokajperc_cat, levels = c("100%", "50-99%", "1-49%", "0%"))
#plot
plot3 <- ggplot(summary_df, aes(x = SurveyYear, y = percentage, fill = DMeatokajperc_cat)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.5), size = 3.5) +
  scale_x_continuous(breaks = summary_df$SurveyYear, labels = custom_x_labels) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_brewer(palette = "Reds", direction = -1) +
  theme_classic() +
  theme(text = element_text(family = "Avenir", size = 12)) +
  labs(x = "Survey Year", y = "Percentage of dinners containing meat") +
  guides(fill = guide_legend(title = "Meat consumption"))
print(plot3)


#combine all into 1 figure
#Remove the legend from plot2 and plot3
plot2 <- plot2 + theme(legend.position = "none")
plot3 <- plot3 + theme(legend.position = "none")
#extract the legend from plot1
legend_grob <- cowplot::get_legend(plot1)
#remove the legend from plot1
plot1 <- plot1 + theme(legend.position = "none")
#combine the plots and legend into a single plot
top_row <- cowplot::plot_grid(plot1, plot2, nrow = 1)
bottom_row <- cowplot::plot_grid(plot3, legend_grob, nrow = 1, rel_widths = c(1, 1))
combined_plot <- cowplot::plot_grid(top_row, bottom_row, ncol = 1, rel_heights = c(1, 1))
print(combined_plot)
ggsave("~/University of Edinburgh/NDNS Meat Trends - General/Results/SI Figure 1.png", combined_plot, width = 16, height = 16, dpi = 600)






#####################DISTRIBUTION TESTS###########################
ggplot(dat, aes(x=MeatDays)) + geom_histogram(binwidth=.5)
ggplot(dat, aes(x=ProcessedDays)) + geom_histogram(binwidth=.5)
ggplot(dat, aes(x=RedDays)) + geom_histogram(binwidth=.5)
ggplot(dat, aes(x=WhiteDays)) + geom_histogram(binwidth=.5)
ggplot(dat, aes(x=NoMeatDays)) + geom_histogram(binwidth=.5)

ggplot(dat, aes(x=avgMeatokaj)) + geom_histogram(binwidth=.5)
ggplot(dat, aes(x=avgProcessedokaj)) + geom_histogram(binwidth=.5)
ggplot(dat, aes(x=avgRedokaj)) + geom_histogram(binwidth=.5)
ggplot(dat, aes(x=avgWhiteokaj)) + geom_histogram(binwidth=.5)
ggplot(dat, aes(x=avgNoMeatokaj)) + geom_histogram(binwidth=.5)

ggplot(dat, aes(x=gperokajMeat)) + geom_histogram(binwidth=.5) #excluded 629
ggplot(dat, aes(x=gperokajProcessed)) + geom_histogram(binwidth=.5) #excluded 2,967
ggplot(dat, aes(x=gperokajRed)) + geom_histogram(binwidth=.5) #excluded 4,095
ggplot(dat, aes(x=gperokajWhite)) + geom_histogram(binwidth=.5) #excluded 3,232
15332-629
15332-2967
15332-4095
15332-3232













































































##############plot effecrts for covariate analysis###############

m1 <- svyglm(gperokajMeat~ SurveyYear + Sex + SurveyYear * Sex,
             family = poisson(link = "log"), dat.design)
plot(allEffects(m1), multiline=T, confint = list(style = "auto"))








#####################MISC##########################3
# get variable names of the data (to make data dictionary)
variable_names <- names(dat)
print(variable_names)

#look at distribution of meat at mealtimes
