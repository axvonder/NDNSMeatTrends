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
library(purrr)
library(tibble)
library(gt)
#set wd
setwd("/Users/alexandervonderschmidt/Library/CloudStorage/OneDrive-SharedLibraries-UniversityofEdinburgh/NDNS Meat Trends - General/Data")
#upload datasets
dat <- read.csv('omega.csv')

#########################EDITS AND CREATIONS#######################

#set a participant's weight to 0 if the participant has <4 diary days completed
#this allows the participant to be excluded from analysis while maintaining original weights
dat$wti[dat$DiaryDaysCompleted < 4] <- 0
check3days <- subset(dat, DiaryDaysCompleted == 3, select = "wti") #do a widdle checky-poo
rm(check3days)

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
#for SMT analyses -> set 0 values to NA (to only capture meals in which meat was consumed)
varnames <- c("BsumMeatg", "BsumProcessedg", "BsumRedg", "BsumWhiteg", 
              "LsumMeatg", "LsumProcessedg", "LsumRedg", "LsumWhiteg", 
              "DsumMeatg", "DsumProcessedg", "DsumRedg", "DsumWhiteg")
#replace 0 with NA for each variable
for (var in varnames) {
  dat[[var]] <- ifelse(dat[[var]] == 0, NA, dat[[var]])
}
rm(var, varnames)

#set survey designs
dat$SurveyYear <- as.numeric(dat$SurveyYear)
#specify survey weighting structure for GLM
dat$fpc <- 15655
dat.design <-
  svydesign(
    id = ~area,
    strata = ~astrata5,
    data = dat,
    weights = ~wti,
    fpc = ~fpc
  )

#####################TABLE 1 - demographic analysis (all years)#######################
#create empty data frames to hold results
results_ageg <- data.frame(characteristic = unique(dat$AgeG))
results_sex <- data.frame(characteristic = unique(dat$Sex))
results_eqv <- data.frame(characteristic = unique(dat$eqv))

#function for demographic unweighted counts & percentages
demographic <- function(dataset, group_var, results_df) {
  results <- results_df
  
  #define survey design
  survey_design <- dataset %>%
    as_survey_design(ids = area, #cluster ids
                     weights = wti, #weight variable
                     strata = astrata5) #sampling was stratified by district
  
  #unweighted counts
  unweighted_counts <- dataset %>%
    filter(DiaryDaysCompleted == 4) %>% #exclude is fine here because it's not weighted
    group_by(!!sym(group_var)) %>%
    summarise(count = n()) %>%
    rename(characteristic = !!sym(group_var))
  
  #weighted percentages
  weighted_percentages <- survey_design %>%
    group_by(!!sym(group_var)) %>%
    summarise(pct = survey_mean(na.rm = TRUE)) %>%
    mutate(pct = round(100 * pct, 1)) %>%
    select(-pct_se) %>% #remove SE row (not needed for demographic analysis)
    rename(characteristic = !!sym(group_var))
  
  #merge counts & percentages
  results <- results %>%
    left_join(unweighted_counts, by = "characteristic") %>%
    rename("Unweighted_Count" := count) %>%
    left_join(weighted_percentages, by = "characteristic") %>%
    rename("Weighted_Percent" := pct)
  
  return(results)
}
#perform the analysis for AgeG, Sex, and eqv
results_ageg <- demographic(dat, "AgeG", results_ageg)
results_sex <- demographic(dat, "Sex", results_sex)
results_eqv <- demographic(dat, "eqv", results_eqv)

#rename characteristic column for display purposes in table and arrange rows
results_ageg$characteristic <- recode(results_ageg$characteristic, "1" = "≤10", "2" = "11–17", "3" = "18–40", "4" = "41–59", "5" = "≥60")
ordered_levels_ageg <- c("≤10", "11–17", "18–40", "41–59", "≥60")
results_ageg <- results_ageg %>% arrange(match(characteristic, ordered_levels_ageg))

results_sex$characteristic <- recode(results_sex$characteristic, "M" = "Male", "F" = "Female")
ordered_levels_sex <- c("Male", "Female")
results_sex <- results_sex %>% arrange(match(characteristic, ordered_levels_sex))

results_eqv$characteristic <- case_when(
  results_eqv$characteristic == "1" ~ "Lowest tertile",
  results_eqv$characteristic == "2" ~ "Middle tertile",
  results_eqv$characteristic == "3" ~ "Highest tertile",
  is.na(results_eqv$characteristic) ~ "Missing",
  TRUE ~ results_eqv$characteristic #leaves other values unchanged (this was messing up before)
)
ordered_levels_eqv <- c("Lowest tertile", "Middle tertile", "Highest tertile", "Missing")
results_eqv <- results_eqv %>% arrange(match(characteristic, ordered_levels_eqv))

#combine three result data frames
final_results <- rbind(results_ageg, results_sex, results_eqv)
table1 <- final_results
rm(final_results, results_ageg, results_sex, results_eqv,
   ordered_levels_ageg, ordered_levels_sex, ordered_levels_eqv,
   demographic)


#################SI TABLE 1 - demographic analysis year by year#############
#create empty data frames to hold results
results_ageg <- data.frame(characteristic = unique(dat$AgeG))
results_sex <- data.frame(characteristic = unique(dat$Sex))
results_eqv <- data.frame(characteristic = unique(dat$eqv))

#function for demographic unweighted counts & percentages (with a year-by-year loop)
demo_yearby <- function(dataset, group_var, results_df) {
  results <- results_df
  
  for (year in 1:11) {
    dat_subset <- dataset %>% filter(SurveyYear == year)
    
    if (nrow(dat_subset) > 0) {
      survey_design <- dat_subset %>%
        as_survey_design(ids = area, weights = wti, strata = astrata5)
      
      unweighted_counts <- dat_subset %>%
        group_by(!!sym(group_var)) %>%
        summarise(count = n()) %>%
        rename(characteristic = !!sym(group_var))
      
      weighted_percentages <- survey_design %>%
        group_by(!!sym(group_var)) %>%
        summarise(pct = survey_mean(na.rm = TRUE)) %>%
        mutate(pct = round(100 * pct, 1)) %>% 
        select(-pct_se) %>% #remove SE row (not needed for demographic analysis)
        rename(characteristic = !!sym(group_var))
      
      colname1 <- paste0("Unweighted_Count_Year_", year)
      colname2 <- paste0("Weighted_Percent_Year_", year)
      
      results <- results %>%
        left_join(unweighted_counts, by = "characteristic") %>%
        rename(!!colname1 := count) %>%
        left_join(weighted_percentages, by = "characteristic") %>%
        rename(!!colname2 := pct)
    }
  }
  return(results)
}

#perform the analysis for AgeG, Sex, and eqv
results_ageg <- demo_yearby(dat, "AgeG", results_ageg)
results_sex <- demo_yearby(dat, "Sex", results_sex)
results_eqv <- demo_yearby(dat, "eqv", results_eqv)

#rename characteristic column for display purposes in table and arrange rows
results_ageg$characteristic <- recode(results_ageg$characteristic, "1" = "≤10", "2" = "11–17", "3" = "18–40", "4" = "41–59", "5" = "≥60")
ordered_levels_ageg <- c("≤10", "11–17", "18–40", "41–59", "≥60")
results_ageg <- results_ageg %>% arrange(match(characteristic, ordered_levels_ageg))

results_sex$characteristic <- recode(results_sex$characteristic, "M" = "Male", "F" = "Female")
ordered_levels_sex <- c("Male", "Female")
results_sex <- results_sex %>% arrange(match(characteristic, ordered_levels_sex))

results_eqv$characteristic <- case_when(
  results_eqv$characteristic == "1" ~ "Lowest tertile",
  results_eqv$characteristic == "2" ~ "Middle tertile",
  results_eqv$characteristic == "3" ~ "Highest tertile",
  is.na(results_eqv$characteristic) ~ "Missing",
  TRUE ~ results_eqv$characteristic #leaves other values unchanged (this was messing up before)
)
ordered_levels_eqv <- c("Lowest tertile", "Middle tertile", "Highest tertile", "Missing")
results_eqv <- results_eqv %>% arrange(match(characteristic, ordered_levels_eqv))

# Combine the three results data frames
final_results <- rbind(results_ageg, results_sex, results_eqv)
sitable1 <- final_results
rm(final_results, results_ageg, results_sex, results_eqv,
   ordered_levels_ageg, ordered_levels_sex, ordered_levels_eqv,
   demo_yearby)

#############################TABLE 2 - main analysis (also SI table 2) ###################

#function to calculate mean and SE
meanies <- function(Xvar, year, dataset) {
  #replace Xvar with variable name
  #(idk why i have to do this, but it wasn't working without it and it wouldn't
  #let me just add the tilda to the beginning of Xvar for some reason...)
  form_var <- as.formula(paste("~", Xvar))
  
  #subset data per year
  dat_subset <- dataset %>% filter(SurveyYear == year)
  
  #set survey design
  survey_design <- dat_subset %>%
    as_survey_design(ids = area, weights = wti, strata = astrata5)
  
  #calculate the survey-weighted mean and SE
  Xval <- svymean(form_var, design = survey_design, na.rm = TRUE)
  
  #extract mean and SE, round to one dp
  mean_val <- round(as.numeric(Xval), 1)
  se_val <- round(sqrt(attr(Xval, "var")), 1) #I feel like there is a simpler way to extract SE,
  #but couldn't figure it out so just pulling it manually
  
  #combine mean & SE into one column
  mean_se_combined <- paste(mean_val, " (", se_val, ")", sep = "")
  
  #create a character vector (to store the mean + SE values together)
  year_values <- mean_se_combined
  names(year_values) <- paste("Year", year)
  
  return(year_values)
}
#function to take "meanies" function and loop through years (separate functions to increase modularity) 
meaniebobeanies <- function(var_list, dataset) {
  #create empty dataframe to store results
  results <- data.frame()
  
  #loop through each variable in the list
  for (var in var_list) {
    combined <- numeric()
    
    #loop through each survey year
    for (year in 1:11) {
      year_values <- meanies(var, year, dataset)
      combined <- c(combined, year_values)
    }
    
    #convert to a one-row data frame
    trans <- data.frame(t(combined))
    
    #add the 'next' variable row to the overall results data frame
    results <- rbind(results, trans)
  }
  
  return(results)
}
#function to get P for trend across all years in Poisson model using existing dat.design
ptrend <- function(variable, dat.design) {
  form_var <- as.formula(paste(variable, "~ SurveyYear"))
  
  #if variable ends with 'Days' or 'okaj', run Poisson model, if not, run glm model
  if (grepl("Days$", variable) || grepl("okaj$", variable)) {
    print(paste("Running Poisson model for:", variable)) #check for correct model
    model <- svyglm(form_var, family=poisson(link = "log"), design = dat.design)
  } else {
    print(paste("Running Linear model for:", variable)) #check for correct model
    model <- svyglm(form_var, design = dat.design)
  }
  
  p_value <- coef(summary(model))["SurveyYear", "Pr(>|t|)"]
  
  #round pval appropriately (3 deimals if <0.01, 2 otherwise)
  if (p_value < 0.001) {
    p_value_str <- "<0.001"
  } else if (p_value < 0.01) {
    p_value_str <- sprintf("%.3f", p_value)
  } else {
    p_value_str <- sprintf("%.2f", p_value)
  }
  
  return(p_value_str)
}
run <- function(analysis) {
  X <- meaniebobeanies(ListToFeed, dat)
  #create vector to store p-values
  pvec <- character()
  #loop through each variable in ListToFeed
  for (var in ListToFeed) {
    #get the p trend for each variable & add it to the vec
    pval <- ptrend(var, dat.design) #using dat.design to pull from survey weighted glm
    pvec <- c(pvec, pval)
  }
  X$`P for trend` <- pvec
  rownames(X) <- ListToFeed
  X <- rownames_to_column(X, var = "Meat Type")
  colnames(X) <- gsub("\\.", " ", colnames(X))
  #create dataframe with name
  assign(analysis, X, envir = globalenv())
}
#list of variables to feed (add all variables desired for analysis here, then run)
ListToFeed <- c("MeatDays", "avgMeatokaj", "gperokajMeat",
                "ProcessedDays", "avgProcessedokaj", "gperokajProcessed",
                "RedDays", "avgRedokaj", "gperokajRed",
                "WhiteDays", "avgWhiteokaj", "gperokajWhite",
                "NoMeatDays")
run("sitable2")
table2 <- sitable2[, c("Meat Type", "Year 1", "Year 11", "P for trend")]
rm(ListToFeed)

#unweighted count of participants in each survey year (not including those who have <4 diary days)
table(dat$SurveyYear[dat$DiaryDaysCompleted == 4])


#########################SI TABLE 3 - STM ANALYSIS########################

ListToFeed <- c("BsumMeatg", "BsumProcessedg", "BsumRedg", "BsumWhiteg",
                "LsumMeatg", "LsumProcessedg", "LsumRedg", "LsumWhiteg",
                "DsumMeatg", "DsumProcessedg", "DsumRedg", "DsumWhiteg")
run("sitable3")

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
  #exponentiated coefficients and CI for combinations
  exp_coef <- round(exp(b_combinations), 2)
  lower_bound <- round(exp(b_combinations - 1.96 * se_combinations), 2)
  upper_bound <- round(exp(b_combinations + 1.96 * se_combinations), 2)
  
  #difference between groups
  diff_coefs <- c(exp_coef[1] - exp_coef[2], exp_coef[3] - exp_coef[4])
  
  #standard error of the difference
  se_diff <- c(sqrt(sum(se_combinations[1:2]^2)), sqrt(sum(se_combinations[3:4]^2)))
  
  #confidence interval for the difference
  ci_diff_lower <- c(diff_coefs[1] - 1.96 * se_diff[1], diff_coefs[2] - 1.96 * se_diff[2])
  ci_diff_upper <- c(diff_coefs[1] + 1.96 * se_diff[1], diff_coefs[2] + 1.96 * se_diff[2])
  
  #data frame to present the results
  result_table <- rbind(
    data.frame(
      Group = c("M_Y1", "M_Y11", "F_Y1", "F_Y11"),
      Beta = round(exp_coef, 1),
      Lower = round(lower_bound, 1),
      Upper = round(upper_bound, 1),
      stringsAsFactors = FALSE
    ),
    data.frame(
      Group = c("Diff_M", "Diff_F"),
      Beta = round(diff_coefs, 1),
      Lower = round(ci_diff_lower, 1),
      Upper = round(ci_diff_upper, 1),
      stringsAsFactors = FALSE
    )
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
  #CI for combinations
  lower_bound <- round(b_combinations - 1.96 * se_combinations, 1)
  upper_bound <- round(b_combinations + 1.96 * se_combinations, 1)
  
  #difference between groups
  diff_coefs <- c(b_combinations[1] - b_combinations[2], b_combinations[3] - b_combinations[4])
  
  #standard error of the difference
  se_diff <- c(sqrt(sum(se_combinations[1:2]^2)), sqrt(sum(se_combinations[3:4]^2)))
  
  #confidence interval for the difference
  ci_diff_lower <- c(diff_coefs[1] - 1.96 * se_diff[1], diff_coefs[2] - 1.96 * se_diff[2])
  ci_diff_upper <- c(diff_coefs[1] + 1.96 * se_diff[1], diff_coefs[2] + 1.96 * se_diff[2])
  
  #data frame to present the results
  result_table <- rbind(
    data.frame(
      Group = c("M_Y1", "M_Y11", "F_Y1", "F_Y11"),
      Beta = round(b_combinations, 1),
      Lower = round(lower_bound, 1),
      Upper = round(upper_bound, 1),
      stringsAsFactors = FALSE
    ),
    data.frame(
      Group = c("Diff_M", "Diff_F"),
      Beta = round(diff_coefs, 1),
      Lower = round(ci_diff_lower, 1),
      Upper = round(ci_diff_upper, 1),
      stringsAsFactors = FALSE
    )
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

#p values (use only after setting SurveyYear to numeric)
summary(svyglm(MeatDays ~ SurveyYear + Sex + SurveyYear * Sex, family = poisson(link = "log"), dat.design))
summary(svyglm(ProcessedDays ~ SurveyYear + Sex + SurveyYear * Sex, family = poisson(link = "log"), dat.design))
summary(svyglm(RedDays ~ SurveyYear + Sex + SurveyYear * Sex, family = poisson(link = "log"), dat.design))
summary(svyglm(WhiteDays ~ SurveyYear + Sex + SurveyYear * Sex, family = poisson(link = "log"), dat.design))
summary(svyglm(NoMeatDays ~ SurveyYear + Sex + SurveyYear * Sex, family = poisson(link = "log"), dat.design))

summary(svyglm(avgMeatokaj ~ SurveyYear + Sex + SurveyYear * Sex, family = poisson(link = "log"), dat.design))
summary(svyglm(avgProcessedokaj ~ SurveyYear + Sex + SurveyYear * Sex, family = poisson(link = "log"), dat.design))
summary(svyglm(avgRedokaj ~ SurveyYear + Sex + SurveyYear * Sex, family = poisson(link = "log"), dat.design))
summary(svyglm(avgWhiteokaj ~ SurveyYear + Sex + SurveyYear * Sex, family = poisson(link = "log"), dat.design))

summary(svyglm(gperokajMeat ~ SurveyYear + Sex + SurveyYear * Sex, dat.design))
summary(svyglm(gperokajProcessed ~ SurveyYear + Sex + SurveyYear * Sex, dat.design))
summary(svyglm(gperokajRed ~ SurveyYear + Sex + SurveyYear * Sex, dat.design))
summary(svyglm(gperokajWhite ~ SurveyYear + Sex + SurveyYear * Sex, dat.design))

summary(svyglm(MeatDays ~ SurveyYear + Sex + SurveyYear * Sex, family = poisson(link = "log"), dat.design))

#AGE
exp_interaction_CI_age <- function(response_var, design) {
  model_formula <- as.formula(paste(response_var, "~ SurveyYear + AgeG + SurveyYear * AgeG"))
  model <- svyglm(model_formula, family = poisson(link = "log"), design = design)
  model_summary <- summary(model)
  betas <- coef(model)
  se <- coef(model_summary)[, "Std. Error"]
  
  # combinations of coefficients
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
  
  # exponentiated coefficients
  exp_coef <- round(exp(b_combinations), 1)
  
  # standard errors for combinations
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
  lower_bound <- round(exp_coef - 1.96 * se_combinations, 1)
  upper_bound <- round(exp_coef + 1.96 * se_combinations, 1)
  
  # differences of exponentiated coefficients for each pair of groups (Y1 - Y11)
  diff_coefs <- exp_coef[seq(1, length(exp_coef), 2)] - exp_coef[seq(2, length(exp_coef), 2)]
  
  # standard errors for the differences
  se_diff <- sqrt(se_combinations[seq(1, length(se_combinations), 2)]^2 + se_combinations[seq(2, length(se_combinations), 2)]^2)
  
  # CI for the differences
  diff_lower_bound <- round(diff_coefs - 1.96 * se_diff, 1)
  diff_upper_bound <- round(diff_coefs + 1.96 * se_diff, 1)
  
  # data frame to present the results
  result_table <- rbind(
    data.frame(
      Group = c("18-40_Y1", "18-40_Y11", "<10_Y1", "<10_Y11", "11-17_Y1", "11-17_Y11", "41-59_Y1", "41-59_Y11", ">=60_Y1", ">=60_Y11"),
      Beta = round(exp_coef, 1),
      Lower = round(lower_bound, 1),
      Upper = round(upper_bound, 1)
    ),
    data.frame(
      Group = c("Diff_18-40", "Diff_<10", "Diff_11-17", "Diff_41-59", "Diff_>=60"),
      Beta = round(diff_coefs, 1),
      Lower = round(diff_lower_bound, 1),
      Upper = round(diff_upper_bound, 1)
    )
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
  lower_bound <- round(b_combinations - 1.96 * se_combinations, 1)
  upper_bound <- round(b_combinations + 1.96 * se_combinations, 1)
  
  #difference between groups
  diff_coefs <- b_combinations[seq(1, length(b_combinations), 2)] - b_combinations[seq(2, length(b_combinations), 2)]
  
  #standard error of the difference
  se_diff <- sqrt(se_combinations[seq(1, length(se_combinations), 2)]^2 + se_combinations[seq(2, length(se_combinations), 2)]^2)
  
  #confidence interval for the difference
  ci_diff_lower <- round(diff_coefs - 1.96 * se_diff, 1)
  ci_diff_upper <- round(diff_coefs + 1.96 * se_diff, 1)
  
  #data frame to present the results
  result_table <- rbind(
    data.frame(
      Group = c("18-40_Y1", "18-40_Y11", "<10_Y1", "<10_Y11", "11-17_Y1", "11-17_Y11", "41-59_Y1", "41-59_Y11", ">=60_Y1", ">=60_Y11"),
      Beta = round(b_combinations, 1),
      Lower = round(lower_bound, 1),
      Upper = round(upper_bound, 1),
      stringsAsFactors = FALSE
    ),
    data.frame(
      Group = c("Diff_18-40", "Diff_<10", "Diff_11-17", "Diff_41-59", "Diff_>=60"),
      Beta = round(diff_coefs, 1),
      Lower = round(ci_diff_lower, 1),
      Upper = round(ci_diff_upper, 1),
      stringsAsFactors = FALSE
    )
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

#p values (use only after setting SurveyYear to numeric)
summary(svyglm(MeatDays ~ SurveyYear + AgeG + SurveyYear * AgeG, family = poisson(link = "log"), dat.design))
summary(svyglm(ProcessedDays ~ SurveyYear + AgeG + SurveyYear * AgeG, family = poisson(link = "log"), dat.design))
summary(svyglm(RedDays ~ SurveyYear + AgeG + SurveyYear * AgeG, family = poisson(link = "log"), dat.design))
summary(svyglm(WhiteDays ~ SurveyYear + AgeG + SurveyYear * AgeG, family = poisson(link = "log"), dat.design))
summary(svyglm(NoMeatDays ~ SurveyYear + AgeG + SurveyYear * AgeG, family = poisson(link = "log"), dat.design))

summary(svyglm(avgMeatokaj ~ SurveyYear + AgeG + SurveyYear * AgeG, family = poisson(link = "log"), dat.design))
summary(svyglm(avgProcessedokaj ~ SurveyYear + AgeG + SurveyYear * AgeG, family = poisson(link = "log"), dat.design))
summary(svyglm(avgRedokaj ~ SurveyYear + AgeG + SurveyYear * AgeG, family = poisson(link = "log"), dat.design))
summary(svyglm(avgWhiteokaj ~ SurveyYear + AgeG + SurveyYear * AgeG, family = poisson(link = "log"), dat.design))

summary(svyglm(gperokajMeat ~ SurveyYear + AgeG + SurveyYear * AgeG, dat.design))
summary(svyglm(gperokajProcessed ~ SurveyYear + AgeG + SurveyYear * AgeG, dat.design))
summary(svyglm(gperokajRed ~ SurveyYear + AgeG + SurveyYear * AgeG, dat.design))
summary(svyglm(gperokajWhite ~ SurveyYear + AgeG + SurveyYear * AgeG, dat.design))

#EQV
exp_interaction_CI_eqv <- function(response_var, design) {
  model_formula <- as.formula(paste(response_var, "~ SurveyYear + eqv + SurveyYear * eqv"))
  model <- svyglm(model_formula, family = poisson(link = "log"), design = design)
  model_summary <- summary(model)
  betas <- coef(model)
  se <- coef(model_summary)[, "Std. Error"]
  
  # combinations of coefficients
  b_combinations <- c(
    betas["(Intercept)"],
    betas["(Intercept)"] + betas["eqv2"],
    betas["(Intercept)"] + betas["eqv3"],
    betas["(Intercept)"] + betas["SurveyYear11"],
    betas["(Intercept)"] + betas["SurveyYear11"] + betas["eqv2"] + betas["SurveyYear11:eqv2"],
    betas["(Intercept)"] + betas["SurveyYear11"] + betas["eqv3"] + betas["SurveyYear11:eqv3"]
  )
  
  # exponentiated coefficients
  exp_coef <- round(exp(b_combinations), 1)
  
  # standard errors for combinations
  se_combinations <- c(
    se["(Intercept)"],
    sqrt(se["(Intercept)"]^2 + se["eqv2"]^2),
    sqrt(se["(Intercept)"]^2 + se["eqv3"]^2),
    sqrt(se["(Intercept)"]^2 + se["SurveyYear11"]^2),
    sqrt(se["(Intercept)"]^2 + se["SurveyYear11"]^2 + se["eqv2"]^2 + se["SurveyYear11:eqv2"]^2),
    sqrt(se["(Intercept)"]^2 + se["SurveyYear11"]^2 + se["eqv3"]^2 + se["SurveyYear11:eqv3"]^2)
  )
  
  # CI for combinations
  lower_bound <- round(exp_coef - 1.96 * se_combinations, 1)
  upper_bound <- round(exp_coef + 1.96 * se_combinations, 1)
  
  # differences of exponentiated coefficients for each pair of groups (Y1 - Y11)
  diff_coefs <- exp_coef[c(1, 2, 3)] - exp_coef[c(4, 5, 6)]
  
  # standard errors for the differences
  se_diff <- sqrt(se_combinations[c(1, 2, 3)]^2 + se_combinations[c(4, 5, 6)]^2)
  
  # CI for the differences
  diff_lower_bound <- round(diff_coefs - 1.96 * se_diff, 1)
  diff_upper_bound <- round(diff_coefs + 1.96 * se_diff, 1)
  
  # data frame to present the results
  result_table <- rbind(
    data.frame(
      Group = c("eqv1_Y1", "eqv2_Y1", "eqv3_Y1", "eqv1_Y11", "eqv2_Y11", "eqv3_Y11"),
      Beta = round(exp_coef, 1),
      Lower = round(lower_bound, 1),
      Upper = round(upper_bound, 1)
    ),
    data.frame(
      Group = c("Diff_eqv1", "Diff_eqv2", "Diff_eqv3"),
      Beta = round(diff_coefs, 1),
      Lower = round(diff_lower_bound, 1),
      Upper = round(diff_upper_bound, 1)
    )
  )
  return(result_table)
}
glm_interaction_CI_eqv <- function(response_var, design) {
  model_formula <- as.formula(paste(response_var, "~ SurveyYear + eqv + SurveyYear * eqv"))
  model <- svyglm(model_formula, design = design)
  model_summary <- summary(model)
  betas <- coef(model)
  se <- coef(model_summary)[, "Std. Error"]
  
  # combinations of coefficients
  b_combinations <- c(
    betas["(Intercept)"],
    betas["(Intercept)"] + betas["eqv2"],
    betas["(Intercept)"] + betas["eqv3"],
    betas["(Intercept)"] + betas["SurveyYear11"],
    betas["(Intercept)"] + betas["SurveyYear11"] + betas["eqv2"] + betas["SurveyYear11:eqv2"],
    betas["(Intercept)"] + betas["SurveyYear11"] + betas["eqv3"] + betas["SurveyYear11:eqv3"]
  )
  
  # standard errors for combinations
  se_combinations <- c(
    se["(Intercept)"],
    sqrt(se["(Intercept)"]^2 + se["eqv2"]^2),
    sqrt(se["(Intercept)"]^2 + se["eqv3"]^2),
    sqrt(se["(Intercept)"]^2 + se["SurveyYear11"]^2),
    sqrt(se["(Intercept)"]^2 + se["SurveyYear11"]^2 + se["eqv2"]^2 + se["SurveyYear11:eqv2"]^2),
    sqrt(se["(Intercept)"]^2 + se["SurveyYear11"]^2 + se["eqv3"]^2 + se["SurveyYear11:eqv3"]^2)
  )
  
  # CI for combinations
  lower_bound <- round(b_combinations - 1.96 * se_combinations, 1)
  upper_bound <- round(b_combinations + 1.96 * se_combinations, 1)
  
  # differences of coefficients for each pair of groups (Y1 - Y11)
  diff_coefs <- b_combinations[c(1, 2, 3)] - b_combinations[c(4, 5, 6)]
  
  # standard errors for the differences
  se_diff <- sqrt(se_combinations[c(1, 2, 3)]^2 + se_combinations[c(4, 5, 6)]^2)
  
  # CI for the differences
  diff_lower_bound <- round(diff_coefs - 1.96 * se_diff, 1)
  diff_upper_bound <- round(diff_coefs + 1.96 * se_diff, 1)
  
  # data frame to present the results
  result_table <- rbind(
    data.frame(
      Group = c("eqv1_Y1", "eqv2_Y1", "eqv3_Y1", "eqv1_Y11", "eqv2_Y11", "eqv3_Y11"),
      Beta = round(b_combinations, 1),
      Lower = round(lower_bound, 1),
      Upper = round(upper_bound, 1)
    ),
    data.frame(
      Group = c("Diff_eqv1", "Diff_eqv2", "Diff_eqv3"),
      Beta = round(diff_coefs, 1),
      Lower = round(diff_lower_bound, 1),
      Upper = round(diff_upper_bound, 1)
    )
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

#p values (use only after setting SurveyYear to numeric)
summary(svyglm(MeatDays ~ SurveyYear + eqv + SurveyYear * eqv, family = poisson(link = "log"), dat.design))
summary(svyglm(ProcessedDays ~ SurveyYear + eqv + SurveyYear * eqv, family = poisson(link = "log"), dat.design))
summary(svyglm(RedDays ~ SurveyYear + eqv + SurveyYear * eqv, family = poisson(link = "log"), dat.design))
summary(svyglm(WhiteDays ~ SurveyYear + eqv + SurveyYear * eqv, family = poisson(link = "log"), dat.design))
summary(svyglm(NoMeatDays ~ SurveyYear + eqv + SurveyYear * eqv, family = poisson(link = "log"), dat.design))

summary(svyglm(avgMeatokaj ~ SurveyYear + eqv + SurveyYear * eqv, family = poisson(link = "log"), dat.design))
summary(svyglm(avgProcessedokaj ~ SurveyYear + eqv + SurveyYear * eqv, family = poisson(link = "log"), dat.design))
summary(svyglm(avgRedokaj ~ SurveyYear + eqv + SurveyYear * eqv, family = poisson(link = "log"), dat.design))
summary(svyglm(avgWhiteokaj ~ SurveyYear + eqv + SurveyYear * eqv, family = poisson(link = "log"), dat.design))

summary(svyglm(gperokajMeat ~ SurveyYear + eqv + SurveyYear * eqv, dat.design))
summary(svyglm(gperokajProcessed ~ SurveyYear + eqv + SurveyYear * eqv, dat.design))
summary(svyglm(gperokajRed ~ SurveyYear + eqv + SurveyYear * eqv, dat.design))
summary(svyglm(gperokajWhite ~ SurveyYear + eqv + SurveyYear * eqv, dat.design))

#######################DECOMPOSITION ANALYSIS###############

#given values (pulled from table 2 analysis)
d1 <- 3.27; d1_lower <- 3.20; d1_upper <- 3.35
d2 <- 3.03; d2_lower <- 2.97; d2_upper <- 3.22
zo1 <- 1.24; zo1_lower <- 1.20; zo1_upper <- 1.28
zo2 <- 1.13; zo2_lower <- 1.11; zo2_upper <- 1.24
p1 <- 85.76; p1_lower <- 82.13; p1_upper <- 89.38
p2 <- 76.12; p2_lower <- 74.71; p2_upper <- 84.78

#calculate o1 and o2 and their CIs
o1 <- (zo1*4)/d1
o1_lower <- (zo1_lower*4)/d1_upper
o1_upper <- (zo1_upper*4)/d1_lower
o2 <- (zo2*4)/d2
o2_lower <- (zo2_lower*4)/d2_upper
o2_upper <- (zo2_upper*4)/d2_lower

#calculate c1, c2 and their CIs (total consumption)
c1 <- p1*o1*d1
c2 <- p2*o2*d2
c1_lower <- p1_lower * o1_lower * d1_lower
c1_upper <- p1_upper * o1_upper * d1_upper
c2_lower <- p2_lower * o2_lower * d2_lower
c2_upper <- p2_upper * o2_upper * d2_upper

#standard errors
SE_c1 <- (c1_upper - c1) / 1.96
SE_c2 <- (c2_upper - c2) / 1.96

#standard error for the difference
SE_delta <- sqrt(SE_c1^2 + SE_c2^2)

#change in consumption
cdelta <- c2 - c1

# Calculate the difference per day
cdelta_per_day <- (c2 - c1) / 4

# Determine the 95% confidence interval for the difference per day
CI_delta_per_day_lower <- cdelta_per_day - 1.96 * (SE_delta / 4)
CI_delta_per_day_upper <- cdelta_per_day + 1.96 * (SE_delta / 4)

list(
  cdelta_per_day = cdelta_per_day,
  CI_delta_per_day = c(CI_delta_per_day_lower, CI_delta_per_day_upper)
)



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
c1/4
c2/4
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
c1/4
c2/4

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
c1/4
c2/4
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
cdelta/4



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
c1/4
c2/4
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
cdelta/4





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
c1/4
c2/4
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
cdelta/4



#######################COLORBLIND PALETTE##################
#define the color palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#data frame with the colors and their corresponding labels
palette_df <- data.frame(color = cbPalette, label = 1:length(cbPalette))
#display colors
ggplot(palette_df, aes(x = factor(label), y = 1, fill = color)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_identity() +
  theme_void() +
  theme(legend.position = "none") +
  coord_fixed(ratio = 0.2)


#######################FIGURE 1###########################
#function for Survey Year x axis labels
custom_x_labels <- function(x) {
  labels <- ifelse(x == 1, "2008/09", sprintf("'%02d/'%02d", x + 7, (x + 7) %% 100 + 1))
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
                        labels = c("solid" = "Actual data", "dotted" = "2008/09-2018/19 trend")) +
  labs(x = "Survey Year", y = "No. meat days/4-day period", color = "Meat category") +
  scale_x_continuous(breaks = predictions$SurveyYear, labels = custom_x_labels) +
  theme_classic() +
  theme(text = element_text(family = "Avenir", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
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
  labs(x = "Survey Year", y = "No. meat-eating occasions/day", color = "Meat category") +
  scale_x_continuous(breaks = predictions$SurveyYear, labels = custom_x_labels) +
  theme_classic() +
  theme(text = element_text(family = "Avenir", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(linetype = guide_legend(override.aes = list(color = "black")))
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
  labs(x = "Survey Year", y = "Portion size (g)/meat-eating occasion", color = "Meat category") +
  scale_x_continuous(breaks = predictions$SurveyYear, labels = custom_x_labels) +
  theme_classic() +
  theme(text = element_text(family = "Avenir", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(linetype = guide_legend(override.aes = list(color = "black")))
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
#create dataset (values pulled from decomposition analysis section of this code)
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
#divide numeric columns by 4 (to represent daily intake)
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
                    labels = c("Total_Delta" = "Total change",
                               "Days_Delta" = "Meat-eating days",
                               "Occasions_Delta" = "Meat-eating occasions",
                               "Portion_Size_Delta" = "Portion size of meat")) +
  labs(x = "Meat sub-type", y = "Change in meat consumption (g/capita/day)", fill = "Meat reduction behaviours") +
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
  labels <- ifelse(x == 1, "2008/09", sprintf("'%02d/'%02d", x + 7, (x + 7) %% 100 + 1))
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
plot1 <- plot1 + ggtitle("Total meat") + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
plot2 <- plot2 + ggtitle("Processed meat") + theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")
plot3 <- plot3 + ggtitle("Red meat") + theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")
plot4 <- plot4 + ggtitle("White meat") + theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")
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




#create counts for MeatDays by SurveyYear
meat_days_counts <- dat_svy %>% 
  group_by(SurveyYear) %>% 
  summarise(days0 = sum(MeatDays == 0),
            days1 = sum(MeatDays == 1),
            days2 = sum(MeatDays == 2),
            days3 = sum(MeatDays == 3),
            days4 = sum(MeatDays == 4))






###############SI FIGURE 1###########################
#############REMOVED FROM ANALYSIS#######################

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

#function for survey year x axis
custom_x_labels <- function(x) {
  labels <- ifelse(x == 1, "2008/09", sprintf("'%02d/'%02d", x + 7, (x + 7) %% 100 + 1))
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
