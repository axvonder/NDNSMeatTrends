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
library(openxlsx)

#set wd
setwd("/Users/alexandervonderschmidt/Library/CloudStorage/OneDrive-SharedLibraries-UniversityofEdinburgh/NDNS Meat Trends - General/Data")
#upload datasets
dat <- read.csv('omega.csv')

#########################EDITS AND CREATIONS#######################

#set a participant's weight to 0 if the participant has <4 diary days completed
#this allows the participant to be excluded from analysis while maintaining original weights
dat$wti[dat$DiaryDaysCompleted < 4] <- 0

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
#set eqv NAs to -99 for missing group
dat$eqv <- ifelse(is.na(dat$eqv), -99, dat$eqv)
#add categorical variable for adult yes/no
dat$LifeStage <- ifelse(dat$Age >=18, "A", "C")
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
#change 2 cluster variables (area [PSU] and serialh [household] to factors)
dat$area <- as.factor(dat$area)
dat$serialh <- as.factor(dat$serialh)
#specify survey weighting structure for GLM & Poisson (used for calculating P-values)
dat.design <-
  svydesign(
    id = ~serialh/area, #multi-level cluster of households and PSUs
    strata = ~astrata5, #stratification by district
    data = dat,
    weights = ~wti, #NDNS-assigned survey weight
    nest = TRUE
  )

#specify survey weighting structure for cross-sectional analysis (weighted means by year)
survey_design <- dat %>%
  as_survey_design(ids = serialh/area, weights = wti, strata = astrata5)
options(survey.lonely.psu = "adjust") #adjust the degrees of freedom for strata with a single PSU

#####################TABLE 1 - demographic analysis #######################
#rounding conditional statement (2 decimals for small numbers, 2 for large numbers - set up to be changeable if desired)
round_condish <- function(x) {
  ifelse(x >= 10, round(x, 2), round(x, 2))
}
#create empty data frames to hold results
results_ageg <- data.frame(characteristic = unique(dat$LifeStage))
results_sex <- data.frame(characteristic = unique(dat$Sex))
results_eqv <- data.frame(characteristic = unique(dat$eqv))

#function for demographic unweighted counts & percentages
demographic <- function(dataset, group_var, results_df) {
  results <- results_df
  
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
    mutate(pct = round_condish(100 * pct)) %>%
    dplyr::select(-pct_se) %>% #remove SE row (not needed for demographic analysis)
    rename(characteristic = !!sym(group_var))
  
  #merge counts & percentages
  results <- results %>%
    left_join(unweighted_counts, by = "characteristic") %>%
    rename("Unweighted_Count" := count) %>%
    left_join(weighted_percentages, by = "characteristic") %>%
    rename("Weighted_Percent" := pct)
  
  return(results)
}
#perform the analysis for LifeStage, Sex, and eqv
results_ageg <- demographic(dat, "LifeStage", results_ageg)
results_sex <- demographic(dat, "Sex", results_sex)
results_eqv <- demographic(dat, "eqv", results_eqv)

#rename characteristic column for display purposes in table and arrange rows
results_ageg$characteristic <- recode(results_ageg$characteristic, "C" = "Children (<18)", "A" = "Adults (≥18)")
ordered_levels_ageg <- c("Children (<18)", "Adults (≥18)")
results_ageg <- results_ageg %>% arrange(match(characteristic, ordered_levels_ageg))

results_sex$characteristic <- recode(results_sex$characteristic, "M" = "Male", "F" = "Female")
ordered_levels_sex <- c("Male", "Female")
results_sex <- results_sex %>% arrange(match(characteristic, ordered_levels_sex))

results_eqv$characteristic <- case_when(
  results_eqv$characteristic == "1" ~ "Lowest tertile",
  results_eqv$characteristic == "2" ~ "Middle tertile",
  results_eqv$characteristic == "3" ~ "Highest tertile",
  results_eqv$characteristic == "-99" ~ "Missing",
  TRUE ~ results_eqv$characteristic #leaves other values unchanged (this was messing up before)
)
ordered_levels_eqv <- c("Lowest tertile", "Middle tertile", "Highest tertile", "Missing")
results_eqv <- results_eqv %>% arrange(match(characteristic, ordered_levels_eqv))

#combine three result data frames
final_results <- rbind(results_ageg, results_sex, results_eqv)
table1 <- final_results
colnames(table1) <- c("Characteristic", "2008/09 - 2018/19", "Survey-weighted %")
rm(final_results, results_ageg, results_sex, results_eqv,
   ordered_levels_ageg, ordered_levels_sex, ordered_levels_eqv,
   demographic)


#################SI TABLE 1 - demographic analysis year by year#############
#create empty data frames to hold results
results_ageg <- data.frame(characteristic = unique(dat$LifeStage))
results_sex <- data.frame(characteristic = unique(dat$Sex))
results_eqv <- data.frame(characteristic = unique(dat$eqv))

#function for demographic unweighted counts & percentages (with a year-by-year loop)
demo_yearby <- function(dataset, group_var, results_df) {
  results <- results_df
  
  for (year in 1:11) {
    dat_subset <- dataset %>% filter(SurveyYear == year)
    
    #set survey design per year
    survey_design_year <- dat_subset %>%
      as_survey_design(ids = serialh/area, weights = wti, strata = astrata5)
      
      unweighted_counts <- dat_subset %>%
        group_by(!!sym(group_var)) %>%
        summarise(count = n()) %>%
        rename(characteristic = !!sym(group_var))
      
      weighted_percentages <- survey_design_year %>%
        group_by(!!sym(group_var)) %>%
        summarise(pct = survey_mean(na.rm = TRUE)) %>%
        mutate(pct = round_condish(100 * pct)) %>% 
        dplyr::select(-pct_se) %>% #remove SE row (not needed for demographic analysis)
        rename(characteristic = !!sym(group_var))
      
      colname1 <- paste0("Unweighted_Count_Year_", year)
      colname2 <- paste0("Weighted_Percent_Year_", year)
      
      results <- results %>%
        left_join(unweighted_counts, by = "characteristic") %>%
        rename(!!colname1 := count) %>%
        left_join(weighted_percentages, by = "characteristic") %>%
        rename(!!colname2 := pct)
    }
  return(results)
}

#perform the analysis for LifeStage, Sex, and eqv
results_ageg <- demo_yearby(dat, "LifeStage", results_ageg)
results_sex <- demo_yearby(dat, "Sex", results_sex)
results_eqv <- demo_yearby(dat, "eqv", results_eqv)

#rename characteristic column for display purposes in table and arrange rows
results_ageg$characteristic <- recode(results_ageg$characteristic, "C" = "Children (<18)", "A" = "Adults (≥18)")
ordered_levels_ageg <- c("Children (<18)", "Adults (≥18)")
results_ageg <- results_ageg %>% arrange(match(characteristic, ordered_levels_ageg))

results_sex$characteristic <- recode(results_sex$characteristic, "M" = "Male", "F" = "Female")
ordered_levels_sex <- c("Male", "Female")
results_sex <- results_sex %>% arrange(match(characteristic, ordered_levels_sex))

results_eqv$characteristic <- case_when(
  results_eqv$characteristic == "1" ~ "Lowest tertile",
  results_eqv$characteristic == "2" ~ "Middle tertile",
  results_eqv$characteristic == "3" ~ "Highest tertile",
  results_eqv$characteristic == "-99" ~ "Missing",
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

#############SI TABLE 2###################
meat_types <- c("Total Meat", "Processed Meat", "Red Meat", "White Meat")
sitable2 <- data.frame(matrix(ncol = 12, nrow = 4))
colnames(sitable2) <- c("Meat Type", paste0("Year", 1:11))
rownames(sitable2) <- meat_types
sitable2$`Meat Type` <- meat_types

meatavgs <- function(year) {
  meat_columns <- c("sumMeatg", "sumProcessedg", "sumRedg", "sumWhiteg") # Replace with actual column names
  
  sapply(meat_columns, function(meat_column) {
    survey_design %>%
      filter(SurveyYear == year) %>%
      summarize(weighted_mean = survey_mean(!!sym(meat_column), na.rm = TRUE)) %>%
      pull(weighted_mean)
  })
}

for (year in 1:11) {
  sitable2[paste0("Year", year)] <- meatavgs(year)
}
#set variables per day (divide by 4) and apply rounding condition
sitable2[, -1] <- apply(sitable2[, -1] / 4, 2, round_condish)
#checked that the sum of all sub meat types add up to their respective total meat amount for each year

#############################TABLE 2 - main analysis (also SI table 3) ###################
#calculate proportion of participants who are meat consumers for each meat subtype
weighted_props <- svyby(~I(sumMeatg > 0) + I(sumProcessedg > 0) + I(sumRedg > 0) + I(sumWhiteg > 0),
                        ~SurveyYear, 
                        survey_design, 
                        svymean, 
                        na.rm = TRUE)
weighted_props <- weighted_props %>%
  dplyr::select(ends_with("TRUE"))
colnames(weighted_props) <- c("PropMeat", "PropProcessed", "PropRed", "PropWhite",
                              "PropMeatSE", "PropProcessedSE", "PropRedSE", "PropWhiteSE")
Props <- as.data.frame(t(weighted_props))
Props <- rownames_to_column(Props, var = "MeatType")
Props[1:8, -1] <- round(Props[1:8, -1] * 100, 2)
PropsFormatted <- Props[1:4, ]
for (i in 1:nrow(PropsFormatted)) { #loop to add the SE values into the same cell as the mean
  for (j in 2:ncol(PropsFormatted)) {
    PropsFormatted[i, j] <- paste0(Props[i, j], " (", Props[i + 4, j], ")")
  }
}
colnames(PropsFormatted)[-1] <- paste0("Year ", colnames(PropsFormatted)[-1])
weighted_props$SurveyYear <- 1:nrow(weighted_props) #add a "survey year" variable (to integrate into main dataset for analysis)
weighted_props <- weighted_props[, c("SurveyYear", "PropMeat", "PropProcessed", "PropRed", "PropWhite")]
dat <- merge(dat, weighted_props, by = "SurveyYear", all.x = TRUE)
dat.design <-
  svydesign( #re-spectify survey design with new proportion variables also need the old survey design to create these variables)
    id = ~serialh/area, #multi-level cluster of households and PSUs
    strata = ~astrata5, #stratification by district
    data = dat,
    weights = ~wti, #NDNS-assigned survey weight
    nest = TRUE
  )

meanies <- function(Xvar, year, dataset, filter_meat = FALSE) {
  #replaces Xvar with variable name
  form_var <- as.formula(paste("~", Xvar))
  
  #subset data per year
  dat_subset <- dataset %>% filter(SurveyYear == year)
  
  if (!is.null(filter_meat) && filter_meat != FALSE) {
    dat_subset <- dat_subset %>% filter(eval(parse(text = filter_meat)))
  }
  
  #set survey design
  survey_design <- dat_subset %>%
    as_survey_design(ids = serialh/area, weights = wti, strata = astrata5)
  
  #calculate the survey-weighted mean and SE
  Xval <- svymean(form_var, design = survey_design, na.rm = TRUE)
  
  #extract mean and SE, round to one dp
  mean_val <- round_condish(as.numeric(Xval))
  se_val <- round_condish(sqrt(attr(Xval, "var"))) #I feel like there is a simpler way to extract SE,
  #but couldn't figure it out so just pulling it manually
  
  #combine mean & SE into one column
  mean_se_combined <- paste(mean_val, " (", se_val, ")", sep = "")
  
  #create a character vector (to store the mean + SE values together)
  year_values <- mean_se_combined
  names(year_values) <- paste("Year", year)
  
  return(year_values)
}
#function to take "meanies" function and loop through years (separate functions to increase modularity) 
meaniebobeanies <- function(var_list, dataset, filter_meat = FALSE) {
  #create empty dataframe to store results
  results <- data.frame()
  
  #loop through each variable in the list
  for (var in var_list) {
    combined <- numeric()
    
    #loop through each survey year
    for (year in 1:11) {
      year_values <- meanies(var, year, dataset, filter_meat)
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
#but first make a wee function for roundning p-values (3 deimals if <0.01, 2 otherwise)
roundies <- function(p_value) {
  if (is.na(p_value)) {
    return(NA)
  } else if (p_value < 0.001) {
    return("<0.001")
  } else if (p_value < 0.01) {
    return(sprintf("%.3f", p_value))
  } else {
    return(sprintf("%.2f", p_value))
  }
}
ptrend <- function(variable, dat.design) {
  form_var <- as.formula(paste(variable, "~ SurveyYear"))
  
  #if variable ends with 'Days', run Poisson model, if not, run glm model
  if (grepl("Days$", variable)) {
    print(paste("Running Poisson model for:", variable)) #check for correct model
    model <- svyglm(form_var, family=poisson(link = "log"), design = dat.design)
  } else {
    print(paste("Running Linear model for:", variable)) #check for correct model
    model <- svyglm(form_var, design = dat.design)
  }
  
  p_value <- coef(summary(model))["SurveyYear", "Pr(>|t|)"]
  
  #round
  peezies <- roundies(p_value)
  
  return(peezies)
}
run <- function(analysis) {
  results <- data.frame()
  pvec <- character()
  
  filter_conditions <- list(
    "MeatDays" = "sumMeatg > 0",
    "ProcessedDays" = "sumProcessedg > 0",
    "RedDays" = "sumRedg > 0",
    "WhiteDays" = "sumWhiteg > 0",
    "avgMeatokaj" = "sumMeatg > 0",
    "avgProcessedokaj" = "sumProcessedg > 0",
    "avgRedokaj" = "sumRedg > 0",
    "avgWhiteokaj" = "sumWhiteg > 0"
  )
  
  for (var in ListToFeed) {
    if (var %in% names(filter_conditions)) {
      filter_meat <- filter_conditions[[var]]
      X <- meaniebobeanies(var, dat, filter_meat)  # Calls with filtering
    } else {
      X <- meaniebobeanies(var, dat)  # Calls without filtering
    }
    
    rownames(X) <- var # Set row names to variable being ran
    
    results <- rbind(results, X)
    pval <- ptrend(var, dat.design)
    pvec <- c(pvec, pval)
  }
  
  results$`P for trend` <- pvec
  results <- rownames_to_column(results, var = "Meat Type")
  colnames(results) <- gsub("\\.", " ", colnames(results))
  
  assign(analysis, results, envir = globalenv())
}


#list of variables to feed (add all variables desired for analysis here, then run)
ListToFeed <- c("PropMeat", "MeatDays", "avgMeatokaj", "gperokajMeat", "sumMeatgdaily", 
                "PropProcessed", "ProcessedDays", "avgProcessedokaj", "gperokajProcessed", "sumProcessedgdaily",
                "PropRed", "RedDays", "avgRedokaj", "gperokajRed", "sumRedgdaily",
                "PropWhite", "WhiteDays", "avgWhiteokaj", "gperokajWhite", "sumWhitegdaily")
run("sitable3")
for (meat_type in c("PropMeat", "PropProcessed", "PropRed", "PropWhite")) {
  row_index <- which(sitable3$`Meat Type` == meat_type) #for loop to add the mean & SE proportion values
  for (year in 1:(ncol(PropsFormatted) - 1)) {
    survey_year_col <- paste0("Year ", year)
    value_str <- PropsFormatted[PropsFormatted$MeatType == meat_type, survey_year_col]
    sitable3[row_index, survey_year_col] <- value_str
  }
}
table2 <- sitable3[, c("Meat Type", "Year 1", "Year 11", "P for trend")]
rm(ListToFeed, Props, PropsFormatted, weighted_props, i, j, meat_type, meat_types, row_index, value_str, survey_year_col, year)


#########################SI TABLE 4 - STM ANALYSIS########################

ListToFeed <- c("BsumMeatg", "BsumProcessedg", "BsumRedg", "BsumWhiteg",
                "LsumMeatg", "LsumProcessedg", "LsumRedg", "LsumWhiteg",
                "DsumMeatg", "DsumProcessedg", "DsumRedg", "DsumWhiteg")
run("sitable4")
rm(ListToFeed)

##########################SI TABLE 5 - analysis by covariates#######################

# Calculate proportion of participants who are meat consumers for each meat subtype by sex
weighted_props_m <- svyby(~I(sumMeatg > 0) + I(sumProcessedg > 0) + I(sumRedg > 0) + I(sumWhiteg > 0),
                          ~SurveyYear + Sex, 
                          subset(survey_design, Sex == "M"), 
                          svymean, 
                          na.rm = TRUE)

weighted_props_f <- svyby(~I(sumMeatg > 0) + I(sumProcessedg > 0) + I(sumRedg > 0) + I(sumWhiteg > 0),
                          ~SurveyYear + Sex, 
                          subset(survey_design, Sex == "F"), 
                          svymean, 
                          na.rm = TRUE)

# Select only the columns ending with "TRUE" and rename them
cols_to_keep_m <- c("PropMeat_M", "PropProcessed_M", "PropRed_M", "PropWhite_M",
                  "PropMeatSE_M", "PropProcessedSE_M", "PropRedSE_M", "PropWhiteSE_M")
cols_to_keep_f <- c("PropMeat_F", "PropProcessed_F", "PropRed_F", "PropWhite_F",
                    "PropMeatSE_F", "PropProcessedSE_F", "PropRedSE_F", "PropWhiteSE_F")

weighted_props_m <- weighted_props_m %>%
  dplyr::select(ends_with("TRUE")) %>%
  setNames(cols_to_keep_m)

weighted_props_f <- weighted_props_f %>%
  dplyr::select(ends_with("TRUE")) %>%
  setNames(cols_to_keep_f)

# Transpose and prepare formatted output
PropsM <- as.data.frame(t(weighted_props_m))
PropsM <- rownames_to_column(PropsM, var = "MeatType")
PropsM[1:8, -1] <- round(PropsM[1:8, -1] * 100, 2)
PropsFormattedM <- PropsM[1:4, ]
for (i in 1:nrow(PropsFormattedM)) {
  for (j in 2:ncol(PropsFormattedM)) {
    PropsFormattedM[i, j] <- paste0(PropsM[i, j], " (", PropsM[i + 4, j], ")")
  }
}
colnames(PropsFormattedM)[-1] <- paste0("Year ", colnames(PropsFormattedM)[-1])

PropsF <- as.data.frame(t(weighted_props_f))
PropsF <- rownames_to_column(PropsF, var = "MeatType")
PropsF[1:8, -1] <- round(PropsF[1:8, -1] * 100, 2)
PropsFormattedF <- PropsF[1:4, ]
for (i in 1:nrow(PropsFormattedF)) {
  for (j in 2:ncol(PropsFormattedF)) {
    PropsFormattedF[i, j] <- paste0(PropsF[i, j], " (", PropsF[i + 4, j], ")")
  }
}
colnames(PropsFormattedF)[-1] <- paste0("Year ", colnames(PropsFormattedF)[-1])
PropsFormattedM$MeatType <- gsub("_M$", "", PropsFormattedM$MeatType)
PropsFormattedF$MeatType <- gsub("_F$", "", PropsFormattedF$MeatType)

# Combine Male and Female data into one dataset, ensure correct merging
weighted_props_m$SurveyYear <- 1:nrow(weighted_props_m) #add a "survey year" variable (to integrate into main dataset for analysis)
weighted_props_f$SurveyYear <- 1:nrow(weighted_props_f) #add a "survey year" variable (to integrate into main dataset for analysis)
weighted_props <- merge(weighted_props_m, weighted_props_f, by = "SurveyYear", all = TRUE)
dat <- merge(dat, weighted_props, by = "SurveyYear", all.x = TRUE)

# Re-specify the survey design with new proportion variables
dat.design <-
  svydesign(
    id = ~serialh/area,  # multi-level cluster of households and PSUs
    strata = ~astrata5,  # stratification by district
    data = dat,
    weights = ~wti,  # NDNS-assigned survey weight
    nest = TRUE
  )





#modified meanies to include a sex parameter
meaniessex <- function(Xvar, year, dataset, sex, filter_meat = FALSE) {
  #replace Xvar with variable name
  form_var <- as.formula(paste("~", Xvar))
  
  dat_subset <- dataset %>% filter(SurveyYear == year & Sex == sex)
  
  if (!is.null(filter_meat) && filter_meat != FALSE) {
    dat_subset <- dat_subset %>% filter(eval(parse(text = filter_meat)))
  }
  
  #set survey design
  survey_design <- dat_subset %>%
    as_survey_design(ids = serialh/area, weights = wti, strata = astrata5)
  
  #calculate the survey-weighted mean and SE
  Xval <- svymean(form_var, design = survey_design, na.rm = TRUE)
  
  #extract mean and SE, round to one dp
  mean_val <- round_condish(as.numeric(Xval))
  se_val <- round_condish(sqrt(attr(Xval, "var"))) #I feel like there is a simpler way to extract SE,
  #but couldn't figure it out so just pulling it manually
  
  #combine mean & SE into one column
  mean_se_combined <- paste(mean_val, " (", se_val, ")", sep = "")
  
  #create a character list (to store the mean + SE values together)
  year_values <- list()
  year_values[[paste("Year", year)]] <- mean_se_combined
  
  return(year_values)
}


meaniebobeaniessex <- function(var_list, dataset, sex, filter_meat = FALSE) {
  results <- data.frame()
  
  for (var in var_list) {
    combined <- numeric()
    
    for (year in 1:11) {
      year_values <- meaniessex(var, year, dataset, sex, filter_meat)
      combined <- c(combined, year_values)
    }
    
    trans <- data.frame(t(combined))
    colnames(trans) <- paste("Year", 1:11)
    results <- rbind(results, trans)
  }
  
  return(results)
}



pintsex <- function(variable, dat.design) {
  form_var <- as.formula(paste(variable, "~ SurveyYear + Sex + SurveyYear * Sex"))
  
  #if variable ends with 'Days', run Poisson model, if not, run glm model
  if (grepl("Days$", variable)) {
    print(paste("Running Poisson model for:", variable))
    model <- svyglm(form_var, family = poisson(link = "log"), design = dat.design)
  } else {
    print(paste("Running Linear model for:", variable))
    model <- svyglm(form_var, design = dat.design)
  }
  
  #extract p-value for men
  modsum <- summary(model)
  p_men <- modsum$coefficients["SurveyYear", "Pr(>|t|)"]
  p_men_women_diff <- modsum$coefficients["SurveyYear:SexF", "Pr(>|t|)"]
  
  #extract coefficients and covariance matrix (to calculate B1 + B3)
  coefs <- coef(model)
  cov_matrix <- vcov(model)
  
  #calculate effect, variance, and SE for change in women (baseline to Y11)
  effect_women <- coefs["SurveyYear"] + coefs["SurveyYear:SexF"]
  var_women <- cov_matrix["SurveyYear", "SurveyYear"] + 
    cov_matrix["SurveyYear:SexF", "SurveyYear:SexF"] + 
    2 * cov_matrix["SurveyYear", "SurveyYear:SexF"]
  se_women <- sqrt(var_women)
  
  #calculate t-value and p-value for change in women
  t_value <- effect_women / se_women
  p_women <- 2 * (1 - pt(abs(t_value), df = df.residual(model)))
  
  #combine B1, B1+B3, & B3 into a one-row data frame (to merge with mean estimations)
  peezies <- data.frame(Men_P = p_men, Women_P = p_women, Men_Women_Pint = p_men_women_diff)
  
  #round - got weird having 3 columns instead of 1, so had to implement this wee loop
  for (col in colnames(peezies)) {
    peezies[[col]] <- roundies(peezies[[col]])
  }
  
  return(peezies)
}

runsex <- function(analysis) {
  results <- data.frame()
  pvecM <- character()
  pvecW <- character()
  pvecMW <- character()
  
  # Define filter conditions
  filter_conditions <- list(
    "MeatDays" = "sumMeatg > 0",
    "ProcessedDays" = "sumProcessedg > 0",
    "RedDays" = "sumRedg > 0",
    "WhiteDays" = "sumWhiteg > 0",
    "avgMeatokaj" = "sumMeatg > 0",
    "avgProcessedokaj" = "sumProcessedg > 0",
    "avgRedokaj" = "sumRedg > 0",
    "avgWhiteokaj" = "sumWhiteg > 0"
  )
  
  # Loop for each variable in the list
  for (var in ListToFeed) {
    filter_meat <- filter_conditions[[var]]
    
    # Process data for Males
    X <- meaniebobeaniessex(var_list = c(var), dataset = dat, sex = "M", filter_meat = filter_meat)
    pvalM <- pintsex(var, dat.design)$Men_P
    pvecM <- c(pvecM, pvalM)
    
    # Process data for Females
    X1 <- meaniebobeaniessex(var_list = c(var), dataset = dat, sex = "F", filter_meat = filter_meat)
    colnames(X1) <- paste0("F_", colnames(X1))
    pvalW <- pintsex(var, dat.design)$Women_P
    pvecW <- c(pvecW, pvalW)
    
    # Calculate p-value for men-women interaction
    pvalMW <- pintsex(var, dat.design)$Men_Women_Pint
    pvecMW <- c(pvecMW, pvalMW)
    
    # Combine Male and Female data
    combinedX <- cbind(X, X1)
    rownames(combinedX) <- var
    results <- rbind(results, combinedX)
  }
  
  # Assemble results
  results$PvalM <- pvecM
  results$PvalW <- pvecW
  results$PvalMW <- pvecMW
  results <- rownames_to_column(results, var = "Meat Type")
  colnames(results) <- gsub("\\.", " ", colnames(results))
  assign(analysis, results, envir = globalenv())
}
ListToFeed <- c("PropMeat", "MeatDays", "avgMeatokaj", "gperokajMeat", "sumMeatgdaily", 
                "PropProcessed", "ProcessedDays", "avgProcessedokaj", "gperokajProcessed", "sumProcessedgdaily",
                "PropRed", "RedDays", "avgRedokaj", "gperokajRed", "sumRedgdaily",
                "PropWhite", "WhiteDays", "avgWhiteokaj", "gperokajWhite", "sumWhitegdaily")
runsex("sitable5a")

# Loop through each meat type in sitable5a
for (meat_type in unique(sitable5a$`Meat Type`)) {
  # Find the row index for this meat type in sitable5a
  row_index <- which(sitable5a$`Meat Type` == meat_type)
  
  # For each year, update the values for males and females
  for (year in 1:(ncol(PropsFormattedM) - 1)) {  # Assuming both formatted dataframes have the same number of year columns
    male_col <- paste0("Year ", year)
    female_col <- paste0("F_Year ", year)
    
    # Update sitable5a with male and female values
    male_value <- PropsFormattedM[PropsFormattedM$MeatType == meat_type, paste0("Year ", year, ".M")]
    female_value <- PropsFormattedF[PropsFormattedF$MeatType == meat_type, paste0("Year ", year, ".F")]
    
    # Only replace if there's a matching value found
    if (!is.na(male_value) && length(male_value) > 0) {
      sitable5a[row_index, male_col] <- male_value
    }
    if (!is.na(female_value) && length(female_value) > 0) {
      sitable5a[row_index, female_col] <- female_value
    }
  }
}



#ADULTS (18+) vs. CHILDREN (<18)




# Calculate proportion of participants who are meat consumers for each meat subtype by LifeStage
weighted_props_a <- svyby(~I(sumMeatg > 0) + I(sumProcessedg > 0) + I(sumRedg > 0) + I(sumWhiteg > 0),
                          ~SurveyYear + LifeStage, 
                          subset(survey_design, LifeStage == "A"), 
                          svymean, 
                          na.rm = TRUE)

weighted_props_c <- svyby(~I(sumMeatg > 0) + I(sumProcessedg > 0) + I(sumRedg > 0) + I(sumWhiteg > 0),
                          ~SurveyYear + LifeStage, 
                          subset(survey_design, LifeStage == "C"), 
                          svymean, 
                          na.rm = TRUE)

# Select only the columns ending with "TRUE" and rename them
cols_to_keep_a <- c("PropMeat_A", "PropProcessed_A", "PropRed_A", "PropWhite_A",
                    "PropMeatSE_A", "PropProcessedSE_A", "PropRedSE_A", "PropWhiteSE_A")
cols_to_keep_c <- c("PropMeat_C", "PropProcessed_C", "PropRed_C", "PropWhite_C",
                    "PropMeatSE_C", "PropProcessedSE_C", "PropRedSE_C", "PropWhiteSE_C")

weighted_props_a <- weighted_props_a %>%
  dplyr::select(ends_with("TRUE")) %>%
  setNames(cols_to_keep_a)

weighted_props_c <- weighted_props_c %>%
  dplyr::select(ends_with("TRUE")) %>%
  setNames(cols_to_keep_c)

# Transpose and prepare formatted output
PropsA <- as.data.frame(t(weighted_props_a))
PropsA <- rownames_to_column(PropsA, var = "MeatType")
PropsA[1:8, -1] <- round(PropsA[1:8, -1] * 100, 2)
PropsFormattedA <- PropsA[1:4, ]
for (i in 1:nrow(PropsFormattedA)) {
  for (j in 2:ncol(PropsFormattedA)) {
    PropsFormattedA[i, j] <- paste0(PropsA[i, j], " (", PropsA[i + 4, j], ")")
  }
}
colnames(PropsFormattedA)[-1] <- paste0("Year ", colnames(PropsFormattedA)[-1])

PropsC <- as.data.frame(t(weighted_props_c))
PropsC <- rownames_to_column(PropsC, var = "MeatType")
PropsC[1:8, -1] <- round(PropsC[1:8, -1] * 100, 2)
PropsFormattedC <- PropsC[1:4, ]
for (i in 1:nrow(PropsFormattedC)) {
  for (j in 2:ncol(PropsFormattedC)) {
    PropsFormattedC[i, j] <- paste0(PropsC[i, j], " (", PropsC[i + 4, j], ")")
  }
}
colnames(PropsFormattedC)[-1] <- paste0("Year ", colnames(PropsFormattedC)[-1])
PropsFormattedA$MeatType <- gsub("_A$", "", PropsFormattedA$MeatType)
PropsFormattedC$MeatType <- gsub("_C$", "", PropsFormattedC$MeatType)

# Combine Adult and Children data into one dataset, ensure correct merging
weighted_props_a$SurveyYear <- 1:nrow(weighted_props_a) #add a "survey year" variable (to integrate into main dataset for analysis)
weighted_props_c$SurveyYear <- 1:nrow(weighted_props_c) #add a "survey year" variable (to integrate into main dataset for analysis)
weighted_props <- merge(weighted_props_a, weighted_props_c, by = "SurveyYear", all = TRUE)
dat <- merge(dat, weighted_props, by = "SurveyYear", all.x = TRUE)

# Re-specify the survey design with new proportion variables
dat.design <-
  svydesign(
    id = ~serialh/area,  # multi-level cluster of households and PSUs
    strata = ~astrata5,  # stratification by district
    data = dat,
    weights = ~wti,  # NDNS-assigned survey weight
    nest = TRUE
  )




#modified meanies to include a lifestage parameter
meanieslifestage <- function(Xvar, year, dataset, lifestage, filter_meat = FALSE) {
  #replace Xvar with variable name
  form_var <- as.formula(paste("~", Xvar))
  
  dat_subset <- dataset %>% filter(SurveyYear == year & LifeStage == lifestage)
  
  if (!is.null(filter_meat) && filter_meat != FALSE) {
    dat_subset <- dat_subset %>% filter(eval(parse(text = filter_meat)))
  }
  
  #set survey design
  survey_design <- dat_subset %>%
    as_survey_design(ids = serialh/area, weights = wti, strata = astrata5)
  
  #calculate the survey-weighted mean and SE
  Xval <- svymean(form_var, design = survey_design, na.rm = TRUE)
  
  #extract mean and SE, round to one dp
  mean_val <- round_condish(as.numeric(Xval))
  se_val <- round_condish(sqrt(attr(Xval, "var"))) #I feel like there is a simpler way to extract SE,
  #but couldn't figure it out so just pulling it manually
  
  #combine mean & SE into one column
  mean_se_combined <- paste(mean_val, " (", se_val, ")", sep = "")
  
  #create a character list (to store the mean + SE values together)
  year_values <- list()
  year_values[[paste("Year", year)]] <- mean_se_combined
  
  return(year_values)
}
#modified meaniebobeanies to include a lifestage parameter
meaniebobeanieslifestage <- function(var_list, dataset, lifestage, filter_meat = FALSE) {
  results <- data.frame()
  for (var in var_list) {
    combined <- numeric()
    for (year in 1:11) {
      year_values <- meanieslifestage(var, year, dataset, lifestage, filter_meat)
      combined <- c(combined, year_values)
    }
    trans <- data.frame(t(combined))
    results <- rbind(results, trans)
  }
  return(results)
}
pintlifestage <- function(variable, dat.design) {
  form_var <- as.formula(paste(variable, "~ SurveyYear + LifeStage + LifeStage * SurveyYear"))
  
  #if variable ends with 'Days', run Poisson model, if not, run glm model
  if (grepl("Days$", variable)) {
    print(paste("Running Poisson model for:", variable))
    model <- svyglm(form_var, family = poisson(link = "log"), design = dat.design)
  } else {
    print(paste("Running Linear model for:", variable))
    model <- svyglm(form_var, design = dat.design)
  }
  
  #extract p-value for children
  modsum <- summary(model)
  p_adult <- modsum$coefficients["SurveyYear", "Pr(>|t|)"]
  p_adult_child_diff <- modsum$coefficients["SurveyYear:LifeStageC", "Pr(>|t|)"]
  
  #extract coefficients and covariance matrix (to calculate B1 + B3)
  coefs <- coef(model)
  cov_matrix <- vcov(model)
  
  #calculate effect, variance, and SE for change in children (baseline to Y11)
  effect_child <- coefs["SurveyYear"] + coefs["SurveyYear:LifeStageC"]
  var_child <- cov_matrix["SurveyYear", "SurveyYear"] + 
    cov_matrix["SurveyYear:LifeStageC", "SurveyYear:LifeStageC"] + 
    2 * cov_matrix["SurveyYear", "SurveyYear:LifeStageC"]
  se_child <- sqrt(var_child)
  
  #calculate t-value and p-value for change in children
  t_value <- effect_child / se_child
  p_child <- 2 * (1 - pt(abs(t_value), df = df.residual(model)))
  
  #combine B1, B1+B3, & B3 into a one-row data frame (to merge with mean estimations)
  peezies <- data.frame(Adult_P = p_adult, Child_P = p_child, Adult_Child_Pint = p_adult_child_diff)
  
  #round - got weird having 3 columns instead of 1, so had to implement this wee loop
  for (col in colnames(peezies)) {
    peezies[[col]] <- roundies(peezies[[col]])
  }
  
  return(peezies)
}
runlifestage <- function(analysis) {
  #adult
  X <- meaniebobeanieslifestage(ListToFeed, dat, "A")
  pvec <- character()
  
  for (var in ListToFeed) {
    #get p-value for adult
    pvalA <- pintlifestage(var, dat.design)$Adult_P
    pvec <- c(pvec, pvalA)
  }
  
  #add the column for adult p-values
  X$pvalA <- pvec
  
  # Run for children
  X1 <- meaniebobeanieslifestage(ListToFeed, dat, "C")
  colnames(X1) <- paste0("C_", colnames(X1))
  pvec1 <- character()
  
  for (var in ListToFeed) {
    #get p-value for children
    pvalC <- pintlifestage(var, dat.design)$Child_P
    pvec1 <- c(pvec1, pvalC)
  }
  
  #add the column for children p-values
  X1$pvalC <- pvec1
  
  #combine
  X <- cbind(X, X1)
  
  pvec2 <- character()
  for (var in ListToFeed) {
    #get p-value for adult-child interaction
    pvalAC <- pintlifestage(var, dat.design)$Adult_Child_Pint
    pvec2 <- c(pvec2, pvalAC)
  }
  
  #add the column for adult and child difference p-values
  X$PintAC <- pvec2
  
  rownames(X) <- ListToFeed
  X <- rownames_to_column(X, var = "Meat Type")
  colnames(X) <- gsub("\\.", " ", colnames(X))
  #create dataframe with name
  assign(analysis, X, envir = globalenv())
}



runlifestage <- function(analysis) {
  results <- data.frame()
  pvecA <- character()
  pvecC <- character()
  pvecAC <- character()
  
  # Define filter conditions
  filter_conditions <- list(
    "MeatDays" = "sumMeatg > 0",
    "ProcessedDays" = "sumProcessedg > 0",
    "RedDays" = "sumRedg > 0",
    "WhiteDays" = "sumWhiteg > 0",
    "avgMeatokaj" = "sumMeatg > 0",
    "avgProcessedokaj" = "sumProcessedg > 0",
    "avgRedokaj" = "sumRedg > 0",
    "avgWhiteokaj" = "sumWhiteg > 0"
  )
  
  # Loop for each variable in the list
  for (var in ListToFeed) {
    filter_meat <- filter_conditions[[var]]
    
    # Process data for Adults
    X <- meaniebobeanieslifestage(var_list = c(var), dataset = dat, lifestage = "A", filter_meat = filter_meat)
    pvalA <- pintlifestage(var, dat.design)$Adult_P
    pvecA <- c(pvecA, pvalA)
    
    # Process data for Children
    X1 <- meaniebobeanieslifestage(var_list = c(var), dataset = dat, lifestage = "C", filter_meat = filter_meat)
    colnames(X1) <- paste0("C_", colnames(X1))
    pvalC <- pintlifestage(var, dat.design)$Child_P
    pvecC <- c(pvecC, pvalC)
    
    # Calculate p-value for adult-child interaction
    pvalAC <- pintlifestage(var, dat.design)$Adult_Child_Pint
    pvecAC <- c(pvecAC, pvalAC)
    
    # Combine Adult and Child data
    combinedX <- cbind(X, X1)
    rownames(combinedX) <- var
    results <- rbind(results, combinedX)
  }
  
  # Assemble results
  results$PvalA <- pvecA
  results$PvalC <- pvecC
  results$PvalAC <- pvecAC
  results <- rownames_to_column(results, var = "Meat Type")
  colnames(results) <- gsub("\\.", " ", colnames(results))
  assign(analysis, results, envir = globalenv())
}

ListToFeed <- c("PropMeat", "MeatDays", "avgMeatokaj", "gperokajMeat", "sumMeatgdaily", 
                "PropProcessed", "ProcessedDays", "avgProcessedokaj", "gperokajProcessed", "sumProcessedgdaily",
                "PropRed", "RedDays", "avgRedokaj", "gperokajRed", "sumRedgdaily",
                "PropWhite", "WhiteDays", "avgWhiteokaj", "gperokajWhite", "sumWhitegdaily")
runlifestage("sitable5b")
rm(ListToFeed)

# Loop through each meat type in sitable5a
for (meat_type in unique(sitable5b$`Meat Type`)) {
  # Find the row index for this meat type in sitable5a
  row_index <- which(sitable5b$`Meat Type` == meat_type)
  
  # For each year, update the values for adults and children
  for (year in 1:(ncol(PropsFormattedA) - 1)) { 
    adult_col <- paste0("Year ", year)
    child_col <- paste0("C_Year ", year)
    
    # Update sitable5a with male and female values
    adult_value <- PropsFormattedA[PropsFormattedA$MeatType == meat_type, paste0("Year ", year, ".A")]
    child_value <- PropsFormattedC[PropsFormattedC$MeatType == meat_type, paste0("Year ", year, ".C")]
    
    # Only replace if there's a matching value found
    if (!is.na(adult_value) && length(adult_value) > 0) {
      sitable5b[row_index, adult_col] <- adult_value
    }
    if (!is.na(child_value) && length(child_value) > 0) {
      sitable5b[row_index, child_col] <- child_value
    }
  }
}













# Calculate proportion of participants who are meat consumers for each meat subtype by eqv levels
weighted_props_1 <- svyby(~I(sumMeatg > 0) + I(sumProcessedg > 0) + I(sumRedg > 0) + I(sumWhiteg > 0),
                          ~SurveyYear + eqv, 
                          subset(survey_design, eqv == "1"), 
                          svymean, 
                          na.rm = TRUE)

weighted_props_2 <- svyby(~I(sumMeatg > 0) + I(sumProcessedg > 0) + I(sumRedg > 0) + I(sumWhiteg > 0),
                          ~SurveyYear + eqv, 
                          subset(survey_design, eqv == "2"), 
                          svymean, 
                          na.rm = TRUE)

weighted_props_3 <- svyby(~I(sumMeatg > 0) + I(sumProcessedg > 0) + I(sumRedg > 0) + I(sumWhiteg > 0),
                          ~SurveyYear + eqv, 
                          subset(survey_design, eqv == "3"), 
                          svymean, 
                          na.rm = TRUE)

# Select only the columns ending with "TRUE" and rename them
cols_to_keep_1 <- c("PropMeat_1", "PropProcessed_1", "PropRed_1", "PropWhite_1",
                    "PropMeatSE_1", "PropProcessedSE_1", "PropRedSE_1", "PropWhiteSE_1")
cols_to_keep_2 <- c("PropMeat_2", "PropProcessed_2", "PropRed_2", "PropWhite_2",
                    "PropMeatSE_2", "PropProcessedSE_2", "PropRedSE_2", "PropWhiteSE_2")
cols_to_keep_3 <- c("PropMeat_3", "PropProcessed_3", "PropRed_3", "PropWhite_3",
                    "PropMeatSE_3", "PropProcessedSE_3", "PropRedSE_3", "PropWhiteSE_3")

weighted_props_1 <- weighted_props_1 %>%
  dplyr::select(ends_with("TRUE")) %>%
  setNames(cols_to_keep_1)

weighted_props_2 <- weighted_props_2 %>%
  dplyr::select(ends_with("TRUE")) %>%
  setNames(cols_to_keep_2)

weighted_props_3 <- weighted_props_3 %>%
  dplyr::select(ends_with("TRUE")) %>%
  setNames(cols_to_keep_3)

# Combine data for all eqv levels
weighted_props_1$SurveyYear <- 1:nrow(weighted_props_1) #add a "survey year" variable
weighted_props_2$SurveyYear <- 1:nrow(weighted_props_2) #add a "survey year" variable
weighted_props_3$SurveyYear <- 1:nrow(weighted_props_3) #add a "survey year" variable

weighted_props <- Reduce(function(x, y) merge(x, y, by = "SurveyYear", all = TRUE), list(weighted_props_1, weighted_props_2, weighted_props_3))
dat <- merge(dat, weighted_props, by = "SurveyYear", all.x = TRUE)

# Re-specify the survey design with new proportion variables
dat.design <-
  svydesign(
    id = ~serialh/area,  # multi-level cluster of households and PSUs
    strata = ~astrata5,  # stratification by district
    data = dat,
    weights = ~wti,  # NDNS-assigned survey weight
    nest = TRUE
  )

# Transpose and prepare formatted output for eqv levels
Props1 <- as.data.frame(t(weighted_props_1))
Props1 <- rownames_to_column(Props1, var = "MeatType")
Props1[1:8, -1] <- round(Props1[1:8, -1] * 100, 2)
PropsFormatted1 <- Props1[1:4, ]
for (i in 1:nrow(PropsFormatted1)) {
  for (j in 2:ncol(PropsFormatted1)) {
    PropsFormatted1[i, j] <- paste0(Props1[i, j], " (", Props1[i + 4, j], ")")
  }
}
colnames(PropsFormatted1)[-1] <- paste0("Year ", colnames(PropsFormatted1)[-1])
PropsFormatted1$MeatType <- gsub("_1$", "", PropsFormatted1$MeatType)

Props2 <- as.data.frame(t(weighted_props_2))
Props2 <- rownames_to_column(Props2, var = "MeatType")
Props2[1:8, -1] <- round(Props2[1:8, -1] * 100, 2)
PropsFormatted2 <- Props2[1:4, ]
for (i in 1:nrow(PropsFormatted2)) {
  for (j in 2:ncol(PropsFormatted2)) {
    PropsFormatted2[i, j] <- paste0(Props2[i, j], " (", Props2[i + 4, j], ")")
  }
}
colnames(PropsFormatted2)[-1] <- paste0("Year ", colnames(PropsFormatted2)[-1])
PropsFormatted2$MeatType <- gsub("_2$", "", PropsFormatted2$MeatType)

Props3 <- as.data.frame(t(weighted_props_3))
Props3 <- rownames_to_column(Props3, var = "MeatType")
Props3[1:8, -1] <- round(Props3[1:8, -1] * 100, 2)
PropsFormatted3 <- Props3[1:4, ]
for (i in 1:nrow(PropsFormatted3)) {
  for (j in 2:ncol(PropsFormatted3)) {
    PropsFormatted3[i, j] <- paste0(Props3[i, j], " (", Props3[i + 4, j], ")")
  }
}
colnames(PropsFormatted3)[-1] <- paste0("Year ", colnames(PropsFormatted3)[-1])
PropsFormatted3$MeatType <- gsub("_3$", "", PropsFormatted3$MeatType)

# Combine the formatted data into one dataset
PropsFormatted <- Reduce(function(x, y) merge(x, y, by = "MeatType", all = TRUE), list(PropsFormatted1, PropsFormatted2, PropsFormatted3))



#modified meanies to include an eqv parameter
meanieseqv <- function(Xvar, year, dataset, schmoney, filter_meat = FALSE) {
  #replace Xvar with variable name
  form_var <- as.formula(paste("~", Xvar))
  
  dat_subset <- dataset %>% filter(SurveyYear == year & eqv == schmoney)
 
  if (!is.null(filter_meat) && filter_meat != FALSE) {
    dat_subset <- dat_subset %>% filter(eval(parse(text = filter_meat)))
  }
  
  #set survey design
  survey_design <- dat_subset %>%
    as_survey_design(ids = serialh/area, weights = wti, strata = astrata5)
  
  #calculate the survey-weighted mean and SE
  Xval <- svymean(form_var, design = survey_design, na.rm = TRUE)
  
  #extract mean and SE, round to one dp
  mean_val <- round_condish(as.numeric(Xval))
  se_val <- round_condish(sqrt(attr(Xval, "var"))) #I feel like there is a simpler way to extract SE,
  #but couldn't figure it out so just pulling it manually
  
  #combine mean & SE into one column
  mean_se_combined <- paste(mean_val, " (", se_val, ")", sep = "")
  
  #create a character list (to store the mean + SE values together)
  year_values <- list()
  year_values[[paste("Year", year)]] <- mean_se_combined
  
  return(year_values)
}
#modified meaniebobeanies to include an eqv parameter
meaniebobeanieseqv <- function(var_list, dataset, schmoney, filter_meat = FALSE) {
  results <- data.frame()
  for (var in var_list) {
    combined <- numeric()
    for (year in 1:11) {
      year_values <- meanieseqv(var, year, dataset, schmoney, filter_meat)
      combined <- c(combined, year_values)
    }
    trans <- data.frame(t(combined))
    results <- rbind(results, trans)
  }
  return(results)
}

pinteqv <- function(variable, dat.design) {
  form_var <- as.formula(paste(variable, "~ SurveyYear + eqv + SurveyYear * eqv"))
  #if variable ends with 'Days', run Poisson model, if not, run glm model
  if (grepl("Days$", variable)) {
    print(paste("Running Poisson model for:", variable))
    model <- svyglm(form_var, family = poisson(link = "log"), design = dat.design)
  } else {
    print(paste("Running Linear model for:", variable))
    model <- svyglm(form_var, design = dat.design)
  }
  #extract p-value for 1st tertile (lowest) & interactiosn with other tertiles
  modsum <- summary(model)
  p_1 <- modsum$coefficients["SurveyYear", "Pr(>|t|)"]
  p_1_2diff <- modsum$coefficients["SurveyYear:eqv2", "Pr(>|t|)"]
  p_1_3diff <- modsum$coefficients["SurveyYear:eqv3", "Pr(>|t|)"]
  #extract coefficients and covariance matrix (to calculate pvals for missing pvalues from summary)
  coefs <- coef(model)
  cov_matrix <- vcov(model)
  
  eqv_groups <- c("eqv2", "eqv3")
  
  p_eqv_list <- list()
  
  for (eqv_group in eqv_groups) {
    effect_eqv <- coefs["SurveyYear"] + coefs[paste0("SurveyYear:", eqv_group)]
    var_eqv <- cov_matrix["SurveyYear", "SurveyYear"] + 
      cov_matrix[paste0("SurveyYear:", eqv_group), paste0("SurveyYear:", eqv_group)] + 
      2 * cov_matrix["SurveyYear", paste0("SurveyYear:", eqv_group)]
    se_eqv <- sqrt(var_eqv)
    t_value <- effect_eqv / se_eqv
    p_eqvX <- 2 * (1 - pt(abs(t_value), df = df.residual(model)))
    p_eqv_list[[eqv_group]] <- p_eqvX
  }
  
  peezies <- data.frame(p_1 = p_1, p_1_2diff = p_1_2diff, p_1_3diff = p_1_3diff,
                        p_2 = p_eqv_list$eqv2, p_3 = p_eqv_list$eqv3)
  
  for (col in colnames(peezies)) {
    peezies[[col]] <- roundies(peezies[[col]])
  }
  
  return(peezies)
}
runeqv <- function(analysis) {
  
  X <- list()
  
  eqv_groups <- list("1", "2", "3")
  pval_cols <- c("p_1", "p_2", "p_3")
  pval_diffs <- c(NULL, "p_1_2diff", "p_1_3diff")
  
  for (i in seq_along(eqv_groups)) {
    eqv_group <- eqv_groups[[i]]
    
    X_temp <- meaniebobeanieseqv(ListToFeed, dat, eqv_group) #add means
    
    names(X_temp)[names(X_temp) == "Year.1"] <- paste0("Year.1_eqv", eqv_group) #add eqv group to col 1
    
    pvec <- character()
    
    for (var in ListToFeed) {#Get p-values and p-interaction values
      pval <- pinteqv(var, dat.design)[[pval_cols[i]]]
      pvec <- c(pvec, pval)
    }
    
    colname <- paste0("pval", sub("^1$", "1", eqv_group))
    X_temp[[colname]] <- pvec
    
    colnames(X_temp)[-1] <- paste0(colnames(X_temp)[-1], "_eqv", eqv_group) #rename columns
    
    X[[eqv_group]] <- X_temp
    
    
    if (!is.null(pval_diffs[i]) && !is.na(pval_diffs[i])) {
      pvec_diff <- character()
      for (var in ListToFeed) {
        pval_diff <- pinteqv(var, dat.design)[[pval_diffs[i]]]
        pvec_diff <- c(pvec_diff, pval_diff)
      }
      
      #if pvec_diff is empty, assign NA to the column (idk, this just helped it run)
      if (length(pvec_diff) == 0) {
        pvec_diff <- rep(NA, nrow(X[[eqv_group]]))
      }
      
      X[[eqv_group]][[pval_diffs[i]]] <- pvec_diff
    }
  }
  #combine 
  result <- Reduce(function(x, y) cbind(x, y), X)
  rownames(result) <- ListToFeed
  result <- rownames_to_column(result, var = "Meat Type")
  colnames(result) <- gsub("\\.", "_", colnames(result))
  #define column order (couldn't think of an 'automated' way to do this, so i just listed it in the order i wanted haha)
  ordered_cols <- c("Meat Type", 
                    "Year_1_eqv1",  "Year_2_eqv1",  "Year_3_eqv1",  "Year_4_eqv1",  "Year_5_eqv1",  "Year_6_eqv1", 
                    "Year_7_eqv1",  "Year_8_eqv1",  "Year_9_eqv1",  "Year_10_eqv1", "Year_11_eqv1", "pval1_eqv1", 
                    "Year_1_eqv2",  "Year_2_eqv2",  "Year_3_eqv2",  "Year_4_eqv2",  "Year_5_eqv2",  "Year_6_eqv2",  
                    "Year_7_eqv2",  "Year_8_eqv2",  "Year_9_eqv2",  "Year_10_eqv2", "Year_11_eqv2", "pval2_eqv2", "p_1_2diff", 
                    "Year_1_eqv3",  "Year_2_eqv3",  "Year_3_eqv3",  "Year_4_eqv3",  "Year_5_eqv3",  "Year_6_eqv3",  
                    "Year_7_eqv3",  "Year_8_eqv3",  "Year_9_eqv3",  "Year_10_eqv3", "Year_11_eqv3", "pval3_eqv3", "p_1_3diff")
  
  result <- result[, ordered_cols]
  return(result)
}
runeqv <- function(analysis) {
  
  # Define filter conditions
  filter_conditions <- list(
    "MeatDays" = "sumMeatg > 0",
    "ProcessedDays" = "sumProcessedg > 0",
    "RedDays" = "sumRedg > 0",
    "WhiteDays" = "sumWhiteg > 0",
    "avgMeatokaj" = "sumMeatg > 0",
    "avgProcessedokaj" = "sumProcessedg > 0",
    "avgRedokaj" = "sumRedg > 0",
    "avgWhiteokaj" = "sumWhiteg > 0"
  )
  
  X <- list()
  
  eqv_groups <- list("1", "2", "3")
  pval_cols <- c("p_1", "p_2", "p_3")
  pval_diffs <- c(NULL, "p_1_2diff", "p_1_3diff")
  
  for (i in seq_along(eqv_groups)) {
    eqv_group <- eqv_groups[[i]]
    
    X_temp <- list()
    
    for (var in ListToFeed) {
      filter_meat <- ifelse(var %in% names(filter_conditions), filter_conditions[[var]], FALSE)
      
      temp_result <- meaniebobeanieseqv(var_list = c(var), dataset = dat, schmoney = eqv_group, filter_meat = filter_meat) # add means
      temp_result <- as.data.frame(temp_result)
      
      names(temp_result)[names(temp_result) == "Year.1"] <- paste0("Year.1_eqv", eqv_group) # add eqv group to col 1
      
      pval <- pinteqv(var, dat.design)[[pval_cols[i]]]
      temp_result[[paste0("pval", eqv_group)]] <- pval
      
      if (!is.null(pval_diffs[i]) && !is.na(pval_diffs[i])) {
        pval_diff <- pinteqv(var, dat.design)[[pval_diffs[i]]]
        temp_result[[pval_diffs[i]]] <- pval_diff
      }
      
      X_temp[[var]] <- temp_result
    }
    
    X_temp <- do.call(rbind, X_temp)
    
    colnames(X_temp)[-1] <- paste0(colnames(X_temp)[-1], "_eqv", eqv_group) # rename columns
    
    # Ensure pval_diffs columns do not have eqv_group suffixes
    for (pval_diff in pval_diffs) {
      if (!is.null(pval_diff) && !is.na(pval_diff)) {
        colnames(X_temp)[colnames(X_temp) == paste0(pval_diff, "_eqv", eqv_group)] <- pval_diff
      }
    }
    
    X[[eqv_group]] <- X_temp
  }
  
  # Combine the data frames
  result <- Reduce(function(x, y) cbind(x, y), X)
  rownames(result) <- ListToFeed
  result <- rownames_to_column(result, var = "Meat Type")
  colnames(result) <- gsub("\\.", "_", colnames(result))
  
  # Check columns present in result
  print("Columns in result before ordering:")
  print(colnames(result))
  
  # Define column order
  ordered_cols <- c("Meat Type", 
                    "Year_1_eqv1",  "Year_2_eqv1",  "Year_3_eqv1",  "Year_4_eqv1",  "Year_5_eqv1",  "Year_6_eqv1", 
                    "Year_7_eqv1",  "Year_8_eqv1",  "Year_9_eqv1",  "Year_10_eqv1", "Year_11_eqv1", "pval1_eqv1", 
                    "Year_1_eqv2",  "Year_2_eqv2",  "Year_3_eqv2",  "Year_4_eqv2",  "Year_5_eqv2",  "Year_6_eqv2",  
                    "Year_7_eqv2",  "Year_8_eqv2",  "Year_9_eqv2",  "Year_10_eqv2", "Year_11_eqv2", "pval2_eqv2", "p_1_2diff", 
                    "Year_1_eqv3",  "Year_2_eqv3",  "Year_3_eqv3",  "Year_4_eqv3",  "Year_5_eqv3",  "Year_6_eqv3",  
                    "Year_7_eqv3",  "Year_8_eqv3",  "Year_9_eqv3",  "Year_10_eqv3", "Year_11_eqv3", "pval3_eqv3", "p_1_3diff")
  
  # Check for missing columns
  missing_cols <- setdiff(ordered_cols, colnames(result))
  if (length(missing_cols) > 0) {
    warning("Missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  result <- result[, ordered_cols, drop = FALSE]
  return(result)
}

ListToFeed <- c("PropMeat", "MeatDays", "avgMeatokaj", "gperokajMeat", "sumMeatgdaily", 
                "PropProcessed", "ProcessedDays", "avgProcessedokaj", "gperokajProcessed", "sumProcessedgdaily",
                "PropRed", "RedDays", "avgRedokaj", "gperokajRed", "sumRedgdaily",
                "PropWhite", "WhiteDays", "avgWhiteokaj", "gperokajWhite", "sumWhitegdaily")
sitable5c <- runeqv("sitable5c")



# Get the column names of the dataframe
column_names <- colnames(sitable5c)

# Iterate through each column name
for (i in 1:length(column_names)) {
  # Remove the underscore between "Year" and the number
  column_names[i] <- gsub("Year_", "Year ", column_names[i])
  # Check if the column name ends with "_eqv1"
  if (grepl("_eqv1$", column_names[i])) {
    # Remove the "_eqv1" part
    column_names[i] <- sub("_eqv1$", "", column_names[i])
  }
}

# Assign the updated column names
colnames(sitable5c) <- column_names


# Loop through each meat type in sitable5a
for (meat_type in unique(sitable5c$`Meat Type`)) {
  # Find the row index for this meat type in sitable5a
  row_index <- which(sitable5c$`Meat Type` == meat_type)
  
  # For each year, update the values for adults and children
  for (year in 1:(ncol(PropsFormatted1) - 1)) { 
    one_col <- paste0("Year ", year)
    two_col <- paste0("Year ", year, "_eqv2")
    three_col <- paste0("Year ", year, "_eqv3")
    
    
    # Update sitable5a with male and female values
    one_value <- PropsFormatted1[PropsFormatted1$MeatType == meat_type, paste0("Year ", year, ".1")]
    two_value <- PropsFormatted2[PropsFormatted2$MeatType == meat_type, paste0("Year ", year, ".2")]
    three_value <- PropsFormatted3[PropsFormatted3$MeatType == meat_type, paste0("Year ", year, ".3")]
    
    # Only replace if there's a matching value found
    if (!is.na(one_value) && length(one_value) > 0) {
      sitable5c[row_index, one_col] <- one_value
    }
    if (!is.na(two_value) && length(two_value) > 0) {
      sitable5c[row_index, two_col] <- two_value
    }
    if (!is.na(three_value) && length(three_value) > 0) {
      sitable5c[row_index, three_col] <- three_value
    }
  }
}








######################ST TABLE 6 - decomp values#########################

#function for mean statistics and decomposition analysis
decompsies <- function(data, survey_design, meat_type) {
  years <- c(1, 11)  #years to decompose betweeen
  results <- list()  #list to store results
  for (year in years) {
    #behaviour suffixes for respective variables
    suffix_days <- paste0(meat_type, "Days")
    suffix_okaj <- paste0("tot", meat_type, "okaj")
    suffix_meatg <- paste0("sum", meat_type, "g")
    #subset data by year
    subz <- survey_design %>% filter(SurveyYear == year)
    datsubz <- data %>% filter(SurveyYear == year)
    #total population (sum of weights)
    totPop <- as.numeric(sum(datsubz$wti))
    #meat eating population (sum of weights)
    Z <- datsubz %>% filter(!!sym(suffix_meatg) > 0)
    meatPop <- as.numeric(sum(Z$wti))
    #total number of days in which meat was eaten (across whole population)
    totDays <- as.numeric(svytotal(reformulate(suffix_days), subz))
    #total number of occasions in which meat was eaten (across whole population)
    totOkaj <- as.numeric(svytotal(reformulate(suffix_okaj), subz))
    #total mass of meat consumed (across all days, whole population)
    totMeat <- as.numeric(svytotal(reformulate(suffix_meatg), subz))
    results[[paste0("r", year)]] <- meatPop / totPop #proportion of population eating meat
    results[[paste0("d", year)]] <- totDays / meatPop #average number of days meat was eaten
    results[[paste0("o", year)]] <- totOkaj / meatPop / (totDays / meatPop) #average number of occasions meat was eaten (per meat-eating day)
    results[[paste0("p", year)]] <- totMeat / totOkaj #average portion size (per meat meal)
    results[[paste0("c", year)]] <- totMeat / totPop #average meat consumption per person
    results[[paste0("per_cap_day", year)]] <- (totMeat / totPop) / 4 #average meat consumption per person per day
  }
  #decomposition
  cdelta <- results$c11 - results$c1 #change in meat consumption
  cdeltaday <- cdelta / 4 #change in meat consumption per day
  rm <- ((cdelta) / (log(results$c11) - log(results$c1))) * log(results$r11 / results$r1)
  dm <- ((cdelta) / (log(results$c11) - log(results$c1))) * log(results$d11 / results$d1)
  om <- ((cdelta) / (log(results$c11) - log(results$c1))) * log(results$o11 / results$o1)
  pm <- ((cdelta) / (log(results$c11) - log(results$c1))) * log(results$p11 / results$p1)
  rm1 <- rm / 4 #change in proportion of population eating meat per day
  dm1 <- dm / 4 #change in average number of days meat was eaten per day
  om1 <- om / 4 #change in average number of occasions meat was eaten per day
  pm1 <- pm / 4 #change in average portion size per day
  results$decomposition <- data.frame( #store results in a data frame
    consumers = rm1,
    days = dm1,
    occasions = om1,
    portion_size = pm1,
    total_change_per_day = cdeltaday
  )
  results #return results
}
results <- list() #list to store the results for each meat type and category level
#define meat types and population subgroups
meattypes <- c("Meat", "Processed", "Red", "White")
sexes <- c("M", "F")
lifestages <- c("A", "C")
eqvs <- c("1", "2", "3")
#function for analysis (main and subgroup)
run_analysis <- function(data, survey_design, meat_type, subgroup = NULL, level = NULL) {
  if (!is.null(subgroup) && !is.null(level)) {
    subset_data <- data %>% filter(!!sym(subgroup) == level)
    subset_survey_design <- survey_design %>% filter(!!sym(subgroup) == level)
  } else {
    subset_data <- data
    subset_survey_design <- survey_design
  }
  decompsies(subset_data, subset_survey_design, meat_type)
}
#loop through all analyses
for (meat in meattypes) {
  key <- paste0(meat, "_Main")
  results[[key]] <- run_analysis(dat, survey_design, meat)
  for (subvar in c("Sex", "LifeStage", "eqv")) {
    levels <- switch(subvar,
                     "Sex" = sexes,
                     "LifeStage" = lifestages,
                     "eqv" = eqvs)
    for (varlvl in levels) {
      key <- paste0(meat, "_", subvar, "_", varlvl)
      results[[key]] <- run_analysis(dat, survey_design, meat, subvar, varlvl)
    }
  }
}
#extract and organize results into a data frame
extraction <- function(results) {
  df_list <- list() #list to store data frames
  for (key in names(results)) {
    decomposition <- results[[key]]$decomposition
    rowname <- key
    decomposition$rowname <- rowname
    df_list[[key]] <- decomposition
  }
  df <- do.call(rbind, df_list) #collapse dfs into one
  #reorder and rename columns for better readability
  df <- df[, c("rowname", "total_change_per_day", "consumers", "days", "occasions", "portion_size")]
  colnames(df) <- c("MeatType", "Total Change", "Consumers", "Days", "Occasions", "Portion Size")
  #order the rows based on population groups
  order <- c(
    paste0(meattypes, "_Main"),
    unlist(lapply(sexes, function(sex) paste0(meattypes, "_Sex_", sex))),
    unlist(lapply(lifestages, function(stage) paste0(meattypes, "_LifeStage_", stage))),
    unlist(lapply(eqvs, function(eqv) paste0(meattypes, "_eqv_", eqv)))
  )
  df$MeatType <- factor(df$MeatType, levels = order)
  df <- df[order(df$MeatType), ]
  return(df)
}
sitable6 <- extraction(results)
rm(results, eqvs, key, levels, lifestages, meat, meattypes, sexes, subvar, varlvl)











########################EXPORT TABLES TO EXCEL#####################
omegatable <- list("Table 1" = table1, "Table 2" = table2, 
                   "SI Table 1" = sitable1, "SI Table 2" = sitable2, 
                   "SI Table 3" = sitable3, "SI Table 4" = sitable4, 
                   "SI Table 5a" = sitable5a, "SI Table 5b" = sitable5b,
                   "SI Table 5c" = sitable5c, "SI Table 6" = sitable6)
wb <- createWorkbook() #create excel workbook
for (sheet_name in names(omegatable)) {
  addWorksheet(wb, sheetName = sheet_name)
  writeData(wb, sheet = sheet_name, x = omegatable[[sheet_name]])
}
filename <- "/Users/alexandervonderschmidt/Library/CloudStorage/OneDrive-SharedLibraries-UniversityofEdinburgh/NDNS Meat Trends - General/Results/omegatable.xlsx"
saveWorkbook(wb, file = filename, overwrite = TRUE)








###################FIGURES##########################

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
#set survey designs
dat$SurveyYear <- as.factor(dat$SurveyYear)
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

#function for Survey Year x axis labels
custom_x_labels <- function(x) {
  labels <- ifelse(x == 1, "2008/09", sprintf("'%02d/'%02d", x + 7, (x + 7) %% 100 + 1))
  return(labels)
}
#line plot of meat trends
#proportion of population that are consumers
m2 <- svyglm(PropProcessed ~ SurveyYear, dat.design)
m3 <- svyglm(PropRed ~ SurveyYear, dat.design)
m4 <- svyglm(PropWhite ~ SurveyYear, dat.design)
survey_years <- unique(dat.design$variables$SurveyYear)
predictions <- data.frame(
  SurveyYear = rep(survey_years, 3),
  Category = factor(rep(c("PropProcessed", "PropRed", "PropWhite"), each = length(survey_years))),
  PredictedConsumers = c(predict(m2, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m3, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m4, newdata = data.frame(SurveyYear = survey_years), type = "response"))
)
#create a custom color palette using colorblind friendly colors
color_palette <- c("#E69F00", "#D55E00", "#0072B2") #order: processed (orange), red (red), white (blue)
#correct order
predictions$Category <- factor(predictions$Category, levels = c("PropProcessed", "PropRed", "PropWhite"))
#category names
levels(predictions$Category) <- c("Processed", "Red", "White")
#convert SurveyYear to numeric
predictions$SurveyYear <- as.numeric(as.character(predictions$SurveyYear))
#create plot
plot0 <- ggplot(predictions, aes(x = SurveyYear, y = PredictedConsumers, color = Category, group = Category)) +
  geom_point(size = 1) +
  geom_line(aes(linetype = "solid")) +
  geom_smooth(method = "glm", formula = 'y ~ x', se = FALSE, aes(linetype = "dotted", group = Category)) +
  scale_color_manual(values = color_palette) +
  scale_linetype_manual(name = "Line type",
                        values = c("solid" = "solid", "dotted" = "dotted"),
                        labels = c("solid" = "Actual data", "dotted" = "2008/09-2018/19 trend")) +
  labs(x = "Survey year", y = "Proportion of the population consuming meat (%)", color = "Meat category") +
  scale_x_continuous(breaks = predictions$SurveyYear, labels = custom_x_labels) +
  theme_classic() +
  theme(text = element_text(family = "Times", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(linetype = guide_legend(override.aes = list(color = "black")))
print(plot0)











# Adjust weights for ProcessedDays
dat$ProcessedWeight <- ifelse(dat$sumProcessedg > 0, dat$wti, 0)
dat.design_processed_days <- svydesign(
  id = ~area,
  strata = ~astrata5,
  data = dat,
  weights = ~ProcessedWeight,
  fpc = ~fpc
)
m2_days <- svyglm(ProcessedDays ~ SurveyYear, family = poisson(link = "log"), dat.design_processed_days)

# Adjust weights for RedDays
dat$RedWeight <- ifelse(dat$sumRedg > 0, dat$wti, 0)
dat.design_red_days <- svydesign(
  id = ~area,
  strata = ~astrata5,
  data = dat,
  weights = ~RedWeight,
  fpc = ~fpc
)
m3_days <- svyglm(RedDays ~ SurveyYear, family = poisson(link = "log"), dat.design_red_days)

# Adjust weights for WhiteDays
dat$WhiteWeight <- ifelse(dat$sumWhiteg > 0, dat$wti, 0)
dat.design_white_days <- svydesign(
  id = ~area,
  strata = ~astrata5,
  data = dat,
  weights = ~WhiteWeight,
  fpc = ~fpc
)
m4_days <- svyglm(WhiteDays ~ SurveyYear, family = poisson(link = "log"), dat.design_white_days)

# Fitted values for each model
survey_years <- unique(dat.design$variables$SurveyYear)
predictions_days <- data.frame(
  SurveyYear = rep(survey_years, 3),
  Category = factor(rep(c("ProcessedDays", "RedDays", "WhiteDays"), each = length(survey_years))),
  PredictedDays = c(predict(m2_days, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m3_days, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m4_days, newdata = data.frame(SurveyYear = survey_years), type = "response"))
)
#create a custom color palette using colorblind friendly colors
color_palette <- c("#E69F00", "#0072B2", "#D55E00")
#correct order
predictions_days$Category <- factor(predictions_days$Category, levels = c("ProcessedDays", "WhiteDays", "RedDays"))
#category names
levels(predictions_days$Category) <- c("Processed", "White", "Red")
#convert SurveyYear to numeric
predictions_days$SurveyYear <- as.numeric(as.character(predictions_days$SurveyYear))
#create plot
plot1 <- ggplot(predictions_days, aes(x = SurveyYear, y = PredictedDays, color = Category, group = Category)) +
  geom_point(size = 1) +
  geom_line(aes(linetype = "solid")) +
  geom_smooth(method = "glm", formula = 'y ~ x', se = FALSE, aes(linetype = "dotted", group = Category)) +
  scale_color_manual(values = color_palette) +
  scale_linetype_manual(name = "Line type",
                        values = c("solid" = "solid", "dotted" = "dotted"),
                        labels = c("solid" = "Actual data", "dotted" = "2008/09-2018/19 trend")) +
  labs(x = "Survey year", y = "Number of meat days/4-day period", color = "Meat category") +
  scale_x_continuous(breaks = predictions_days$SurveyYear, labels = custom_x_labels) +
  theme_classic() +
  theme(text = element_text(family = "Times", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(linetype = guide_legend(override.aes = list(color = "black")))
print(plot1)







# 
# 
# #Days
# m2 <- svyglm(ProcessedDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
# m3 <- svyglm(RedDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
# m4 <- svyglm(WhiteDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
# #fitted values for each model
# survey_years <- unique(dat.design$variables$SurveyYear)
# predictions <- data.frame(
#   SurveyYear = rep(survey_years, 3),
#   Category = factor(rep(c("ProcessedDays", "RedDays", "WhiteDays"), each = length(survey_years))),
#   PredictedDays = c(predict(m2, newdata = data.frame(SurveyYear = survey_years), type = "response"),
#                     predict(m3, newdata = data.frame(SurveyYear = survey_years), type = "response"),
#                     predict(m4, newdata = data.frame(SurveyYear = survey_years), type = "response"))
# )
# #create a custom color palette using colorblind friendly colors
# color_palette <- c("#E69F00", "#0072B2", "#D55E00") #order: processed (orange), white (blue), red (red)
# #correct order
# predictions$Category <- factor(predictions$Category, levels = c("ProcessedDays", "WhiteDays", "RedDays"))
# #category names
# levels(predictions$Category) <- c("Processed", "White", "Red")
# #convert SurveyYear to numeric
# predictions$SurveyYear <- as.numeric(as.character(predictions$SurveyYear))
# #create plot
# plot1 <- ggplot(predictions, aes(x = SurveyYear, y = PredictedDays, color = Category, group = Category)) +
#   geom_point(size = 1) +
#   geom_line(aes(linetype = "solid")) +
#   geom_smooth(method = "glm", formula = 'y ~ x', se = FALSE, aes(linetype = "dotted", group = Category)) +
#   scale_color_manual(values = color_palette) +
#   scale_linetype_manual(name = "Line type",
#                         values = c("solid" = "solid", "dotted" = "dotted"),
#                         labels = c("solid" = "Actual data", "dotted" = "2008/09-2018/19 trend")) +
#   labs(x = "Survey year", y = "Number of meat days/4-day period", color = "Meat category") +
#   scale_x_continuous(breaks = predictions$SurveyYear, labels = custom_x_labels) +
#   theme_classic() +
#   theme(text = element_text(family = "Times", size = 12),
#         axis.text.x = element_text(angle = 45, hjust = 1)) +
#   guides(linetype = guide_legend(override.aes = list(color = "black")))
# print(plot1)










# Adjust weights for avgProcessedokaj
dat$ProcessedOccasionsWeight <- ifelse(dat$sumProcessedg > 0, dat$wti, 0)
dat.design_processed_occasions <- svydesign(
  id = ~area,
  strata = ~astrata5,
  data = dat,
  weights = ~ProcessedOccasionsWeight,
  fpc = ~fpc
)
m2_occasions <- svyglm(avgProcessedokaj ~ SurveyYear, dat.design_processed_occasions)

# Adjust weights for avgRedokaj
dat$RedOccasionsWeight <- ifelse(dat$sumRedg > 0, dat$wti, 0)
dat.design_red_occasions <- svydesign(
  id = ~area,
  strata = ~astrata5,
  data = dat,
  weights = ~RedOccasionsWeight,
  fpc = ~fpc
)
m3_occasions <- svyglm(avgRedokaj ~ SurveyYear, dat.design_red_occasions)

# Adjust weights for avgWhiteokaj
dat$WhiteOccasionsWeight <- ifelse(dat$sumWhiteg > 0, dat$wti, 0)
dat.design_white_occasions <- svydesign(
  id = ~area,
  strata = ~astrata5,
  data = dat,
  weights = ~WhiteOccasionsWeight,
  fpc = ~fpc
)
m4_occasions <- svyglm(avgWhiteokaj ~ SurveyYear, dat.design_white_occasions)

# Fitted values for each model
survey_years <- unique(dat.design$variables$SurveyYear)
predictions_occasions <- data.frame(
  SurveyYear = rep(survey_years, 3),
  Category = factor(rep(c("avgProcessedokaj", "avgRedokaj", "avgWhiteokaj"), each = length(survey_years))),
  PredictedOccasions = c(predict(m2_occasions, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                         predict(m3_occasions, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                         predict(m4_occasions, newdata = data.frame(SurveyYear = survey_years), type = "response"))
)
#colorblind friendly colors
color_palette <- c("#E69F00", "#0072B2", "#D55E00") #order: processed (orange), white (blue), red (red)
#correct order
predictions_occasions$Category <- factor(predictions_occasions$Category, levels = c("avgProcessedokaj", "avgWhiteokaj", "avgRedokaj"))
#names for xlab
levels(predictions_occasions$Category) <- c("Processed", "White", "Red")
#convert SurveyYear to numeric
predictions_occasions$SurveyYear <- as.numeric(as.character(predictions_occasions$SurveyYear))
#create plot
plot2 <- ggplot(predictions_occasions, aes(x = SurveyYear, y = PredictedOccasions, color = Category, group = Category)) +
  geom_point(size = 1) +
  geom_line() +
  geom_smooth(method = "glm", se = FALSE, linetype = "dotted", aes(group = Category)) + #this adds the fitted line
  scale_color_manual(values = color_palette) +
  labs(x = "Survey year", y = "Number of meat-eating occasions/day", color = "Meat category") +
  scale_x_continuous(breaks = predictions_occasions$SurveyYear, labels = custom_x_labels) +
  theme_classic() +
  theme(text = element_text(family = "Times", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(linetype = guide_legend(override.aes = list(color = "black")))
print(plot2)














# #Occasions
# m2 <- svyglm(avgProcessedokaj ~ SurveyYear, dat.design)
# m3 <- svyglm(avgRedokaj ~ SurveyYear, dat.design)
# m4 <- svyglm(avgWhiteokaj ~ SurveyYear, dat.design)
# #fitted values for each model
# survey_years <- unique(dat.design$variables$SurveyYear)
# predictions <- data.frame(
#   SurveyYear = rep(survey_years, 3),
#   Category = factor(rep(c("avgProcessedokaj", "avgRedokaj", "avgWhiteokaj"), each = length(survey_years))),
#   PredictedOccasions = c(predict(m2, newdata = data.frame(SurveyYear = survey_years), type = "response"),
#                          predict(m3, newdata = data.frame(SurveyYear = survey_years), type = "response"),
#                          predict(m4, newdata = data.frame(SurveyYear = survey_years), type = "response"))
# )
# #colorblind friendly colors
# color_palette <- c("#E69F00", "#0072B2", "#D55E00") #order: processed (orange), white (blue), red (red)
# #correct order
# predictions$Category <- factor(predictions$Category, levels = c("avgProcessedokaj", "avgWhiteokaj", "avgRedokaj"))
# #names for xlab
# levels(predictions$Category) <- c("Processed", "White", "Red")
# #convert SurveyYear to numeric
# predictions$SurveyYear <- as.numeric(as.character(predictions$SurveyYear))
# #create plot
# plot2 <- ggplot(predictions, aes(x = SurveyYear, y = PredictedOccasions, color = Category, group = Category)) +
#   geom_point(size = 1) +
#   geom_line() +
#   geom_smooth(method = "glm", se = FALSE, linetype = "dotted", aes(group = Category)) + #this adds the fitted line
#   scale_color_manual(values = color_palette) +
#   labs(x = "Survey year", y = "Number of meat-eating occasions/day", color = "Meat category") +
#   scale_x_continuous(breaks = predictions$SurveyYear, labels = custom_x_labels) +
#   theme_classic() +
#   theme(text = element_text(family = "Times", size = 12),
#         axis.text.x = element_text(angle = 45, hjust = 1)) +
#   guides(linetype = guide_legend(override.aes = list(color = "black")))
# print(plot2)










#portion size
m2 <- svyglm(gperokajProcessed ~ SurveyYear, dat.design)
m3 <- svyglm(gperokajRed ~ SurveyYear, dat.design)
m4 <- svyglm(gperokajWhite ~ SurveyYear, dat.design)
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
  labs(x = "Survey year", y = "Portion size (g)/meat-eating occasion", color = "Meat category") +
  scale_x_continuous(breaks = predictions$SurveyYear, labels = custom_x_labels) +
  theme_classic() +
  theme(text = element_text(family = "Times", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(linetype = guide_legend(override.aes = list(color = "black")))
print(plot3)

#transparent backgrounds
transparent_theme <- theme(
  panel.background = element_rect(fill = "transparent"),
  plot.background = element_rect(fill = "transparent", color = NA),
  legend.background = element_rect(fill = "transparent", color = NA),
  legend.key = element_rect(fill = "transparent", color = NA)
)
plot0 <- plot0 + transparent_theme
plot1 <- plot1 + transparent_theme
plot2 <- plot2 + transparent_theme
plot3 <- plot3 + transparent_theme

#combine all into 1 figure
#Remove the legend from plot2 and plot3
plot0 <- plot0 + theme(legend.position = "none")
plot2 <- plot2 + theme(legend.position = "none")
plot3 <- plot3 + theme(legend.position = "none")
#extract the legend from plot1
legend_grob <- cowplot::get_legend(plot1)
#remove the legend from plot1
plot1 <- plot1 + theme(legend.position = "none")
#combine the plots and legend into a single plot
top_row <- cowplot::plot_grid(plot0, plot1, nrow = 1, rel_widths = c(1, 1))
bottom_row <- cowplot::plot_grid(plot2, plot3, nrow = 1, rel_widths = c(1, 1))
combined_plot <- cowplot::plot_grid(top_row, bottom_row, legend_grob, ncol = 1, rel_heights = c(1, 1, 0.2))
combined_plots <- cowplot::plot_grid(
  cowplot::plot_grid(plot0, plot1, plot2, plot3, nrow = 2),
  legend_grob,
  ncol = 2,
  rel_widths = c(4, 1)
)
print(combined_plots)
ggsave("~/University of Edinburgh/NDNS Meat Trends - General/Results/Figure 1.png", combined_plots, width = 10, height = 10, dpi = 600)
#used these for a prezzi - gonna save them just in case
ggsave("~/University of Edinburgh/NDNS Meat Trends - General/Results/plot1.png", plot1, width = 4, height = 4, dpi = 900)
ggsave("~/University of Edinburgh/NDNS Meat Trends - General/Results/plot2.png", plot2, width = 4, height = 4, dpi = 900)
ggsave("~/University of Edinburgh/NDNS Meat Trends - General/Results/plot3.png", plot3, width = 4, height = 4, dpi = 900)

################FIGURE 2#########################
#decomposition analysis plot
#create dataset (values pulled from decomposition analysis section of this code)
meat_data <- data.frame(
  Meat = factor(c("Total meat", "Processed meat", "Red meat", "White meat"),
                levels = c("Total meat", "Processed meat", "Red meat", "White meat")),
  Total_Delta = c(-17.5305056, -6.8380170, -13.8349454, 3.1424567),
  Portion_Size_Delta = c(-9.07413433, -4.24183672, -5.63292764, -0.61954963),
  Days_Delta = c(-4.28302706, -0.66615783, -2.40294280, 2.19495175),
  Consumers_Delta = c(-3.03213833, -2.49714327, -4.77907992, 1.18941247),
  Occasions_Delta = c(-1.14120588, 0.56712086, -1.01999501, 0.37764214)
)
numeric_columns <- sapply(meat_data, is.numeric)
print(meat_data)
#transform
melted_data <- reshape2::melt(meat_data, id.vars = "Meat")
#make plot

f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
(cols <- f("Spectral"))
(cols <- f("RdYlGn"))
(cols <- f("Oranges"))
(cols <- f("YlOrRd"))
(cols <- f("Reds"))

bar_plot <- ggplot(melted_data, aes(x = Meat, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  scale_fill_manual(values = c("Total_Delta" = "black", "Consumers_Delta" = "#FC9272", "Days_Delta" = "#EF3B2C", "Occasions_Delta" = "#FEE0D2", "Portion_Size_Delta" = "#A50F15"),
                    labels = c("Total_Delta" = "Total change",
                               "Consumers_Delta" = "Proportion of meat eaters",
                               "Days_Delta" = "Meat-eating days",
                               "Occasions_Delta" = "Meat-eating occasions",
                               "Portion_Size_Delta" = "Portion size of meat")) +
  labs(x = "Meat sub-type", y = "Change in meat consumption (g/capita/day)", fill = "Meat reduction behaviours") +
  theme_classic() +
  theme(text = element_text(family = "Times", size = 12)) 
#define the y-axis limits (didn't like the cuts it was giving me)
y_limits <- c(-20, 5)
#update plot with modified y-axis limits/breaks and a dashed line at y=0
bar_plot <- bar_plot +
  scale_y_continuous(limits = y_limits, expand = c(0, 0), 
                     breaks = seq(-20, 5, by = 5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(label = sprintf("%.1f", value), y = value, group = variable, vjust = ifelse(value >= 0, -0.5, 1.5)), 
            position = position_dodge(width = 0.5), size = 2.5)
bar_plot <- bar_plot + transparent_theme + theme(
  legend.position = c(0.86, 0.15),
  legend.box.background = element_rect(color = "black", size = 0.5),
  legend.box.margin = margin(8, 8, 8, 8))
print(bar_plot)
file_path <- "~/University of Edinburgh/NDNS Meat Trends - General/Results/Figure 2.png"
ggsave(file_path, bar_plot, width = 9, height = 7, dpi = 600)

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
  labs(x = "Survey year", y = "Proportion of participants", fill = "Meat days") +
  scale_x_continuous(breaks = meat_days_prop$SurveyYear, labels = custom_x_labels) +
  geom_text(aes(label = paste0(round(proportion*100),"%")), 
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(text = element_text(family = "Times", size = 12)) +
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
  labs(x = "Survey year", y = "Proportion of participants", fill = "Processed Days") +
  scale_x_continuous(breaks = Processed_days_prop$SurveyYear, labels = custom_x_labels) +
  geom_text(aes(label = paste0(round(proportion*100),"%")), 
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(text = element_text(family = "Times", size = 12)) +
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
  labs(x = "Survey year", y = "Proportion of participants", fill = "Red Days") +
  scale_x_continuous(breaks = Red_days_prop$SurveyYear, labels = custom_x_labels) +
  geom_text(aes(label = paste0(round(proportion*100),"%")), 
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(text = element_text(family = "Times", size = 12)) +
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
  labs(x = "Survey year", y = "Proportion of participants", fill = "White Days") +
  scale_x_continuous(breaks = white_days_prop$SurveyYear, labels = custom_x_labels) +
  geom_text(aes(label = paste0(round(proportion*100),"%")), 
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(text = element_text(family = "Times", size = 12)) +
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

#create counts for MeatDays by SurveyYear (needed for results section text)
meat_days_counts <- dat_svy %>% 
  group_by(SurveyYear) %>% 
  summarise(days0 = sum(MeatDays == 0),
            days1 = sum(MeatDays == 1),
            days2 = sum(MeatDays == 2),
            days3 = sum(MeatDays == 3),
            days4 = sum(MeatDays == 4))








###################SI FIGURE 2#####################


#set survey designs
dat$SurveyYear <- as.factor(dat$SurveyYear)
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

#function for Survey Year x axis labels
custom_x_labels <- function(x) {
  labels <- ifelse(x == 1, "2008/09", sprintf("'%02d/'%02d", x + 7, (x + 7) %% 100 + 1))
  return(labels)
}
#line plot of meat trends
#Days
m2 <- svyglm(MeatDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
#fitted values for each model
survey_years <- unique(dat.design$variables$SurveyYear)
predictions <- data.frame(
  SurveyYear = rep(survey_years, 1),
  Category = factor(rep(c("MeatDays"), each = length(survey_years))),
  PredictedDays = c(predict(m2, newdata = data.frame(SurveyYear = survey_years), type = "response"))
)
#create a custom color palette using colorblind friendly colors
color_palette <- c("#CC79A7") #only total meat
#correct order
predictions$Category <- factor(predictions$Category, levels = c("MeatDays"))
#category names
levels(predictions$Category) <- c("Total meat")
#convert SurveyYear to numeric
predictions$SurveyYear <- as.numeric(as.character(predictions$SurveyYear))
#create plot
plot1 <- ggplot(predictions, aes(x = SurveyYear, y = PredictedDays, color = Category, group = Category)) +
  geom_point(size = 1) +
  geom_line(aes(linetype = "solid")) +
  geom_smooth(method = "glm", formula = 'y ~ x', se = FALSE, aes(linetype = "dotted", group = Category)) +
  scale_color_manual(values = color_palette) +
  scale_linetype_manual(name = "Line type",
                        values = c("solid" = "solid", "dotted" = "dotted"),
                        labels = c("solid" = "Actual data", "dotted" = "2008/09-2018/19 trend")) +
  labs(x = "Survey year", y = "Number of meat days/4-day period", color = "Meat category") +
  scale_x_continuous(breaks = predictions$SurveyYear, labels = custom_x_labels) +
  ylim(2.95, 3.35) +
  theme_classic() +
  theme(text = element_text(family = "Times", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(linetype = guide_legend(override.aes = list(color = "black")))
print(plot1)

#Occasions
m2 <- svyglm(avgMeatokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
#fitted values for each model
survey_years <- unique(dat.design$variables$SurveyYear)
predictions <- data.frame(
  SurveyYear = rep(survey_years, 1),
  Category = factor(rep(c("avgMeatokaj"), each = length(survey_years))),
  PredictedOccasions = c(predict(m2, newdata = data.frame(SurveyYear = survey_years), type = "response"))
)
#colorblind friendly colors
color_palette <- c("#CC79A7") #only total meat
#correct order
predictions$Category <- factor(predictions$Category, levels = c("avgMeatokaj"))
#names for xlab
levels(predictions$Category) <- c("Total meat")
#convert SurveyYear to numeric
predictions$SurveyYear <- as.numeric(as.character(predictions$SurveyYear))
#create plot
plot2 <- ggplot(predictions, aes(x = SurveyYear, y = PredictedOccasions, color = Category, group = Category)) +
  geom_point(size = 1) +
  geom_line() +
  geom_smooth(method = "glm", se = FALSE, linetype = "dotted", aes(group = Category)) + #this adds the fitted line
  scale_color_manual(values = color_palette) +
  labs(x = "Survey year", y = "No. meat-eating occasions/day", color = "Meat category") +
  scale_x_continuous(breaks = predictions$SurveyYear, labels = custom_x_labels) +
  ylim(1.1, 1.3) +
  theme_classic() +
  theme(text = element_text(family = "Times", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(linetype = guide_legend(override.aes = list(color = "black")))
print(plot2)

#portion size
m2 <- svyglm(gperokajMeat ~ SurveyYear, family=poisson(link = "log"), dat.design)
#fitted values for each model
survey_years <- unique(dat.design$variables$SurveyYear)
predictions <- data.frame(
  SurveyYear = rep(survey_years, 1),
  Category = factor(rep(c("gperokajMeat"), each = length(survey_years))),
  PredictedPortion = c(predict(m2, newdata = data.frame(SurveyYear = survey_years), type = "response"))
)
#colorblind friendly
color_palette <- c("#CC79A7") #only total meat
#correct order
predictions$Category <- factor(predictions$Category, levels = c("gperokajMeat"))
#names for xlab
levels(predictions$Category) <- c("Total meat")
#convert SurveyYear to numeric
predictions$SurveyYear <- as.numeric(as.character(predictions$SurveyYear))
#create plot
plot3 <- ggplot(predictions, aes(x = SurveyYear, y = PredictedPortion, color = Category, group = Category)) +
  geom_point(size = 1) +
  geom_line() +
  geom_smooth(method = "glm", se = FALSE, linetype = "dotted", aes(group = Category)) + #this adds the fitted line
  scale_color_manual(values = color_palette) +
  labs(x = "Survey year", y = "Portion size (g)/meat-eating occasion", color = "Meat category") +
  scale_x_continuous(breaks = predictions$SurveyYear, labels = custom_x_labels) +
  ylim(74, 88) +
  theme_classic() +
  theme(text = element_text(family = "Times", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(linetype = guide_legend(override.aes = list(color = "black")))
print(plot3)

#transparent backgrounds
transparent_theme <- theme(
  panel.background = element_rect(fill = "transparent"),
  plot.background = element_rect(fill = "transparent", color = NA),
  legend.background = element_rect(fill = "transparent", color = NA),
  legend.key = element_rect(fill = "transparent", color = NA)
)
plot1 <- plot1 + transparent_theme
plot2 <- plot2 + transparent_theme
plot3 <- plot3 + transparent_theme

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
ggsave("~/University of Edinburgh/NDNS Meat Trends - General/Results/SI Figure 2.png", combined_plot, width = 8, height = 8, dpi = 600)









