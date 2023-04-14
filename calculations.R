library(dplyr)
library(tidyverse)
library(srvyr)
library(survey)
library(effects)
library(ggplot2)
library(RColorBrewer)
library(scales)
#set wd
setwd("/Users/alexandervonderschmidt/Library/CloudStorage/OneDrive-SharedLibraries-UniversityofEdinburgh/NDNS Meat Trends - General/Data")
#upload datasets
dat <- read.csv('omega.csv')

#remove participants with only 3 diary days (removed n = 323; n = 15,332)
dat <- dat[!(dat$DiaryDaysCompleted == 3),]



######################SANDBOX####################




#create weighted proportion of total meat occasions per day
# Create the survey design object
survey_design <- svydesign(id = ~area, strata = ~astrata5, weights = ~wti, data = dat)
# Create a new categorical variable based on "avgMeatokaj"
dat$Meatokajcat <- cut(dat$avgMeatokaj, 
                       breaks = c(-Inf, 1, 1.5, 2, Inf), 
                       labels = c("<1", "1-1.5", "1.5-2", ">2"), 
                       right = FALSE, 
                       include.lowest = TRUE)
#
survey_design <- svydesign(id = ~area, strata = ~astrata5, weights = ~wti, data = dat)
# categorical variable as factor
dat$Meatokajcat <- as.factor(dat$Meatokajcat)
# Calculate the weighted proportions for each category, by year
weighted_proportions_by_year <- svyby(~Meatokajcat, ~SurveyYear, survey_design, svymean)
#subset dat, check calculations
dat_subset <- dat[, c("seriali", "avgMeatokaj", "Meatokajcat")]
# Convert the weighted_proportions_by_year to a data frame
weighted_proportions_by_year_df <- as.data.frame(weighted_proportions_by_year)
# Remove the standard error columns
weighted_proportions_no_se <- weighted_proportions_by_year_df %>%
  select(-starts_with("se."))
#long data
long_weighted_proportions <- weighted_proportions_no_se %>%
  pivot_longer(cols = starts_with("Meatokajcat"), 
               names_to = "Category", 
               values_to = "Proportion", 
               names_prefix = "Meatokajcat")
#with % text (excluding bottom 2 categories because they muddied the text a bit and are all <2%)
plot <- ggplot(long_weighted_proportions, aes(x = SurveyYear, y = Proportion, fill = factor(Category, levels = unique(Category)))) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(Proportion*100),"%")), position = position_stack(vjust = 0.5)) +
  labs(title = NULL, x = "Survey Year", y = "Proportion", fill = "Meat occasions/day") +
  theme_classic() +
  scale_fill_manual(values = brewer.pal(4, "Reds"), labels = c("<1", "1-1.5", "1.5-2", ">2")) +
  scale_x_continuous(breaks = unique(long_weighted_proportions$SurveyYear)) +
  theme(text = element_text(family = "Avenir", size = 12))

plot
#thinking about combining the bottom two categories since they both contribute so little to the overall proportion
file_path <- "~/University of Edinburgh/NDNS Meat Trends - General/Results/MeatOccasionsProp.png"
# Save plot to file
ggsave(file_path, plot, width = 10, height = 8, dpi = 300)

















































#create weighted proportion of total meat occasions per day
# Create the survey design object
survey_design <- svydesign(id = ~area, strata = ~astrata5, weights = ~wti, data = dat)
# Create a new categorical variable based on "avgMeatokaj"
dat$Meatokajcat <- cut(dat$avgMeatokaj, 
                       breaks = c(-Inf, 1, 1.5, 2.5, 3, Inf), 
                       labels = c("<1", "1", "2", "3", ">3"), 
                       right = FALSE, 
                       include.lowest = TRUE)
#
survey_design <- svydesign(id = ~area, strata = ~astrata5, weights = ~wti, data = dat)
# categorical variable as factor
dat$Meatokajcat <- as.factor(dat$Meatokajcat)
# Calculate the weighted proportions for each category, by year
weighted_proportions_by_year <- svyby(~Meatokajcat, ~SurveyYear, survey_design, svymean)
#subset dat, check calculations
dat_subset <- dat[, c("seriali", "avgMeatokaj", "Meatokajcat")]
# Convert the weighted_proportions_by_year to a data frame
weighted_proportions_by_year_df <- as.data.frame(weighted_proportions_by_year)
# Remove the standard error columns
weighted_proportions_no_se <- weighted_proportions_by_year_df %>%
  select(-starts_with("se."))
#with % text (excluding bottom 2 categories because they muddied the text a bit and are all <2%)
plot <- ggplot(long_weighted_proportions, aes(x = SurveyYear, y = Proportion, fill = factor(Category, levels = unique(Category)))) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Proportion > 0.03, paste0(round(Proportion*100),"%"), "")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = NULL,
       x = "Year",
       y = "Proportion",
       fill = "Meat occasions/day") +
  theme_classic() +
  scale_fill_manual(values = brewer.pal(5, "Reds"), 
                    labels = c("<1", "1", "2", "3", ">3")) +
  scale_x_continuous(breaks = unique(long_weighted_proportions$SurveyYear)) +
  theme(text = element_text(family = "Avenir", size = 12))
#thinking about combining the bottom two categories since they both contribute so little to the overall proportion
file_path <- "~/University of Edinburgh/NDNS Meat Trends - General/Results/MeatOccasionsProp.png"
# Save plot to file
ggsave(file_path, plot, width = 10, height = 8, dpi = 300)




# Create a srvyr object with the survey design
dat_svy <- as_survey(survey_design)

# Calculate the weighted proportion for each level of MeatDays by SurveyYear
meat_days_prop <- dat_svy %>% 
  group_by(SurveyYear) %>% 
  summarize(prop_0 = survey_mean(MeatDays == 0),
            prop_1 = survey_mean(MeatDays == 1),
            prop_2 = survey_mean(MeatDays == 2),
            prop_3 = survey_mean(MeatDays == 3),
            prop_4 = survey_mean(MeatDays == 4))

# View the results
meat_days_prop
# Identify the columns that end in "_se"
se_cols <- grep("_se$", names(meat_days_prop))
# Remove the columns that end in "_se"
meat_days_prop_no_se <- meat_days_prop[, -se_cols]
# Reshape the data from wide to long format
meat_days_prop_long <- pivot_longer(meat_days_prop_no_se, cols = -SurveyYear, names_to = "MeatDays", values_to = "proportion")
# Plot the stacked bar plot
plot <- ggplot(meat_days_prop_long, aes(x = SurveyYear, y = proportion, fill = str_remove(MeatDays, "prop_"))) + 
  geom_col() +
  scale_fill_brewer(palette = "Reds", direction = 1) +
  labs(x = "Survey Year", y = "Proportion", fill = "Meat Days") +
  scale_x_continuous(breaks = meat_days_prop$SurveyYear, labels = meat_days_prop$SurveyYear) +
  geom_text(aes(label = paste0(round(proportion*100),"%")), 
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(text = element_text(family = "Avenir", size = 12))

file_path <- "~/University of Edinburgh/NDNS Meat Trends - General/Results/MeatDaysProp.png"
# Save plot to file
ggsave(file_path, plot, width = 10, height = 8, dpi = 300)




#####################TABLE 1 - DEMOGRAPHICS#######################

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





#############################TABLE 2 - MAIN ANALYSIS ###################
#make survey year numeric
dat$SurveyYear <- as.factor(dat$SurveyYear)
dat$SurveyYear <- as.numeric(dat$SurveyYear)
dat$Sex <- as.factor(dat$Sex)
dat$Sex <- as.numeric(dat$Sex)
dat$eqv <- as.factor(dat$eqv)
dat$eqv <- as.numeric(dat$eqv)
dat$Age <- as.factor(dat$Age)
dat$Age <- as.numeric(dat$Age)
dat$AgeG <- as.factor(dat$AgeG)
dat$AgeG <- as.numeric(dat$AgeG)
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

##MEAT DAYS##
m1 <- svyglm(MeatDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(1.184801)
exp(1.184801-0.077232)
m1 <- svyglm(ProcessedDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(0.570189)
exp(0.570189-0.104735)
m1 <- svyglm(RedDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(0.44447)
exp(0.44447-0.23973)
m1 <- svyglm(WhiteDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(0.350942)
exp(0.350942+0.099129)
m1 <- svyglm(NoMeatDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(-0.31476)
exp(-0.31476+0.28740)


##Meat occasions##
m1 <- svyglm(avgMeatokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(0.21369)
exp(0.21369-0.08928)
m1 <- svyglm(avgProcessedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(-0.6168960)
exp(-0.6168960-0.0859576)
m1 <- svyglm(avgRedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(-0.82162)
exp(-0.82162-0.27378)
m1 <- svyglm(avgWhiteokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(-0.905580)
exp(-0.905580+0.110190)


##g per occasion##
m1 <- svyglm(gperokajMeat ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(4.4515255)
exp(4.4515255-0.1192086)
m1 <- svyglm(gperokajProcessed ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(4.146524)
exp(4.146524-0.183016)
m1 <- svyglm(gperokajRed ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(4.49681)
exp(4.49681-0.25282)
m1 <- svyglm(gperokajWhite ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(4.438522)
exp(4.438522-0.059742)
m1 <- svyglm(okajTotalGrams ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(6.150279)
exp(6.150279+0.096986)





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




#########################TABLE 3 - STM ANALYSIS########################
#BREAKFAST
##Meat occasions##
m1 <- svyglm(BMeatokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(-1.27281)
exp(-1.27281-0.05088)
m1 <- svyglm(BProcessedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(-1.490848)
exp(-1.490848-0.010233)
m1 <- svyglm(BRedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(-3.1706)
exp(-3.1706-0.3312)
m1 <- svyglm(BWhiteokaj ~ SurveyYear, family=poisson(link = "log"), dat.design.y1)
summary(m1) #exponentiate
exp(-3.48036)
exp(-3.48036-0.17600)


##g per occasion##
m1 <- svyglm(BgperokajMeat ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(4.29765)
exp(4.29765-0.09692)
m1 <- svyglm(BgperokajProcessed ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(4.28983)
exp(4.28983-0.10131)
m1 <- svyglm(BgperokajRed ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(3.99114)
exp(3.99114-0.49028)
m1 <- svyglm(BgperokajWhite ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(3.59971)
exp(3.59971+0.60952)
m1 <- svyglm(BokajGrams ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(6.09873)
exp(6.09873+0.08560)



#LUNCH
##Meat occasions##
m1 <- svyglm(LMeatokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(0.60394)
exp(0.60394-0.08771)
m1 <- svyglm(LProcessedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(0.01739)
exp(0.01739-0.19697)
m1 <- svyglm(LRedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(-0.70918)
exp(-0.70918-0.26963)
m1 <- svyglm(LWhiteokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(-0.647568)
exp(-0.647568+0.190062)


##g per occasion##
m1 <- svyglm(LgperokajMeat ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(4.182490)
exp(4.182490-0.118474)
m1 <- svyglm(LgperokajProcessed ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(4.02694)
exp(4.02694-0.16758)
m1 <- svyglm(LgperokajRed ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(4.17327)
exp(4.17327-0.17812)
m1 <- svyglm(LgperokajWhite ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(4.102199)
exp(4.102199+0.035625)
m1 <- svyglm(LokajGrams ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(6.231770)
exp(6.231770+0.025879)




#DINNER
##Meat occasions##
m1 <- svyglm(DMeatokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(0.895225)
exp(0.895225-0.083727)
m1 <- svyglm(DProcessedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(-0.27349)
exp(-0.27349+0.02917)
m1 <- svyglm(DRedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(0.07869)
exp(0.07869-0.26195)
m1 <- svyglm(DWhiteokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(-0.1018312)
exp(-0.1018312+0.1016680)


##g per occasion##
m1 <- svyglm(DgperokajMeat ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(4.62109)
exp(4.62109-0.13262)
m1 <- svyglm(DgperokajProcessed ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(4.304573)
exp(4.304573-0.227260)
m1 <- svyglm(DgperokajRed ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(4.60369)
exp(4.60369-0.27837)
m1 <- svyglm(DgperokajWhite ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(4.579926)
exp(4.579926-0.067861)
m1 <- svyglm(DokajGrams ~ SurveyYear, family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
exp(6.377098)
exp(6.377098+0.027697)




##########################TABLE 4 - analysis by covariates########################
#Sex
#Days
m1 <- svyglm(MeatDays ~ SurveyYear + Sex +
               SurveyYear*Sex,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
#men
exp(1.2067741)
exp(1.2067741-0.0643641)
#women
exp(1.2067741-0.0424610)
exp(1.2067741-0.0424610-0.0643641-0.0250790)
m1 <- svyglm(ProcessedDays ~ SurveyYear + Sex +
               SurveyYear*Sex,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
#men
exp(0.6380294)
exp(0.6380294-0.0788875)
#women
exp(0.6380294-0.1340621)
exp(0.6380294-0.1340621-0.0788875-0.0529647)
m1 <- svyglm(RedDays ~ SurveyYear + Sex +
               SurveyYear*Sex,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
#men
exp(0.475398)
exp(0.475398-0.233433)
#women
exp(0.475398-0.060021)
exp(0.475398-0.060021-0.233433-0.012222)
m1 <- svyglm(WhiteDays ~ SurveyYear + Sex +
               SurveyYear*Sex,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
#men
exp(0.33375)
exp(0.33375+0.16677)
#women
exp(0.33375+0.03262)
exp(0.33375+0.03262+0.16677-0.13116)
m1 <- svyglm(NoMeatDays ~ SurveyYear + Sex +
               SurveyYear*Sex,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
#men
exp(-0.419590)
exp(-0.419590+0.275359)
#women
exp(-0.419590+0.191784)
exp(-0.419590+0.191784+0.275359+0.020469)

#Occasions
m1 <- svyglm(avgMeatokaj ~ SurveyYear + Sex +
               SurveyYear*Sex,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
#men
exp(0.270496)
exp(0.270496-0.049780)
#women
exp(0.270496-0.111629)
exp(0.270496-0.111629-0.049780-0.080881)
m1 <- svyglm(avgProcessedokaj ~ SurveyYear + Sex +
               SurveyYear*Sex,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
#men
exp(-0.534707)
exp(-0.534707-0.025953)
#women
exp(-0.534707-0.163611)
exp(-0.534707-0.163611-0.025953-0.127826)
m1 <- svyglm(avgRedokaj ~ SurveyYear + Sex +
               SurveyYear*Sex,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
#men
exp(-0.760957)
exp(-0.760957-0.276008)
#women
exp(-0.760957-0.119445)
exp(-0.760957-0.119445-0.276008+0.004873)
m1 <- svyglm(avgWhiteokaj ~ SurveyYear + Sex +
               SurveyYear*Sex,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
#men
exp(-0.886878)
exp(-0.886878+0.168537)
#women
exp(-0.886878-0.036084)
exp(-0.886878-0.036084+0.168537-0.116412)

#portion size
m1 <- svyglm(gperokajMeat ~ SurveyYear + Sex +
               SurveyYear*Sex,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
#men
exp(4.56634)
exp(4.56634-0.16850)
#women
exp(4.56634-0.23724)
exp(4.56634-0.23724-0.16850+0.10772)
m1 <- svyglm(gperokajProcessed ~ SurveyYear + Sex +
               SurveyYear*Sex,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
#men
exp(4.30866)
exp(4.30866-0.24235)
#women
exp(4.30866-0.34648)
exp(4.30866-0.34648-0.24235+0.12996)
m1 <- svyglm(gperokajRed ~ SurveyYear + Sex +
               SurveyYear*Sex,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
#men
exp(4.620899)
exp(4.620899-0.325364)
#women
exp(4.620899-0.256947)
exp(4.620899-0.256947-0.325364+0.154732)
m1 <- svyglm(gperokajWhite ~ SurveyYear + Sex +
               SurveyYear*Sex,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
#men
exp(4.53128)
exp(4.53128-0.07439)
#women
exp(4.53128-0.18695)
exp(4.53128-0.18695-0.07439+0.03077)

















#Age
#Days
m1 <- svyglm(MeatDays ~ SurveyYear + AgeG +
               SurveyYear*AgeG,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- 1.1894705
b1 <- -0.0340831
b2.1 <- 0.0297407
b2.2 <- -0.0230809
b2.3 <- 0.0036768
b2.4 <- -0.0028029
b3.1 <- -0.0283212
b3.2 <- -0.0675092
b3.3 <- -0.0775060
b3.4 <- -0.0319511
#<=10
exp(b0)
exp(b0+b1)
#11-17
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#18-40
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
#41-59
exp(b0+b2.3)
exp(b0+b2.3+b1+b3.3)
#>=60
exp(b0+b2.4)
exp(b0+b2.4+b1+b3.4)
m1 <- svyglm(ProcessedDays ~ SurveyYear + AgeG +
               SurveyYear*AgeG,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- 0.633770
b1 <- -0.089836
b2.1 <- 0.039491
b2.2 <- -0.041940
b2.3 <- -0.085439
b2.4 <- -0.150173
b3.1 <- 0.045656
b3.2 <- -0.023126
b3.3 <- -0.049088
b3.4 <- 0.007174
#<=10
exp(b0)
exp(b0+b1)
#11-17
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#18-40
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
#41-59
exp(b0+b2.3)
exp(b0+b2.3+b1+b3.3)
#>=60
exp(b0+b2.4)
exp(b0+b2.4+b1+b3.4)
m1 <- svyglm(RedDays ~ SurveyYear + AgeG +
               SurveyYear*AgeG,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- 0.177370
b1 <- -0.111996
b2.1 <- 0.110808
b2.2 <- 0.163762
b2.3 <- 0.378715
b2.4 <- 0.442790
b3.1 <- 0.166835
b3.2 <- -0.168771
b3.3 <- -0.287373
b3.4 <- -0.107989
#<=10
exp(b0)
exp(b0+b1)
#11-17
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#18-40
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
#41-59
exp(b0+b2.3)
exp(b0+b2.3+b1+b3.3)
#>=60
exp(b0+b2.4)
exp(b0+b2.4+b1+b3.4)
m1 <- svyglm(WhiteDays ~ SurveyYear + AgeG +
               SurveyYear*AgeG,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- 0.369195
b1 <- 0.137145
b2.1 <- 0.150334
b2.2 <- 0.125631
b2.3 <- -0.114505
b2.4 <- -0.237396
b3.1 <- -0.042828
b3.2 <- -0.053921
b3.3 <- -0.003351
b3.4 <- -0.033346
#<=10
exp(b0)
exp(b0+b1)
#11-17
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#18-40
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
#41-59
exp(b0+b2.3)
exp(b0+b2.3+b1+b3.3)
#>=60
exp(b0+b2.4)
exp(b0+b2.4+b1+b3.4)
m1 <- svyglm(NoMeatDays ~ SurveyYear + AgeG +
               SurveyYear*AgeG,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- -3.360e-01
b1 <- 1.433e-01
b2.1 <- -1.494e-01
b2.2 <- 9.975e-02
b2.3 <- -1.708e-02
b2.4 <- 1.278e-02
b3.1 <- -1.122e-01
b3.2 <- 1.880e-01
b3.3 <- 2.592e-01
b3.4 <- 1.108e-01
#<=10
exp(b0)
exp(b0+b1)
#11-17
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#18-40
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
#41-59
exp(b0+b2.3)
exp(b0+b2.3+b1+b3.3)
#>=60
exp(b0+b2.4)
exp(b0+b2.4+b1+b3.4)


#occasions
m1 <- svyglm(avgMeatokaj ~ SurveyYear + AgeG +
               SurveyYear*AgeG,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- 0.209181
b1 <- -0.047309
b2.1 <- 0.085918
b2.2 <- 0.020069
b2.3 <- 0.009435
b2.4 <- -0.053317
b3.1 <- 0.050396
b3.2 <- -0.049978
b3.3 <- -0.111845
b3.4 <- -0.015472
#<=10
exp(b0)
exp(b0+b1)
#11-17
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#18-40
exp(b0+b2.2)
exp(b0+b2.2+b1-b3.2)
#41-59
exp(b0+b2.3)
exp(b0+b2.3+b1-b3.3)
#>=60
exp(b0+b2.4)
exp(b0+b2.4+b1-b3.4)
m1 <- svyglm(avgProcessedokaj ~ SurveyYear + AgeG +
               SurveyYear*AgeG,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- -0.526574
b1 <- -0.123416
b2.1 <- 0.082042
b2.2 <- -0.083231
b2.3 <- -0.101490
b2.4 <- -0.215984
b3.1 <- 0.066305
b3.2 <- 0.062725
b3.3 <- -0.010699
b3.4 <- 0.073181
#<=10
exp(b0)
exp(b0+b1)
#11-17
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#18-40
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
#41-59
exp(b0+b2.3)
exp(b0+b2.3+b1+b3.3)
#>=60
exp(b0+b2.4)
exp(b0+b2.4+b1+b3.4)
m1 <- svyglm(avgRedokaj ~ SurveyYear + AgeG +
               SurveyYear*AgeG,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- -1.151053
b1 <- -0.098625
b2.1 <- 0.159694
b2.2 <- 0.214555
b2.3 <- 0.485506
b2.4 <- 0.494483
b3.1 <- 0.129310
b3.2 <- -0.194824
b3.3 <- -0.405457
b3.4 <- -0.126305
#<=10
exp(b0)
exp(b0+b1)
#11-17
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#18-40
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
#41-59
exp(b0+b2.3)
exp(b0+b2.3+b1+b3.3)
#>=60
exp(b0+b2.4)
exp(b0+b2.4+b1+b3.4)
m1 <- svyglm(avgWhiteokaj ~ SurveyYear + AgeG +
               SurveyYear*AgeG,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- -0.889205
b1 <- 0.184171
b2.1 <- 0.136605
b2.2 <- 0.130556
b2.3 <- -0.123462
b2.4 <- -0.218695
b3.1 <- -0.057937
b3.2 <- -0.074999
b3.3 <- -0.030895
b3.4 <- -0.139041
#<=10
exp(b0)
exp(b0+b1)
#11-17
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#18-40
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
#41-59
exp(b0+b2.3)
exp(b0+b2.3+b1+b3.3)
#>=60
exp(b0+b2.4)
exp(b0+b2.4+b1+b3.4)

#portion size
m1 <- svyglm(gperokajMeat ~ SurveyYear + AgeG +
               SurveyYear*AgeG,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- 3.896593
b1 <- -0.062788
b2.1 <- 0.454402
b2.2 <- 0.647417
b2.3 <- 0.654171
b2.4 <- 0.573507
b3.1 <- -0.030292
b3.2 <- -0.032417
b3.3 <- -0.034108
b3.4 <- -0.129732
#<=10
exp(b0)
exp(b0+b1)
#11-17
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#18-40
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
#41-59
exp(b0+b2.3)
exp(b0+b2.3+b1+b3.3)
#>=60
exp(b0+b2.4)
exp(b0+b2.4+b1+b3.4)
m1 <- svyglm(gperokajProcessed ~ SurveyYear + AgeG +
               SurveyYear*AgeG,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- 3.845292
b1 <- -0.132629
b2.1 <- 0.287309
b2.2 <- 0.362839
b2.3 <- 0.417002
b2.4 <- 0.213756
b3.1 <- -0.004823
b3.2 <- -0.039640
b3.3 <- -0.086390
b3.4 <- -0.036885
#<=10
exp(b0)
exp(b0+b1)
#11-17
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#18-40
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
#41-59
exp(b0+b2.3)
exp(b0+b2.3+b1+b3.3)
#>=60
exp(b0+b2.4)
exp(b0+b2.4+b1+b3.4)
m1 <- svyglm(gperokajRed ~ SurveyYear + AgeG +
               SurveyYear*AgeG,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- 3.772161
b1 <- -0.139029
b2.1 <- 0.502541
b2.2 <- 0.863288
b2.3 <- 0.785196
b2.4 <- 0.765099
b3.1 <- 0.069718
b3.2 <- -0.189478
b3.3 <- 0.008835
b3.4 <- -0.181812
#<=10
exp(b0)
exp(b0+b1)
#11-17
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#18-40
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
#41-59
exp(b0+b2.3)
exp(b0+b2.3+b1+b3.3)
#>=60
exp(b0+b2.4)
exp(b0+b2.4+b1+b3.4)
m1 <- svyglm(gperokajWhite ~ SurveyYear + AgeG +
               SurveyYear*AgeG,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- 3.913609
b1 <- -0.005890
b2.1 <- 0.467545
b2.2 <- 0.604110
b2.3 <- 0.630811
b2.4 <- 0.528555
b3.1 <- -0.156613
b3.2 <- -0.016405
b3.3 <- -0.053572
b3.4 <- -0.077834
#<=10
exp(b0)
exp(b0+b1)
#11-17
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#18-40
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
#41-59
exp(b0+b2.3)
exp(b0+b2.3+b1+b3.3)
#>=60
exp(b0+b2.4)
exp(b0+b2.4+b1+b3.4)











#eqv
m1 <- svyglm(MeatDays ~ SurveyYear + eqv +
               SurveyYear*eqv,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- 1.1817281
b1 <- -0.0439812
b2.1 <- 0.0365458
b2.2 <- 0.0157946
b3.1 <- -0.0228805
b3.2 <- -0.0523464
#1st tertile (highest)
exp(b0)
exp(b0+b1)
#2nd tertile (middle)
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#3rd tertile (lowest)
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
m1 <- svyglm(ProcessedDays ~ SurveyYear + eqv +
               SurveyYear*eqv,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- 0.584166
b1 <- -0.137938
b2.1 <- 0.028083
b2.2 <- 0.001426
b3.1 <- 0.112661
b3.2 <- 0.040929
#1st tertile (highest)
exp(b0)
exp(b0+b1)
#2nd tertile (middle)
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#3rd tertile (lowest)
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
m1 <- svyglm(RedDays ~ SurveyYear + eqv +
               SurveyYear*eqv,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- 0.458500
b1 <- -0.218373
b2.1 <- -0.014614
b2.2 <- 0.003518
b3.1 <- 0.053163
b3.2 <- -0.102749
#1st tertile (highest)
exp(b0)
exp(b0+b1)
#2nd tertile (middle)
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#3rd tertile (lowest)
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
m1 <- svyglm(WhiteDays ~ SurveyYear + eqv +
               SurveyYear*eqv,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- 0.425166
b1 <- -0.007130
b2.1 <- -0.118976
b2.2 <- -0.041638
b3.1 <- 0.199113
b3.2 <- 0.106002
#1st tertile (highest)
exp(b0)
exp(b0+b1)
#2nd tertile (middle)
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#3rd tertile (lowest)
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
m1 <- svyglm(NoMeatDays ~ SurveyYear + eqv +
               SurveyYear*eqv,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- -0.30111
b1 <- 0.17358
b2.1 <- -0.17910
b2.2 <- -0.07272
b3.1 <- 0.12911
b3.2 <- 0.19245
#1st tertile (highest)
exp(b0)
exp(b0+b1)
#2nd tertile (middle)
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#3rd tertile (lowest)
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)


#occasions
m1 <- svyglm(avgMeatokaj ~ SurveyYear + eqv +
               SurveyYear*eqv,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- 0.252250
b1 <- -0.112100
b2.1 <- -0.033772
b2.2 <- -0.023469
b3.1 <- 0.098460
b3.2 <- -0.010679
#1st tertile (highest)
exp(b0)
exp(b0+b1)
#2nd tertile (middle)
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#3rd tertile (lowest)
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
m1 <- svyglm(avgProcessedokaj ~ SurveyYear + eqv +
               SurveyYear*eqv,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- -5.806e-01
b1 <- -1.335e-01
b2.1 <- -3.039e-02
b2.2 <- -3.804e-03
b3.1 <- 1.685e-01
b3.2 <- 3.316e-02
#1st tertile (highest)
exp(b0)
exp(b0+b1)
#2nd tertile (middle)
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#3rd tertile (lowest)
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
m1 <- svyglm(avgRedokaj ~ SurveyYear + eqv +
               SurveyYear*eqv,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- -0.80043
b1 <- -0.25798
b2.1 <- -0.04190
b2.2 <- -0.01643
b3.1 <- 0.11597
b3.2 <- -0.11030
#1st tertile (highest)
exp(b0)
exp(b0+b1)
#2nd tertile (middle)
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#3rd tertile (lowest)
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
m1 <- svyglm(avgWhiteokaj ~ SurveyYear + eqv +
               SurveyYear*eqv,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- -0.8370416
b1 <- 0.0225800
b2.1 <- -0.1052534
b2.2 <- -0.0516702
b3.1 <- 0.1407563
b3.2 <- 0.1139817
#1st tertile (highest)
exp(b0)
exp(b0+b1)
#2nd tertile (middle)
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#3rd tertile (lowest)
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)


#portion size
m1 <- svyglm(gperokajMeat ~ SurveyYear + eqv +
               SurveyYear*eqv,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- 4.395762
b1 <- -0.155880
b2.1 <- 0.104690
b2.2 <- 0.059354
b3.1 <- 0.058441
b3.2 <- 0.065801
#1st tertile (highest)
exp(b0)
exp(b0+b1)
#2nd tertile (middle)
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#3rd tertile (lowest)
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
m1 <- svyglm(gperokajProcessed ~ SurveyYear + eqv +
               SurveyYear*eqv,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- 4.1851402
b1 <- -0.2386114
b2.1 <- 0.0083387
b2.2 <- -0.1055891
b3.1 <- 0.0735354
b3.2 <- 0.1114715
#1st tertile (highest)
exp(b0)
exp(b0+b1)
#2nd tertile (middle)
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#3rd tertile (lowest)
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
m1 <- svyglm(gperokajRed ~ SurveyYear + eqv +
               SurveyYear*eqv,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- 4.328
b1 <- -1.178e-01
b2.1 <- 2.136e-01
b2.2 <- 2.115e-01
b3.1 <- -1.344e-01
b3.2 <- -1.645e-01
#1st tertile (highest)
exp(b0)
exp(b0+b1)
#2nd tertile (middle)
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#3rd tertile (lowest)
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)
m1 <- svyglm(gperokajWhite ~ SurveyYear + eqv +
               SurveyYear*eqv,
             family=poisson(link = "log"), dat.design)
summary(m1) #exponentiate
b0 <- 4.359230
b1 <- -0.074787
b2.1 <- 0.093906
b2.2 <- 0.141523
b3.1 <- 0.088421
b3.2 <- -0.065769
#1st tertile (highest)
exp(b0)
exp(b0+b1)
#2nd tertile (middle)
exp(b0+b2.1)
exp(b0+b2.1+b1+b3.1)
#3rd tertile (lowest)
exp(b0+b2.2)
exp(b0+b2.2+b1+b3.2)



mean(dat$avgProcessedokaj)
mean(dat$avgProcessedokajpmd, na.rm = TRUE)





#######################TABLE 5 - DECOMPOSITION ANALYSIS###############


#total meat
#convert occasions per day variable to occasions per MEAT day (to add up for decomp)
zo1 <- 1.238239
zo2 <- 1.13248
#total meat decomp
d1 <- 3.270036 #meat days y1
d2 <- 3.026991 #meat days y11
o1 <- (zo1*4)/d1 #meat occasions per meat day y1
o2 <- (zo2*4)/d2 #meat occasions per meat day y11
p1 <- 85.75767 #meat portion size y1
p2 <- 76.12045 #meat portion size y11
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
p1 <- 63.21389 #meat portion size y1
p2 <- 52.64167 #meat portion size y11
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
#because opposite trends, calculate reductions responsible % for reductions
dm1*(dm1+om1+pm1)/(dm1+pm1)
pm1*(dm1+om1+pm1)/(dm1+pm1)




#Red meat
#convert occasions per day variable to occasions per MEAT day (to add up for decomp)
zo1 <- 0.4397187
zo2 <- 0.3344058
#processed meat decomp
d1 <- 1.559663 #meat days y1
d2 <- 1.227206 #meat days y11
o1 <- (zo1*4)/d1 #meat occasions per meat day y1
o2 <- (zo2*4)/d2 #meat occasions per meat day y11
p1 <- 89.73043 #meat portion size y1
p2 <- 69.68534 #meat portion size y11
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
p1 <- 84.64974 #meat portion size y1
p2 <- 79.74069 #meat portion size y11
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

abs(dm1)/(abs(dm1)+abs(om1)+abs(pm1))
abs(om1)/(abs(dm1)+abs(om1)+abs(pm1))
abs(pm1)/(abs(dm1)+abs(om1)+abs(pm1))

#because opposite trends, calculate reductions responsible % for reductions
dm1*(dm1+om1+pm1)/(dm1+om1)
om1*(dm1+om1+pm1)/(dm1+om1)




##########################plots playings#########################

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

#THIS WORKS DON'T TOUCH; with fitted dashed lines
#MeatDays
m2 <- svyglm(ProcessedDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
m3 <- svyglm(RedDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
m4 <- svyglm(WhiteDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
m5 <- svyglm(NoMeatDays ~ SurveyYear, family=poisson(link = "log"), dat.design)

# Predict fitted values for each model
survey_years <- unique(dat.design$variables$SurveyYear)
predictions <- data.frame(
  SurveyYear = rep(survey_years, 4),
  Category = factor(rep(c("ProcessedDays", "RedDays", "WhiteDays", "NoMeatDays"), each = length(survey_years))),
  PredictedDays = c(predict(m2, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m3, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m4, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m5, newdata = data.frame(SurveyYear = survey_years), type = "response"))
)

# Create a custom color palette using the darker colors from the "PuBuGn" palette
color_palette <- c("#FDAE61", "#ABD9E9", "#D53E4F", "#41AB5D") #order: processed (orange), white (blue), red (red), no meat (green)

# Create a custom factor level order based on the correct order
predictions$Category <- factor(predictions$Category, levels = c("ProcessedDays", "WhiteDays", "RedDays", "NoMeatDays"))

# Update the category names with proper spacing
levels(predictions$Category) <- c("Processed", "White", "Red", "No Meat")

# Create a combined plot with ggplot2 using the custom color palette, scatter points, and connected lines
plot <- ggplot(predictions, aes(x = SurveyYear, y = PredictedDays, color = Category, group = Category)) +
  geom_point(size = 1) +
  geom_line() +
  geom_smooth(method = "glm", se = FALSE, linetype = "dashed", aes(group = Category)) +
  scale_color_manual(values = color_palette) +
  labs(x = "Survey Year", y = "Number of days (avg. across 4-day period)", color = "Meat category") +
  theme_classic() +
  theme(text = element_text(family = "Avenir", size = 12))

# Print the plot
print(plot)

ggsave("~/University of Edinburgh/NDNS Meat Trends - General/Results/Days plot.png", plot, width = 6, height = 6)


#WORKS DON'T TOUCH - with fitted lines
#Occasions
m2 <- svyglm(avgProcessedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
m3 <- svyglm(avgRedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
m4 <- svyglm(avgWhiteokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)

# Predict fitted values for each model
survey_years <- unique(dat.design$variables$SurveyYear)
predictions <- data.frame(
  SurveyYear = rep(survey_years, 3),
  Category = factor(rep(c("avgProcessedokaj", "avgRedokaj", "avgWhiteokaj"), each = length(survey_years))),
  PredictedDays = c(predict(m2, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m3, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m4, newdata = data.frame(SurveyYear = survey_years), type = "response"))
)

# Create a custom color palette using the darker colors from the "PuBuGn" palette
color_palette <- c("#FDAE61", "#ABD9E9", "#D53E4F") #order: processed (orange), white (blue), red (red)

# Create a custom factor level order based on the correct order
predictions$Category <- factor(predictions$Category, levels = c("avgProcessedokaj", "avgWhiteokaj", "avgRedokaj"))

# Update the category names with proper spacing
levels(predictions$Category) <- c("Processed", "White", "Red")

# Create a combined plot with ggplot2 using the custom color palette, scatter points, and connected lines
plot <- ggplot(predictions, aes(x = SurveyYear, y = PredictedDays, color = Category, group = Category)) +
  geom_point(size = 1) +
  geom_line() +
  geom_smooth(method = "glm", se = FALSE, linetype = "dashed", aes(group = Category)) + #this adds the fitted line
  scale_color_manual(values = color_palette) +
  labs(x = "Survey Year", y = "Number of meat-containing occasions/day", color = "Meat category") +
  theme_classic() +
  theme(text = element_text(family = "Avenir", size = 12))

# Print the plot
print(plot)

ggsave("~/University of Edinburgh/NDNS Meat Trends - General/Results/Occasions plot.png", plot, width = 6, height = 6)




#DON'T TOUCH THIS WORKS -- with fitted lines
#portion size
m2 <- svyglm(gperokajProcessed ~ SurveyYear, family=poisson(link = "log"), dat.design)
m3 <- svyglm(gperokajRed ~ SurveyYear, family=poisson(link = "log"), dat.design)
m4 <- svyglm(gperokajWhite ~ SurveyYear, family=poisson(link = "log"), dat.design)

# Predict fitted values for each model
survey_years <- unique(dat.design$variables$SurveyYear)
predictions <- data.frame(
  SurveyYear = rep(survey_years, 3),
  Category = factor(rep(c("gperokajProcessed", "gperokajRed", "gperokajWhite"), each = length(survey_years))),
  PredictedDays = c(predict(m2, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m3, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m4, newdata = data.frame(SurveyYear = survey_years), type = "response"))
)

# Create a custom color palette using the darker colors from the "PuBuGn" palette
color_palette <- c("#ABD9E9", "#D53E4F", "#FDAE61") #order: white (blue), red (red), processed (orange)

# Create a custom factor level order based on the correct order
predictions$Category <- factor(predictions$Category, levels = c("gperokajWhite", "gperokajRed", "gperokajProcessed"))

# Update the category names with proper spacing
levels(predictions$Category) <- c("White", "Red", "Processed")

# Create a combined plot with ggplot2 using the custom color palette, scatter points, and connected lines
plot <- ggplot(predictions, aes(x = SurveyYear, y = PredictedDays, color = Category, group = Category)) +
  geom_point(size = 1) +
  geom_line() +
  geom_smooth(method = "glm", se = FALSE, linetype = "dashed", aes(group = Category)) + #this adds the fitted line
  scale_color_manual(values = color_palette) +
  labs(x = "Survey Year", y = "Portion size (g)/meat-containing occasion", color = "Meat category") +
  theme_classic() +
  theme(text = element_text(family = "Avenir", size = 12))

# Print the plot
print(plot)


ggsave("~/University of Edinburgh/NDNS Meat Trends - General/Results/Portion size plot.png", plot, width = 6, height = 6)





















###################3SEPARATE PLOTS WITH HUGE FONT FOR POSTER###################





# Set font family and font size
font_size <- 24
font_family <- "Avenir"

# MeatDays
m2 <- svyglm(ProcessedDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
m3 <- svyglm(RedDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
m4 <- svyglm(WhiteDays ~ SurveyYear, family=poisson(link = "log"), dat.design)
m5 <- svyglm(NoMeatDays ~ SurveyYear, family=poisson(link = "log"), dat.design)

# Predict fitted values for each model
survey_years <- unique(dat.design$variables$SurveyYear)
predictions <- data.frame(
  SurveyYear = rep(survey_years, 4),
  Category = factor(rep(c("ProcessedDays", "RedDays", "WhiteDays", "NoMeatDays"), each = length(survey_years))),
  PredictedDays = c(predict(m2, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m3, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m4, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m5, newdata = data.frame(SurveyYear = survey_years), type = "response"))
)

# Create a custom color palette using the darker colors from the "PuBuGn" palette
color_palette <- c("#FDAE61", "#ABD9E9", "#D53E4F", "#41AB5D") #order: processed (orange), white (blue), red (red), no meat (green)

# Create a custom factor level order based on the correct order
predictions$Category <- factor(predictions$Category, levels = c("ProcessedDays", "WhiteDays", "RedDays", "NoMeatDays"))

# Update the category names with proper spacing
levels(predictions$Category) <- c("Processed", "White", "Red", "No Meat")

# Create a combined plot with ggplot2 using the custom color palette, scatter points, and connected lines
plot <- ggplot(predictions, aes(x = SurveyYear, y = PredictedDays, color = Category, group = Category)) +
  geom_point(size = 1) +
  geom_line() +
  geom_smooth(method = "glm", se = FALSE, linetype = "dashed", aes(group = Category)) +
  scale_color_manual(values = color_palette) +
  labs(title = "Meat days/4-day diary period",
       x = "Survey Year",
       y = "No. days",
       color = "Category") +
  theme_classic(base_size = font_size, base_family = font_family) +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") # add this line to hide the legend

# Print the plot
print(plot)

ggsave("~/University of Edinburgh/NDNS Meat Trends - General/Results/Days plot.png", plot, width = 8, height = 10)




#Occasions
m2 <- svyglm(avgProcessedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
m3 <- svyglm(avgRedokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)
m4 <- svyglm(avgWhiteokaj ~ SurveyYear, family=poisson(link = "log"), dat.design)

# Predict fitted values for each model
survey_years <- unique(dat.design$variables$SurveyYear)
predictions <- data.frame(
  SurveyYear = rep(survey_years, 3),
  Category = factor(rep(c("avgProcessedokaj", "avgRedokaj", "avgWhiteokaj"), each = length(survey_years))),
  PredictedDays = c(predict(m2, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m3, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m4, newdata = data.frame(SurveyYear = survey_years), type = "response"))
)

# Create a custom color palette using the darker colors from the "PuBuGn" palette
color_palette <- c("#FDAE61", "#ABD9E9", "#D53E4F") #order: processed (orange), white (blue), red (red)

# Create a custom factor level order based on the correct order
predictions$Category <- factor(predictions$Category, levels = c("avgProcessedokaj", "avgWhiteokaj", "avgRedokaj"))

# Update the category names with proper spacing
levels(predictions$Category) <- c("Processed meat", "White meat", "Red meat")

# Create a combined plot with ggplot2 using the custom color palette, scatter points, and connected lines
plot <- ggplot(predictions, aes(x = SurveyYear, y = PredictedDays, color = Category, group = Category)) +
  geom_point(size = 1) +
  geom_line() +
  geom_smooth(method = "glm", se = FALSE, linetype = "dashed", aes(group = Category)) + 
  scale_color_manual(values = color_palette) +
  labs(title = "Meat-eating occasions/day",
       x = "Survey Year",
       y = "No. occasions",
       color = "Category") +
  theme_classic(base_size = font_size, base_family = font_family) +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") # add this line to hide the legend

# Print the plot
print(plot)

ggsave("~/University of Edinburgh/NDNS Meat Trends - General/Results/Occasions plot.png", plot, width = 8, height = 10)






#portion size
m2 <- svyglm(gperokajProcessed ~ SurveyYear, family=poisson(link = "log"), dat.design)
m3 <- svyglm(gperokajRed ~ SurveyYear, family=poisson(link = "log"), dat.design)
m4 <- svyglm(gperokajWhite ~ SurveyYear, family=poisson(link = "log"), dat.design)

# Predict fitted values for each model
survey_years <- unique(dat.design$variables$SurveyYear)
predictions <- data.frame(
  SurveyYear = rep(survey_years, 3),
  Category = factor(rep(c("gperokajProcessed", "gperokajRed", "gperokajWhite"), each = length(survey_years))),
  PredictedDays = c(predict(m2, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m3, newdata = data.frame(SurveyYear = survey_years), type = "response"),
                    predict(m4, newdata = data.frame(SurveyYear = survey_years), type = "response"))
)

# Create a custom color palette using the darker colors from the "PuBuGn" palette
color_palette <- c("#ABD9E9", "#D53E4F", "#FDAE61") #order: white (blue), red (red), processed (orange)

# Create a custom factor level order based on the correct order
predictions$Category <- factor(predictions$Category, levels = c("gperokajWhite", "gperokajRed", "gperokajProcessed"))

# Update the category names with proper spacing
levels(predictions$Category) <- c("White meat", "Red meat", "Processed meat")

# Create a combined plot with ggplot2 using the custom color palette, scatter points, and connected lines
plot <- ggplot(predictions, aes(x = SurveyYear, y = PredictedDays, color = Category, group = Category)) +
  geom_point(size = 1) +
  geom_line() +
  geom_smooth(method = "glm", se = FALSE, linetype = "dashed", aes(group = Category)) + 
  scale_color_manual(values = color_palette) +
  labs(title = "Portion size/meat-containing occasion",
       x = "Survey Year",
       y = "Portion size (g)",
       color = "Category") +
  theme_classic(base_size = font_size, base_family = font_family) +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") # add this line to hide the legend

# Print the plot
print(plot)

ggsave("~/University of Edinburgh/NDNS Meat Trends - General/Results/Portion size plot.png", plot, width = 8, height = 10)








###################more plots sandboxing##################3





# Define data
data <- data.frame(
  Category = c("Portion Size", "Days", "Occasions"),
  Percentage = c(57, 37, 6)
)

# Create the bar chart
ggplot(data, aes(x = Category, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("tomato", "steelblue", "gold")) +
  labs(
    title = "Reductions in Meat Consumption",
    x = "Factors",
    y = "Percentage"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


















coef(m1)
ggcoef(m1)
ggplot(data = m1,aes(x=MeatDays,y=SurveyYear))+
  geom_point(size=0.5)+
  geom_smooth(method = "lm",formula = y~x)

plot(allEffects(m1), lines = list(multiline = TRUE),
     confint = list(style = "auto"))

mz1 <- plot(allEffects(m1), selection = 1)
mz2 <- plot(allEffects(m2), selection = 1)

library(GGally)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
imap_dfr(allEffects(m1), ~ as_tibble(.x) %>% 
           mutate(dname = grep("d_", names(.), value = TRUE)) %>%
           select(dname, dvalue = starts_with('d_'), grade, fit) %>%
           mutate(grp = .y)) %>%
  unite(dname, dname, dvalue, sep=" = ") %>% 
  ggplot(aes(x = grade, y = fit, color = dname)) +
  geom_line() +
  theme_bw() #+
# facet_wrap(~ grp)

grid.arrange( mz1, mz2,  nrow=1,  ncol=2)



















###############TESTS######################
#####################DISTRIBUTION PLOTS###########################
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















#################KEEP JUST IN CASE NEED AGAIN? NON WEIGHTED DEMOGRAPHICS###############

#age (30.06 yr)
mean(dat$Age)
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
table(dat$AgeG)
#gender (M = 7072, F = 8260)
table(dat$Sex)
#income breakdown (1= 4,299, 2= 4,285, 3= 4,258, 12,842 total; 1,861 missing)
table(dat$eqv)
