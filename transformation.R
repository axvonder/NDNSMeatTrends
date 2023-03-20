library(chron)
library(tidyverse)
library(lubridate)
library(janitor)
library(dplyr)
#set wd
setwd("/Users/alexandervonderschmidt/Library/CloudStorage/OneDrive-SharedLibraries-UniversityofEdinburgh/NDNS Meat Trends - General/Data")
#upload datasets
yr1_4 <- read.csv('ndns_rp_yr1-4a_foodleveldietarydata_uk_v2.csv')
yr5_6 <- read.csv('ndns_rp_yr5-6a_foodleveldietarydata_v2.csv')
yr7_8 <- read.csv('ndns_rp_yr7-8a_foodleveldietarydata.csv')
yr9 <- read.csv('ndns_rp_yr9a_foodleveldietarydata_uk_20210831.csv')
yr10 <- read.csv('ndns_rp_yr10a_foodleveldietarydata_uk_20210831.csv')
yr11 <- read.csv('ndns_rp_yr11a_foodleveldietarydata_uk_20210831.csv')
#combine datasets
pair1 <- merge.data.frame(yr1_4, yr5_6, all = TRUE)
pair2 <- merge.data.frame(yr7_8, yr9, all = TRUE)
pair3 <- merge.data.frame(yr10, yr11, all = TRUE)
pentult <- merge.data.frame(pair1, pair2, all = TRUE)
ult <- merge.data.frame(pentult, pair3, all = TRUE)
#save final merged dataset
write.csv(ult,"/Users/alexandervonderschmidt/Library/CloudStorage/OneDrive-SharedLibraries-UniversityofEdinburgh/NDNS Meat Trends - General/Data/combfoodlev.csv", row.names = F)
##################### COMBINING DATA CHECKPOINT #################################
#read in combined dataset (years 1 - 11 food level data)
ult <- read.csv('combfoodlev.csv')
#combine two age variables (age variable switched from "Age" to "AgeR" in years 7 & 8)
ult$Age <- ifelse(is.na(ult$Age), ult$AgeR, ult$Age)
#delete AgeR
ult <- ult[ , -which(names(ult) %in% c("AgeR"))]
#change survey year to just numbers
ult$SurveyYear <- ifelse((ult$SurveyYear == "NDNS Year 1"), 1, ult$SurveyYear)
ult$SurveyYear <- ifelse((ult$SurveyYear == "NDNS Year 2"), 2, ult$SurveyYear)
ult$SurveyYear <- ifelse((ult$SurveyYear == "NDNS Year 3"), 3, ult$SurveyYear)
ult$SurveyYear <- ifelse((ult$SurveyYear == "NDNS Year 4"), 4, ult$SurveyYear)
ult$SurveyYear <- ifelse((ult$SurveyYear == "NDNS Year 5"), 5, ult$SurveyYear)
ult$SurveyYear <- ifelse((ult$SurveyYear == "NDNS Year 6"), 6, ult$SurveyYear)
ult$SurveyYear <- ifelse((ult$SurveyYear == "NDNS Year 7"), 7, ult$SurveyYear)
ult$SurveyYear <- ifelse((ult$SurveyYear == "NDNS Year 8"), 8, ult$SurveyYear)
ult$SurveyYear <- ifelse((ult$SurveyYear == "NDNS Year 9"), 9, ult$SurveyYear)
ult$SurveyYear <- ifelse((ult$SurveyYear == "NDNS Year 10"), 10, ult$SurveyYear)
ult$SurveyYear <- ifelse((ult$SurveyYear == "NDNS Year 11"), 11, ult$SurveyYear)
table(ult$SurveyYear)
#convert to useable time variable
time<- c(ult$MealTime)
ult$MealTime <- chron(times = time)
#check
options(max.print=1000000)
table(ult$MealTime)
#convert all times to seconds (easier for me to organize)
ult$MealTime <- as.numeric(hms(ult$MealTime))
#definte "breakfast" "lunch" and "dinner"
ult <- ult %>%
  mutate(MealBlock = case_when(
    (MealTime >= 21600 & MealTime <= 36000) ~ 1,
    (MealTime >= 37800 & MealTime <= 52200) ~ 2,
    (MealTime >= 59400 & MealTime <= 77400) ~ 3,
  ))
#define eating occasions
ult <- ult %>%
  #grouping by ID and Day
  group_by(seriali,DayNo) %>%
  #Sorting by ID and Mealtime - this gets n_food variable in the right order
  arrange(seriali, DayNo, MealTime) %>% 
  # A handy bit of code that lets you count observations within group (specified above with group_by)
  mutate(n_food=ave(1:length(seriali), seriali, FUN = seq_along)) %>%
  #creating a variable for difference in mealtime. Lag referes to meal time in last observation above
  mutate(diff = MealTime - lag(MealTime)) %>% 
  # this was a trick because I couldnt figure out how to use lag within case_when or ifelse. Hopefully nothing is actually coded as 99 but should check
  mutate(new_meal=case_when(
    n_food==1 ~ 1,
    diff>=3600 ~ 1, 
    diff<3600 ~ 0,
    TRUE ~ 99
  )) %>% 
  #new variable which is cumulative sum of the trick variable created above
  mutate(EatingOkaj=cumsum(new_meal)) %>%
  ungroup()
#cut out unnecessary variables for our analyses
trim <- ult[ , -which(names(ult) %in% c("CoreBoost", "DiaryDate", "MealTimeDescription",
                                        "MainFoodGroupCode", "MainFoodGroupDesc",
                                        "SubFoodGroupCode", "SubFoodGroupDesc", "RecipeName",
                                        "RecipeMainFoodGroupCode", "RecipeMainFoodGroupDesc",
                                        "RecipeSubFoodGroupCode", "RecipeSubFoodGroupDesc",
                                        "EnergykJ", "Othersugarsg", "Starchg", "Glucoseg",
                                        "Fructoseg", "Sucroseg", "Maltoseg", "Lactoseg",
                                        "Nonmilkextrinsicsugarsg", "Intrinsicandmilksugarsg",
                                        "FreeSugars", "Englystfibreg"))]
dat <- trim
#add variable for "item contains [type] meat"
dat$itemContainProcessed <- ifelse((dat$ProcessedRedMeatg > 0 | dat$ProcessedPoultryg > 0 |
                                  dat$Burgersg > 0 | dat$Sausagesg > 0), 1, 0)
dat$itemContainRed <- ifelse((dat$Beefg > 0 | dat$Lambg > 0 | dat$Porkg > 0 |
                            dat$OtherRedMeatg > 0 | dat$Offalg > 0), 1, 0)
dat$itemContainWhite <- ifelse((dat$Poultryg > 0 | dat$GameBirdsg > 0), 1, 0)
dat$itemContainNoMeat <- ifelse((dat$ProcessedRedMeatg == 0 & dat$ProcessedPoultryg == 0 &
                               dat$Burgersg == 0 & dat$Sausagesg == 0 &
                               dat$Beefg == 0 & dat$Lambg == 0 & dat$Porkg == 0 &
                               dat$OtherRedMeatg == 0 & dat$Offalg == 0 &
                               dat$Poultryg == 0 & dat$GameBirdsg == 0), 1, 0)
dat$itemMixedMeat <- ifelse((dat$itemContainProcessed == 1 & (dat$itemContainRed == 1 |
                                                         dat$itemContainWhite == 1)) |
                            (dat$itemContainRed == 1 & (dat$itemContainProcessed == 1 |
                                                      dat$itemContainWhite == 1)) |
                           (dat$itemContainWhite == 1 & (dat$itemContainProcessed == 1 |
                                                       dat$itemContainRed == 1)), 1, 0)
dat$itemContainMeat <- ifelse((dat$ProcessedRedMeatg > 0 | dat$ProcessedPoultryg > 0 |
                             dat$Burgersg > 0 | dat$Sausagesg > 0 |
                             dat$Beefg > 0 | dat$Lambg > 0 | dat$Porkg > 0 |
                             dat$OtherRedMeatg > 0 | dat$Offalg > 0 |
                             dat$Poultryg > 0 | dat$GameBirdsg > 0), 1, 0)
table(dat$itemContainProcessed)
table(dat$itemContainRed)
table(dat$itemContainWhite)
table(dat$itemMixedMeat)
table(dat$itemContainMeat)
#create singular "meat type" variable
#ONLY for meat items that have ONE meat type. Mixed meat items will remain unclassified (-99)
#Unclassified, mixed meat items, n = 3,566
# 0 = no meat, 1 = processed meat, 2 = unprocessed red meat, 3 = unprocessed white meat
dat$itemMeatType <- -99
dat$itemMeatType <- ifelse(dat$itemContainNoMeat == 1, dat$itemMeatType == 0, dat$itemMeatType)
dat$itemMeatType <- ifelse((dat$itemMeatType == -99 & dat$itemContainProcessed == 1 &
                          dat$itemMixedMeat == 0), 1, dat$itemMeatType)
dat$itemMeatType <- ifelse((dat$itemMeatType == -99 & dat$itemContainRed == 1 &
                          dat$itemMixedMeat == 0), 2, dat$itemMeatType)
dat$itemMeatType <- ifelse((dat$itemMeatType == -99 & dat$itemContainWhite == 1 &
                          dat$itemMixedMeat == 0), 3, dat$itemMeatType)
table(dat$itemMeatType)
#counts total number of food items consumed per day
zz <- dat[dat$DayNo == 1, c("seriali", "Sex")]
zz1 <- as_tibble(zz %>% group_by(seriali, Sex) %>% mutate(Day1TotalNo = n()))
zz <- dat[dat$DayNo == 2, c("seriali", "Sex")]
zz2 <- as_tibble(zz %>% group_by(seriali, Sex) %>% mutate(Day2TotalNo = n()))
zz <- dat[dat$DayNo == 3, c("seriali", "Sex")]
zz3 <- as_tibble(zz %>% group_by(seriali, Sex) %>% mutate(Day3TotalNo = n()))
zz <- dat[dat$DayNo == 4, c("seriali", "Sex")]
zz4 <- as_tibble(zz %>% group_by(seriali, Sex) %>% mutate(Day4TotalNo = n()))
#isolate 'total items' count column (with id column)
zz1mod <- zz1[,c(1,3)]
zz1clean <- zz1mod %>% distinct(seriali, .keep_all = TRUE)
zz2mod <- zz2[,c(1,3)]
zz2clean <- zz2mod %>% distinct(seriali, .keep_all = TRUE)
zz3mod <- zz3[,c(1,3)]
zz3clean <- zz3mod %>% distinct(seriali, .keep_all = TRUE)
zz4mod <- zz4[,c(1,3)]
zz4clean <- zz4mod %>% distinct(seriali, .keep_all = TRUE)
#add 'total items' variable
dat <- merge(dat, zz1clean, by = "seriali", all.x = TRUE)
dat <- merge(dat, zz2clean, by = "seriali", all.x = TRUE)
dat <- merge(dat, zz3clean, by = "seriali", all.x = TRUE)
dat <- merge(dat, zz4clean, by = "seriali", all.x = TRUE)
#categorize a day as processed-meat-containing, red-meat-containing, white-meat-containing, no-meat-containing
dat <- dat %>%
  #group by ID & day number
  group_by(seriali, DayNo) %>%
  arrange(seriali, DayNo, MealTime) %>%
  #count up all meat items consumed per day by meat type
  mutate(ProcessedSum = sum(itemContainProcessed)) %>%
  mutate(RedSum = sum(itemContainRed)) %>%
  mutate(WhiteSum = sum(itemContainWhite)) %>%
  mutate(MeatSum = sum(itemContainMeat)) %>%
  #binary did day contain meat type variable
  mutate(anyProcessed = case_when(
    ProcessedSum > 0 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(anyRed = case_when(
    RedSum > 0 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(anyWhite = case_when(
    WhiteSum > 0 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(anyMeat = case_when(
    MeatSum > 0 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(anyNoMeat = case_when(
    anyMeat == 0 ~ 1,
    TRUE ~ 0
  ))
placeholder <- dat %>%
  #count number of meat type days
  distinct(seriali, .keep_all = TRUE) %>%
  #regroup by ID only
  ungroup() %>%
  group_by(seriali) %>%
  #count number of meat type days
  mutate(ProcessedDays = sum(anyProcessed)) %>%
  mutate(RedDays = sum(anyRed)) %>%
  mutate(WhiteDays = sum(anyWhite)) %>%
  mutate(MeatDays = sum(anyMeat)) %>%
  mutate(NoMeatDays = sum(anyNoMeat))
dat <- merge.data.frame(placeholder, dat, all = TRUE)
dat <- dat %>%
  #group by ID
  group_by(seriali) %>%
  fill(ProcessedDays, RedDays, WhiteDays, MeatDays, NoMeatDays) %>%
  fill(ProcessedDays, RedDays, WhiteDays, MeatDays, NoMeatDays, .direction = "up")
#sum food(g) information by eating occasion
okaj <- dat %>% group_by(seriali, DayNo, EatingOkaj) %>% summarise(okajBeefg = sum(Beefg))
dat <- merge.data.frame(okaj, dat, all = TRUE)
okaj <- dat %>% group_by(seriali, DayNo, EatingOkaj) %>% summarise(okajLambg = sum(Lambg))
dat <- merge.data.frame(okaj, dat, all = TRUE)
okaj <- dat %>% group_by(seriali, DayNo, EatingOkaj) %>% summarise(okajPorkg = sum(Porkg))
dat <- merge.data.frame(okaj, dat, all = TRUE)
okaj <- dat %>% group_by(seriali, DayNo, EatingOkaj) %>% summarise(okajProcessedRedMeatg = sum(ProcessedRedMeatg))
dat <- merge.data.frame(okaj, dat, all = TRUE)
okaj <- dat %>% group_by(seriali, DayNo, EatingOkaj) %>% summarise(okajOtherRedMeatg = sum(OtherRedMeatg))
dat <- merge.data.frame(okaj, dat, all = TRUE)
okaj <- dat %>% group_by(seriali, DayNo, EatingOkaj) %>% summarise(okajBurgersg = sum(Burgersg))
dat <- merge.data.frame(okaj, dat, all = TRUE)
okaj <- dat %>% group_by(seriali, DayNo, EatingOkaj) %>% summarise(okajSausagesg = sum(Sausagesg))
dat <- merge.data.frame(okaj, dat, all = TRUE)
okaj <- dat %>% group_by(seriali, DayNo, EatingOkaj) %>% summarise(okajOffalg = sum(Offalg))
dat <- merge.data.frame(okaj, dat, all = TRUE)
okaj <- dat %>% group_by(seriali, DayNo, EatingOkaj) %>% summarise(okajPoultryg = sum(Poultryg))
dat <- merge.data.frame(okaj, dat, all = TRUE)
okaj <- dat %>% group_by(seriali, DayNo, EatingOkaj) %>% summarise(okajProcessedPoultryg = sum(ProcessedPoultryg))
dat <- merge.data.frame(okaj, dat, all = TRUE)
okaj <- dat %>% group_by(seriali, DayNo, EatingOkaj) %>% summarise(okajGameBirdsg = sum(GameBirdsg))
dat <- merge.data.frame(okaj, dat, all = TRUE)
okaj <- dat %>% group_by(seriali, DayNo, EatingOkaj) %>% summarise(okajTotalGrams = sum(TotalGrams))
dat <- merge.data.frame(okaj, dat, all = TRUE)
okaj <- dat %>% group_by(seriali, DayNo, EatingOkaj) %>% summarise(okajEnergykcal = sum(Energykcal))
dat <- merge.data.frame(okaj, dat, all = TRUE)
#add variable for "occasion contains [type] meat"
dat <- dat %>%
  mutate(okajContainProcessed = case_when(
    (okajProcessedRedMeatg >0 | okajProcessedPoultryg >0 | okajBurgersg >0 | okajSausagesg >0) ~ 1,
    TRUE ~ 0
  ))
dat <- dat %>%
  mutate(okajContainRed = case_when(
    (okajBeefg >0 | okajLambg >0 | okajPorkg >0 | okajOtherRedMeatg >0 | okajOffalg >0) ~ 1,
    TRUE ~ 0
  ))
dat <- dat %>%
  mutate(okajContainWhite = case_when(
    (okajPoultryg >0 | okajGameBirdsg >0) ~ 1,
    TRUE ~ 0
  ))
dat <- dat %>%
  mutate(okajContainNoMeat = case_when(
    (okajProcessedRedMeatg == 0 & okajProcessedPoultryg == 0 & okajBurgersg == 0 &
       okajSausagesg == 0 & okajBeefg == 0 & okajLambg == 0 & okajPorkg == 0 &
       okajOtherRedMeatg == 0 & okajOffalg == 0 & okajPoultryg == 0 &
       okajGameBirdsg == 0) ~ 1,
    TRUE ~ 0
  ))
dat <- dat %>%
  mutate(okajMixedMeat = case_when(
    ((okajContainProcessed == 1 & (okajContainRed == 1 | okajContainWhite == 1)) |
      (okajContainRed == 1 & (okajContainProcessed == 1 | okajContainWhite == 1)) |
      (okajContainWhite == 1 & (okajContainProcessed == 1 | okajContainRed == 1))) ~ 1,
    TRUE ~ 0
  ))
dat <- dat %>%
  mutate(okajContainMeat = case_when(
    (okajProcessedRedMeatg > 0 | okajProcessedPoultryg > 0 | okajBurgersg > 0 |
       okajSausagesg > 0 | okajBeefg > 0 | okajLambg > 0 | okajPorkg > 0 |
       okajOtherRedMeatg > 0 | okajOffalg > 0 | okajPoultryg > 0 | okajGameBirdsg > 0) ~ 1,
    TRUE ~ 0
  ))
#transform dataset to go from per item (current) to per occasion
trans <- dat[(dat$new_meal == 1), ]
trans <- trans %>%
  #grouping by ID and Day
  group_by(seriali,DayNo) %>%
  #Sorting by ID and Mealtime - this gets n_food variable in the right order
  arrange(seriali, DayNo, MealTime)
#do a little checkypoo on the data
tes <- trans[, c("seriali", "DayNo", "MealTime", "diff", "new_meal")]
#get rid of 'per-item' variables (slims down dataset, makes it easier to work with)
transtrim <- trans[ , -which(names(trans) %in% c("FoodNumber", "FoodName", "Energykcal",
                                                 "Proteing", "Fatg", "Carbohydrateg", "TotalGrams",
                                                 "Sodiummg", "Potassiummg", "Calciummg",
                                                 "Magnesiummg", "Phosphorusmg", "Ironmg",
                                                 "Haemironmg", "Nonhaemironmg", "Coppermg",
                                                 "Zincmg", "Chloridemg", "Retinol.µg", "Thiaminmg",
                                                 "Totalcarotene.µg", "Alphacarotene.µg",
                                                 "Betacarotene.µg", "Betacryptoxanthin.µg",
                                                 "VitaminAretinolequivalents.µg", "VitaminD.µg",
                                                 "VitaminEmg", "Riboflavinmg", "Niacinequivalentmg",
                                                 "VitaminB6mg", "VitaminB12.µg", "Folate.µg",
                                                 "Pantothenicacidmg", "Biotin.µg", "VitaminCmg",
                                                 "Alcoholg", "Waterg", "Totalsugarsg",
                                                 "FreeSugarsg", "AOACFibreg", "Totalnitrogeng",
                                                 "Manganesemg", "Iodine.µg", "Selenium.µg",
                                                 "Cholesterolmg", "Saturatedfattyacidsg",
                                                 "CisMonounsaturatedfattyacidsg", "Cisn6fattyacidsg",
                                                 "Cisn3fattyacidsg", "Transfattyacidsg",
                                                 "Fruitg", "FriedFruitg", "FruitJuiceg",
                                                 "SmoothieFruitg", "Tomatoesg", "TomatoPureeg",
                                                 "Brassicaceaeg", "YellowRedGreeng", "Beansg",
                                                 "Nutsg", "OtherVegg", "Beefg", "Lambg",
                                                 "Porkg", "ProcessedRedMeatg", "OtherRedMeatg",
                                                 "Burgersg", "Sausagesg", "Offalg", "Poultryg",
                                                 "ProcessedPoultryg", "GameBirdsg", "WhiteFishg",
                                                 "OilyFishg", "CannedTunag", "Shellfishg",
                                                 "CottageCheeseg", "CheddarCheeseg", "OtherCheeseg"))]
#remove all eating occasions that have less than 50 kcal (n = 68,407; no participants lost)
datokaj <- transtrim[(transtrim$okajEnergykcal >= 50), ]
#re-create eating occasion numbers after removing all occasions that don't meet 50 kcal requirement
datokaj <- datokaj %>%
  #grouping by ID and Day
  group_by(seriali,DayNo) %>%
  #Sorting by ID and Mealtime - this gets n_food variable in the right order
  arrange(seriali, DayNo, MealTime) %>% 
  # A handy bit of code that lets you count observations within group (specified above with group_by)
  mutate(n_food=ave(1:length(seriali), seriali, FUN = seq_along)) %>%
  #creating a variable for difference in mealtime. Lag referes to meal time in last observation above
  mutate(diff = MealTime - lag(MealTime)) %>% 
  # this was a trick because I couldnt figure out how to use lag within case_when or ifelse. Hopefully nothing is actually coded as 99 but should check
  mutate(new_meal=case_when(
    n_food==1 ~ 1,
    diff>=3600 ~ 1, 
    diff<3600 ~ 0,
    TRUE ~ 99
  )) %>% 
  #new variable which is cumulative sum of the trick variable created above
  mutate(EatingOkaj=cumsum(new_meal)) %>%
  ungroup()
dat <- datokaj
#create variables for number of meat-containing occasions per day (per meat type)
dat <- dat %>%
  #grouping by ID and Day
  group_by(seriali, DayNo) %>%
  #Sorting by ID and Mealtime
  arrange(seriali, DayNo, MealTime) %>% 
  #count observations within group (specified above with group_by)
  mutate(okajProcessedperday = cumsum(okajContainProcessed)) %>%
  mutate(okajRedperday = cumsum(okajContainRed)) %>%
  mutate(okajWhiteperday = cumsum(okajContainWhite)) %>%
  mutate(okajMeatperday = cumsum(okajContainMeat)) %>%
  mutate(okajNoMeatperday = cumsum(okajContainNoMeat)) %>%
  # max value per day
  mutate(okajProcessedperday = max(okajProcessedperday)) %>%
  mutate(okajRedperday = max(okajRedperday)) %>%
  mutate(okajWhiteperday = max(okajWhiteperday)) %>%
  mutate(okajMeatperday = max(okajMeatperday)) %>%
  mutate(okajNoMeatperday = max(okajNoMeatperday))
#create variables for g meat per occasion
dat <- dat %>%
  rowwise() %>%
  #add meat per occasion total g
  mutate(Processedgperokaj = sum(okajProcessedRedMeatg, okajProcessedPoultryg,
                                okajBurgersg, okajSausagesg)) %>%
  mutate(Redgperokaj = sum(okajBeefg, okajLambg, okajPorkg, okajOtherRedMeatg,
                           okajOffalg)) %>%
  mutate(Whitegperokaj = sum(okajPoultryg, okajGameBirdsg)) %>%
  mutate(Meatgperokaj = sum(okajProcessedRedMeatg, okajProcessedPoultryg,
                            okajBurgersg, okajSausagesg, okajBeefg, okajLambg,
                            okajPorkg, okajOtherRedMeatg, okajOffalg, okajPoultryg,
                            okajGameBirdsg))
#prep for avg g of meat per occasion variable
dat <- dat %>%
  #grouping by ID
  group_by(seriali) %>%
  #sum all occasions of meat consumption
  mutate(sumProcessedg = sum(Processedgperokaj)) %>%
  mutate(sumRedg = sum(Redgperokaj)) %>%
  mutate(sumWhiteg = sum(Whitegperokaj)) %>%
  mutate(sumMeatg = sum(Meatgperokaj))
#split data real quick to form 'occasions per meat day' variables
ph <- dat %>%
  #select only meat-containing days
  filter('1' %in% anyMeat) %>%
  #group
  group_by(seriali, DayNo) %>%
  #Sorting by ID and Mealtime
  arrange(seriali, DayNo, MealTime) %>%
  #count observations within group (specified above with group_by)
  mutate(okajMeatperMeatday = cumsum(okajContainMeat)) %>%
  # max value per day
  mutate(okajMeatperMeatday = max(okajMeatperday))
dat <- merge.data.frame(ph, dat, all = TRUE)
ph <- dat %>%
  #select only meat-containing days
  filter('1' %in% anyProcessed) %>%
  #group
  group_by(seriali, DayNo) %>%
  #Sorting by ID and Mealtime
  arrange(seriali, DayNo, MealTime) %>%
  #count observations within group (specified above with group_by)
  mutate(okajProcessedperProcessedday = cumsum(okajContainProcessed)) %>%
  # max value per day
  mutate(okajProcessedperProcessedday = max(okajProcessedperProcessedday))
dat <- merge.data.frame(ph, dat, all = TRUE)
ph <- dat %>%
  #select only meat-containing days
  filter('1' %in% anyRed) %>%
  #group
  group_by(seriali, DayNo) %>%
  #Sorting by ID and Mealtime
  arrange(seriali, DayNo, MealTime) %>%
  #count observations within group (specified above with group_by)
  mutate(okajRedperRedday = cumsum(okajContainRed)) %>%
  # max value per day
  mutate(okajRedperRedday = max(okajRedperRedday))
dat <- merge.data.frame(ph, dat, all = TRUE)
ph <- dat %>%
  #select only meat-containing days
  filter('1' %in% anyWhite) %>%
  #group
  group_by(seriali, DayNo) %>%
  #Sorting by ID and Mealtime
  arrange(seriali, DayNo, MealTime) %>%
  #count observations within group (specified above with group_by)
  mutate(okajWhiteperWhiteday = cumsum(okajContainWhite)) %>%
  # max value per day
  mutate(okajWhiteperWhiteday = max(okajWhiteperWhiteday))
dat <- merge.data.frame(ph, dat, all = TRUE)
#create variable for number of meat-eating meal blocks per day (breakfast, lunch, dinner)
#breakfast
ph <- dat %>% 
  #grouping by ID
  group_by(seriali, DayNo, MealBlock) %>%
  #select only breakfast meals - n = 58,697 breakfast occasions
  filter('1' %in% MealBlock) %>%
  ungroup() %>%
  group_by(seriali) %>%
  #create variable for each meat type averaged by meal block
  mutate(BProcessedokaj = sum(okajContainProcessed)) %>%
  mutate(BRedokaj = sum(okajContainRed)) %>%
  mutate(BWhiteokaj = sum(okajContainWhite)) %>%
  mutate(BMeatokaj = sum(okajContainMeat)) %>%
  mutate(BNoMeatokaj = sum(okajContainNoMeat)) %>%
  mutate(BsumProcessedg = sum(Processedgperokaj)) %>%
  mutate(BsumRedg = sum(Redgperokaj)) %>%
  mutate(BsumWhiteg = sum(Whitegperokaj)) %>%
  mutate(BsumMeatg = sum(Meatgperokaj)) %>%
  mutate(BokajGrams = mean(okajTotalGrams, na.rm = TRUE))
dat <- merge.data.frame(ph, dat, all = TRUE)
#lunch
ph <- dat %>% 
  #grouping by ID
  group_by(seriali, DayNo, MealBlock) %>%
  #select only breakfast meals - n = 80,282 lunch occasions
  filter('2' %in% MealBlock) %>%
  ungroup() %>%
  group_by(seriali) %>%
  #create variable for each meat type averaged by meal block
  mutate(LProcessedokaj = sum(okajContainProcessed)) %>%
  mutate(LRedokaj = sum(okajContainRed)) %>%
  mutate(LWhiteokaj = sum(okajContainWhite)) %>%
  mutate(LMeatokaj = sum(okajContainMeat)) %>%
  mutate(LNoMeatokaj = sum(okajContainNoMeat)) %>%
  mutate(LsumProcessedg = sum(Processedgperokaj)) %>%
  mutate(LsumRedg = sum(Redgperokaj)) %>%
  mutate(LsumWhiteg = sum(Whitegperokaj)) %>%
  mutate(LsumMeatg = sum(Meatgperokaj)) %>%
  mutate(LokajGrams = mean(okajTotalGrams, na.rm = TRUE))
dat <- merge.data.frame(ph, dat, all = TRUE)
#dinner
ph <- dat %>% 
  #grouping by ID
  group_by(seriali, DayNo, MealBlock) %>%
  #select only breakfast meals - n = 94,259 lunch occasions
  filter('3' %in% MealBlock) %>%
  ungroup() %>%
  group_by(seriali) %>%
  #create variable for each meat type averaged by meal block
  mutate(DProcessedokaj = sum(okajContainProcessed)) %>%
  mutate(DRedokaj = sum(okajContainRed)) %>%
  mutate(DWhiteokaj = sum(okajContainWhite)) %>%
  mutate(DMeatokaj = sum(okajContainMeat)) %>%
  mutate(DNoMeatokaj = sum(okajContainNoMeat)) %>%
  mutate(DsumProcessedg = sum(Processedgperokaj)) %>%
  mutate(DsumRedg = sum(Redgperokaj)) %>%
  mutate(DsumWhiteg = sum(Whitegperokaj)) %>%
  mutate(DsumMeatg = sum(Meatgperokaj)) %>%
  mutate(DokajGrams = mean(okajTotalGrams, na.rm = TRUE))
dat <- merge.data.frame(ph, dat, all = TRUE)
#create a variable for the total number of meat occasions per day across the diary days
ph <- dat %>%
  #group by ID and Day number, select only 1 meal per day
  group_by(seriali, DayNo) %>%
  filter(EatingOkaj == 1) %>%
  ungroup() %>%
  #just do ID grouping
  group_by(seriali) %>%
  mutate(totProcessedokaj = sum(okajProcessedperday)) %>%
  mutate(totRedokaj = sum(okajRedperday)) %>%
  mutate(totWhiteokaj = sum(okajWhiteperday)) %>%
  mutate(totMeatokaj = sum(okajMeatperday)) %>%
  mutate(totNoMeatokaj = sum(okajNoMeatperday)) %>%
  mutate(totMeatokajpmd = sum(okajMeatperMeatday)) %>%
  mutate(totProcessedokajpmd = sum(okajProcessedperProcessedday)) %>%
  mutate(totRedokajpmd = sum(okajRedperRedday)) %>%
  mutate(totWhiteokajpmd = sum(okajWhiteperWhiteday))
dat <- merge.data.frame(ph, dat, all = TRUE)
#create variables for meal block portion sizes
#fill in the NAs with surrounding data (for the B/L/D variables and the totalokaj variable)
dat <- dat %>%
  #group by ID
  group_by(seriali) %>%
  #fill in variables that can/should be identical across the occasions (sumnmnarized variables)
  fill(BProcessedokaj, BProcessedokaj, BRedokaj, BWhiteokaj, BMeatokaj, BokajGrams,
       BNoMeatokaj, BsumProcessedg, BsumRedg, BsumWhiteg, BsumMeatg, LProcessedokaj,
       LProcessedokaj, LRedokaj, LWhiteokaj, LMeatokaj, LNoMeatokaj, LsumProcessedg,
       LsumRedg, LsumWhiteg, LsumMeatg, LokajGrams, DProcessedokaj, DProcessedokaj, DRedokaj,
       DWhiteokaj, DMeatokaj, DNoMeatokaj, DsumProcessedg, DsumRedg, DsumWhiteg, DokajGrams,
       DsumMeatg, totProcessedokaj, totRedokaj, totWhiteokaj, totMeatokaj, totNoMeatokaj,
       totMeatokajpmd, totProcessedokajpmd, totRedokajpmd, totWhiteokajpmd) %>%
  fill(BProcessedokaj, BProcessedokaj, BRedokaj, BWhiteokaj, BMeatokaj, BokajGrams,
       BNoMeatokaj, BsumProcessedg, BsumRedg, BsumWhiteg, BsumMeatg, LProcessedokaj,
       LProcessedokaj, LRedokaj, LWhiteokaj, LMeatokaj, LNoMeatokaj, LsumProcessedg,
       LsumRedg, LsumWhiteg, LsumMeatg, LokajGrams, DProcessedokaj, DProcessedokaj, DRedokaj,
       DWhiteokaj, DMeatokaj, DNoMeatokaj, DsumProcessedg, DsumRedg, DsumWhiteg, DokajGrams,
       DsumMeatg, totProcessedokaj, totRedokaj, totWhiteokaj, totMeatokaj, totNoMeatokaj,
       totMeatokajpmd, totProcessedokajpmd, totRedokajpmd, totWhiteokajpmd,
       .direction = "up")
#only 1 row per participant
dat <- dat %>%
  #only unique IDs
  distinct(seriali, .keep_all = TRUE)
#create an avg meat occasions per day variable
dat <- dat %>%
  mutate(avgProcessedokaj = totProcessedokaj/DiaryDaysCompleted) %>%
  mutate(avgRedokaj = totRedokaj/DiaryDaysCompleted) %>%
  mutate(avgWhiteokaj = totWhiteokaj/DiaryDaysCompleted) %>%
  mutate(avgMeatokaj = totMeatokaj/DiaryDaysCompleted) %>%
  mutate(avgNoMeatokaj = totNoMeatokaj/DiaryDaysCompleted) %>%
  mutate(avgMeatokajpmd = totMeatokajpmd/MeatDays) %>%
  mutate(avgProcessedokajpmd = totProcessedokajpmd/ProcessedDays) %>%
  mutate(avgRedokajpmd = totRedokajpmd/RedDays) %>%
  mutate(avgWhiteokajpmd = totWhiteokajpmd/WhiteDays)
#create variable for avg (across all days) g per meat category per corresponding meat occasion
dat <- dat %>%
  mutate(gperokajProcessed = (sumProcessedg/totProcessedokaj)) %>%
  mutate(gperokajRed = (sumRedg/totRedokaj)) %>%
  mutate(gperokajWhite = (sumWhiteg/totWhiteokaj)) %>%
  mutate(gperokajMeat = (sumMeatg/totMeatokaj))
#create variable for avg g per meat category per corresponding meat occasion for SMTs (breakfast, lunch, dinner)
dat <- dat %>%
  mutate(BgperokajProcessed = (BsumProcessedg/BProcessedokaj)) %>%
  mutate(BgperokajRed = (BsumRedg/BRedokaj)) %>%
  mutate(BgperokajWhite = (BsumWhiteg/BWhiteokaj)) %>%
  mutate(BgperokajMeat = (BsumMeatg/BMeatokaj))
dat <- dat %>%
  mutate(LgperokajProcessed = (LsumProcessedg/LProcessedokaj)) %>%
  mutate(LgperokajRed = (LsumRedg/LRedokaj)) %>%
  mutate(LgperokajWhite = (LsumWhiteg/LWhiteokaj)) %>%
  mutate(LgperokajMeat = (LsumMeatg/LMeatokaj))
dat <- dat %>%
  mutate(DgperokajProcessed = (DsumProcessedg/DProcessedokaj)) %>%
  mutate(DgperokajRed = (DsumRedg/DRedokaj)) %>%
  mutate(DgperokajWhite = (DsumWhiteg/DWhiteokaj)) %>%
  mutate(DgperokajMeat = (DsumMeatg/DMeatokaj))
#remove all unnecessary variables
final <- dat[ , which(names(dat) %in% c("seriali", "SurveyYear", "Sex", "Country",
                                        "DiaryDaysCompleted", "Age", "ProcessedDays",
                                        "RedDays", "WhiteDays", "MeatDays", "NoMeatDays",
                                        "okajTotalGrams", "okajperday", "BProcessedokaj",
                                        "BRedokaj", "BWhiteokaj", "BMeatokaj",
                                        "BNoMeatokaj", "BsumProcessedg", "BsumRedg",
                                        "BsumWhiteg", "BsumMeatg", "LProcessedokaj",
                                        "LRedokaj", "LWhiteokaj", "LMeatokaj",
                                        "LNoMeatokaj", "LsumProcessedg", "LsumRedg",
                                        "LsumWhiteg", "LsumMeatg", "DProcessedokaj",
                                        "DRedokaj", "DWhiteokaj", "DMeatokaj",
                                        "DNoMeatokaj", "DsumProcessedg", "DsumRedg",
                                        "DsumWhiteg", "DsumMeatg", "avgProcessedokaj",
                                        "avgRedokaj", "avgWhiteokaj", "avgMeatokaj",
                                        "avgNoMeatokaj", "okajProcessedperc", "okajRedperc",
                                        "okajWhiteperc", "okajMeatperc", "okajNoMeatperc",
                                        "gperokajProcessed", "gperokajRed", "gperokajWhite",
                                        "gperokajMeat", "sumMeatg", "sumProcessedg",
                                        "sumRedg", "sumWhiteg", "BgperokajProcessed",
                                        "BgperokajRed", "BgperokajWhite", "BgperokajMeat",
                                        "LgperokajProcessed", "LgperokajRed", "LgperokajWhite",
                                        "LgperokajMeat", "DgperokajProcessed", "DgperokajRed",
                                        "DgperokajWhite", "DgperokajMeat",
                                        "BokajGrams", "LokajGrams", "DokajGrams",
                                        "avgMeatokajpmd", "avgProcessedokajpmd",
                                        "avgRedokajpmd", "avgWhiteokajpmd"))]
#write data to dataset
write.csv(final,"/Users/alexandervonderschmidt/Library/CloudStorage/OneDrive-SharedLibraries-UniversityofEdinburgh/NDNS Meat Trends - General/Data/final.csv", row.names = F)


















