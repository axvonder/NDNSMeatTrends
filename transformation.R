library(chron)
library(tidyverse)
library(janitor)
#set wd
setwd("/Users/alexandervonderschmidt/Library/CloudStorage/OneDrive-SharedLibraries-UniversityofEdinburgh/NDNS Meat Trends - General/Data")
#upload datasets (raw diet data from NDNS; access instructions in publication)
file_names <- c("ndns_rp_yr1-4a_foodleveldietarydata_uk_v2.csv",
                "ndns_rp_yr5-6a_foodleveldietarydata_v2.csv",
                "ndns_rp_yr7-8a_foodleveldietarydata.csv",
                "ndns_rp_yr9a_foodleveldietarydata_uk_20210831.csv",
                "ndns_rp_yr10a_foodleveldietarydata_uk_20210831.csv",
                "ndns_rp_yr11a_foodleveldietarydata_uk_20210831.csv")
# Combine datasets
data_list <- lapply(file_names, read.csv)
combined_data <- Reduce(function(x, y) merge(x, y, all = TRUE), data_list)
#save final merged dataset
write.csv(combined_data,"/Users/alexandervonderschmidt/Library/CloudStorage/OneDrive-SharedLibraries-UniversityofEdinburgh/NDNS Meat Trends - General/Data/combfoodlev.csv", row.names = F)
##################### COMBINING DATA CHECKPOINT #################################
#read in combined dataset (years 1 - 11 food level data)
ult <- read.csv('combfoodlev.csv')
#combine two age variables (age variable switched from "Age" to "AgeR" in years 7 & 8)
ult$Age <- ifelse(is.na(ult$Age), ult$AgeR, ult$Age)
#delete AgeR (so that only 1 age variable remains)
ult <- ult[ , -which(names(ult) %in% c("AgeR"))]
#change survey year to just numbers
for (year_num in 1:11) {
  ult$SurveyYear <- ifelse(ult$SurveyYear == paste("NDNS Year", year_num), year_num, ult$SurveyYear)
}
table(ult$SurveyYear)
#convert mealtime to usable time variable
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
    (MealTime >= 21600 & MealTime <= 39600) ~ 1,     # 6:00 AM - 11:00 AM
    (MealTime >= 43200 & MealTime <= 54000) ~ 2,     # 12:00 PM - 3:00 PM
    (MealTime >= 57600 & MealTime <= 82800) ~ 3      # 4:00 PM - 11:00 PM
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
dat$itemContainProcessed <- as.integer(rowSums(dat[, c("ProcessedRedMeatg",
                                                       "ProcessedPoultryg",
                                                       "Burgersg", "Sausagesg")] > 0) > 0)
dat$itemContainRed <- as.integer(rowSums(dat[, c("Beefg", "Lambg", "Porkg",
                                                 "OtherRedMeatg", "Offalg")] > 0) > 0)
dat$itemContainWhite <- as.integer(rowSums(dat[, c("Poultryg", "GameBirdsg")] > 0) > 0)
dat$itemContainNoMeat <- as.integer(rowSums(dat[, c("ProcessedRedMeatg", "ProcessedPoultryg",
                                                    "Burgersg", "Sausagesg", "Beefg",
                                                    "Lambg", "Porkg", "OtherRedMeatg",
                                                    "Offalg", "Poultryg", "GameBirdsg")] > 0) == 0)
# Calculate itemMixedMeat and itemContainMeat
dat$itemMixedMeat <- as.integer((dat$itemContainProcessed & (dat$itemContainRed | dat$itemContainWhite)) |
                                  (dat$itemContainRed & (dat$itemContainProcessed | dat$itemContainWhite)) |
                                  (dat$itemContainWhite & (dat$itemContainProcessed | dat$itemContainRed)))
dat$itemContainMeat <- as.integer(dat$itemContainProcessed | dat$itemContainRed | dat$itemContainWhite)
#check counts
table(dat$itemContainProcessed)
table(dat$itemContainRed)
table(dat$itemContainWhite)
table(dat$itemMixedMeat)
table(dat$itemContainMeat)
#create singular "meat type" variable
#ONLY for meat items that have ONE meat type. Mixed meat items will remain unclassified (-99)
#Unclassified, mixed meat items, n = 3,566 (4.1%)
#0 = no meat, 1 = processed meat, 2 = unprocessed red meat, 3 = unprocessed white meat
dat$itemMeatType <- case_when(
  dat$itemContainNoMeat == 1 ~ 0,
  dat$itemContainProcessed == 1 & dat$itemMixedMeat == 0 ~ 1,
  dat$itemContainRed == 1 & dat$itemMixedMeat == 0 ~ 2,
  dat$itemContainWhite == 1 & dat$itemMixedMeat == 0 ~ 3,
  TRUE ~ -99
)
table(dat$itemMeatType)
#counts total number of food items consumed per day
#calculate and clean total items per day
calc_total_items_per_day <- function(data, day) {
  zz <- data[data$DayNo == day, c("seriali", "Sex")]
  zz_day <- as_tibble(zz %>% group_by(seriali, Sex) %>% mutate(!!paste0("Day", day, "TotalNo") := n()))
  zz_day_clean <- zz_day %>% distinct(seriali, .keep_all = TRUE)
  return(zz_day_clean[, c(1, 3)])
}
#calculate total items per day and merge with dat
for (day in 1:4) {
  zz_day_clean <- calc_total_items_per_day(dat, day)
  dat <- merge(dat, zz_day_clean, by = "seriali", all.x = TRUE)
}
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
column_names <- c("Beefg", "Lambg", "Porkg", "ProcessedRedMeatg", "OtherRedMeatg",
                  "Burgersg", "Sausagesg", "Offalg", "Poultryg", "ProcessedPoultryg",
                  "GameBirdsg", "TotalGrams", "Energykcal")
for (col in column_names) {
  okaj <- dat %>% group_by(seriali, DayNo, EatingOkaj) %>% summarise(!!paste0("okaj", col) := sum(!!sym(col)))
  dat <- merge.data.frame(okaj, dat, all = TRUE)
}
#add variable for "occasion contains [type] meat"
dat <- dat %>%
  mutate(
    okajContainProcessed = case_when(
      (okajProcessedRedMeatg > 0 | okajProcessedPoultryg > 0 | okajBurgersg > 0 | okajSausagesg > 0) ~ 1,
      TRUE ~ 0
    ),
    okajContainRed = case_when(
      (okajBeefg > 0 | okajLambg > 0 | okajPorkg > 0 | okajOtherRedMeatg > 0 | okajOffalg > 0) ~ 1,
      TRUE ~ 0
    ),
    okajContainWhite = case_when(
      (okajPoultryg > 0 | okajGameBirdsg > 0) ~ 1,
      TRUE ~ 0
    ),
    okajContainNoMeat = case_when(
      (okajProcessedRedMeatg == 0 & okajProcessedPoultryg == 0 & okajBurgersg == 0 &
         okajSausagesg == 0 & okajBeefg == 0 & okajLambg == 0 & okajPorkg == 0 &
         okajOtherRedMeatg == 0 & okajOffalg == 0 & okajPoultryg == 0 &
         okajGameBirdsg == 0) ~ 1,
      TRUE ~ 0
    ),
    okajMixedMeat = case_when(
      ((okajContainProcessed == 1 & (okajContainRed == 1 | okajContainWhite == 1)) |
         (okajContainRed == 1 & (okajContainProcessed == 1 | okajContainWhite == 1)) |
         (okajContainWhite == 1 & (okajContainProcessed == 1 | okajContainRed == 1))) ~ 1,
      TRUE ~ 0
    ),
    okajContainMeat = case_when(
      (okajProcessedRedMeatg > 0 | okajProcessedPoultryg > 0 | okajBurgersg > 0 |
         okajSausagesg > 0 | okajBeefg > 0 | okajLambg > 0 | okajPorkg > 0 |
         okajOtherRedMeatg > 0 | okajOffalg > 0 | okajPoultryg > 0 | okajGameBirdsg > 0) ~ 1,
      TRUE ~ 0
    )
  )
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
                                                 "Fruitg", "DriedFruitg", "FruitJuiceg",
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
#create variable for number of meat-eating meal blocks per day (breakfast, lunch, dinner)
#breakfast
ph <- dat %>% 
  #grouping by ID
  group_by(seriali, DayNo, MealBlock) %>%
  #select only breakfast meals - n = 58,697 breakfast occasions
  filter('1' %in% MealBlock) %>%
  ungroup() %>%
  group_by(seriali) %>%
  #create variable for each meat type by meal block
  mutate(BProcessedokaj = sum(okajContainProcessed)) %>%
  mutate(BRedokaj = sum(okajContainRed)) %>%
  mutate(BWhiteokaj = sum(okajContainWhite)) %>%
  mutate(BMeatokaj = sum(okajContainMeat)) %>%
  mutate(BNoMeatokaj = sum(okajContainNoMeat)) %>%
  mutate(Btotokaj = sum(new_meal)) %>%
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
  #create variable for each meat type by meal block
  mutate(LProcessedokaj = sum(okajContainProcessed)) %>%
  mutate(LRedokaj = sum(okajContainRed)) %>%
  mutate(LWhiteokaj = sum(okajContainWhite)) %>%
  mutate(LMeatokaj = sum(okajContainMeat)) %>%
  mutate(LNoMeatokaj = sum(okajContainNoMeat)) %>%
  mutate(Ltotokaj = sum(new_meal)) %>%
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
  #create variable for each meat type by meal block
  mutate(DProcessedokaj = sum(okajContainProcessed)) %>%
  mutate(DRedokaj = sum(okajContainRed)) %>%
  mutate(DWhiteokaj = sum(okajContainWhite)) %>%
  mutate(DMeatokaj = sum(okajContainMeat)) %>%
  mutate(DNoMeatokaj = sum(okajContainNoMeat)) %>%
  mutate(Dtotokaj = sum(new_meal)) %>%
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
  mutate(totNoMeatokaj = sum(okajNoMeatperday))
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
       Btotokaj, Ltotokaj, Dtotokaj) %>%
  fill(BProcessedokaj, BProcessedokaj, BRedokaj, BWhiteokaj, BMeatokaj, BokajGrams,
       BNoMeatokaj, BsumProcessedg, BsumRedg, BsumWhiteg, BsumMeatg, LProcessedokaj,
       LProcessedokaj, LRedokaj, LWhiteokaj, LMeatokaj, LNoMeatokaj, LsumProcessedg,
       LsumRedg, LsumWhiteg, LsumMeatg, LokajGrams, DProcessedokaj, DProcessedokaj, DRedokaj,
       DWhiteokaj, DMeatokaj, DNoMeatokaj, DsumProcessedg, DsumRedg, DsumWhiteg, DokajGrams,
       DsumMeatg, totProcessedokaj, totRedokaj, totWhiteokaj, totMeatokaj, totNoMeatokaj,
       Btotokaj, Ltotokaj, Dtotokaj,
       .direction = "up")
#only 1 row per participant
dat <- dat %>%
  #only unique IDs
  distinct(seriali, .keep_all = TRUE)
#create an avg meat occasions per meat-eating day variable
dat <- dat %>%
  mutate(avgProcessedokaj = if_else(ProcessedDays > 0, totProcessedokaj/ProcessedDays, 0)) %>%
  mutate(avgRedokaj = if_else(RedDays > 0, totRedokaj/RedDays, 0)) %>%
  mutate(avgWhiteokaj = if_else(WhiteDays > 0, totWhiteokaj/WhiteDays, 0)) %>%
  mutate(avgMeatokaj = if_else(MeatDays > 0, totMeatokaj/MeatDays, 0))
#create variable for avg (across all days) g per meat category per corresponding meat occasion
dat <- dat %>%
  mutate(gperokajProcessed = (sumProcessedg/totProcessedokaj)) %>%
  mutate(gperokajRed = (sumRedg/totRedokaj)) %>%
  mutate(gperokajWhite = (sumWhiteg/totWhiteokaj)) %>%
  mutate(gperokajMeat = (sumMeatg/totMeatokaj))
#create variable for avg g of meat consumed per day
dat <- dat %>%
  mutate(sumMeatgdaily = sumMeatg/DiaryDaysCompleted) %>%
  mutate(sumProcessedgdaily = sumProcessedg/DiaryDaysCompleted) %>%
  mutate(sumRedgdaily = sumRedg/DiaryDaysCompleted) %>%
  mutate(sumWhitegdaily = sumWhiteg/DiaryDaysCompleted)
#Keep only necessary variables
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
                                        "sumRedg", "sumWhiteg",
                                        "BokajGrams", "LokajGrams", "DokajGrams",
                                        "Btotokaj", "Ltotokaj", "Dtotokaj",
                                        "sumMeatgdaily", "sumProcessedgdaily",
                                        "sumRedgdaily", "sumWhitegdaily",
                                        "totProcessedokaj", "totRedokaj", "totMeatokaj", "totWhiteokaj"))]
#write data to dataset
write.csv(final,"/Users/alexandervonderschmidt/Library/CloudStorage/OneDrive-SharedLibraries-UniversityofEdinburgh/NDNS Meat Trends - General/Data/final.csv", row.names = F)
















































###CHECKS###

#look at distribution of meat occasions
# Filter for non-zero meat consumption
test <- dat %>%
  filter(Meatgperokaj > 0)

# Convert MealTime to hours from seconds for easier interpretation
test$MealTime <- test$MealTime / 3600

# Print the range of MealTime
print(range(test$MealTime))

# Assign meal blocks
test <- test %>%
  mutate(MealBlock = case_when(
    (MealTime >= 6 & MealTime <= 11) ~ "Breakfast",
    (MealTime >= 12 & MealTime <= 15) ~ "Lunch",
    (MealTime >= 16 & MealTime <= 23) ~ "Dinner",
    TRUE ~ "Other"
  ))

# Calculate the proportion of eating occasions for each meal block
proportion <- test %>%
  group_by(MealBlock) %>%
  summarise(Proportion = n() / nrow(test) * 100)

# Print the proportion of eating occasions for each meal block
print(proportion)
# Create a histogram of eating occasions throughout the day with meal blocks overlaid
ggplot(test, aes(x = MealTime)) +
  geom_histogram(bins = 24, fill = "lightblue", color = "black") +
  geom_vline(xintercept = c(6, 11, 12, 15, 16, 23), linetype = "dashed", color = "red", size = 1) +
  labs(x = "Time of day (hours)", y = "Number of eating occasions",
       title = "Distribution of eating occasions throughout the day") +
  theme_minimal()

# Calculate the total meat consumption for each meal block
total_consumption <- test %>%
  group_by(MealBlock) %>%
  summarise(TotalConsumption = sum(Meatgperokaj))

# Calculate the total meat consumption
total_meat_consumption <- sum(test$Meatgperokaj)

# Calculate the proportion of total consumption for each meal block
total_consumption <- total_consumption %>%
  mutate(ProportionOfTotal = TotalConsumption / total_meat_consumption * 100)

# Print the proportion of total consumption for each meal block
print(total_consumption)















