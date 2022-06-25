library(tidyverse) #the universe of useful tidy packages (includes dplyr)
library(stringr) #will use for parsing character data
library(skimr) #has a useful summary function called skim for a quick look at the data
library(dplyr)

#setwd("C:/Users/p075313//Desktop/XU MSCA/BAIS 660 - Statistical Programming/assignments - final project")


##### Load Datasets #####
#read original data from web-hosted shared link
data_train_0 <- read_csv("https://cheetahanalytics.com/finalexamdata_training")
data_predict_0 <- read_csv("https://cheetahanalytics.com/finalexamdata_prediction")

#we created two additional random samples for training and prediction for the purpose of testing our final model
#these will be used after we're finished fitting the model
data_test1_train <- read_csv("https://cheetahanalytics.com/finalexamdata_test1_train")
data_test1_predict <- read_csv("https://cheetahanalytics.com/finalexamdata_test1_predict")
data_test2_train <- read_csv("https://cheetahanalytics.com/finalexamdata_test2_train")
data_test2_predict <- read_csv("https://cheetahanalytics.com/finalexamdata_test2_predict")


##### Choose Dataset for Model #####
#specify which data set we're going to use to fit the model
#note that test1 and test2 data sets are just for testing our model predictions, 
#  similar to what Joel will do with his prediction data set, but shouldn't be used
#  to fit the final model that we submit.

data_train <- data_train_0
data_predict <- data_predict_0


##### Set Data Types #####
#convert columns in training dataset to appropriate data types
data_train$AVG_COST_ATTEND <- as.numeric(data_train$AVG_COST_ATTEND)
data_train$STABBR <- as.factor(data_train$STABBR)
data_train$MAIN <- as.factor(data_train$MAIN)
data_train$ICLEVEL <- as.factor(data_train$ICLEVEL)
data_train$HIGHDEG <- as.factor(data_train$HIGHDEG)
data_train$CONTROL <- as.factor(data_train$CONTROL)
data_train$OPENADMP <- as.factor(data_train$OPENADMP)
data_train$ADM_RATE <- as.numeric(data_train$ADM_RATE)
data_train$SAT_AVG <- as.numeric(data_train$SAT_AVG)
data_train$UGDS_NRA <- as.numeric(data_train$UGDS_NRA)
data_train$AVGFACSAL <- as.numeric(data_train$AVGFACSAL)
data_train$PCTPELL <- as.numeric(data_train$PCTPELL)
data_train$PCTFLOAN <- as.numeric(data_train$PCTFLOAN)
data_train$MD_FAMINC <- as.numeric(data_train$MD_FAMINC)
data_train$UGDS <- as.numeric(data_train$UGDS)
data_train$UGNONDS <- as.numeric(data_train$UGNONDS)
data_train$GRADS <- as.numeric(data_train$GRADS)
data_train$APPL_SCH_PCT_GE5 <- as.numeric(data_train$APPL_SCH_PCT_GE5)

#convert columns in prediction dataset to appropriate data types
data_predict$STABBR <- as.factor(data_predict$STABBR)
data_predict$MAIN <- as.factor(data_predict$MAIN)
data_predict$ICLEVEL <- as.factor(data_predict$ICLEVEL)
data_predict$HIGHDEG <- as.factor(data_predict$HIGHDEG)
data_predict$CONTROL <- as.factor(data_predict$CONTROL)
data_predict$OPENADMP <- as.factor(data_predict$OPENADMP)
data_predict$ADM_RATE <- as.numeric(data_predict$ADM_RATE)
data_predict$SAT_AVG <- as.numeric(data_predict$SAT_AVG)
data_predict$UGDS_NRA <- as.numeric(data_predict$UGDS_NRA)
data_predict$AVGFACSAL <- as.numeric(data_predict$AVGFACSAL)
data_predict$PCTPELL <- as.numeric(data_predict$PCTPELL)
data_predict$PCTFLOAN <- as.numeric(data_predict$PCTFLOAN)
data_predict$MD_FAMINC <- as.numeric(data_predict$MD_FAMINC)
data_predict$UGDS <- as.numeric(data_predict$UGDS)
data_predict$UGNONDS <- as.numeric(data_predict$UGNONDS)
data_predict$GRADS <- as.numeric(data_predict$GRADS)
data_predict$APPL_SCH_PCT_GE5 <- as.numeric(data_predict$APPL_SCH_PCT_GE5)


##### Create User-defined Variables #####

#SAT_AVG: need to derive an indicator {0,1} variable where SAT scores are missing
data_train$SAT_FLAG <- as.factor(ifelse(is.na(data_train$SAT_AVG) == TRUE, 0, 1))
data_predict$SAT_FLAG <- as.factor(ifelse(is.na(data_predict$SAT_AVG) == TRUE, 0, 1))

#The "CONTROL" variable is extremely important because it separates private (for profit vs nonprofit)
#and public universities, which have dramatically different costs to attend.
#We create a dummy variable coding for this 3-level factor variable:
data_train$type_public <- as.factor(ifelse(data_train$CONTROL == 1, 1, 0))
data_train$type_private_forprofit <- as.factor(ifelse(data_train$CONTROL == 3, 1, 0))
data_predict$type_public <- as.factor(ifelse(data_predict$CONTROL == 1, 1, 0))
data_predict$type_private_forprofit <- as.factor(ifelse(data_predict$CONTROL == 3, 1, 0))

#The UGDS_NRA variable measures the percentage of Non-Resident Alien enrollment in the university.
#(aka "International enrollment"). We determined this variable to be much more significant than the
# other demographic variables in the original dataset. However, more than half of all records (over 13,000)
# report 0 NRA enrollment, so we created an NRA_FLAG dummy variable to separate the two groups.
# Those with no NRA enrollment and those with NRA enrollment. We believe that magnitude of the
# NRA enrollment is significant (when it exists) as a continuous variable.
data_train$NRA_FLAG <- as.factor(ifelse(data_train$UGDS_NRA == 0, 0, 1))
data_predict$NRA_FLAG <- as.factor(ifelse(data_predict$UGDS_NRA == 0, 0, 1))

#The "ICLEVEL" indicates whether an institution is 4-year, 2-year, or less. There doesn't appear to be
# a major difference in costs between the "2-year" and "less than 2-year" institutions, so we're going
# to introduce a simpler "4-year institution" dummy variable to replace this "ICLEVEL" variable.
#Note that this variable is likely correlated with the "HIGHDEG" variable, so we should revisit before
#submitting final model.
data_train$flag_4year <- as.factor(ifelse(data_train$ICLEVEL == 1, 1, 0))
data_predict$flag_4year <- as.factor(ifelse(data_predict$ICLEVEL == 1, 1, 0))

# HIGHDEG is an important variable because it differentiates institutes based on the highest
# level degree offered, which is known to be associated with cost to attend. We recode the HIGHDEG variable
# so that its levels are more-intuitive for the reader.
data_train$HIGHDEG <- as.factor(ifelse((data_train$HIGHDEG == 0 | data_train$HIGHDEG == "NONDEG"), "NONDEG", 
                                ifelse((data_train$HIGHDEG == 1 | data_train$HIGHDEG == "CERT"), "CERT",
                                ifelse((data_train$HIGHDEG == 2 | data_train$HIGHDEG == "ASSC"),  "ASSC",
                                ifelse((data_train$HIGHDEG == 3 | data_train$HIGHDEG == "BACH"),  "BACH",
                                ifelse((data_train$HIGHDEG == 4 | data_train$HIGHDEG == "GRAD"),  "GRAD", 
                                "ERROR"))))))

data_predict$HIGHDEG <- as.factor(ifelse((data_predict$HIGHDEG == 0 | data_predict$HIGHDEG == "NONDEG"), "NONDEG", 
                                ifelse((data_predict$HIGHDEG == 1 | data_predict$HIGHDEG == "CERT"), "CERT",
                                ifelse((data_predict$HIGHDEG == 2 | data_predict$HIGHDEG == "ASSC"),  "ASSC",
                                ifelse((data_predict$HIGHDEG == 3 | data_predict$HIGHDEG == "BACH"),  "BACH",
                                ifelse((data_predict$HIGHDEG == 4 | data_predict$HIGHDEG == "GRAD"),  "GRAD", 
                                "ERROR"))))))

#create a total_enrollment variable. Note that enrollment doesn't appear to be highly correlated overall,
#until you drill down on the different types of institutions. For example, instutions only offering Associates
#degrees or lower (no bachelors or grad degrees) have a correlation of 0.40 to cost to attend.
data_train$UGDS <- ifelse(is.na(data_train$UGDS) == TRUE, 0, data_train$UGDS)
data_train$UGNONDS <- ifelse(is.na(data_train$UGNONDS) == TRUE, 0, data_train$UGNONDS)
data_train$GRADS <- ifelse(is.na(data_train$GRADS) == TRUE, 0, data_train$GRADS)
data_train$total_enrollment <-  data_train$UGDS + data_train$UGNONDS + data_train$GRADS

data_predict$UGDS <- ifelse(is.na(data_predict$UGDS) == TRUE, 0, data_predict$UGDS)
data_predict$UGNONDS <- ifelse(is.na(data_predict$UGNONDS) == TRUE, 0, data_predict$UGNONDS)
data_predict$GRADS <- ifelse(is.na(data_predict$GRADS) == TRUE, 0, data_predict$GRADS)
data_predict$total_enrollment <-  data_predict$UGDS + data_predict$UGNONDS + data_predict$GRADS

#APPL_SCH_PCT_GE5 appears to be an important variable, but there are missing and suppressed values
#We created an indicator variable to handle cases where there is no GE5 percent reported (percent of applicants who applied to 5 or more schools)
data_train$APPLY_5_FLAG <- as.factor(ifelse(((is.na(data_train$APPL_SCH_PCT_GE5) == TRUE) | (data_train$APPL_SCH_PCT_GE5 == "PrivacySuppresed")), 0, 1))
data_predict$APPLY_5_FLAG <- as.factor(ifelse(((is.na(data_predict$APPL_SCH_PCT_GE5) == TRUE) | (data_predict$APPL_SCH_PCT_GE5 == "PrivacySuppresed")), 0, 1))


##### Select Variables for Model #####
#define columns that will be used in the initial model run
#note that we may need to add other columns if later we determine we missed something.

dep_var <- c("AVG_COST_ATTEND")


indep_var <- c("STABBR",
               "MAIN",
               "flag_4year",                      #THIS VARIABLE IS USER-CREATED
               "HIGHDEG",                         #recoded by user
               "type_public",
               "type_private_forprofit",
               "ADM_RATE",
               "SAT_FLAG",                        #THIS VARIABLE IS USER-CREATED
               "SAT_AVG",
               "NRA_FLAG",                        #THIS VARIABLE IS USER-CREATED
               "UGDS_NRA",
               "AVGFACSAL",
               "PCTPELL",
               "PCTFLOAN",
               "MD_FAMINC",
               "total_enrollment",                #THIS VARIABLE IS USER-CREATED
               "APPLY_5_FLAG",                    #THIS VARIABLE IS USER-CREATED; Include WITH APPL_SCH_PCT_GE5 (not instead of)
               "APPL_SCH_PCT_GE5")

#PARKING LOT: DO NOT USE THESE
                #"ICLEVEL",         #Do not use ICLEVEL if using "flag_4year" dummy variable
                #"CONTROL",         #Do not use CONTROL if using "type_public" and "type_private_forprofit" dummy variables


##### Handling of Missing Values - Basic #####

#AMD_RATE: missing values all correspond to 100% admission. Plug with 1's, not with averages
data_train$ADM_RATE <- ifelse(is.na(data_train$ADM_RATE) == TRUE, 1, data_train$ADM_RATE)
data_predict$ADM_RATE <- ifelse(is.na(data_predict$ADM_RATE) == TRUE, 1, data_predict$ADM_RATE)


##### Handling of Missing Values - Advanced - Calculate Grouped Means for Continuous Variables #####

#define grouping variables for the purpose of determining grouped means that we will use to plug missing values
#exclude continuous variables
#only use categorical variables
#this method should provide better estimates for missing values than if we were to use GRAND MEANS
grouping_var_1 <- c("STABBR",
               "MAIN",
               "flag_4year",                     
               "type_public",
               "type_private_forprofit",
               "NRA_FLAG") 

grouping_var_2 <- c("MAIN",
                    "type_public",
                    "type_private_forprofit")


var1a <- "AVGFACSAL"
group_means_1 <- data_train %>% select(grouping_var_1, var1a = var1a) %>% 
                              filter(is.na(var1a) == FALSE) %>% 
                              group_by_at(vars(-var1a)) %>%  #the next line accomplishes the same as "group_by_at(vars(...))"
                              #group_by(STABBR,MAIN,flag_4year,type_public,type_private_forprofit,SAT_FLAG,NRA_FLAG,HIGHDEG) %>% 
                              summarize(avg_var1b = mean(var1a))

group_means_2 <- data_train %>% select(grouping_var_2, var1a = var1a) %>% 
                              filter(is.na(var1a) == FALSE) %>% 
                              group_by_at(vars(-var1a)) %>%  #the next line accomplishes the same as "group_by_at(vars(...))"
                              summarize(avg_var1c = mean(var1a))

var2a <- "MD_FAMINC"
group_means_3 <- data_train %>% select(grouping_var_1, var2a = var2a) %>% 
                              filter(is.na(var2a) == FALSE) %>% 
                              group_by_at(vars(-var2a)) %>%  #the next line accomplishes the same as "group_by_at(vars(...))"
                              #group_by(STABBR,MAIN,flag_4year,type_public,type_private_forprofit,SAT_FLAG,NRA_FLAG,HIGHDEG) %>% 
                              summarize(avg_var2b = mean(var2a))

group_means_4 <- data_train %>% select(grouping_var_2, var2a = var2a) %>% 
                              filter(is.na(var2a) == FALSE) %>% 
                              group_by_at(vars(-var2a)) %>%  #the next line accomplishes the same as "group_by_at(vars(...))"
                              summarize(avg_var2c = mean(var2a))


data_train <- data_train %>% 
                      left_join(group_means_1, by = grouping_var_1) %>% 
                      left_join(group_means_2, by = grouping_var_2) %>% 
                      left_join(group_means_3, by = grouping_var_1) %>% 
                      left_join(group_means_4, by = grouping_var_2) %>% 
                      mutate(AVGFACSAL = ifelse(is.na(AVGFACSAL) == FALSE, AVGFACSAL,
                                               ifelse(is.na(avg_var1b) == FALSE, avg_var1b,
                                               ifelse(is.na(avg_var1c) == FALSE, avg_var1c)))) %>% 
                      mutate(MD_FAMINC = ifelse(is.na(MD_FAMINC) == FALSE, MD_FAMINC,
                                         ifelse(is.na(avg_var2b) == FALSE, avg_var2b,
                                         ifelse(is.na(avg_var2c) == FALSE, avg_var2c)))) %>% 
                      #select(AVGFACSAL, avg_var1b, avg_var1c, MD_FAMINC, avg_var2b, avg_var2c)  
                      select(-avg_var1b,-avg_var1c,-avg_var2b,-avg_var2c)

###REPEAT FOR PREDICTION DATA ###

data_predict <- data_predict %>% 
                     left_join(group_means_1, by = grouping_var_1) %>% 
                     left_join(group_means_2, by = grouping_var_2) %>% 
                     left_join(group_means_3, by = grouping_var_1) %>% 
                     left_join(group_means_4, by = grouping_var_2) %>% 
                     mutate(AVGFACSAL = ifelse(is.na(AVGFACSAL) == FALSE, AVGFACSAL,
                                        ifelse(is.na(avg_var1b) == FALSE, avg_var1b,
                                        ifelse(is.na(avg_var1c) == FALSE, avg_var1c)))) %>% 
                     mutate(MD_FAMINC = ifelse(is.na(MD_FAMINC) == FALSE, MD_FAMINC,
                                        ifelse(is.na(avg_var2b) == FALSE, avg_var2b,
                                        ifelse(is.na(avg_var2c) == FALSE, avg_var2c)))) %>% 
                     #select(AVGFACSAL, avg_var1b, avg_var1c, MD_FAMINC, avg_var2b, avg_var2c)
                     select(-avg_var1b,-avg_var1c,-avg_var2b,-avg_var2c)


##### Final Data for Model #####
data_train   <- data_train   %>%  select("TRAIN_ID", dep_var, indep_var)
data_predict <- data_predict %>%  select("PREDICT_ID", indep_var)


#model <- lm(AVG_COST_ATTEND~STABBR+MAIN+flag_4year+HIGHDEG+type_public+type_private_forprofit+ADM_RATE+SAT_FLAG+SAT_AVG+NRA_FLAG+UGDS_NRA+PCTPELL+PCTFLOAN+AVGFACSAL+MD_FAMINC+total_enrollment+APPLY_5_FLAG+APPL_SCH_PCT_GE5, data=data_train)
model <- lm(AVG_COST_ATTEND~STABBR+MAIN+flag_4year+HIGHDEG+type_public+type_private_forprofit+ADM_RATE+SAT_FLAG+NRA_FLAG+UGDS_NRA+PCTPELL+PCTFLOAN+AVGFACSAL+MD_FAMINC+total_enrollment+APPLY_5_FLAG, data = data_train)
summary(model)

predict.lm(model, newdata=data_predict)
data_predict$AVG_COST_ATTEND <- predict.lm(simplemodel, newdata=data_predict)

write.csv(predict.lm(simplemodel, newdata=data_predict))


VIF(model)
vif(model)

plot(data_train$ADM_RATE,data_train$AVG_COST_ATTEND)
plot(data_train$UGDS_NRA,data_train$AVG_COST_ATTEND)
plot(data_train$PCTPELL,data_train$AVG_COST_ATTEND)
plot(data_train$PCTFLOAN,data_train$AVG_COST_ATTEND)
plot(data_train$AVGFACSAL,data_train$AVG_COST_ATTEND)
plot(data_train$MD_FAMINC,data_train$AVG_COST_ATTEND)
plot(data_train$total_enrollment,data_train$AVG_COST_ATTEND)
plot(data_train$SAT_AVG,data_train$AVG_COST_ATTEND)
plot(data_train$APPL_SCH_PCT_GE5,data_train$AVG_COST_ATTEND)




crPlots(data_train$ADM_RATE,data_train$AVG_COST_ATTEND)
crPlots(data_train$UGDS_NRA,data_train$AVG_COST_ATTEND)
crPlots(data_train$PCTPELL,data_train$AVG_COST_ATTEND)
crPlots(data_train$PCTFLOAN,data_train$AVG_COST_ATTEND)
crPlots(data_train$AVGFACSAL,data_train$AVG_COST_ATTEND)
crPlots(data_train$MD_FAMINC,data_train$AVG_COST_ATTEND)
crPlots(data_train$total_enrollment,data_train$AVG_COST_ATTEND)
crPlots(data_train$SAT_AVG,data_train$AVG_COST_ATTEND)
crPlots(data_train$APPL_SCH_PCT_GE5,data_train$AVG_COST_ATTEND)



