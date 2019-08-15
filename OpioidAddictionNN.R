install.packages("reshape2")
install.packages("mltools")
install.packages("forecats")
install.packages("tabplot")
install.packages("randomForest")
install.packages("neuralnet")
install.packages("dplyr")
library(caret)
library(neuralnet)
library(randomForest)
library(tabplot)
library(forcats)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(mltools)
library(naniar)
library(data.table)

##READ IN DATA##
data <- read.csv("C:/Users/blume/Desktop/practicumII/Data/DATA1.csv", stringsAsFactors = FALSE, header = TRUE, sep = ',')

###DATA CLEANSING###
data$YEAR <- NULL #all samples from 2012
data$DAYWAIT <- NULL #irrelevant to outcome
data$ARRESTS[data$ARRESTS == '-9'] <- 0 #-9 in this case means 0 numerically
names <- c(2:30)
data[,names] <- lapply(data[,names] , factor) #convert all variables to factor and reorder

data <- data[,c(1,14,15,2,3,4,5,6,7,8,9,10,11,12,13,16,17,18,19,20,21,22,23,24,25,26,27,28,30,29)]


##ALL VALUES ARE CURRENTLY NUMERIC FACTORS, WHICH ARE NOT INTUITIVE, SO WE ARE RENAMING VALUES FOR UNDERSTANDING

data$AGE <- revalue(data$AGE, c("2"="12_14", "3"="15_17","4"="18_20", "5"="21_24", "6"="25_29", "7"="30_34", "8"="35_39", "9"="40_44", "10"="45_49", "11"="50_54", "12"="55_OR_OLDER"))
data$GENDER <- revalue(data$GENDER, c("1"="MALE", "2"="FEMALE", "-9" = "UNKNOWN"))
data$RACE <- revalue(data$RACE, c("1"="ALASKA_NATIVE","2"="AMERICAN_INDIAN", "3"="ASIAN_OR_PACIFIC_ISLANDER","4"="BLACK", "5"="WHITE", "13"="ASIAN", "20"="OTHER_SINGLE_RACE", "21"="TWO_OR_MORE_RACES", "23"="NATIVE_HAWAIIAN", "-9"="UNKNOWN"))
data$ETHNIC <- revalue(data$ETHNIC, c("1"="PUERTO_RICAN", "2"="MEXICAN", "3"="CUBAN","4"="OTHER_SPECIFIC_HISPANIC", "5"="NOT_HISPANIC", "6"="HISPANIC_NOT_SPECIFIED", "-9"="UNKNOWN"))
data$MARSTAT <- revalue(data$MARSTAT, c("1"="NEVER_MARRIED", "2"="MARRIED","3"="SEPARATED", "4"="DIVORCED", "-9"="UNKNOWN"))
data$EDUC <- revalue(data$EDUC, c("1"="LESS_HS", "2"="SOME_HS", "3"="HS","4"="SOME_COLLEGE", "5"="BACHELORS_OR_MORE", "-9"="UNKNOWN"))
data$EMPLOY <- revalue(data$EMPLOY, c("-9"="UNKNOWN", "1"="FULL_TIME", "2"="PART_TIME", "3"="UNEMPLOYED","4"="NOT_IN_LABOR_FORCE"))
data$DETNLF <- revalue(data$DETNLF, c(
  '-9'='EMPLOYED',
  '1'='HOMEMAKER',
  '2'='STUDENT',
  '3'='RETIRED_OR_DISABLED',
  '5'='INMATE_OF_INSTITUTION',
  '6'='OTHER'))
  
data$VET <- revalue(data$VET, c("-9"="UNKNOWN", "1"="VETERAN", "2"="NON_VETERAN"))
data$LIVARAG <- revalue(data$LIVARAG, c("-9"="UNKNOWN", "1"="HOMELESS", "2"="DEPENDENT_LIVING", "3"="INDEPENDENT_LIVING"))
data$PRIMINC <- revalue(data$PRIMINC, c("-9"="NONE", "1"="WAGE_OR_SALARY", "2"="PUBLIC_ASSISTANCE", "3"="RETIREMENT_OR_DISABILITY","20"="OTHER", "21"="NONE"))
data$ARRESTS <- revalue(data$ARRESTS, c("-9"="UNKNOWN", "0"="0", "1"="1", "2"="2_OR_MORE"))
data$DETCRIM <- revalue(data$DETCRIM, c("-9"="NONE", "1"="STATE_OR_FEDERAL_COURT", "3"="PROBATION_OR_PAROLE", "5"="DIVERSION_PROGRAM","6"="PRISON", "7"="DUI", "8"="OTHER_LEGAL_ENTITY"))
data$PSOURCE <- revalue(data$PSOURCE, c("-9"="UNKNOWN", "1"="INDIVIDUAL", "2"="ALCOHOL_OR_DRUG_CARE_PROVIDER", "3"="OTHER_HEALTH_PROVIDER","4"="SCHOOL", "5"="EMPLOYER","6"="OTHER_COMMUNITY_REFERRAL","7"="COURT_OR_CRIMINAL_JUSTICE"))
data$NOPRIOR <- revalue(data$NOPRIOR, c("-9"="UNKNOWN", "1"="1", "2"="2", "3"="3","4"="4", "5"="5_OR_MORE"))
data$SUB1 <- revalue(data$SUB1, c(
  '-9'='UNKNOWN',
  '1'='NONE',
  '2'='ALCOHOL',
  '3'='COCAINE_OR_CRACK',
  '4'='MARIJUANA_OR_HASHISH',
  '5'='HEROIN',
  '6'='NON_PRESCRIPTION_METHADONE',
  '7'='OTHER_OPIATES_AND_SYNTHETICS',
  '8'='PCP',
  '9'='OTHER_HALLUCINOGENS',
  '10'='METHAMPHETAMINE',
  '11'='OTHER_AMPHETAMINES',
  '12'='OTHER_STIMULANTS',
  '13'='BENZODIAZEPINES',
  '14'='OTHER_NON_BENZODIAZEPINE_TRANQUILIZERS',
  '15'='BARBITURATES',
  '16'='OTHER_NON_BARBITURATE_SEDATIVES_OR_HYPNOTICS',
  '17'='INHALANTS',
  '18'='OVER_THE_COUNTER_MEDICATIONS',
  '20'='OTHER'))
  
data$ROUTE1 <- revalue(data$ROUTE1, c("-9"="UNKNOWN", "1"="ORAL", "2"="SMOKING", "3"="INHALATION","4"="INJECTION", "20"="OTHER"))
data$FREQ1 <- revalue(data$FREQ1, c("-9"="UNKNOWN", "1"="NO_USE_IN_PAST_MONTH", "2"="1_3_TIMES_IN_THE_PAST_MONTH", "3"="1_2_TIMES_IN_THE_PAST_WEEK","4"="3_6_TIMES_IN_THE_PAST_WEEK","5"='DAILY'))
data$FRSTUSE1 <- revalue(data$FRSTUSE1, c(
  '-9'='UNKNOWN',
  '1'='11_AND_UNDER',
  '2'='12_14',
  '3'='15_17',
  '4'='18_20',
  '5'='21_24',
  '6'='25_29',
  '7'='30_34',
  '8'='35_39',
  '9'='40_44',
  '10'='45_49',
  '11'='50_54',
  '12'='55_OR_OLDER'))
  
data$SUB2 <- revalue(data$SUB2, c(
  '-9'='UNKNOWN',
  '1'='NONE',
  '2'='ALCOHOL',
  '3'='COCAINE_OR_CRACK',
  '4'='MARIJUANA_OR_HASHISH',
  '5'='HEROIN',
  '6'='NON_PRESCRIPTION_METHADONE',
  '7'='OTHER_OPIATES_AND_SYNTHETICS',
  '8'='PCP',
  '9'='OTHER_HALLUCINOGENS',
  '10'='METHAMPHETAMINE',
  '11'='OTHER_AMPHETAMINES',
  '12'='OTHER_STIMULANTS',
  '13'='BENZODIAZEPINES',
  '14'='OTHER_NON_BENZODIAZEPINE_TRANQUILIZERS',
  '15'='BARBITURATES',
  '16'='OTHER_NON_BARBITURATE_SEDATIVES_OR_HYPNOTICS',
  '17'='INHALANTS',
  '18'='OVER_THE_COUNTER_MEDICATIONS',
  '20'='OTHER'))

data$SUB3 <- revalue(data$SUB3, c(
  '-9'='UNKNOWN',
  '1'='NONE',
  '2'='ALCOHOL',
  '3'='COCAINE_OR_CRACK',
  '4'='MARIJUANA_OR_HASHISH',
  '5'='HEROIN',
  '6'='NON_PRESCRIPTION_METHADONE',
  '7'='OTHER_OPIATES_AND_SYNTHETICS',
  '8'='PCP',
  '9'='OTHER_HALLUCINOGENS',
  '10'='METHAMPHETAMINE',
  '11'='OTHER_AMPHETAMINES',
  '12'='OTHER_STIMULANTS',
  '13'='BENZODIAZEPINES',
  '14'='OTHER_NON_BENZODIAZEPINE_TRANQUILIZERS',
  '15'='BARBITURATES',
  '16'='OTHER_NON_BARBITURATE_SEDATIVES_OR_HYPNOTICS',
  '17'='INHALANTS',
  '18'='OVER_THE_COUNTER_MEDICATIONS',
  '20'='OTHER'))

data$HERFLG <- revalue(data$HERFLG, c("0"="NO", "1"="YES"))
data$METHFLG <- revalue(data$METHFLG, c("0"="NO", "1"="YES"))
data$OPSYNFLG <- revalue(data$OPSYNFLG, c("0"="NO", "1"="YES"))
data$DSMCRIT <- revalue(data$DSMCRIT, c(
  '-9'='UNKNOWN',
  '0'='NO_DIAGNOSIS',
  '1'='ALCOHOL_INDUCED_DISORDER',
  '2'='SUBSTANCE_INDUCED_DISORDER',
  '3'='ALCOHOL_INTOXICATION',
  '4'='ALCOHOL_DEPENDENCE',
  '5'='OPIOID_DEPENDENCE',
  '6'='COCAINE_DEPENDENCE',
  '7'='CANNABIS_DEPENDENCE',
  '8'='OTHER_SUBSTANCE_DEPENDENCE',
  '9'='ALCOHOL_ABUSE',
  '10'='CANNABIS_ABUSE',
  '11'='OTHER_SUBSTANCE_ABUSE',
  '12'='OPIOID_ABUSE',
  '13'='COCAINE_ABUSE',
  '14'='ANXIETY_DISORDERS',
  '15'='DEPRESSIVE_DISORDERS',
  '16'='SCHIZOPHRENIA_OTHER_PSYCHOTIC_DISORDERS',
  '17'='BIPOLAR_DISORDERS',
  '18'='ATTENTION_DEFICIT_DISRUPTIVE_BEHAVIOR_DISORDERS',
  '19'='OTHER_MENTAL_HEALTH_CONDITION',
  '20'='OTHER_CONDITION'))

data$PSYPROB <- revalue(data$PSYPROB, c("-9"="UNKNOWN", "1"="YES", "2"="NO"))

data$STFIPS <- revalue(data$STFIPS, c(
'9'='Connecticut',
'23'='Maine',
'25'='Massachusetts',
'33'='New_Hampshire',
'44'='Rhode_Island',
'50'='Vermont',
'34'='New_Jersey',
'36'='New_York',
'42'='Pennsylvania',
'17'='Illinois',
'18'='Indiana',
'26'='Michigan',
'39'='Ohio',
'55'='Wisconsin',
'19'='Iowa',
'20'='Kansas',
'27'='Minnesota',
'29'='Missouri',
'31'='Nebraska',
'38'='North_Dakota',
'46'='South_Dakota',
'10'='Delaware',
'11'='District_of_Columbia',
'12'='Florida',
'13'='Georgia',
'24'='Maryland',
'37'='North_Carolina',
'45'='South_Carolina',
'51'='Virginia',
'54'='West_Virginia',
'1'='Alabama',
'21'='Kentucky',
'28'='Mississippi',
'47'='Tennessee',
'5'='Arkansas',
'22'='Louisiana',
'40'='Oklahoma',
'48'='Texas',
'4'='Arizona',
'8'='Colorado',
'16'='Idaho',
'30'='Montana',
'32'='Nevada',
'35'='New_Mexico',
'49'='Utah',
'56'='Wyoming',
'2'='Alaska',
'6'='California',
'15'='Hawaii',
'41'='Oregon',
'53'='Washington',
'72'='Puerto_Rico'))

#Missing values cannot be imputed, so we need to remove samples with missing values for NN to work. Also helps to reduce
#data totals for reducing processing needs
data[data == 'UNKNOWN'] <- NA
data <- na.omit(data)


##CONFIRM THAT WE ARE NOT MISSING ANY DATA
#########FIND BETTER WAY########


##READ IN DATA FOR OPIOID DEATHS US##
od_rate <- read.csv("C:/Users/blume/Desktop/practicumII/Data/od_rate_us.csv", stringsAsFactors = FALSE, header = TRUE, sep = ',')
od_rate <- as.data.table(od_rate)
od_rate <- melt(od_rate)
head(od_rate)
od_ts <- ts(od_rate$value, start=c(1999), end=c(2017), frequency=1) 
plot(od_ts)

plot(od_ts, main="US Opioid Overdose Rate",
     xlab="Year", ylab="Rate per 100,000",
     col="steelblue")




##READ IN DATA FOR OPIOID DEATHS BY STATE##
od_rate_st <- read.csv("C:/Users/blume/Desktop/practicumII/Data/od_rate.csv", stringsAsFactors = FALSE, header = TRUE, sep = ',')
od_rate_st <- as.data.table(od_rate_st)
od_rate_st <- melt(od_rate_st)

names(od_rate_st) <- c("Location", "Year", "Value")
od_rate_st$Year <- as.integer(od_rate_st$Year)

ggplot(od_rate_st, aes(x = Year, y = Value)) + 
  geom_line(aes(color = Location), size = 1) +
  #scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()+
  theme(legend.position = "none") +
  labs(subtitle="", 
       y="Rate per 100,000 Deaths", 
       x="Year", 
       title="Overdose Rate by State", 
       caption = "Source: Henry J Kaiser Family Foundation")


     

#### VISUALIZATIONS ####
###THESE LOOK AT THE TOP 5 MOST COMMON ADMISSIONS REASONS. WE ARE PROVING STEREOTYPES (WHITE,MALE,HOMELESS,MH PROBLEMS)
#DATA EXPLORATTION
options(scipen=999)


ggplot(data, aes(GENDER)) +
  geom_bar() +
  theme_classic(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, vjust = 1)) +
  scale_fill_brewer( palette = "YlGnBu" ) +
  theme_minimal() + theme( legend.position = "bottom" )



ggplot(data, aes(RACE)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, vjust = 1))


tableplot(data[5:6])
tableplot(data[6:8])

#---------------------
#VISUALIZATIONS RELATIVE TO SUBSTANCE DATA
#---------------------

#percent of whole

data %>% 
  filter(DSMCRIT != "UNKNOWN") %>% 
  mutate(DSMCRIT = fct_lump(DSMCRIT, n = 10)) %>% 
  count(DSMCRIT) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(reorder(DSMCRIT, pct), pct)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer( palette = "YlGnBu" ) +
  theme_minimal() + theme( legend.position = "bottom" )


#"other" category. An easy way to do that is to use fct_lump.4 Here we use n = 5 to retain the top 5
data %>% 
  filter(SUB1 != "ALCOHOL" & SUB1 != "MARIJUANA/HASHISH") %>% 
  mutate(SUB1 = fct_lump(SUB1, n = 5)) %>% 
  count(SUB1) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(reorder(SUB1, pct), pct)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer( palette = "YlGnBu" ) +
  theme_minimal() + theme( legend.position = "bottom" )


#Lollipop chart with lines

data %>%  
  filter(PSOURCE != "NA") %>% 
  count(PSOURCE) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(pct, reorder(PSOURCE, pct))) +
  geom_point() +
  geom_segment(aes(x = 0, xend = pct, y = PSOURCE, yend = PSOURCE), size = .15)


data %>%  
  filter(EMPLOY != "NA") %>% 
  count(EMPLOY) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(pct, reorder(EMPLOY, pct))) +
  geom_point() +
  geom_segment(aes(x = 0, xend = pct, y = EMPLOY, yend = EMPLOY), size = .15)


data %>%  
  filter(LIVARAG != "NA") %>% 
  count(LIVARAG) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(pct, reorder(LIVARAG, pct))) +
  geom_point() +
  geom_segment(aes(x = 0, xend = pct, y = LIVARAG, yend = LIVARAG), size = .15)


data %>%  
  filter(RACE != "NA") %>% 
  count(RACE) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(pct, reorder(RACE, pct))) +
  geom_point() +
  geom_segment(aes(x = 0, xend = pct, y = RACE, yend = RACE), size = .15)


#---------------------
#VISUALIZATIONS RELATIVE TO OPIATES
#---------------------

use_by_drug <- data %>% 
  filter(SUB1 %in% c("OTHER_OPIATES_AND_SYNTHETICS", "HEROIN", "METHAMPHETAMINE", "COCAINE_CRACK", "NON_PRESCRIPTION_METHADONE")) 
  


##ADMISSIONS BY GENDER
ggplot(use_by_drug, aes(x = GENDER, y= '', fill = GENDER))+
  geom_bar(stat="identity")+
  scale_fill_brewer( palette = "YlGnBu" ) +
  labs(subtitle="", 
      y="Total Admissions", 
      x="Gender", 
      title="Rehab Admissions by Gender", 
      caption = "Source: Institute for Social Research")



##SUBSTANCE BY AGE AT FIRST USE
ggplot(use_by_drug, aes(x = SUB1, y = '', fill = FRSTUSE1)) + 
  geom_bar( stat = "identity", position = "stack" ) +
  coord_flip() +
  theme_minimal() + theme( legend.position = "bottom" )+
  labs(subtitle="", 
      y="Substance", 
      x="", 
      title="Substance by Age at First Use", 
      caption = "Source: Institute for Social Research")

## SUBSTANCE BY PSYHCOLOGICAL DIAGNOSIS
ggplot(use_by_drug, aes(x = SUB1, y = '', fill = PSYPROB)) + 
  geom_bar( stat = "identity", position = "stack" ) +
  coord_flip() +
  theme_minimal() + theme( legend.position = "bottom" )+
  labs(subtitle="", 
       y="Substance", 
       x="", 
       title="Substance by Psychological Diagnoses", 
       caption = "Source: USDA Economic Research Service")

## SUBSTANCE BY LVING ARRANGEMENT
ggplot(use_by_drug, aes(x = SUB1, y = '', fill = LIVARAG)) + 
  geom_bar( stat = "identity", position = "stack" ) +
  coord_flip() +
  theme_minimal() + theme( legend.position = "bottom" )+
  labs(subtitle="", 
       y="Substance", 
       x="", 
       title="Substance by Living Arrangement", 
       caption = "Source: USDA Economic Research Service")

opiate <- data %>% filter(SUB1 %in% c("OTHER_OPIATES_AND_SYNTHETICS")) %>% 
  select(AGE, GENDER, RACE, EDUC, EMPLOY, PSOURCE, LIVARAG)

tableplot(opiate)


##############ONE HOT ENCODING################

###REMOVE THE STATE AND COUNTY VARIABLES

data$STFIPS <- NULL
data$CBSA <- NULL
data$CASEID <- NULL

#filter for response variable
data$DSMCRIT <- ifelse(data$DSMCRIT == "OPIOID_DEPENDENCE" | data$DSMCRIT == "OPIOID_ABUSE", 1, 0)

hot_data <- as.data.table(data)
hot_data <- one_hot(hot_data, dropCols = TRUE)

###############FEATURE COLUMNS#################

#remove bit columns that contain 'no's' and are confounding with SUB1

hot_data$HERFLG_NO <- NULL
hot_data$HERFLG_YES <- NULL
hot_data$METHFLG_NO <- NULL
hot_data$METHFLG_YES <- NULL
hot_data$OPSYNFLG_YES <- NULL
hot_data$OPSYNFLG_NO <- NULL
hot_data$PSYPROB_UNKNOWN <- NULL
hot_data$PSYPROB_NO <- NULL

feature_cols <- hot_data[,1:181]


###########SPLIT DATA TRAIN/TEST#################
set.seed(123)
hot_data <- hot_data[sample(nrow(hot_data), 10000), ]


row_indices <- sample(1:nrow(hot_data), 
                      size = 0.8 * nrow(hot_data))
data_train <- hot_data[row_indices, ]
data_test <- hot_data[-row_indices, ]


#########CODE FOR CREATING A FORMULA##############
vars <- paste("",colnames(data_train[,1:180]),sep="")
fla <- paste("DSMCRIT ~", paste(vars, collapse="+"))
fla <- as.formula(fla)
fla


# fit neural network
set.seed(123)
NN1 <- neuralnet(DSMCRIT ~ .,
                     data = data_train, 
                     linear.output = FALSE, 
                     err.fct = 'ce', 
                     likelihood = TRUE)



set.seed(123)
# 2-Hidden Layers, Layer-1 2-neurons, Layer-2, 1-neuron
NN2 <- neuralnet(DSMCRIT ~ .,
                     data = data_train, 
                     linear.output = FALSE, 
                     err.fct = 'ce', 
                     likelihood = 
                     TRUE, hidden = c(2,1))

# 2-Hidden Layers, Layer-1 2-neurons, Layer-2, 2-neurons
set.seed(123)
NN3 <- NN2 <- neuralnet(DSMCRIT ~ .,
                                data = data_train,
                                linear.output = FALSE, 
                                err.fct = 'ce', 
                                likelihood = TRUE, 
                                hidden = c(2,2))

# 2-Hidden Layers, Layer-1 1-neuron, Layer-2, 2-neuron
set.seed(123)
NN4 <- NN2 <- neuralnet(DSMCRIT ~ .,
                                data = data_train,
                                linear.output = FALSE, 
                                err.fct = 'ce', 
                                likelihood = TRUE, 
                                hidden = c(1,2))

# Bar plot of results



Class_NN_ICs <- tibble('Network' = rep(c("NN1", "NN2", "NN3", "NN4"), each = 3), 
                       'Metric' = rep(c('AIC', 'BIC', 'ce Error * 100'), length.out = 12),
                       'Value' = c(NN1$result.matrix[4,1], NN1$result.matrix[5,1], 
                                   100*NN1$result.matrix[1,1], NN2$result.matrix[4,1], 
                                   NN2$result.matrix[5,1], 100*NN2$result.matrix[1,1],
                                   NN3$result.matrix[4,1], NN3$result.matrix[5,1], 
                                   100*NN3$result.matrix[1,1], NN4$result.matrix[4,1], 
                                   NN4$result.matrix[5,1], 100*NN4$result.matrix[1,1]))

Class_NN_ICs %>%
  ggplot(aes(Network, Value, fill = Metric)) +
  geom_col(position = 'dodge')  +
  ggtitle("AIC, BIC, and Cross-Entropy Error of the Classification ANNs", "Note: ce Error displayed is 100 times its true value")
#http://uc-r.github.io/ann_classification#one-layer




#Test the resulting output
myvars <- data_test %>% select(-DSMCRIT)
myvars <- colnames(myvars)

temp_test <- subset(data_test, select = myvars)
head(temp_test)
nn.results <- neuralnet::compute(NN4, temp_test)
results <- data.frame(actual = data_test$DSMCRIT, prediction = nn.results$net.result)

results

#CONFUSION MATRIX
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)



plot(NN4, rep = 'best')





#RANDOM FOREST FEAUTRE EXPLORATION 
#FEATURE ENGINEERING
#get feature importance
set.seed(123)
fit_rf = randomForest(as.formula(fla), data=data_train, importance = TRUE, na.action = na.roughfix) #fit the model using the formula created above


# Create an importance based on mean decreasing gini
varImp <- varImp(fit_rf)
varImp
impplot <- varImpPlot(fit_rf)
#results
fit_rf

