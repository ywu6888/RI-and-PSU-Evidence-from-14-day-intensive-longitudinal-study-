####### Load required packages #######
library(readxl)
library(dplyr)
library(misty)
library(lmerTest)
library(performance)

##################################################################################
#########################        Read data        #################################
##################################################################################

# Read daily diary data
file_path <- "xxx/daily dairy.xlsx" 
data_dairy <- read_excel(file_path)
summary(data_dairy)


# Read screen time data
file_path1 <- "xxx/screen time.xlsx"
data_ST <- read_excel(file_path1)
summary(data_ST)


##################################################################################
#######################        Data processing     ################################
##################################################################################

# Data type conversion
data_dairy$MPAI	 <- as.numeric(data_dairy$MPAI)
data_dairy$RI <- as.numeric(data_dairy$RI)
data_dairy$SA <- as.numeric(data_dairy$SA)

data_ST$ST <- as.numeric(data_ST$ST)
data_ST$RI <- as.numeric(data_ST$RI)
data_ST$SA <- as.numeric(data_ST$SA)


## Calculate multilevel correlations
data_Total <- merge(data_dairy, data_ST, by = c("Number","RI","SA","Time","gender","age"), all.x = TRUE, sort = FALSE)
summary(data_Total)
result <- multilevel.cor(data[,c("MPAI","ST","RI", "SA")],sig = TRUE,cluster = data_Total$Number, print = c("all"),p.adj = c("bonferroni"))
print(result)


# Compute group-mean centering for RI and SA
data_dairy <- data_dairy %>%
  group_by(Number) %>%
  mutate(
    MPAI_centered = MPAI - mean(MPAI, na.rm = TRUE),
    RI_centered = RI	- mean(RI, na.rm = TRUE),
    SA_centered = SA	- mean(SA, na.rm = TRUE)
  ) %>%
  ungroup()

print(data_dairy)


data_ST <- data_ST %>%
  group_by(Number) %>%
  mutate(
    ST_centered = ST - mean(ST, na.rm = TRUE),
    RI_centered = RI	- mean(RI, na.rm = TRUE),
    SA_centered = SA	- mean(SA, na.rm = TRUE)
  ) %>%
  ungroup()

print(data_ST)


## Add holiday variable
data_dairy <- data_dairy %>%
  # Create 'holiday' column based on the value of Time
  mutate(holiday = ifelse(Time >= 1 & Time <= 7, 0, ifelse(Time >= 8 & Time <= 14, 1, NA))) %>%
  # Convert 'holiday' column to a factor variable
  mutate(holiday = factor(holiday, levels = c(0, 1)))

data_ST <- data_ST %>%
  # Create 'holiday' column based on the value of Time
  mutate(holiday = ifelse(Time >= 1 & Time <= 7, 0, ifelse(Time >= 8 & Time <= 14, 1, NA))) %>%
  # Convert 'holiday' column to a factor variable
  mutate(holiday = factor(holiday, levels = c(0, 1)))



##################################################################################
#######################        Linear mixed models     ###########################
##################################################################################

### Modeling the daily diary data
## Build random intercept model
model_RI_intercept_dairy <- lmer(MPAI~ RI_centered+Time+holiday+gender+age +(1|Number), data_dairy)
summary(model_RI_intercept_dairy)

# Create lagged terms
data_dairy_lag <- data_dairy %>%
  # Sort by subject and Time
  arrange(Number, Time) %>%
  # Use lag() to create lagged column, computing RI lag within each subject
  group_by(Number) %>%
  mutate(RI_lag = lag(RI_centered)) %>%
  ungroup()
data_dairy_lag <- data_dairy_lag %>% filter(!is.na(RI_lag))

## Add lagged term
model_RI_intercept_dairy_lag <- lmer(MPAI~ RI_centered+RI_lag+Time+holiday+gender+age +(1|Number), data_dairy_lag)
summary(model_RI_intercept_dairy_lag)

## Compare model with lagged term to random intercept model
model_RI_intercept_dairy_new <- lmer(MPAI~ RI_centered+Time+holiday+gender+age +(1|Number), data_dairy_lag)

compare_performance(model_RI_intercept_dairy_lag, model_RI_intercept_dairy_new)
anova(model_RI_intercept_dairy_lag, model_RI_intercept_dairy_new)

## Build random slope model with lagged term
model_RI_slope_dairy_lag <- lmer(MPAI~ RI_centered+RI_lag+Time+holiday+gender+age +(1+RI_centered|Number), data_dairy_lag)
summary(model_RI_slope_dairy_lag)

## Compare random slope model with lag to random intercept model with lag
compare_performance(model_RI_intercept_dairy_lag, model_RI_slope_dairy_lag)
anova(model_RI_intercept_dairy_lag, model_RI_slope_dairy_lag)


### Modeling the screen time data
## Build random intercept model
model_RI_intercept_ST <- lmer(ST~ RI_centered+Time+holiday+gender+age +(1|Number), data_ST)
summary(model_RI_intercept_ST)

# Create lagged terms
data_ST_lag <- data_ST %>%
  # Sort by subject and Time
  arrange(Number, Time) %>%
  # Use lag() to create lagged column, computing RI lag within each subject
  group_by(Number) %>%
  mutate(RI_lag = lag(RI_centered)) %>%
  ungroup()
data_ST_lag <- data_ST_lag %>% filter(!is.na(RI_lag))

## Add lagged term
model_RI_intercept_ST_lag <- lmer(ST~ RI_centered+RI_lag+Time+holiday+gender+age +(1|Number), data_ST_lag)
summary(model_RI_intercept_ST_lag)

## Compare model with lagged term to random intercept model
model_RI_intercept_ST_new <- lmer(ST~ RI_centered+Time+holiday+gender+age +(1|Number), data_ST_lag)

compare_performance(model_RI_intercept_ST_lag, model_RI_intercept_ST_new)
anova(model_RI_intercept_ST_lag, model_RI_intercept_ST_new)

## Build random slope model
model_RI_slope_ST <- lmer(ST~ RI_centered+Time+holiday+gender+age +(1+RI_centered|Number), data_ST)
summary(model_RI_slope_ST)

## Compare random slope model with random intercept model
compare_performance(model_RI_intercept_ST, model_RI_slope_ST)
anova(model_RI_intercept_ST, model_RI_slope_ST)
