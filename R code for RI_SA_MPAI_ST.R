
#######加载必要的包#######
library(readxl)
library(dplyr)
library(misty)
library(lmerTest)
library(performance)

##################################################################################
#########################        读取数据        #################################
##################################################################################

#读取每日日记数据
file_path <- "xxx/daily dairy.xlsx" 
data_dairy <- read_excel(file_path)
summary(data_dairy)


#读取屏幕时间数据
file_path1 <- "xxx/screen time.xlsx"
data_ST <- read_excel(file_path1)
summary(data_ST)


##################################################################################
#######################        数据处理     ######################################
##################################################################################

#数据转换
data_dairy$MPAI	 <- as.numeric(data_dairy$MPAI)
data_dairy$RI <- as.numeric(data_dairy$RI)
data_dairy$SA <- as.numeric(data_dairy$SA)

data_ST$ST <- as.numeric(data_ST$ST)
data_ST$RI <- as.numeric(data_ST$RI)
data_ST$SA <- as.numeric(data_ST$SA)


##计算多水平相关
data_Total <- merge(data_dairy, data_ST, by = c("Number","RI","SA","Time","gender","age"), all.x = TRUE, sort = FALSE)
summary(data_Total)
result <- multilevel.cor(data[,c("MPAI","ST","RI", "SA")],sig = TRUE,cluster = data_Total$Number, print = c("all"),p.adj = c("bonferroni"))
print(result)


#计算RI和SA的group-mean centering
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


##加入holiday变量
data_dairy <- data_dairy %>%
  # 根据 Time 的值创建 holiday 列
  mutate(holiday = ifelse(Time >= 1 & Time <= 7, 0, ifelse(Time >= 8 & Time <= 14, 1, NA))) %>%
  # 将 holiday 列转为因子变量
  mutate(holiday = factor(holiday, levels = c(0, 1)))

data_ST <- data_ST %>%
  # 根据 Time 的值创建 holiday 列
  mutate(holiday = ifelse(Time >= 1 & Time <= 7, 0, ifelse(Time >= 8 & Time <= 14, 1, NA))) %>%
  # 将 holiday 列转为因子变量
  mutate(holiday = factor(holiday, levels = c(0, 1)))



##################################################################################
#######################        线性混合模型     ##################################
##################################################################################

###对daily dairy数据建模
##构建随机截距模型
model_RI_intercept_dairy <- lmer(MPAI~ RI_centered+Time+holiday+gender+age +(1|Number), data_dairy)
summary(model_RI_intercept_dairy)

#构建滞后项
data_dairy_lag <- data_dairy %>%
  # 按照被试和时间排序
  arrange(Number, Time) %>%
  # 使用 lag() 创建滞后列，按照每个被试计算RI的滞后值
  group_by(Number) %>%
  mutate(RI_lag = lag(RI_centered)) %>%
  ungroup()
data_dairy_lag <- data_dairy_lag %>% filter(!is.na(RI_lag))

##加入滞后项
model_RI_intercept_dairy_lag <- lmer(MPAI~ RI_centered+RI_lag+Time+holiday+gender+age +(1|Number), data_dairy_lag)
summary(model_RI_intercept_dairy_lag)

##加入滞后项的模型与随机截距模型比较
model_RI_intercept_dairy_new <- lmer(MPAI~ RI_centered+Time+holiday+gender+age +(1|Number), data_dairy_lag)

compare_performance(model_RI_intercept_dairy_lag, model_RI_intercept_dairy_new)
anova(model_RI_intercept_dairy_lag, model_RI_intercept_dairy_new)

##构建带滞后项的随机斜率模型
model_RI_slope_dairy_lag <- lmer(MPAI~ RI_centered+RI_lag+Time+holiday+gender+age +(1+RI_centered|Number), data_dairy_lag)
summary(model_RI_slope_dairy_lag)

##加入滞后项的随机斜率模型与加入滞后项的随机截距模型比较
compare_performance(model_RI_intercept_dairy_lag, model_RI_slope_dairy_lag)
anova(model_RI_intercept_dairy_lag, model_RI_slope_dairy_lag)


###对screen time数据建模
##构建随机截距模型
model_RI_intercept_ST <- lmer(ST~ RI_centered+Time+holiday+gender+age +(1|Number), data_ST)
summary(model_RI_intercept_ST)

#构建滞后项
data_ST_lag <- data_ST %>%
  # 按照被试和时间排序
  arrange(Number, Time) %>%
  # 使用 lag() 创建滞后列，按照每个被试计算RI的滞后值
  group_by(Number) %>%
  mutate(RI_lag = lag(RI_centered)) %>%
  ungroup()
data_ST_lag <- data_ST_lag %>% filter(!is.na(RI_lag))

##加入滞后项
model_RI_intercept_ST_lag <- lmer(ST~ RI_centered+RI_lag+Time+holiday+gender+age +(1|Number), data_ST_lag)
summary(model_RI_intercept_ST_lag)

##加入滞后项的模型与随机截距模型比较
model_RI_intercept_ST_new <- lmer(ST~ RI_centered+Time+holiday+gender+age +(1|Number), data_ST_lag)

compare_performance(model_RI_intercept_ST_lag, model_RI_intercept_ST_new)
anova(model_RI_intercept_ST_lag, model_RI_intercept_ST_new)

##构建随机斜率模型
model_RI_slope_ST <- lmer(ST~ RI_centered+Time+holiday+gender+age +(1+RI_centered|Number), data_ST)
summary(model_RI_slope_ST)

##随机斜率模型与随机截距模型比较
compare_performance(model_RI_intercept_ST, model_RI_slope_ST)
anova(model_RI_intercept_ST, model_RI_slope_ST)

