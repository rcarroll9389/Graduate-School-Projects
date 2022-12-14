setwd("/Users/ryancarroll/Desktop/Spring 2019/R")
getwd()

library(sqldf)
library(readxl)
library(ggplot2)
library(dplyr)
library(tseries)
library(xts)
library(aTSA)
library(lubridate)
library(stargazer)


econ_vars <- read_excel("updated_data_0315.xlsx", sheet = 12)
econ_vars <- as.data.frame(econ_vars)

new_by_MOB <- as.data.frame(read_excel("updated_data_0315.xlsx", sheet = 1))

used_by_MOB <- as.data.frame(read_excel("updated_data_0315.xlsx", sheet = 2))

new_by_MOB_DV <- new_by_MOB[,c(1,10,11)]
used_by_MOB_DV <- used_by_MOB[,c(1,10,11)]

new_by_MOB_DV <- na.omit(new_by_MOB_DV)
used_by_MOB_DV <- na.omit(used_by_MOB_DV)

colnames(new_by_MOB_DV) <- c("date", "segment", "PORA")
colnames(used_by_MOB_DV) <- c("date", "segment", "PORA")

new_by_MOB_DV$segment <- ifelse(new_by_MOB_DV$segment == "MOB <= 12", "12 >= MOB",
                                ifelse(new_by_MOB_DV$segment == "MOB >= 49", "49 <= MOB",
                                       new_by_MOB_DV$segment))

new_by_MOB_plot <- ggplot(new_by_MOB_DV, aes(x = date, y = PORA, color = segment)) + geom_line() +
  geom_smooth(method = "lm", se = FALSE) + theme_dark() + scale_color_brewer(palette = "Set3") + 
  ggtitle("PO Rate Annualized by MOB Cohort") + ylab("PO Rate Annualized %")
new_by_MOB_plot
new_by_MOB_plot
# generate separate series for each

nbm_lt12 <- filter(new_by_MOB_DV, segment == "12 >= MOB")
nbm_1324 <- filter(new_by_MOB_DV, segment == "13 <= MOB <= 24")
nbm_2536 <- filter(new_by_MOB_DV, segment == "25 <= MOB <= 36")
nbm_3748 <- filter(new_by_MOB_DV, segment == "37 <= MOB <= 48")
nbm_gt49 <- filter(new_by_MOB_DV, segment == "49 <= MOB")

### check for stationarity via ADF ### 

# recall that we want to reject
# weirdly these all fail beyond k = 4 :-(

aTSA::adf.test(nbm_lt12$PORA, nlag = 4)
aTSA::adf.test(nbm_lt12$PORA, nlag = 12)
aTSA::adf.test(nbm_lt12$PORA, nlag = 24)

tseries::adf.test(nbm_lt12$PORA, k = 4)
tseries::adf.test(nbm_lt12$PORA, k = 12)
tseries::adf.test(nbm_lt12$PORA, k = 24)

aTSA::adf.test(nbm_gt49$PORA, nlag = 12) # fail at 8 lags
aTSA::adf.test(nbm_3748$PORA, nlag = 12) # fail at 7 lags
aTSA::adf.test(nbm_2536$PORA, nlag = 12) # fail at 8 lags
aTSA::adf.test(nbm_1324$PORA, nlag = 12) # fail at 8 lags
aTSA::adf.test(nbm_lt12$PORA, nlag = 12) # fail at 5 lags

### create joined and cleaned sets for lag purposes

#lt12

nbm_lt12_join <- sqldf("select nbm_lt12.date, nbm_lt12.PORA, econ_vars.* 
                       from nbm_lt12
                       left join econ_vars
                       on nbm_lt12.date = econ_vars.Date")

nbm_lt12_join <- select(nbm_lt12_join, -Date, -FAN_30Y_MORTG_FIX_US, -PCHG_FAN_30Y_MORTG_FIX_US)

nbm_lt12_dates <- as.data.frame(as.Date(nbm_lt12_join$date))
colnames(nbm_lt12_dates) <- c("date")

nbm_lt12_join <- select(nbm_lt12_join, -date)

nbm_lt12_xts <- xts(nbm_lt12_join, order.by = nbm_lt12_dates$date)



#1324

nbm_1324_join <- sqldf("select nbm_1324.date, nbm_1324.PORA, econ_vars.* 
                       from nbm_1324
                       left join econ_vars
                       on nbm_1324.date = econ_vars.Date")

nbm_1324_join <- select(nbm_1324_join, -Date, -FAN_30Y_MORTG_FIX_US, -PCHG_FAN_30Y_MORTG_FIX_US)

nbm_1324_dates <- as.data.frame(as.Date(nbm_1324_join$date))
colnames(nbm_1324_dates) <- c("date")

nbm_1324_join <- select(nbm_1324_join, -date)

nbm_1324_xts <- xts(nbm_1324_join, order.by = nbm_1324_dates$date)



#2536

nbm_2536_join <- sqldf("select nbm_2536.date, nbm_2536.PORA, econ_vars.* 
                       from nbm_2536
                       left join econ_vars
                       on nbm_2536.date = econ_vars.Date")

nbm_2536_join <- select(nbm_2536_join, -Date, -FAN_30Y_MORTG_FIX_US, -PCHG_FAN_30Y_MORTG_FIX_US)

nbm_2536_dates <- as.data.frame(as.Date(nbm_2536_join$date))
colnames(nbm_2536_dates) <- c("date")

nbm_2536_join <- select(nbm_2536_join, -date)

nbm_2536_xts <- xts(nbm_2536_join, order.by = nbm_2536_dates$date)



#3748

nbm_3748_join <- sqldf("select nbm_3748.date, nbm_3748.PORA, econ_vars.* 
                       from nbm_3748
                       left join econ_vars
                       on nbm_3748.date = econ_vars.Date")

nbm_3748_join <- select(nbm_3748_join, -Date, -FAN_30Y_MORTG_FIX_US, -PCHG_FAN_30Y_MORTG_FIX_US)

nbm_3748_dates <- as.data.frame(as.Date(nbm_3748_join$date))
colnames(nbm_3748_dates) <- c("date")

nbm_3748_join <- select(nbm_3748_join, -date)

nbm_3748_xts <- xts(nbm_3748_join, order.by = nbm_3748_dates$date)



#gt49

nbm_gt49_join <- sqldf("select nbm_gt49.date, nbm_gt49.PORA, econ_vars.* 
                       from nbm_gt49
                       left join econ_vars
                       on nbm_gt49.date = econ_vars.Date")

nbm_gt49_join <- select(nbm_gt49_join, -Date, -FAN_30Y_MORTG_FIX_US, -PCHG_FAN_30Y_MORTG_FIX_US)

nbm_gt49_dates <- as.data.frame(as.Date(nbm_gt49_join$date))
colnames(nbm_gt49_dates) <- c("date")

nbm_gt49_join <- select(nbm_gt49_join, -date)

nbm_gt49_xts <- xts(nbm_gt49_join, order.by = nbm_gt49_dates$date)


# now do lags...

lag_lt12_1q <- lag.xts(nbm_lt12_xts, k = 3)
lag_lt12_2q <- lag.xts(nbm_lt12_xts, k = 6)
lag_lt12_3q <- lag.xts(nbm_lt12_xts, k = 9)
lag_lt12_4q <- lag.xts(nbm_lt12_xts, k = 12)

complete_lt12 <- cbind(nbm_lt12_xts, lag_lt12_1q, lag_lt12_2q, lag_lt12_3q, lag_lt12_4q)


lag_1324_1q <- lag.xts(nbm_1324_xts, k = 3)
lag_1324_2q <- lag.xts(nbm_1324_xts, k = 6)
lag_1324_3q <- lag.xts(nbm_1324_xts, k = 9)
lag_1324_4q <- lag.xts(nbm_1324_xts, k = 12)

complete_1324 <- cbind(nbm_1324_xts, lag_1324_1q, lag_1324_2q, lag_1324_3q, lag_1324_4q)


lag_2536_1q <- lag.xts(nbm_2536_xts, k = 3)
lag_2536_2q <- lag.xts(nbm_2536_xts, k = 6)
lag_2536_3q <- lag.xts(nbm_2536_xts, k = 9)
lag_2536_4q <- lag.xts(nbm_2536_xts, k = 12)

complete_2536 <- cbind(nbm_2536_xts, lag_2536_1q, lag_2536_2q, lag_2536_3q, lag_2536_4q)


lag_3748_1q <- lag.xts(nbm_3748_xts, k = 3)
lag_3748_2q <- lag.xts(nbm_3748_xts, k = 6)
lag_3748_3q <- lag.xts(nbm_3748_xts, k = 9)
lag_3748_4q <- lag.xts(nbm_3748_xts, k = 12)

complete_3748 <- cbind(nbm_3748_xts, lag_3748_1q, lag_3748_2q, lag_3748_3q, lag_3748_4q)


lag_gt49_1q <- lag.xts(nbm_gt49_xts, k = 3)
lag_gt49_2q <- lag.xts(nbm_gt49_xts, k = 6)
lag_gt49_3q <- lag.xts(nbm_gt49_xts, k = 9)
lag_gt49_4q <- lag.xts(nbm_gt49_xts, k = 12)

complete_gt49 <- cbind(nbm_gt49_xts, lag_gt49_1q, lag_gt49_2q, lag_gt49_3q, lag_gt49_4q)

rm(list=ls(pattern="^lag")) # regex kill all temp tables

summary(complete_gt49$FREDDIE_30Y_FIX_US.1)



### convert to df's, add rownames() as date column for sharing
# note that the issue here was complete_* being xts which are matrices and coerce all data to the same type

df_lt12 <- as.data.frame(complete_lt12)
df_1324 <- as.data.frame(complete_1324)
df_2536 <- as.data.frame(complete_2536)
df_3748 <- as.data.frame(complete_3748)
df_gt49 <- as.data.frame(complete_gt49)

df_lt12$date <- nbm_lt12_dates$date
df_1324$date <- nbm_1324_dates$date
df_2536$date <- nbm_2536_dates$date
df_3748$date <- nbm_3748_dates$date
df_gt49$date <- nbm_gt49_dates$date


### add time trends

df_lt12$time_trend <- seq(1:nrow(df_lt12))
df_1324$time_trend <- seq(1:nrow(df_1324))
df_2536$time_trend <- seq(1:nrow(df_2536))
df_3748$time_trend <- seq(1:nrow(df_3748))
df_gt49$time_trend <- seq(1:nrow(df_gt49))

### add seasonal [monthly] dummies

df_lt12$month <- month(df_lt12$date)
df_1324$month <- month(df_1324$date)
df_2536$month <- month(df_2536$date)
df_3748$month <- month(df_3748$date)
df_gt49$month <- month(df_gt49$date)

m <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
mn <- c(1,2,3,4,5,6,7,8,9,10,11,12)
df_m <- as.data.frame(cbind(m,mn))

df_lt12$month <- ifelse(df_lt12$month == 1, "JAN", ifelse(df_lt12$month == 2, "FEB", ifelse(df_lt12$month == 3, "MAR",
                  ifelse(df_lt12$month == 4, "APR", ifelse(df_lt12$month == 5, "MAY", ifelse(df_lt12$month == 6, "JUN",
                  ifelse(df_lt12$month == 7, "JUL", ifelse(df_lt12$month == 8, "AUG", ifelse(df_lt12$month == 9, "SEP",
                  ifelse(df_lt12$month == 10, "OCT", ifelse(df_lt12$month == 11, "NOV", "DEC")))))))))))

df_lt12$month <- as.factor(df_lt12$month)

df_1324$month <- ifelse(df_1324$month == 1, "JAN", ifelse(df_1324$month == 2, "FEB", ifelse(df_1324$month == 3, "MAR",
                  ifelse(df_1324$month == 4, "APR", ifelse(df_1324$month == 5, "MAY", ifelse(df_1324$month == 6, "JUN",
                  ifelse(df_1324$month == 7, "JUL", ifelse(df_1324$month == 8, "AUG", ifelse(df_1324$month == 9, "SEP",
                  ifelse(df_1324$month == 10, "OCT", ifelse(df_1324$month == 11, "NOV", "DEC")))))))))))

df_1324$month <- as.factor(df_1324$month)

df_2536$month <- ifelse(df_2536$month == 1, "JAN", ifelse(df_2536$month == 2, "FEB", ifelse(df_2536$month == 3, "MAR",
                  ifelse(df_2536$month == 4, "APR", ifelse(df_2536$month == 5, "MAY", ifelse(df_2536$month == 6, "JUN",
                  ifelse(df_2536$month == 7, "JUL", ifelse(df_2536$month == 8, "AUG", ifelse(df_2536$month == 9, "SEP",
                  ifelse(df_2536$month == 10, "OCT", ifelse(df_2536$month == 11, "NOV", "DEC")))))))))))

df_2536$month <- as.factor(df_2536$month)

df_3748$month <- ifelse(df_3748$month == 1, "JAN", ifelse(df_3748$month == 2, "FEB", ifelse(df_3748$month == 3, "MAR",
                  ifelse(df_3748$month == 4, "APR", ifelse(df_3748$month == 5, "MAY", ifelse(df_3748$month == 6, "JUN",
                  ifelse(df_3748$month == 7, "JUL", ifelse(df_3748$month == 8, "AUG", ifelse(df_3748$month == 9, "SEP",
                  ifelse(df_3748$month == 10, "OCT", ifelse(df_3748$month == 11, "NOV", "DEC")))))))))))

df_3748$month <- as.factor(df_3748$month)

df_gt49$month <- ifelse(df_gt49$month == 1, "JAN", ifelse(df_gt49$month == 2, "FEB", ifelse(df_gt49$month == 3, "MAR",
                  ifelse(df_gt49$month == 4, "APR", ifelse(df_gt49$month == 5, "MAY", ifelse(df_gt49$month == 6, "JUN",
                  ifelse(df_gt49$month == 7, "JUL", ifelse(df_gt49$month == 8, "AUG", ifelse(df_gt49$month == 9, "SEP",
                  ifelse(df_gt49$month == 10, "OCT", ifelse(df_gt49$month == 11, "NOV", "DEC")))))))))))

df_gt49$month <- as.factor(df_gt49$month)



### write CSVs

write.csv(df_lt12, "lt12.csv")
write.csv(df_1324, "1324.csv")
write.csv(df_2536, "2536.csv")
write.csv(df_3748, "3748.csv")
write.csv(df_gt49, "gt49.csv")



### add cohort

df_lt12$cohort <- "lt12"
df_1324$cohort <- "1324"
df_2536$cohort <- "2536"
df_3748$cohort <- "3748"
df_gt49$cohort <- "gt49"

### rbind

df_mob_comp <- rbind(df_lt12, df_1324, df_2536, df_3748, df_gt49)

library(plm)

mob_comp_panel <- pdata.frame(df_mob_comp, index = c("cohort", "date"))
pdim(mob_comp_panel)

plm_pool <- plm(formula = PORA ~ FREDDIE_30Y_FIX_US.1 + LN_INDUST_PROD_US + month,
                data = mob_comp_panel,
                effect = "twoways")

base::summary(mob_comp_panel$FREDDIE_30Y_FIX_US.1)

mob_comp_panel_subset <- select(mob_comp_panel, cohort, date, PORA, FREDDIE_30Y_FIX_US.1, LN_INDUST_PROD_US, month,
                                time_trend)



plm_pool <- plm(formula = PORA ~ FREDDIE_30Y_FIX_US.1 + LN_INDUST_PROD_US + month + time_trend,
                data = mob_comp_panel_subset,
                effect = "individual")

stargazer(plm_pool, type = "text") # this works, which is good,
summary(plm_pool)

### validate stepwise selection...

df_mob_comp$cohort <- as.factor(df_mob_comp$cohort)

df_mob_comp$cohort <- relevel(df_mob_comp$cohort, ref = "gt49")
df_mob_comp$month <- relevel(df_mob_comp$month, ref = "DEC")

df_mob_comp_ss <- select(df_mob_comp, -PORA.1, -PORA.2, -PORA.3, -PORA.4, -date)

df_mob_comp_ss <- na.omit(df_mob_comp_ss)

library(My.stepwise)

varnames <- names(df_mob_comp_ss)
varnames <- varnames[-1]

### NOT RUN: BAD ###

# mob_stepwise <- My.stepwise.lm(Y = "PORA", variable.list = varnames,
                               # in.variable = c("cohort", "month", "time_trend"),
                               # data = df_mob_comp_ss, sle = 0.0001, sls = 0.001)

# so this works but is insanely slow
# cap_util_us & tbond_30_yr appear...time trend highly significant, eats industrial prod's lunch

model_null <- lm(PORA ~ cohort + month + time_trend, data = df_mob_comp)
summary(model_null)

library(glmnet)

x <- model.matrix(PORA ~ ., df_mob_comp_ss)
y <- df_mob_comp_ss$PORA

lasso_mod <- glmnet(x, y, alpha = 1,
                    penalty.factor = c(rep(1,1651), rep(0,16)))
plot(lasso_mod, xvar = "lambda")
print(lasso_mod)

cv_mod <- cv.glmnet(x, y, type.measure = "mse", nfolds = 20,
                    penalty.factor = c(rep(1,1651), rep(0,16))) # assigning a penalty of 0 to time trend, cohort, month
cv_mod$lambda.min

cv_matrix <- coef(cv_mod, s = "lambda.min")
cv_matrix

cv_matrix_2 <- coef(cv_mod, s = 0.0019)
cv_matrix_2

library(broom)
tidy(cv_matrix)
tidy(cv_matrix_2)

#cv_matrix_2:  CAP_UTIL_US(.1), LN_FAMM_HOUS_START_US, PCHG_FX_USD_YEN_US, YOY_FIX_INVSMT_US.2
                  # && time_trend, month, cohort
                  # drop lag of cap_util, drop housing starts

mob_null <- lm(PORA ~ cohort + month + time_trend,
  data = df_mob_comp_ss)

summary(mob_null)

mob_fundamentals <- lm(PORA ~ CAP_UTIL_US + PCHG_FX_USD_YEN_US + YOY_FIX_INVSMT_US.2 + time_trend + month + cohort,
                       data = df_mob_comp_ss)

summary(mob_fundamentals)

stargazer(mob_null, mob_fundamentals, type = "text", title = "Pooled Cross Sections of MOB",
          column.labels = c("Controls Only", "Fundamentals"))

summary(df_mob_comp_ss$PCHG_FX_USD_YEN_US)
min(nbm_gt49_dates$date)
head(df_mob_comp_ss$PCHG_FX_USD_YEN_US)
head(df_mob_comp_ss$FX_USD_YEN_US)
summary(df_mob_comp_ss$FX_USD_YEN_US)

### cross-validation for cohort models...

df_x <- as.data.frame(x)

tail(names(df_x))

x_lt12 <- filter(df_x, cohortlt12 == 1)
x_1324 <- filter(df_x, cohort1324 == 1)
x_2536 <- filter(df_x, cohort2536 == 1)
x_3748 <- filter(df_x, cohort3748 == 1)
x_gt49 <- filter(df_x, cohortlt12 == 0 & cohort1324 == 0 & cohort2536 == 0 & cohort3748 == 0)

y_lt12 <- df_mob_comp_ss %>% filter(cohort == "lt12") %>% select(PORA)
y_1324 <- df_mob_comp_ss %>% filter(cohort == "1324") %>% select(PORA)
y_2536 <- df_mob_comp_ss %>% filter(cohort == "2536") %>% select(PORA)
y_3748 <- df_mob_comp_ss %>% filter(cohort == "3748") %>% select(PORA)
y_gt49 <- df_mob_comp_ss %>% filter(cohort == "gt49") %>% select(PORA)

