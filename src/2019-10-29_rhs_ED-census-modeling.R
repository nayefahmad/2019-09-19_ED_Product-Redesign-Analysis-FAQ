

#'--- 
#' title: "RHS ED Census modeling"
#' author: "Nayef Ahmad"
#' date: "2019-10-28"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: show
#'     toc: true
#'     toc_float: true
#'     toc_folding: false
#' ---
#' 
#' 

#+ lib, include = FALSE 
library(tidyverse)
library(denodoExtractor)
library(janitor)
library(lubridate)
library(prophet)
library(DT)
library(plotly)

# setup_denodo()

cnx <- DBI::dbConnect(odbc::odbc(), dsn = "cnx_SPDBSCSTA001")

vw_ed_census <- dplyr::tbl(cnx, dbplyr::in_schema("DSSI.dbo", 
                                                  "[AISAKE_Hourly_Census]"))

#+ rest

#' # Todo:
#'
#' 1. test set RMSE
#'
#' 2. Residual analysis
#'
#' 3. Will the seasonality across the year smoothen out a lot if we increase the
#' window around the holidays?
#' 


#' # Overview 
#' 
#' **What are we doing and why are we doing it? **
#' 

#' # Parameters 
#' 
#' `prophet` hangs with ten years of hourly data. Let's start with 3 years. 
#' 
# Parameters -----------
start_date_id_param <- "20170101"  # todo: experiment with this
site <- "RHS"


#+ data
#' # Data
#' 
# Data -----------
df1.census_raw <- 
  vw_ed_census %>% 
  filter(FacilityShortName == site, 
         short_dt >= start_date_id_param) %>% 
  group_by(short_dt,
           TOD,
           DOW, 
           CalendarMonth, 
           CalendarYear, 
           HolidayName
           ) %>% 
  summarise(ED_Census = sum(ED_Census, na.rm = TRUE)) %>% 
  collect() %>% 
  ungroup() %>% 
  clean_names()


# str(df1.census_raw)
# summary(df1.census_raw)

#' ## Data wrangling
#' 

# > Data wrangling------------
df2.census <- 
  df1.census_raw %>% 
  mutate(date = date(short_dt), 
         hour = hour(short_dt) %>% as.factor(), 
         weekday = weekdays(short_dt) %>% factor(levels = c("Monday", 
                                                            "Tuesday", 
                                                            "Wednesday", 
                                                            "Thursday", 
                                                            "Friday",
                                                            "Saturday", 
                                                            "Sunday")), 
         calendar_month = calendar_month %>% factor(levels = month.name), 
         calendar_year = as.factor(calendar_year), 
         holiday_name = if_else(is.na(holiday_name),
                                "not_holiday", 
                                holiday_name)) %>% 
  mutate(holiday_name = holiday_name %>% as.factor()) %>% 
  select(short_dt, 
         date, 
         hour, 
         weekday, 
         month = calendar_month,  # rename within select( )
         year = calendar_year, 
         hol = holiday_name, 
         ed_census)
  

# str(df2.census)
# summary(df2.census)

#' Sample of 100 observations: 
#' 

df2.census %>% 
  sample_n(100) %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))
                           


#' Pull out holidays as a dataframe: 
#' 

df3.hols <- 
  df2.census %>% 
  filter(hol != "not_holiday") %>% 
  mutate(lower_window = 0, 
         upper_window = 0) %>% # include 0 days before and after the hol
  select(holiday = hol, 
         ds = date, 
         lower_window, 
         upper_window) %>% # %>% View
  distinct() %>% 
  arrange(ds)

# df3.hols %>% write.table(file = "clipboard", sep = "\t", row.names = FALSE)



df3.hols %>%
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip',
                           buttons = c('excel', "csv")))
                           


#' # Plots 
#' 
# Plots ---------
df2.census %>% 
  ggplot(aes(x = date, 
             y = ed_census)) + 
  geom_line() +
  geom_smooth()

df2.census %>% 
  ggplot(aes(x = date, 
             y = ed_census)) + 
  geom_line() +
  geom_smooth() + 
  facet_wrap(~year)


df2.census %>% 
  ggplot(aes(x = ed_census)) + 
  geom_density() + 
  facet_wrap(~year)

p <- df2.census %>% 
  ggplot(aes(x = month, 
             y = ed_census, 
             label = date)) + 
  geom_boxplot() + 
  facet_wrap(~year) + 
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)); ggplotly(p)

p <- df2.census %>% 
  ggplot(aes(x = weekday, 
             y = ed_census, 
             label = date)) + 
  geom_boxplot() + 
  facet_wrap(~year) + 
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)); ggplotly(p)

p <- df2.census %>% 
  ggplot(aes(x = hour, 
             y = ed_census, 
             label = date)) + 
  geom_boxplot() + 
  facet_wrap(~year) + 
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)); ggplotly(p)

p <- df2.census %>% 
  ggplot(aes(x = hol, 
             y = ed_census, 
             label = date)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) + 
  facet_wrap(~year); ggplotly(p)

p <- df2.census %>% 
  filter(hour %in% seq(7,14)) %>% 
  ggplot(aes(x = hol, 
             y = ed_census, 
             label = date)) + 
  geom_boxplot() + 
  labs(title = "Hours from 7am to 2pm") + 
  
  facet_wrap(~year) + 
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)); ggplotly(p)




#+ models 
#' # Models 

#' ## Parameters and data splitting
#'
#' Different models are just different lenses through which we look at our data.
#' We don't believe they're *true*, but we believe they can *highlight key
#' features of the system.*
#'
#' We're using a forecasting model called from the R package `{prophet}`. 
#' 
#' See here for details: https://facebook.github.io/prophet/ 
#' 

# Models ----------
# > Train/test split -------------
t1.train_start <- df2.census$date %>% min

t4.test_end <- df2.census$date %>% max
t3.test_start <- t4.test_end - 30 

t2.train_end <- t3.test_start - 1 

horizon_param <- 24*7*2  # unit: hours 

#' **We take train data from `r t1.train_start` to `r t2.train_end`.** 
#' 
#' **Test data is from `r t3.test_start` to `r t4.test_end`**
#' 


df4.train <-
  df2.census %>%
  select(ds = short_dt,
         y = ed_census) %>% 
  filter(ds <= t2.train_end)

df5.test <- 
  df2.census %>%
  select(ds = short_dt,
         y = ed_census) %>% 
  filter(ds >= t3.test_start) %>% 
  mutate(date = date(ds), 
         hour = hour(ds)) %>% 
  arrange(ds)


#+ model 1
#' ## Prophet model 1 
#' 

# > Prophet model 1  ---------
# fit prophet model:  
m1 <- prophet(df4.train,
              holidays = df3.hols, 
              changepoint.prior.scale = 0.05) # default is 0.05; Increasing it will make the trend more flexible:

# future df: 
future <- make_future_dataframe(m1,
                                periods = horizon_param,
                                freq = "hour")


# Fcast:  
fcast1 <- predict(m1, future)

# Examine the fitted model and fcast
plot(m1, fcast1) + 
  add_changepoints_to_plot(m1)

prophet_plot_components(m1, fcast1)
prophet:::plot_yearly(m1)

# look at fitted values and actuals over last 14 days 
fcast1 %>% 
  filter(ds > t2.train_end - 14) %>% 
  inner_join(df4.train) %>% 
  
  ggplot(aes(x = ds, y = yhat)) + 
  geom_ribbon(aes(ymin = yhat_lower, 
                  ymax = yhat_upper), 
              col = "grey", alpha = 0.3) + 
  # fitted value: 
  geom_line(aes(y = trend + additive_terms), 
            col = "black") + 
  # actuals: 
  geom_line(aes(y = y), 
            col = "blue") + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))
  

#' Sampe of 100 rows: 

# fcast df: 
fcast1 %>%
  sample_n(100) %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip',
                           buttons = c('excel', "csv")))

#' ### Cross-validation
#' 
# Cross val: 
df7.1_metrics_m1 <- 
  cross_validation(m1, 
                   period = 1200, 
                   horizon = horizon_param, 
                   units = "hours") %>% 
  performance_metrics()

df7.1_metrics_m1 %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))

#' **Prophet's default is to return 80% uncertainty intervals.** 
#' 
#' See: https://facebook.github.io/prophet/docs/uncertainty_intervals.html 
#' 
#' I'm not sure if "uncertainty" interval maps exactly onto "prediction interval"
#' 

#+ model 2
#' ## Prophet model 2
#' 
# > Prophet model 2  ---------
# fit prophet model:  
m2 <- prophet(df4.train,
              holidays = df3.hols,
              changepoint.prior.scale = 0.01) # default is 0.05; Increasing it will make the trend more flexible:

# future df: 
future <- make_future_dataframe(m2,
                                periods = horizon_param,
                                freq = "hour")


# Fcast:  
fcast2 <- predict(m2, future)

# Examine the fitted model and fcast
plot(m2, fcast2) + 
  add_changepoints_to_plot(m2)

prophet_plot_components(m2, fcast2)
prophet:::plot_yearly(m2)

# look at fitted values and actuals over last 14 days 
fcast2 %>% 
  filter(ds > t2.train_end - 14) %>% 
  inner_join(df4.train) %>% 
  
  ggplot(aes(x = ds, y = yhat)) + 
  geom_ribbon(aes(ymin = yhat_lower, 
                  ymax = yhat_upper), 
              col = "grey", alpha = 0.3) + 
  # fitted value: 
  geom_line(aes(y = trend + additive_terms), 
            col = "black") + 
  # actuals: 
  geom_line(aes(y = y), 
            col = "blue") + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))


# fcast2 df: 
fcast2 %>%
  head %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip',
                           buttons = c('excel', "csv")))
#' ### Cross-validation
#' 

# Cross val: 
df7.2_metrics_m2 <- 
  cross_validation(m2, 
                   period = 1200, 
                   horizon = horizon_param, 
                   units = "hours") %>% 
  performance_metrics()

df7.2_metrics_m2 %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))


#+ model 3
#' ## Prophet model 3
#' 
# > Prophet model 3  ---------
# fit prophet model:  
m3 <- prophet(df4.train,
              holidays = df3.hols,
              changepoint.prior.scale = 0.05, # default is 0.05; Increasing it will make the trend more flexible:
              changepoint.range = 0.90, 
              yearly.seasonality = 5)  # default is 10; add Fourier terms for more flexibility in fitting yearly seasonality  

# future df: 
future <- make_future_dataframe(m3,
                                periods = horizon_param,
                                freq = "hour")


# Fcast:  
fcast3 <- predict(m3, future)

# Examine the fitted model and fcast
plot(m3, fcast3) + 
  add_changepoints_to_plot(m3)

prophet_plot_components(m3, fcast3)
prophet:::plot_yearly(m3)

# look at fitted values and actuals over last 14 days 
fcast3 %>% 
  filter(ds > t2.train_end - 14) %>% 
  inner_join(df4.train) %>% 
  
  ggplot(aes(x = ds, y = yhat)) + 
  geom_ribbon(aes(ymin = yhat_lower, 
                  ymax = yhat_upper), 
              col = "grey", alpha = 0.3) + 
  # fitted value: 
  geom_line(aes(y = trend + additive_terms), 
            col = "black") + 
  # actuals: 
  geom_line(aes(y = y), 
            col = "blue") + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))


# fcast df: 
fcast3 %>%
  head %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip',
                           buttons = c('excel', "csv")))

#' ### Cross-validation
#' 

# Cross val: 
df7.3_metrics_m3 <- 
  cross_validation(m3, 
                   period = 1200, 
                   horizon = horizon_param, 
                   units = "hours") %>% 
  performance_metrics()

df7.3_metrics_m3 %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))










#+ model-select
#' ## Model selection 
#' 
#' To perform model selection, we get a cross-validated RMSE for each model for
#' a 14-day horizon fcast, based on the training data (see above).
#'
#' Test data is not used for model selection. It is only used after model
#' selection to get our best possible estimate of the performance of the
#' selected model on completely new data.
#' 
#' **Based on cross-validated RMSE, Model 1 is clearly better than Model 2 and Model 3**
#' 
#' 


#' ## Test set accuracy
#'
#' If we haven't overfitted, test set accuracy should be pretty close to
#' cross-validated train set accuracy.

# > Test set accuracy -------------
df8.test_accuracy <-
  fcast1 %>%
  select(ds,
         yhat,
         yhat_lower,
         yhat_upper) %>%
  mutate(date = date(ds),
         hour = hour(ds)) %>%
  filter(date >= t3.test_start) %>%

  left_join(df5.test,
            by = c("date" = "date",
                   "hour" = "hour")) %>%
  mutate(error = y - yhat)
  
# str(df8.test_accuracy)
# summary(df8.test_accuracy)
# sqrt(mean((df8.test_accuracy$error)^2))

#' **RMSE on test set is `r sqrt(mean((df8.test_accuracy$error)^2))`**
#' 

# look at fitted values and actuals over fcast horizon:  
df8.test_accuracy %>%   
  ggplot(aes(x = ds.x,
             y = yhat)) + 
  geom_ribbon(aes(ymin = yhat_lower, 
                  ymax = yhat_upper), 
              col = "grey",
              alpha = 0.3) + 
  # fitted value: 
  geom_line(aes(y = yhat), 
            col = "black") + 
  # actuals: 
  geom_line(aes(y = y), 
            col = "blue") + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))





  
#' # Analyzing residuals from model 
#' 
# Analyzing residuals --------

df9.train_errors <- 
  fcast1 %>%
  select(ds,
         yhat,
         yhat_lower,
         yhat_upper) %>%
  mutate(date = date(ds),
         hour = hour(ds) %>% as.factor()) %>%
  filter(date < t3.test_start) %>%
  
  left_join(df2.census,
            by = c("date" = "date",
                   "hour" = "hour")) %>%
  mutate(error = ed_census - yhat) %>% 
  select(ds, 
         ed_census, 
         yhat, 
         error)

# sample 100: 
df9.train_errors %>% 
  sample_n(100) %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))
                           
# plot
df9.train_errors %>% 
  ggplot(aes(x = error)) + 
  geom_density() + 
  geom_vline(xintercept = 0, 
             col = "blue") + 
  
  labs(title = "Training data errors (actual minus fitted)") + 
  
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))
  

df9.train_errors %>% 
  ggplot(aes(x = ds, 
             y = error)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth()
  
qqnorm(df9.train_errors$error)  
qqline(df9.train_errors$error, col = "red")  

# try fitting a normal dist: 
m4.normal_train_error <- 
  df9.train_errors$error %>% 
  fitdistrplus::fitdist("norm")

# summary(m4.normal_train_error)
# m4.normal_train_error$estimate[1] %>% unname
# m4.normal_train_error$estimate[2] %>% unname

#' In train data, approx 95% of all fitted values are 
#' within `r m4.normal_train_error$estimate[2] %>% unname *2` patients of the 
#' actual census value.
#' 

#' # Retrain selected model on full data 
#' 
# Retrain selected model on full data ----------
# todo













#' *****************************************************************

#' # Appendix
#' 
#' ## Libraries and connections 
#' 
# library(tidyverse)
# library(denodoExtractor)
# library(janitor)
# library(lubridate)
# library(prophet)
# library(DT)
# library(plotly)
# 
# setup_denodo()
# 
# cnx <- DBI::dbConnect(odbc::odbc(), dsn = "cnx_SPDBSCSTA001")
# 
# vw_ed_census <- dplyr::tbl(cnx, dbplyr::in_schema("DSSI.dbo", 
#                                                   "[AISAKE_Hourly_Census]"))

#' ## Outputs 
#' 
# write_csv(df2.census,
#           here::here("results", 
#                      "dst", 
#                      "2019-11-06_rhs_ed-census-by-hour-from-2017.csv"))
             