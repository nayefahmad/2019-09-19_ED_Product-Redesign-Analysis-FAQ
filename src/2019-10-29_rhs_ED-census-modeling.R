

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

setup_denodo()

cnx <- DBI::dbConnect(odbc::odbc(), dsn = "cnx_SPDBSCSTA001")

vw_ed_census <- dplyr::tbl(cnx, dbplyr::in_schema("DSSI.dbo", 
                                                  "[AISAKE_hourly_daily_EDCensus_2]"))

#+ rest

#' # Todo: 
#' 
#' 1. check NAs in census table 
#' 
#' 2. Missing holidays? 
#' 
#' 

#' # Parameters 
#' 
#' Prophet hangs with ten years of hourly data. Let's start with 3 years. 
#' 
# Parameters -----------
start_date_id_param <- "20170101"
site <- "RHS"

#' # Data
#' 
# Data -----------
df1.census_raw <- 
  vw_ed_census %>% 
  filter(FacilityShortName == site, 
         short_dt >= start_date_id_param) %>% 
  group_by(short_dt,
           ShortDate,
           TOD,
           DOW, 
           CalendarMonth, 
           CalendarYear, 
           Effective_Holiday_Flag) %>% 
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
         effective_holiday_flag = ifelse(is.na(effective_holiday_flag), 
                                               0, 
                                               effective_holiday_flag) %>% as.factor) %>% 
  select(short_dt, 
         date, 
         hour, 
         weekday, 
         month = calendar_month,  # rename within select( )
         year = calendar_year, 
         hol = effective_holiday_flag, 
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
  filter(hol == 1) %>% 
  mutate(holiday = NA, 
         lower_window = 0, 
         upper_window = 0) %>% # include 0 days before and after the hol
  select(holiday, 
         ds = date, 
         lower_window, 
         upper_window) %>% # %>% View
  distinct() %>% 
  arrange(ds)

# df3.hols %>% write.table(file = "clipboard", sep = "\t", row.names = FALSE)



# df3.hols %>% 
#   datatable(extensions = 'Buttons',
#             options = list(dom = 'Bfrtip', 
#                            buttons = c('excel', "csv")))
                           


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
  facet_wrap(~year); ggplotly(p)

p <- df2.census %>% 
  filter(hour %in% seq(7,14)) %>% 
  ggplot(aes(x = hol, 
             y = ed_census, 
             label = date)) + 
  geom_boxplot() + 
  labs(title = "Hours from 7am to 2pm") + 
  facet_wrap(~year); ggplotly(p)




#+ models 
#' # Models 
#' 
#' ## Parameters and data splitting  
#' 
# Models ----------
# > Train/test split -------------
train_start <- df2.census$date %>% min
test_end <- df2.census$date %>% max

num_days <- difftime(test_end, train_start, units = "days") %>% as.numeric()

# 80% training data 
train_length <- (.80 * num_days) %>% round()
train_end <- train_start + train_length

# 20% test data 
# validation_length <- (.90 * num_days) %>% round()
test_start <- train_end + 1 
# validation_end <- train_start + validation_length

horizon_param <- 24*7*6  # unit: hours 

#' **We take train data from `r train_start` to `r train_end`.** 
#' 
#' **Test data is from `r test_start` to `r test_end`**
#' 


df4.train <-
  df2.census %>%
  select(ds = short_dt,
         y = ed_census) %>% 
  filter(ds <= train_end)

df5.test <- 
  df2.census %>%
  select(ds = short_dt,
         y = ed_census) %>% 
  filter(ds >= test_start) %>% 
  mutate(date = date(ds), 
         hour = hour(ds)) %>% 
  arrange(ds)



#' ## Prophet model 1 
#' 

# > Prophet model 1  ---------
# fit prophet model:  
m1 <- prophet(df4.train,
              changepoint.prior.scale = 0.05) # default is 0.05; Increasing it will make the trend more flexible:

# future df: 
future <- make_future_dataframe(m1,
                                periods = horizon_param,
                                freq = "hour")


# Fcast:  
fcast <- predict(m1, future)

# Examine the fitted model and fcast
plot(m1, fcast) + 
  add_changepoints_to_plot(m1)

prophet_plot_components(m1, fcast)
prophet:::plot_yearly(m1)


# fcast df: 
fcast %>%
  head %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip',
                           buttons = c('excel', "csv")))

# Cross val: 
df7.1_metrics_m1 <- 
  cross_validation(m1, 
                   period = 1000, 
                   horizon = horizon_param, 
                   units = "hours") %>% 
  performance_metrics()

df7.1_metrics_m1 %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))

#' Note that CI coverage is quite poor - below 70%
#'                            
                           
#' ## Model selection 
#' 



#' ## Test set accuracy 
#' 


df6.test_accuracy <- 
  fcast %>% 
  select(ds, 
         yhat, 
         yhat_lower, 
         yhat_upper) %>% 
  mutate(date = date(ds), 
         hour = hour(ds)) %>% 
  filter(date >= min_date + train_end & hour > 0) %>% 
  
  left_join(df5.test, 
            by = c("date" = "date", 
                   "hour" = "hour")) %>% 
  mutate(error = y - yhat)
  
str(df6.test_accuracy)
summary(df6.test_accuracy$error)

df6.test_accuracy %>% 
  filter(date < "2019-04-15") %>%
  ggplot(aes(x = ds.x, 
             y = yhat)) + 
  geom_ribbon(aes(x = ds.x, 
                  ymin = yhat_lower,
                  ymax = yhat_upper), 
              fill = "grey80",
              alpha = 0.5) + 
  geom_line() + 
  geom_point() + 
  
  geom_line(aes(x = ds.x, 
                y = y), 
            col = "skyblue")
  
  

