

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
  mutate(hol_name = NA, 
         lower_window = 0, 
         upper_window = 0) %>% # include 0 days before and after the hol
  select(hol_name, 
         ds = date, 
         lower_window, 
         upper_window) %>% # %>% View
  distinct() %>% 
  arrange(ds)

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



#' # Time series analysis 
#' 
#' ## Parameters 
#' 
# Time series analysis ----------
horizon_param <- 30 

# df3.census_for_prophet <- 
#   df2.census %>% 
#   select(ds = date,
#          y = ed_census)
# 
# # future df: 
# future <- make_future_dataframe(m, 
#                                 periods = horizon_param,
#                                 freq = "day")  
# 
# # fit prophet model:  
# m <- prophet(df3.census_for_prophet)
# 
# fcast <- predict(m, future)

# plot(m, fcast)

