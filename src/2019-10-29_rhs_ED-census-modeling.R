

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


#' # Data
#' 

df1.census_raw <- 
  vw_ed_census %>% 
  filter(FacilityShortName == "RHS") %>% 
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

df2.census <- 
  df1.census_raw %>% 
  mutate(date = date(short_dt), 
         hour = hour(short_dt) %>% as.factor(), 
         weekday = weekdays(short_dt) %>% as.factor(), 
         calendar_month = as.factor(calendar_month), 
         calendar_year = as.factor(calendar_year), 
         effective_holiday_flag = ifelse(is.na(effective_holiday_flag), 
                                               0, 
                                               effective_holiday_flag)) %>% 
  select(short_dt, 
         date, 
         hour, 
         weekday, 
         month = calendar_month, 
         year = calendar_year, 
         hol = effective_holiday_flag, 
         ed_census, )
  

# str(df2.census)
# summary(df2.census)


#' Pull out holidays as a dataframe: 
#' 

df3.hols <- 
  df2.census %>% 
  filter(hol == 1) %>% 
  mutate(hol_name = NA, 
         lower_window = -1, 
         upper_window = 1) %>% 
  select(hol_name, 
         ds = date, 
         lower_window, 
         upper_window) %>% View
  distinct()

df3.hols %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))
                           


#' # Plots 
#' 

df2.census %>% 
  ggplot(aes(x = date, 
             y = ed_census)) + 
  geom_line() +
  geom_smooth()


df2.census %>% 
  ggplot(aes(x = ed_census)) + 
  geom_density() + 
  facet_wrap(~year)


#' # Time series analysis 
#' 
#' ## Parameters 
#' 
horizon_param <- 30 

df3.census_for_prophet <- 
  df2.census %>% 
  select(ds = date,
         y = ed_census)

# future df: 
future <- make_future_dataframe(m, 
                                periods = horizon_param,
                                freq = "day")  

# fit prophet model:  
m <- prophet(df3.census_for_prophet)

fcast <- predict(m, future)

plot(m, fcast)

