rm(list=ls())
graphics.off()
knitr::opts_chunk$set(echo = TRUE,
                      message = TRUE,
                      warning = TRUE)
require(fpp3)
require(tsibble)
require(tidyverse)
require(tidyquant)
require(kableExtra)
require(reshape2)

# Vector of variables to be gathered:
vars <- c("PCEPI",
          "UNRATE",
          "EXPINF1YR",
          "MICH",
          "INDPRO")

# Use tidyquant function tq_get to gather economic data from FRED and format as a tsibble
fred_data <- tq_get(vars,
                    get = "economic.data",
                    from = "1982-01-01",
                    to = "2022-04-01") %>%
  mutate(Month = yearmonth(date), value = price) %>%
  select(-c(date, price)) %>%
  as_tsibble(index = Month, key = symbol)

# Pivot the data to be in a more conventional wide format
fred_dataw <- fred_data %>%
  pivot_wider(names_from = symbol, values_from = value) %>%
  as_tsibble()

# Transform variables
tdata <- fred_dataw %>% select(c(PCEPI, UNRATE, EXPINF1YR, MICH, INDPRO)) %>%
  # transformed inflation
  mutate(infl = 1200*log(PCEPI/lag(PCEPI))) %>%
  # differenced inflation
  mutate(dinfl = infl - lag(infl,1)) %>%
  # differenced inflation 12
  mutate(dinfl12 = 100*log(PCEPI/lag(PCEPI,12)) - lag(infl,12)) %>%
  # differenced unrate
  mutate(unrate = UNRATE - lag(UNRATE)) %>%
  # differenced expected inf
  mutate(expinf1yr = EXPINF1YR - lag(EXPINF1YR)) %>%
  # differenced mich
  mutate(mich = MICH - lag(MICH)) %>% 
  # transformed indpro
  mutate(indpro = 1200*log(INDPRO/lag(INDPRO))) %>%                       
  # keep only transformed variables
  select(-c(PCEPI, UNRATE, EXPINF1YR, MICH, INDPRO)) %>%
  drop_na()

# Split the data into training and testing groups
train_data <- tdata %>% filter_index(~ "2018-12")
test_data <- tdata %>% filter_index("2019-01" ~ .)

# Compare ACF plots before and after transformation
# fred_dataw %>% ACF(MICH) %>%
#   autoplot()
# tdata %>% ACF(mich) %>%
#   autoplot()

# Use facet grid to visualize raw data for variables
fred_data %>%
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(vars(symbol), scales = "free_y") +
  labs(y = " ") +
  theme_bw()

#Observe the plots of the transformed variables
tdatam <- melt(tdata, "Month")
ggplot(tdatam, aes(Month, value)) +
  geom_line() +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  theme_bw()

# Fit four models with the Phillips curve model specifications using unemployment rate, 1yr expected inflation, Michigan inflation expectations, and industrial production
fit_all <- train_data %>%
  model(
    mUN = TSLM(dinfl12 ~ 1 +
                 lag(dinfl,12) + lag(dinfl,13) + lag(dinfl,14) +
                 lag(dinfl,15) + lag(dinfl,16) + lag(dinfl,17) +
                 lag(dinfl,18) + lag(dinfl,19) + lag(dinfl,20) +
                 lag(dinfl,21) + lag(dinfl,22) + lag(dinfl,23) +
                 lag(unrate,12) + lag(unrate,13) + lag(unrate,14) +
                 lag(unrate,15) + lag(unrate,16) + lag(unrate,17) +
                 lag(unrate,18) + lag(unrate,19) + lag(unrate,20) +
                 lag(unrate,21) + lag(unrate,22) + lag(unrate,23)
    ),
    mEXPINF = TSLM(dinfl12 ~ 1 +
                     lag(dinfl,12) + lag(dinfl,13) + lag(dinfl,14) +
                     lag(dinfl,15) + lag(dinfl,16) + lag(dinfl,17) +
                     lag(dinfl,18) + lag(dinfl,19) + lag(dinfl,20) +
                     lag(dinfl,21) + lag(dinfl,22) + lag(dinfl,23) +
                     lag(expinf1yr,12) + lag(expinf1yr,13) + lag(expinf1yr,14) +
                     lag(expinf1yr,15) + lag(expinf1yr,16) + lag(expinf1yr,17) +
                     lag(expinf1yr,18) + lag(expinf1yr,19) + lag(expinf1yr,20) +
                     lag(expinf1yr,21) + lag(expinf1yr,22) + lag(expinf1yr,23) 
    ),
    mMICH = TSLM(dinfl12 ~ 1 +
                   lag(dinfl,12) + lag(dinfl,13) + lag(dinfl,14) +
                   lag(dinfl,15) + lag(dinfl,16) + lag(dinfl,17) +
                   lag(dinfl,18) + lag(dinfl,19) + lag(dinfl,20) +
                   lag(dinfl,21) + lag(dinfl,22) + lag(dinfl,23) +
                   lag(mich,12) + lag(mich,13) + lag(mich,14) +
                   lag(mich,15) + lag(mich,16) + lag(mich,17) +
                   lag(mich,18) + lag(mich,19) + lag(mich,20) +
                   lag(mich,21) + lag(mich,22) + lag(mich,23) 
    ),
    mINDPRO = TSLM(dinfl12 ~ 1 +
                     lag(dinfl,12) + lag(dinfl,13) + lag(dinfl,14) +
                     lag(dinfl,15) + lag(dinfl,16) + lag(dinfl,17) +
                     lag(dinfl,18) + lag(dinfl,19) + lag(dinfl,20) +
                     lag(dinfl,21) + lag(dinfl,22) + lag(dinfl,23) +
                     lag(indpro,12) + lag(indpro,13) + lag(indpro,14) +
                     lag(indpro,15) + lag(indpro,16) + lag(indpro,17) +
                     lag(indpro,18) + lag(indpro,19) + lag(indpro,20) +
                     lag(indpro,21) + lag(indpro,22) + lag(indpro,23) 
    )
  )

# Fit the combination model
fit_combo <- fit_all %>% mutate(ensem = (mUN + mEXPINF + mMICH + mINDPRO)/4)

# Create the forecast using the combination model
forecast <- fit_combo %>% forecast(new_data = test_data)
# Assess in-sample forecast accuracy
insample <- accuracy(fit_combo)
# Assess out-of-sample forecast accuracy
outsample <- accuracy(forecast, tdata)

#Plot the estimated models
forecast %>% autoplot(filter(tdata, year(Month) > 2016), level = c(95)) +
  labs(x = "Month", y = "Percent Inflation") +
  ggtitle("Predicted Monthly Inflation Rate Using 5 Models") +
  theme_bw()

# Create a table to display in-sample accuracy between models
insample %>%
  select(c(".model", ".type", "MAPE")) %>%
  arrange(MAPE) %>%
  kbl(col.names = c("Model", "Type", "MAPE"), align = "ccc") %>% 
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  row_spec(1,bold=T,hline_after = T)

# Create a table to display out-of-sample accuracy between models
outsample %>%
  select(c(".model", ".type", "MAPE")) %>%
  arrange(MAPE) %>%
  kbl(col.names = c("Model", "Type", "MAPE"), align = "ccc") %>% 
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  row_spec(1,bold=T,hline_after = T)