require(fpp3)
require(tidyquant)
require(stargazer)
require(kableExtra)

# Vector of variables to be gathered
vars <- c("PCEPI","UNRATE","EXPINF1YR","MICH","INDPRO")
# Use tidyquant function tq_get to gather economic data from FRED and format as a tsibble
fred_data <- tq_get(vars,
                    get = "economic.data",
                    from = "1982-01-01") %>%
  mutate(Month = yearmonth(date), value = price) %>%
  select(-c(date, price)) %>%
  as_tsibble(index = Month, key = symbol)
# Visualize the raw data
fred_data %>%
  autoplot()