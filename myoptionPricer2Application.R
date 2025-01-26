# loading packages
library(tidyverse)

# 1. remove package if it exists ===============================================
remove.packages("myoptionPricer2")
detach("package:myoptionPricer2", unload = TRUE) # if it still is in memory

# or from source (rebuilt automatically)
install.packages("myoptionPricer2_0.1.0.tar.gz",
                 type = "source",
                 repos = NULL)


# 3. call the function from the package ========================================
myoptionPricer2::getEuropeanUpAndInCallPrice(126, 100, 95, 0.2, 0.06, 0.5,150, 10000)

# 4. build an R wrapping function: option price vs. time to maturity ===========
getMCEuropeanCallPriceWithExpiry <- function (expiry) {
  return(
    myoptionPricer2::getEuropeanUpAndInCallPrice(126, 100, 95, 0.2, 0.06, expiry,150, 10000)
  )
}

# call the wrapping function
getMCEuropeanCallPriceWithExpiry(0.5)

# arguments values of values of function
expiry <- seq(0.01, 1, by = 0.01)
prices <- sapply(expiry, getMCEuropeanCallPriceWithExpiry)

# visualization: options price vs. expiry
tibble( expiry, prices) %>%
  ggplot(aes(expiry, prices)) +
  geom_point(col = "red") +
  labs(
    x     = "time to maturity",
    y     = "option price",
    title = "price of arithmetic European call option vs. time  to maturity",
    caption = "source: own calculations with the optionPricer2 package")

# 5. build an R wrapping function: option price vs. number of loops ============
getMCEuropeanCallPriceWithLoops <- function (loops) {
  return(
    myoptionPricer2::getEuropeanUpAndInCallPrice(126, 100, 95, 0.2, 0.06, 0.5, loops,150)
  )
}

# call the wrapping function
getMCEuropeanCallPriceWithLoops(500)

# arguments values of values of function
loops  <- seq(100, 10000, by = 100)
prices <- sapply(loops, getMCEuropeanCallPriceWithLoops)

# visualization: options price vs. numbers of loops
tibble(expiry, prices) %>%
  ggplot(aes(expiry, prices)) +
  geom_point(col = "blue") +
  labs(
    x     = "number of loops",
    y     = "option price",
    title = "price of arithmetic European call option vs. number of loops",
    caption = "source: own calculations with the optionPricer2 package")

# note the same seed within one second!




# 6. build an R wrapping function: option price vs. time to maturity and volatility =======
getMCEuropeanCallPriceWithExpiryAndVol <- function (vol,expiry) {
  return(
    myoptionPricer2::getEuropeanUpAndInCallPrice(126, 110, 105, vol, 0.05,expiry ,150, 10000))
}

# call function once
getMCEuropeanCallPriceWithExpiryAndVol(0.21, 0.75)

# sequences of argument values
expiry <- seq(0.01, 1, by = 0.01)
vol  <- c(0.001, 0.01, 0.02, 0.05, 0.1, 0.15, 0.2, 0.3, 0.5, 1)

grid      <- expand.grid( vol = vol,expiry = expiry)
prices    <- mapply(getMCEuropeanCallPriceWithExpiryAndVol,
                    vol = grid$vol,expiry = grid$expiry)


result.df <- data.frame(grid)
head(result.df)

# visualization: options price vs. time to maturity of the option and volatility
grid %>%
  as_tibble() %>%
  bind_cols(price = prices) %>%
  ggplot(aes(x = expiry, y = price, group = vol, colour = vol)) +
  geom_line() +
  geom_point(size = 1, shape = 21, fill = "white") +
  labs(
    x     = "time to maturity of the option",
    y     = "option price",
    title = "price of arithmetic European call option vs. time to maturity of the option and volatility",
    caption = "source: own calculations with the myoptionPricer2 package")



