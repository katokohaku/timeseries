require(tidyverse)
require(pforeach)

date_seq <- seq(as.Date("2016-1-01"), as.Date("2016-12-31"), by="days")
max.measure <- 20
min.measure <- 3
n.major <- 80
n.minor <- 20
n.pop <- n.major + n.minor

trend.down <- rnorm(n = 10^4, mean = -0.3)
trend.up   <- rnorm(n = 10^4, mean = +0.3)

rbind(data.frame(trend="down", value = trend.down),
      data.frame(trend="up",   value = trend.up)) %>% 
  ggplot(aes(x = value, fill = trend)) +
  geom_histogram(position = "identity", alpha = 0.8)


# population with negative trends
p.negative <- npforeach(i = 1:n.pop, .combine = rbind)({
  
  samp <- trend.down
  label <- "down"
  if(i > n.major){
    samp <- trend.up
    label <- "up"
  }
  n.obs <- sample(min.measure:max.measure,1)
  dates <- sample(date_seq,   size = n.obs, replace = FALSE) %>% sort
  moment<- sample(samp, size = n.obs, replace = FALSE)
  
  data.frame(ID = i, date = dates, value = cumsum(moment), src = as.factor(label), class = "negative")
})
tail(p.negative)
# population with positive trends
p.positive <- npforeach(i = 1:n.pop, .combine = rbind)({

  samp <- trend.down
  label <- "down"
  if(i > n.minor){
    samp <- trend.up
    label <- "up"
  }
  n.obs <- sample(min.measure:max.measure,1)
  dates <- sample(date_seq,   size = n.obs, replace = FALSE) %>% sort
  moment<- sample(samp, size = n.obs, replace = FALSE)
  
  data.frame(ID = i, date = dates, value = cumsum(moment), src = as.factor(label), class = "positive")
})
tail(p.positive)

ggplot(data=p.positive, aes(x=date, y=value, group = factor(ID), colour=factor(src))) +
  geom_line()
ggplot(data=p.negative, aes(x=date, y=value, group = factor(ID), colour=factor(src))) +
  geom_line()
