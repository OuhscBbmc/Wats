rm(list=ls(all=TRUE))
library(Wats)
ds_linear <- county_month_birth_rate_2014_version[county_month_birth_rate_2014_version$county_name=="oklahoma", ]

tsData <- stats::ts(
  data = ds_linear$birth_rate,
  start = as.integer(ds_linear[1, c("year", "month")]),
  end = as.integer(ds_linear[nrow(ds_linear), c("year", "month")]),
  frequency = 12
)

#Create unsmoothed and smoothed version
seasonalClassic <- stats::decompose(tsData)
plot(seasonalClassic)

seasonalLoess <- stats::stl(x = tsData, s.window = "periodic") #Watch out, the 2nd & 3rd columns are swapped, compared to `decompose()`
plot(seasonalLoess)

#Seasonality isn't accounted for at all
library(BayesSingleSub)
beforeNaive <- ds_linear[ds_linear$stage_id==1, "birth_rate", ]
afterNaive <- ds_linear[ds_linear$stage_id==2, "birth_rate", ]
BayesSingleSub::trendtest.Gibbs.AR(beforeNaive, afterNaive)
BayesSingleSub::trendtest.MC.AR(beforeNaive, afterNaive)

### Bayes
# Seasonality is accounted for without a smoother
beforeClassic <- seasonalClassic$trend[ds_linear$stage_id==1]
afterClassic <- seasonalClassic$trend[ds_linear$stage_id==2]
mcmcRepCount <- 1000#000
(gClassic <- BayesSingleSub::trendtest.Gibbs.AR(beforeClassic[!is.na(beforeClassic)], afterClassic[!is.na(afterClassic)], return.chains=FALSE, iterations=mcmcRepCount))
(mcClassic <- BayesSingleSub::trendtest.MC.AR(beforeClassic[!is.na(beforeClassic)], afterClassic[!is.na(afterClassic)], iterations=mcmcRepCount))
summary(mcClassic)
# coda::gelman.diag(g$chains) #it needs multiple chains

#Seasonality is accounted for after a loess is fit through it.
beforeLoess <- seasonalLoess$time.series[ds_linear$stage_id==1, 2]
afterLoess <- seasonalLoess$time.series[ds_linear$stage_id==2, 2]
BayesSingleSub::trendtest.Gibbs.AR(beforeLoess, afterLoess, iterations=mcmcRepCount)
BayesSingleSub::trendtest.MC.AR(beforeLoess, afterLoess, iterations=mcmcRepCount)

### McLeod et al approach, which is consistent with many others, including Rodgers et al, 2005.
# Seasonality is accounted for without a smoother
lag <- 1 #Significant for many different values of lag, including 1
y <- seasonalClassic$trend[(lag+1):length(seasonalClassic$trend)]
y1 <- seasonalClassic$trend[1:(length(seasonalClassic$trend)-lag)]
step <- c(rep(0, times=sum(ds_linear$stage_id==1)-lag), rep(1, times=sum(ds_linear$stage_id==2)))
dsClassic <- tibble::tibble(y=y, y1=y1, step=step)
rm(lag, y, y1, step)
fitClassic <-  glm(y ~ 1 + step + y1, data=dsClassic)
summary(fitClassic)

#Seasonality is accounted for after a loess is fit through it.
lag <- 1 #Significant for many different values of lag, including 1
trendLineLoess <- as.numeric(seasonalLoess$time.series[[2]])
y <- trendLineLoess[(lag+1):length(trendLineLoess)]
y1 <- trendLineLoess[1:(length(trendLineLoess) - lag)]
step <- c(rep(0, times=sum(ds_linear$stage_id==1)-lag), rep(1, times=sum(ds_linear$stage_id==2)))
dsLoess <- tibble::tibble(y=y, y1=y1, step=step)
rm(lag, y, y1, step)
fitLoess <-  glm(y ~ 1 + step + y1, data=dsLoess)
summary(fitLoess)

#Potentially useful links:
# http://cran.us.r-project.org/web/packages/BayesSingleSub/BayesSingleSub.pdf
# http://cran.r-project.org/web/packages/forecast/forecast.pdf
# http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html#decomposing-seasonal-data
# http://books.google.com/books?id=oAIuJ2JQIngC&pg=PA57&lpg=PA57&dq=interrupted+time+series++seasonal&source=bl&ots=sKARoXV6p9&sig=gQQnybPUu5wxZu4jR6rDX2cs84o&hl=en&sa=X&ei=Ea24UvPhCcbbyQG8sYHYAw&ved=0CGwQ6AEwBg#v=onepage&q=interrupted%20time%20series%20%20seasonal&f=false
