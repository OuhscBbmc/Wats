rm(list=ls(all=TRUE))
require(Wats)
dsLinear <- CountyMonthBirthRate2014Version[CountyMonthBirthRate2014Version$CountyName=="oklahoma", ]

tsData <- stats::ts(
  data = dsLinear$BirthRate, 
  start = as.integer(dsLinear[1, c("Year", "Month")]), 
  end = as.integer(dsLinear[nrow(dsLinear), c("Year", "Month")]),
  frequency = 12
)

#Create unsmoothed and smoothed version
seasonalClassic <- stats::decompose(tsData)
plot(seasonalClassic)

seasonalLoess <- stats::stl(x = tsData, s.window = "periodic") #Watch out, the 2nd & 3rd columns are swapped, compared to `decompose()`
plot(seasonalLoess)

#Seasonality isn't accounted for at all
require(BayesSingleSub)
beforeNaive <- dsLinear[dsLinear$StageID==1, "BirthRate", ]
afterNaive <- dsLinear[dsLinear$StageID==2, "BirthRate", ]
BayesSingleSub::trendtest.Gibbs.AR(beforeNaive, afterNaive)
BayesSingleSub::trendtest.MC.AR(beforeNaive, afterNaive)

### Bayes
# Seasonality is accounted for without a smoother
beforeClassic <- seasonalClassic$trend[dsLinear$StageID==1]
afterClassic <- seasonalClassic$trend[dsLinear$StageID==2]
repCount <- 1000#000
(gClassic <- BayesSingleSub::trendtest.Gibbs.AR(beforeClassic[!is.na(beforeClassic)], afterClassic[!is.na(afterClassic)], return.chains=F, iterations=repCount))
(mcClassic <- BayesSingleSub::trendtest.MC.AR(beforeClassic[!is.na(beforeClassic)], afterClassic[!is.na(afterClassic)], iterations=repCount))
summary(mcClassic)
# coda::gelman.diag(g$chains) #it needs multiple chains

#Seasonality is accounted for after a loess is fit through it.
beforeLoess <- seasonalLoess$time.series[dsLinear$StageID==1, 2]
afterLoess <- seasonalLoess$time.series[dsLinear$StageID==2, 2]
BayesSingleSub::trendtest.Gibbs.AR(beforeLoess, afterLoess)
BayesSingleSub::trendtest.MC.AR(beforeLoess, afterLoess)

### McLeod et al approach, which is consistent with many others, including Rodgers et al, 2005.
# Seasonality is accounted for without a smoother
lag <- 1 #Significant for many different values of lag, including 1
y <- seasonalClassic$trend[(lag+1):length(seasonalClassic$trend)]
y1 <- seasonalClassic$trend[1:(length(seasonalClassic$trend)-lag)]
step <- c(rep(0, times=sum(dsLinear$StageID==1)-lag), rep(1, times=sum(dsLinear$StageID==2)))
dsClassic <- data.frame(y=y, y1=y1, step=step)
rm(lag, y, y1, step)
fitClassic <-  glm(y ~ 1 + step + y1, data=dsClassic)
summary(fitClassic)

#Seasonality is accounted for after a loess is fit through it.
lag <- 1 #Significant for many different values of lag, including 1
trendLineLoess <- as.numeric(seasonalLoess$time.series[, 2])
y <- trendLineLoess[(lag+1):length(trendLineLoess)]
y1 <- trendLineLoess[1:(length(trendLineLoess) - lag)]
step <- c(rep(0, times=sum(dsLinear$StageID==1)-lag), rep(1, times=sum(dsLinear$StageID==2)))
dsLoess <- data.frame(y=y, y1=y1, step=step)
rm(lag, y, y1, step)
fitLoess <-  glm(y ~ 1 + step + y1, data=dsLoess)
summary(fitLoess)

#Potentially useful links:
# http://cran.us.r-project.org/web/packages/BayesSingleSub/BayesSingleSub.pdf
# http://cran.r-project.org/web/packages/forecast/forecast.pdf
# http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html#decomposing-seasonal-data
# http://books.google.com/books?id=oAIuJ2JQIngC&pg=PA57&lpg=PA57&dq=interrupted+time+series++seasonal&source=bl&ots=sKARoXV6p9&sig=gQQnybPUu5wxZu4jR6rDX2cs84o&hl=en&sa=X&ei=Ea24UvPhCcbbyQG8sYHYAw&ved=0CGwQ6AEwBg#v=onepage&q=interrupted%20time%20series%20%20seasonal&f=false
