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
seasonalClassic <- decompose(tsData)
plot(seasonalClassic)


seasonalLoess <- stl(x = tsData, s.window = "periodic")
plot(seasonalLoess)

#Seasonality isn't accounted for at all
require(BayesSingleSub)
before <- dsLinear[dsLinear$StageID==1, "BirthRate", ]
after <- dsLinear[dsLinear$StageID==2, "BirthRate", ]
trendtest.Gibbs.AR(before, after)
trendtest.MC.AR(before, after)

#Seasonality is accounted for without a smoother
before <- seasonalClassic$trend[dsLinear$StageID==1]
after <- seasonalClassic$trend[dsLinear$StageID==2]
repCount <- 1000#000
(g <- trendtest.Gibbs.AR(before[!is.na(before)], after[!is.na(after)], return.chains=F, iterations=repCount))
(mc <- trendtest.MC.AR(before[!is.na(before)], after[!is.na(after)], iterations=repCount))
summary(mc)
# coda::gelman.diag(g$chains) #it neds multiple chains

#Seasonality is accounted for after a loess is fit through it.
before <- seasonalLoess$time.series[dsLinear$StageID==1, 2]
after <- seasonalLoess$time.series[dsLinear$StageID==2, 2]
trendtest.Gibbs.AR(before, after)
trendtest.MC.AR(before, after)

#McLeod et al approach without smoothed trend
lag <- 3 #Works for many values, including 1
y <- seasonalClassic$trend[(lag+1):length(seasonalClassic$trend)]
y1 <- seasonalClassic$trend[1:(length(seasonalClassic$trend)-lag)]
step <- c(rep(0, times=sum(dsLinear$StageID==1)-lag), rep(1, times=sum(dsLinear$StageID==2)))
lag1 <-  glm(y ~ 1 + step + y1)
summary(lag1)


#McLeod et al approach with smoothed trend
lag <- 1 #Works for many values, including 1
#y <- seasonalLoess$time.series[(lag+1):length(seasonalLoess$time.series), 2]

trendLine <- as.numeric(seasonalLoess$time.series[, 2])
y <- trendLine[(lag+1):length(trendLine)]
y1 <- trendLine[1:(length(trendLine) - lag)]
step <- c(rep(0, times=sum(dsLinear$StageID==1)-lag), rep(1, times=sum(dsLinear$StageID==2)))
lag1 <-  glm(y ~ 1 + step + y1)
summary(lag1)

#Potentially useful links:
# http://cran.us.r-project.org/web/packages/BayesSingleSub/BayesSingleSub.pdf
# http://cran.r-project.org/web/packages/forecast/forecast.pdf
# http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html#decomposing-seasonal-data
# http://books.google.com/books?id=oAIuJ2JQIngC&pg=PA57&lpg=PA57&dq=interrupted+time+series++seasonal&source=bl&ots=sKARoXV6p9&sig=gQQnybPUu5wxZu4jR6rDX2cs84o&hl=en&sa=X&ei=Ea24UvPhCcbbyQG8sYHYAw&ved=0CGwQ6AEwBg#v=onepage&q=interrupted%20time%20series%20%20seasonal&f=false
