<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Figures for the MBR Manuscript}
-->

# Figures for the MBR Manuscript
This vignette produces the graphs included in the initial MBR manuscript.




## Figure 2: Rolling Linear
Smoothed monthly birth rates (General Fertility Rates; GFRs) for Oklahoma County, 1990-1999, plotted in a linear plot.  The top plot shows the connected raw data with a February smoother; the middle plot shows smoothing with a 12-month moving average, blue/green line, superimposed on a February smoother, red line); the bottom plot shows the smoothers and confidence bands, which are H-spreads defined using the distribution of GFR's for the given month and 11 previous months.


```r
library(Wats) #Load the package into the current R session.
```


## Session Info
For the sake of documentation and reproducibility, the current vignette was build on a system using the following software.


```
Report created by Will at 12/15/2013 7:39:36 PM, Central Standard Time
```

```
R Under development (unstable) (2013-12-06 r64409)
Platform: x86_64-w64-mingw32/x64 (64-bit)

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] Wats_0.1-7 knitr_1.5 

loaded via a namespace (and not attached):
[1] evaluate_0.5.1 formatR_0.10   stringr_0.6.2  tools_3.1.0   
```

