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
Report created by Will at 12/15/2013 9:12:26 PM, Central Standard Time
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
 [1] NlsyLinks_1.302    knitr_1.5          Wats_0.1-8         devtools_1.4.1     testit_0.3         zoo_1.7-10        
 [7] RColorBrewer_1.0-5 plyr_1.8           lubridate_1.3.2    ggplot2_0.9.3.1    colorspace_1.2-4   roxygen2_3.0.0    

loaded via a namespace (and not attached):
 [1] brew_1.0-6      codetools_0.2-8 dichromat_2.0-0 digest_0.6.4    evaluate_0.5.1  formatR_0.10    grid_3.1.0     
 [8] gtable_0.1.2    httr_0.2        labeling_0.2    lattice_0.20-24 MASS_7.3-29     memoise_0.1     munsell_0.4.2  
[15] parallel_3.1.0  proto_0.3-10    RCurl_1.95-4.1  reshape2_1.2.2  scales_0.2.3    stringr_0.6.2   tools_3.1.0    
[22] whisker_0.3-2  
```

