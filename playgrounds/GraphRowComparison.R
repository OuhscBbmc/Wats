rm(list=ls(all=TRUE))
library(Wats)
vpLayout <- function(x, y) { viewport(layout.pos.row=x, layout.pos.col=y) }

fullSpread <- function( scores ) {
  return( range(scores) )
}
hSpread <- function( scores ) {
  return( quantile(x=scores, probs=c(.25, .75)) )
}
seSpread <- function( scores ) {
  return( base::mean(scores) + base::c(-1, 1) * stats::sd(scores) / base::sqrt(base::sum(!base::is.na(scores))) )
}
bootSpread <- function( scores, conf=.68 ) {
  plugin <- function( d, i ) { mean(d[i]) }

  distribution <- boot(data=scores, plugin, R=999)
  ci <- boot.ci(distribution, type=c("bca"), conf=conf)
  return( ci$bca[4:5] ) #The fourth & fifth elements correspond to the lower & upper bound.
}
lightTheme <- ggplot2::theme(
  axis.title         =element_text(color="gray60", size=9),
  axis.text.x        =element_text(color="gray80", hjust=0),
  axis.text.y        =element_text(color="gray80"),
  axis.ticks.length  =grid::unit(0, "cm"), #g <- g + theme(axis.ticks=element_blank())
  axis.ticks.margin  =grid::unit(.00001, "cm"),
  panel.grid.minor.y =element_line(color="gray99", size=.1),
  panel.grid.major   =element_line(color="gray95", size=.1),
  panel.margin       =grid::unit(c(0, 0, 0, 0), "cm"),
  plot.margin        =grid::unit(c(0, 0, 0, 0), "cm")
)

GraphCountyComparison <- function( rowLabel="", countyName="oklahoma", spread_function=hSpread, changeMonth=as.Date("1996-02-15") ) {
  ds_linear <- county_month_birth_rate_2005_version[county_month_birth_rate_2005_version$CountyName==countyName, ]
  ds_linear <- augment_year_data_with_month_resolution(ds_linear=ds_linear, date_name="Date")
  portfolioCartesian <- annotate_data(ds_linear, dv_name="BirthRate", center_function=median, spread_function=spread_function)
  portfolioPolar <- polarize_cartesian(ds_linear=portfolioCartesian$ds_linear, ds_stage_cycle=portfolioCartesian$ds_stage_cycle, y_name="BirthRate", stage_id_name="StageID", plotted_point_count_per_cycle=7200)
  cartesian_periodic <- cartesian_periodic(portfolioCartesian$ds_linear, portfolioCartesian$ds_periodic, x_name="Date", y_name="BirthRate", stage_id_name="StageID", change_points=changeMonth, change_point_labels=""  )

  pushViewport(viewport(
    layout=grid.layout(nrow=1, ncol=3, respect=FALSE, widths=unit(c(2,1,3), c("line", "null", "null"))),
    gp=gpar(cex=1, fill=NA)
  ))
  pushViewport(viewport(layout.pos.col=1))
  grid.rect(gp=gpar(fill="gray90", col=NA))
  grid.text(rowLabel, rot=90)
  popViewport()

  pushViewport(viewport(layout.pos.col=2))
#   grid.rect()
  polar_periodic <- polar_periodic(ds_linear=portfolioPolar$dsObservedPolar, ds_stage_cycle_polar=portfolioPolar$ds_stage_cycle_polar, draw_observed_line=FALSE, y_name="Radius", stage_id_name="StageID", origin_label=NULL)
  popViewport()

  pushViewport(viewport(layout.pos.col=3))
  print(cartesian_periodic + lightTheme, vp=vpLayout(x=1, y=1))
  popViewport()
  popViewport() #Finish the row
}

counties <- c("tulsa", "oklahoma", "cleveland", "comanche")
spreads <- c("hSpread", "fullSpread", "seSpread", "bootSpread")

grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=length(counties), ncol=1), gp=gpar(cex=1, fill=NA)))
for( i in seq_along(counties) ) {
  pushViewport(viewport(layout.pos.col=1, layout.pos.row=i))
  GraphCountyComparison(countyName=counties[i], rowLabel=counties[i])
  popViewport()
}
popViewport()
#
# grid.newpage()
# pushViewport(viewport(layout=grid.layout(nrow=length(spreads), ncol=1), gp=gpar(cex=1, fill=NA)))
# for( i in seq_along(spreads) ) {
#   pushViewport(viewport(layout.pos.col=1, layout.pos.row=i))
#   GraphCountyComparison(spread_function=get(spreads[i]), rowLabel=spreads[i])
#   upViewport()
# }
# upViewport()
