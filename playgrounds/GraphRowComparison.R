rm(list=ls(all=TRUE))
library(Wats)
vp_layout <- function(x, y) { viewport(layout.pos.row=x, layout.pos.col=y) }

full_spread <- function( scores ) {
  return( range(scores) )
}
h_spread <- function( scores ) {
  return( quantile(x=scores, probs=c(.25, .75)) )
}
se_spread <- function( scores ) {
  return( base::mean(scores) + base::c(-1, 1) * stats::sd(scores) / base::sqrt(base::sum(!base::is.na(scores))) )
}
boot_spread <- function( scores, conf=.68 ) {
  plugin <- function( d, i ) { mean(d[i]) }

  distribution <- boot(data=scores, plugin, R=999)
  ci <- boot.ci(distribution, type=c("bca"), conf=conf)
  return( ci$bca[4:5] ) #The fourth & fifth elements correspond to the lower & upper bound.
}
light_theme <- ggplot2::theme(
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

GraphCountyComparison <- function( row_label="", county_name="oklahoma", spread_function=h_spread, change_month=as.Date("1996-02-15") ) {
  ds_linear <- county_month_birth_rate_2005_version[county_month_birth_rate_2005_version$county_name==county_name, ]
  ds_linear <- augment_year_data_with_month_resolution(ds_linear=ds_linear, date_name="date")
  portfolio_cartesian <- annotate_data(ds_linear, dv_name="birth_rate", center_function=median, spread_function=spread_function)
  portfolio_polar <- polarize_cartesian(ds_linear=portfolio_cartesian$ds_linear, ds_stage_cycle=portfolio_cartesian$ds_stage_cycle, y_name="birth_rate", stage_id_name="stage_id", plotted_point_count_per_cycle=7200)
  cartesian_periodic <- cartesian_periodic(portfolio_cartesian$ds_linear, portfolio_cartesian$ds_periodic, x_name="date", y_name="birth_rate", stage_id_name="stage_id", change_points=change_month, change_point_labels=""  )

  pushViewport(viewport(
    layout=grid.layout(nrow=1, ncol=3, respect=FALSE, widths=unit(c(2,1,3), c("line", "null", "null"))),
    gp=gpar(cex=1, fill=NA)
  ))
  pushViewport(viewport(layout.pos.col=1))
  grid.rect(gp=gpar(fill="gray90", col=NA))
  grid.text(row_label, rot=90)
  popViewport()

  pushViewport(viewport(layout.pos.col=2))
#   grid.rect()
  polar_periodic <- polar_periodic(ds_linear=portfolio_polar$ds_observed_polar, ds_stage_cycle_polar=portfolio_polar$ds_stage_cycle_polar, draw_observed_line=FALSE, y_name="radius", stage_id_name="stage_id", origin_label=NULL)
  popViewport()

  pushViewport(viewport(layout.pos.col=3))
  print(cartesian_periodic + light_theme, vp=vp_layout(x=1, y=1))
  popViewport()
  popViewport() #Finish the row
}

counties <- c("tulsa", "oklahoma", "cleveland", "comanche")
spreads <- c("h_spread", "full_spread", "se_spread", "boot_spread")

grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=length(counties), ncol=1), gp=gpar(cex=1, fill=NA)))
for( i in seq_along(counties) ) {
  pushViewport(viewport(layout.pos.col=1, layout.pos.row=i))
  GraphCountyComparison(county_name=counties[i], row_label=counties[i])
  popViewport()
}
popViewport()
#
# grid.newpage()
# pushViewport(viewport(layout=grid.layout(nrow=length(spreads), ncol=1), gp=gpar(cex=1, fill=NA)))
# for( i in seq_along(spreads) ) {
#   pushViewport(viewport(layout.pos.col=1, layout.pos.row=i))
#   GraphCountyComparison(spread_function=get(spreads[i]), row_label=spreads[i])
#   upViewport()
# }
# upViewport()
