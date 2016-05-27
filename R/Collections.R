#
# collections.R
#

#plot fn
plotCumulativeLine <- function(data, measure, target, cum = TRUE, lower_title = TRUE,currency=TRUE) {
  d <- filter(data, variable == measure, !is.na(value))
  d$month <- month(as.Date(as.yearmon(d$date)), label = TRUE)
  d$year <- as.character(year(as.Date(as.yearmon(d$date))))
  
  #subset to most recent three years
  yrs_n <- length(unique(d$year))
  yrs <- unique(d$year)[(yrs_n - 2):yrs_n]
  d <- filter(d, year %in% yrs)
  
  #title stuff
  if(lower_title == TRUE) {
    title_measure <- tolower(measure)
  } else {
    title_measure <- measure
  }
  
  #add target
  if(hasArg(target)) {
    for(i in 1:12) {
      m <- month(i, label = TRUE)
      r <- list(NA, measure, (target/ 12), m, "Target")
      d <- rbind(d, r)
    }
  }
  
  #turn target into cumulative sum if cum == FALSE
  if(cum == FALSE) {
    r <- cumsum(d$value[d$year == "Target"])
    d$value[d$year == "Target"] <- r
  }
  
  d <- group_by(d, year) %>%
    mutate (annual_cum_sum = cumsum(value))
  
  #custom color scale
  n <- length(unique(d$year)) - 2
  gray_highlight <- c(colorRampPalette(c("gray77", "gray55"))(n), darkBlue, "grey10")
  names(gray_highlight) <- unique(d$year)
  gray_highlight_scale <- scale_colour_manual(name = "year", values = gray_highlight)
  
  gray_highlight_no_target <- c(colorRampPalette(c("gray77", "gray55"))(n + 1), darkBlue)
  names(gray_highlight_no_target) <- unique(d$year)
  gray_highlight_scale_no_target <- scale_colour_manual(name = "year", values = gray_highlight_no_target)
  
  rep_n <- length(unique(d$year)) - 1
  
  if(cum == TRUE & hasArg(target)) {
    p <- lineOPA(d, "month", "annual_cum_sum", paste("YTD", title_measure), group = "year", linetype = c(rep("solid", length(unique(data$year)) - 1), "dashed")) +
      gray_highlight_scale +
      geom_text(data = filter(d, date == r_period), aes(label = format(annual_cum_sum, big.mark = ",", scientific = FALSE), y = annual_cum_sum), size = 4, colour = "grey33", hjust = -0.2, vjust = -1)
  } else if(cum == TRUE & !hasArg(target)) {
    p <- lineOPA(d, "month", "annual_cum_sum", paste("YTD", title_measure), group = "year") +
      gray_highlight_scale_no_target +
      geom_text(data = filter(d, date == r_period), aes(label = format(annual_cum_sum, big.mark = ",", scientific = FALSE), y = annual_cum_sum), size = 4, colour = "grey33", hjust = -0.2, vjust = -1)
  } else if(cum == FALSE & !hasArg(target)) {
    p <- lineOPA(d, "month", "value", paste("YTD", title_measure), group = "year") +
      gray_highlight_scale_no_target +
      geom_text(data = filter(d, date == r_period), aes(label = format(value, big.mark = ",", scientific = FALSE), y = value), size = 4, colour = "grey33", hjust = -0.2, vjust = -1)
  } else if(cum == FALSE & hasArg(target)) {
    p <- lineOPA(d, "month", "value", paste("YTD", title_measure), group = "year", linetype = c(rep("solid", length(unique(data$year)) - 1), "dashed")) +
      gray_highlight_scale +
      geom_text(data = filter(d, date == r_period), aes(label = format(value, big.mark = ",", scientific = FALSE), y = value), size = 4, colour = "grey33", hjust = -0.2, vjust = -1)
  }
  
  print(p)
  
  p <- buildChart(p)
  
  ggsave( file = paste("bottomLine-STAT/output/", measure, "YTD.png"), plot = p, width = 7.42, height = 5.75 )
}



#### Load data
data1 <- read.csv("BottomLine_scripts/data/Collections.csv", header = TRUE)
data <- melt(data1, id.vars = "date")
data$date <- as.factor(as.yearmon(data$date))
data$variable <- gsub(".", " ", data$variable, fixed = TRUE)


theme_set(theme_opa( base_size = 12 ))

#### Scale revenue items by $1M for better axis rendering, rounded to two decimals 
data$value<-round(data$value/1000000,2)

# for (i in unique(data$variable)){
#   
#   data$value<-data$value/1000000
#   
#   d<-filter(data,variable==i)
#   
#   e<-plotCumulativeLine(d,i)
#   
#   e
# }  
#   

###### Plot
plotCumulativeLine(data, "Ticket Collections",currency=TRUE)
plotCumulativeLine(data, "Booting and Towing Collections",currency=TRUE)
plotCumulativeLine(data, "Photo Safety Collections",currency=TRUE)
plotCumulativeLine(data, "Retail Sales Tax Collections",currency=TRUE)
plotCumulativeLine(data, "Hotel Motel Sales Tax Collections",currency=TRUE)
plotCumulativeLine(data, "Vehicle Sales Tax Collections",currency=TRUE)
plotCumulativeLine(data, "Occupational License Collections",currency=TRUE)
plotCumulativeLine(data, "EMS Collections",currency=TRUE)
plotCumulativeLine(data, "Property Tax Collections",currency=TRUE)
plotCumulativeLine(data, "Sanitation Collections",currency=TRUE)
plotCumulativeLine(data, "Subrogation Collections",currency=TRUE)
