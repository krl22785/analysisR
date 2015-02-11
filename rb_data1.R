
rb.roi <- rb.ad

rb.roi$Country <- as.character(rb.roi$Country)
rb.roi$Country[rb.roi$Country == 'United States'] <- 'US'

rb.roi$monthstartdate <- as.Date(rb.roi$monthstartdate, '%m/%d/%y')
rb.roi$EventDate <- as.Date(rb.roi$EventDate, '%m/%d/%y')

rb.roi <- rb.roi[,c(1,3:10)]

rb.sum <- ddply(rb.roi, .(monthstartdate), summarise,
                Impressions = sum(Impressions),
                Clicks = sum(Clicks),
                Installs = sum(Installs))

rb.general$monthstartdate <- as.Date(rb.general$monthstartdate, '%m/%d/%y')

rb.month <- merge(rb.sum, rb.general, "monthstartdate", all=TRUE)
rb.month$Installs[is.na(rb.month$Installs) == TRUE] <- 0

t <- rb.month[,c(2:4,6:8)]




  ggplot(rb.month, aes(x=Impressions, y=FTVisitors)) + geom_point(size=3)

  rb.monthly.rel <- lm(newinstalls ~ Impressions, data=rb.month)

  






  attach(rb.week)
  lm(FTVisitors ~ Impressions, data=rb.week)



rb.roi2 <- rb.ad2

rb.roi2$Country <- as.character(rb.roi2$Country)
rb.roi2$Country[rb.roi2$Country == 'United States'] <- 'US'

rb.roi2$weekstartdate <- as.Date(rb.roi2$weekstartdate, '%m/%d/%y')
rb.roi2$EventDate <- as.Date(rb.roi2$EventDate, '%m/%d/%y')

rb.roi2 <- rb.roi2[,c(1,3:10)]

rb.sum2 <- ddply(rb.roi2, .(weekstartdate), summarise,
                Impressions = sum(Impressions),
                Clicks = sum(Clicks),
                Installs = sum(Installs))

rb.general2$weekstartdate <- as.character(rb.general2$weekstartdate)
rb.general2$weekstartdate <- as.Date(rb.general2$weekstartdate, '%m/%d/%y')



rb.week <- merge(rb.sum2, rb.general2, "weekstartdate", all=TRUE)



ggplot(rb.sum2, aes(x=Impressions, y=Installs)) + geom_point()

summary(lm(Installs ~ Impressions, data=rb.sum2))









##rb user age   
rb.age <- rb.userage
names(rb.age) <- c("esn", "userage")

  plot <- ggplot(rb.age, aes(x=userage)) + geom_histogram(binwidth=30, fill="white", colour="black") + 
  labs(title="DVP Age Distribution for Redbox", y="Unique DVP Count", x="Age in Days (30 Day Cohorts") + 
  theme(axis.text.y = element_text(size=rel(1.5))) +  theme(axis.text.x = element_text(size=rel(1.5))) 

  
n <- ggplot_build(plot)$data
n.list <- n[2:37,c(2,12:13)]
n.list$share <- round(n.list$y/197243, 2)



##sampling 

sample <- rb.sample
names(sample) <- c("esn", "totalchannels", "hoursplayed", "daysplayed", "streamcount", "dvpage", "userage")

sample$hoursplayed <- sample$hoursplayed/3600

profile <- redbox.sample
names(profile) <- c("esn", "totalchannels", "hoursplayed", "daysplayed", "streamcount", "dvpage", "userage")

profile$hoursplayed <- profile$hoursplayed/3600






