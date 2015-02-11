
profile <- profilemarch
names(profile) <- c("ESN", "totalchannelsplayed", "hoursplayed", "daysplayed", "streamtotal")

profile$hoursplayed <- profile$hoursplayed/3600

vars <- c("totalchannelsplayed", "hoursplayed", "daysplayed", "streamtotal")


amazon <- profileamazononlymarch

names(amazon) <- c("ESN", "totalchannelsplayed", "hoursplayed", "daysplayed", "streamtotal")
amazon$hoursplayed <- amazon$hoursplayed/3600

rp <- rokuprofile

names(rp) <- c("ESN", "totalchannelsplayed", "hoursplayed", "daysplayed", "streamtotal",
               "dvpage", "userage")

  rp$hoursplayed <- rp$hoursplayed/3600


ap <- amazonprofile

names(ap) <- c("ESN", "totalchannelsplayed", "hoursplayed", "daysplayed", "streamtotal",
               "dvpage", "userage")

---

roi <- amazonROI

names(roi) <- c("monthstartdate", "ID", "AdvertiserName",
                "EventDate", "Impressions", "Clicks", "Installs",
                "Country", "Campaign", "AdBanner")


roi$Country <- as.character(roi$Country)
roi$Country[roi$Country == 'United States'] <- 'US'

roi$AdvertiserName <- as.character(roi$AdvertiserName)
roi$AdvertiserName[is.na(roi$AdvertiserName) == TRUE] <- 'Amazon'

roi$monthstartdate <- as.Date(roi$monthstartdate, '%m/%d/%y')
roi$EventDate <- as.Date(roi$EventDate, '%m/%d/%y')

nroi <- roi[,c(1,3:10)]

sumroi <- ddply(nroi, .(monthstartdate), summarise,
                Impressions = sum(Impressions),
                Clicks = sum(Clicks),
                Installs = sum(Installs))





vbfp <- vbfp_amazon
names(vbfp) <- c("esn", "frequencyofvisits","frequencyofdayswithvisits")

  ggplot(vbfp.1, aes(x=frequencyofvisits)) + 
  geom_histogram(binwidth=1, fill="white", colour="black", origin = 0) + 
  labs(title="Histogram of Visit Count Before First Play", x=NULL, y="Unique Devices") +
  theme(axis.text.y = element_text(size=rel(1.5)))
  
  
  
  geom_text(aes(label=round(frequencyofvisits/1000,2)), vjust=-.25)
                                                            


  n <- ddply(vbfp.1, .(frequencyofvisits), summarise,
             count = length(esn))

  n$cum <- cumsum(n$count)

ggplot(vbfp.1, aes(x=frequencyofvisits)) + geom_line(stat="density")

vbfp.1 <- vbfp[which(vbfp$frequencyofvisits <= 25),]









