##conversion funnel 
funnel$X <- NULL
funnel <- na.omit(funnel)

funnelall$X <- NULL
funnelall$X.1 <- NULL
funnelall <- na.omit(funnelall)
funnelall$Name <- factor(funnelall$Name, order=TRUE, 
                         levels=c("Installs","Visitors","Viewers in W1", "Viewers in W2"))


conversionfunnel <- ggplot(funnelall, aes(x=Name, y=Count/1000)) + geom_bar(stat="identity") + 
  labs(title="Hulu Conversion Funnel as of 02/01/14", y="Uniques (in thousands)", x="User Stage")


##frequencyofvisits
names(huluvisitsbefore) <- c("ESN", "frequencyofvisits", "frequencyofdays")

newvisitsignore <- huluvisitsbefore[which(huluvisitsbefore$frequencyofvisits <= 10),]

ggplot(newvisits, aes(x=frequencyofvisits)) + geom_histogram(binwidth=1, origin=1)

visitsbeforeplay <- ggplot(test, aes(x=frequencyofvisits, y=N/1000)) + geom_bar(stat="identity") +
  labs(title="Visits Before First View", x="Visit Count Cohort", y="Unique Users (in thousands)") + 
  scale_x_continuous(breaks=c(1:11)) + geom_text(aes(label=round(N/1000,2)), vjust=-.25)

visitsbeforeshare <- ggplot(test, aes(x=frequencyofvisits, y=sharesum*100)) + geom_line() + geom_point(size=3) + 
  scale_x_continuous(breaks=c(1:11)) + geom_text(aes(label=round(sharesum*100,2)), vjust=-.25) +
  labs(title="Share of Cumulative", x="Visit Count Cohort", y="Share of Total") + 
  


test <- ddply(newvisits, c("frequencyofvisits"), summarize,
              N = length(ESN))

test$cum <- cumsum(test$N)
test$share <- test$N/792400
test_rb$share



##monthlyuniques
monthly$date <- as.character(monthly$date)
monthly$date <- as.Date(monthly$date, "%m/%d/%y")
monthly$channelname[monthly$channelname == "Vudu"] <- "VUDU"
monthly$channelname <- factor(monthly$channelname, order=TRUE, 
                              levels=c("Netflix", "Amazon Instant Video",
                                       "Hulu Plus", "VUDU", "Redbox Instant by Verizon"))
monthly$countstreams <- as.numeric(monthly$countstreams)

monthly <- monthly[which(monthly$date >= "2012-01-01"),]


monthlyuniques <- ggplot(monthly, aes(x=date, y=esnplayed/1000, colour=channelname)) + 
  geom_line() + geom_point(size=3) + scale_colour_brewer(palette="Set1") + 
  labs(title="Monthly Uniques", y="Uniques (in thousands)", x="Date")

monthly$minsperesn <- monthly$minsplayed / monthly$esnplayed

minspermonth <- ggplot(monthly, aes(x=date, y=minsperesn, colour=channelname)) + 
  geom_line() + geom_point(size=3) + scale_colour_brewer(palette="Set1") + 
  labs(title="Mins. Per Device", y="Mins per Month", x="Date")


##userage 
names(huluuserage) <- c("esn", "firstvisitdate", "visits", "dvpcreated", "userage")

huluuserage1 <- huluuserage[which(huluuserage$userage > 0),]

nonzero <- huluuserage[which(huluuserage$userage != 0),]
nonzero_1 <- nonzero[which(nonzero$userage <= 50),]


ageofonetimevisitors <- ggplot(nonzero_1, aes(x=userage)) + 
  geom_histogram(binwidth=1, fill="white", colour="black") + 
  labs(title="Distribution of User Age for One Time Visitors", x="User Age", y="Count")



##bouncerate 
bounce$monthstart <- as.character(bounce$monthstart)
bounce$monthstart <- as.Date(bounce$monthstart, "%Y-%m-%d")
bounce$bouncerate <- as.numeric(bounce$bouncerate)

bounce$channelname <- factor(bounce$channelname, order=TRUE, 
                             levels=c("Netflix", "Amazon Instant Video",
                                      "Hulu Plus", "VUDU", "Redbox Instant by Verizon"))


ggplot(bounce, aes(x=monthstart, y=bouncerate, colour=channelname)) + 
  geom_line() + geom_point(size=3) + scale_colour_brewer(palette="Set1") +
  labs(title="Monthly Bounce Rate", y="Bounce Rate", x="Date") +  theme(legend.position="none")


##repeat visitors 

rv$monthstart <- as.character(rv$monthstart)
rv$monthstart <- as.Date(rv$monthstart, "%Y-%m-%d")
rv$visitors <- as.numeric(rv$visitors)

rv$channelname <- factor(rv$channelname, order=TRUE, 
                         levels=c("Netflix", "Amazon Instant Video",
                                  "Hulu Plus", "VUDU", "Redbox Instant by Verizon"))


rvrb$channelname <- as.character(rvrb$channelname)
rvrb$channelname[is.na(rvrb$channelname)] <- "Redbox Instant by Verizon"

ggplot(rvrb, aes(x=monthstartdat, y=visitors/1000, colour=channelname)) + 
  geom_line() + geom_point(size=3) + scale_colour_brewer(palette="Set1") +
  labs(title="Monthly Repeat Visitors (in thousands)", y="Unique Visitors", x="Date") + 
  theme(legend.position="none")





