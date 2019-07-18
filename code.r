#Someone used this to recreate dollars
data$PDMGEXP <- 0
data$PDMGEXP[which(data$PROPDMGEXP == 'k' | data$PROPDMGEXP == 'K')] <- 1000
data$PDMGEXP[which(data$PROPDMGEXP == 'm' | data$PROPDMGEXP == 'M')] <- 10^6
data$PDMGEXP[which(data$PROPDMGEXP == 'b' | data$PROPDMGEXP == 'M')] <- 10^9
data$PDMGEXP[which(is.numeric(data$PROPDMGEXP))] <- data$PROPDMGEXP
data$CDMGEXP <- 0
data$CDMGEXP[which(data$CROPDMGEXP == 'k' | data$CROPDMGEXP == 'K')] <- 1000
data$CDMGEXP[which(data$CROPDMGEXP == 'm' | data$CROPDMGEXP == 'M')] <- 10^6
data$CDMGEXP[which(data$CROPDMGEXP == 'b' | data$CROPDMGEXP == 'M')] <- 10^9
data$CDMGEXP[which(is.numeric(data$CROPDMGEXP))] <- data$CROPDMGEXP


PopHealth <- storm_data %>% 
        group_by(Event = EVTYPE) %>%
        summarise(Fatalaties = sum(FATALITIES), Injuries = sum(INJURIES))%>%
        arrange(desc(Fatalaties))
ggplot(Top6_Pophealth2, aes(Event, Fatalaties, fill = Event))+
        geom_bar(stat="identity")+
        facet_grid(~.)


PopHealth2 <- storm_data %>% 
        group_by(Event = EVTYPE) %>%
        summarise(Fatalaties = sum(FATALITIES+INJURIES)) %>%
        arrange(desc(Fatalaties))

Top6_Pophealth2 <- head(PopHealth2)

ggplot(Top6_Pophealth2, aes(Event, Fatalaties, fill = Event))+geom_bar(stat="identity")

EconImpact <- storm_data %>% 
        group_by(Event = EVTYPE) %>%
        summarise(Damages = sum(CROPDMG+PROPDMG)) %>%
        arrange(desc(Damages))

Top6_EconImpact <- head(EconImpact)
ggplot(Top6_EconImpact, aes(Event, Damages, fill = Event))+geom_bar(stat="identity")

