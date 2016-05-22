ToTest<-head(ToTest,n=15)

# to find distinct combination of "Origin.State","Dest.State","Ship.Date" from New_data
TestShipmentGroup<- distinct(ToTest[c("Origin.State","Dest.State","Ship.Date")])

# To Differentiate the column names
names(TestShipmentGroup) <- paste0(names(TestShipmentGroup), "2")

# The date column is factor so to convert it to date format 
TestShipmentGroup$Ship.Date2<-as.Date(TestShipmentGroup$Ship.Date2, format = "%m/%d/%Y")

# The date column is factor so to convert it to date format 
ToTest$Ship.Date<-as.Date(ToTest$Ship.Date, format = "%m/%d/%Y")


library(dplyr)

#SHIPMENT_WEEK_SUMMARY
finaldf1 <- merge(select(ToTest, Origin.State, Dest.State, Ship.Date,Cost),
                  select(TestShipmentGroup, Origin.State2, Dest.State2, Ship.Date2), all=TRUE) %>%
  filter ((Ship.Date >= (Ship.Date2-as.difftime(7, unit="days")))
          & (Ship.Date < Ship.Date2)
          & (Origin.State == Origin.State2)
          & (Dest.State == Dest.State2))

ShipmentWeekSummary<-finaldf1%>%
  group_by(Ship.Date2, Origin.State2,Dest.State2)%>%
  summarise(AverageCostWeek=mean(Cost,na.rm=TRUE),ShipmentCountWeek=n())%>%arrange(desc(Ship.Date2))



#SHIPMENT_MONTH_SUMMARY
finaldf2 <- merge(select(ToTest, Origin.State, Dest.State, Ship.Date,Cost),
                  select(TestShipmentGroup, Origin.State2, Dest.State2, Ship.Date2), all=TRUE) %>%
  
  filter ((Ship.Date >= (Ship.Date2-as.difftime(30, unit="days")))
          & (Ship.Date < Ship.Date2)
          & (Origin.State == Origin.State2)
          & (Dest.State == Dest.State2))

ShipmentMonthSummary<-finaldf2%>%
  group_by(Ship.Date2, Origin.State2,Dest.State2)%>%
  summarise(AverageCostWeek=mean(Cost,na.rm=TRUE),ShipmentCountWeek=n())%>%arrange(desc(Ship.Date2))


#SHIPMENT_LAST_YEAR_WEEK_SUMMARY
finaldf3 <- merge(select(ToTest, Origin.State, Dest.State, Ship.Date,Cost),
                  select(TestShipmentGroup, Origin.State2, Dest.State2, Ship.Date2), all=TRUE) %>%
  
  filter ((Ship.Date >= (Ship.Date2-as.difftime(372, unit="days")))
          & (Ship.Date < (Ship.Date2-as.difftime(365,unit="days")))
          & (Origin.State == Origin.State2)
          & (Dest.State == Dest.State2))

ShipmentLastYearWeekSummary<-finaldf3%>%
  group_by(Ship.Date2, Origin.State2,Dest.State2)%>%
  summarise(AverageCostWeek=mean(Cost,na.rm=TRUE),ShipmentCountWeek=n())%>%arrange(desc(Ship.Date2))


#SHIPMENT_LAST_YEAR_MONTH_SUMMARY
finaldf4 <- merge(select(ToTest, Origin.State, Dest.State, Ship.Date,Cost),
                  select(TestShipmentGroup, Origin.State2, Dest.State2, Ship.Date2), all=TRUE) %>%
  
  filter ((Ship.Date >= (Ship.Date2-as.difftime(395, unit="days")))
          & (Ship.Date < (Ship.Date2-as.difftime(365,unit="days")))
          & (Origin.State == Origin.State2)
          & (Dest.State == Dest.State2))

ShipmentLastYearMonthSummary<-finaldf4%>%
  group_by(Ship.Date2, Origin.State2,Dest.State2)%>%
  summarise(AverageCostWeek=mean(Cost,na.rm=TRUE),ShipmentCountWeek=n())%>%arrange(desc(Ship.Date2))



