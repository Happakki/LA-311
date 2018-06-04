### 1. Bulky item per househol by zipcode
### 2. Call & app volume by zipcode


### 1. Bulky Item per Household
bulky=
  data1 %>%
  filter(RequestType=="Bulky Items")

bulkyzip=
  bulky %>%
  group_by(ZipCode) %>%
  summarise(count= n())

bulkyzip= merge(bulkyzip, pop, by.x="ZipCode", by.y="Zip.Code")

bulkyzip=
  bulkyzip %>%
  mutate(ReqPerHouse= count/Total.Households) %>%
  mutate(ReqPerPerson= count/Total.Population)

## read shapefile
library(maptools)
zipshp <- readShapeSpatial("geo_export_3d86418b-6004-4d7b-8595-a1666788d0b5.shp")
print(zipshp$objectid)
dfshp= zipshp@data
plotbulky= merge(dfshp, bulkyzip, by.x="zipcode", by.y="ZipCode")
groupdata= fortify(zipshp)
merge.bulky<-merge(groupdata, plotbulky, by.x="id", by.y="objectid")
final.merge.bulky= arrange(merge.bulky, group, order)

### Plot the map
la=qmap("Los Angeles", maptype="roadmap", color="bw")
install.packages("scales")
library(ggplot2)
library(scales)

final.merge.bulky.filter= filter(final.merge.bulky, zipcode!=90071 & zipcode!=91330)

la +
  geom_polygon(data= final.merge.bulky.filter, 
               aes(x = long, y = lat, group = group, fill = ReqPerHouse), alpha=0.6) +
  scale_fill_gradient(name= "value", 
                      low= "white", high="#660099") +
  ggtitle("Bulky Items per Household")

test1=arrange(bulkyzip, -ReqPerHouse)


### CALL & APP by ZIPCODE ###

## CALL
levels(data1$RequestSource)
call=filter(data1, RequestSource== "Call")
callzip=
  call %>%
  group_by(ZipCode) %>%
  summarise(count= n())
#plot the map
plotcall= merge(dfshp, callzip, by.x="zipcode", by.y="ZipCode")
groupdata= fortify(zipshp)
merge.call<-merge(groupdata, plotcall, by.x="id", by.y="objectid")
final.merge.call= arrange(merge.call, group, order)

la +
  geom_polygon(data= final.merge.call, 
               aes(x = long, y = lat, group = group, fill = count), alpha=0.6) +
  scale_fill_gradient(name= "Volume", 
                      low= "white", high="#006666") +
  ggtitle("Call Volume by Zip Code")

## APP
levels(data1$RequestSource)
mobile=filter(data1, RequestSource== "Mobile App")
mobzip=
  mobile %>%
  group_by(ZipCode) %>%
  summarise(count= n())
#plot the map
plotmob= merge(dfshp, mobzip, by.x="zipcode", by.y="ZipCode")
groupdata= fortify(zipshp)
merge.mob<-merge(groupdata, plotmob, by.x="id", by.y="objectid")
final.merge.mob= arrange(merge.mob, group, order)

la +
  geom_polygon(data= final.merge.mob, 
               aes(x = long, y = lat, group = group, fill = count), alpha=0.6) +
  scale_fill_gradient(name= "Volume", 
                      low= "white", high="#660066") +
  ggtitle("App Volume by Zip Code")


