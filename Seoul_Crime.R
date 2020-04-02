Seoul_address2 <-read.csv("crime_in_Seoul_address.csv")
str(Seoul_address)

Seoul_address <- as.character(Seoul_address$주소)

googleAPIkey <-"AIzaSyDrukGXGw3S_tIfEFMvdpaOVcI9FVzvXUY"

register_google(googleAPIkey)

Seoul_address <-geocode(Seoul_address)

head(Seoul_address)
table(Seoul_address)

seoul_map <- get_googlemap("seoul",maptype="roadmap",zoom=12)

ggmap(seoul_map)

Seoul_Crime <-read.csv("crime_in_Seoul.csv")

names(Seoul_Crime)

class(Seoul_Crime$절도.발생)
class(Seoul_Crime$절도.검거)
class(Seoul_Crime$폭력.발생)
class(Seoul_Crime$폭력.검거)
table(is.na(Seoul_Crime))


Seoul_Crime$절도.발생 <-  gsub(",","",Seoul_Crime$절도.발생)
Seoul_Crime$절도.검거 <-  gsub(",","",Seoul_Crime$절도.검거) 
Seoul_Crime$폭력.발생 <-  gsub(",","",Seoul_Crime$폭력.발생) 
Seoul_Crime$폭력.검거 <-  gsub(",","",Seoul_Crime$폭력.검거)
  
View(Seoul_Crime)

Seoul_Crime$절도.발생 <- as.integer(Seoul_Crime$절도.발생)
Seoul_Crime$절도.검거 <- as.integer(Seoul_Crime$절도.검거)
Seoul_Crime$폭력.발생 <- as.integer(Seoul_Crime$폭력.발생)
Seoul_Crime$폭력.검거 <- as.integer(Seoul_Crime$폭력.검거)



View(Seoul_Crime)
Seoul_address_final <- cbind(Seoul_address,Seoul_address2)
Seoul_Crime_pct <- Seoul_Crime %>% 
  group_by(관서명) %>% 
  mutate(범죄.발생 = 살인.발생+강도.발생+강간.발생+절도.발생+폭력.발생) %>% 
  mutate(범죄.검거 = 살인.검거+강도.검거+강간.검거+절도.검거+폭력.검거)

Seoul_Crime_pct$서울시.발생 <- sum(Seoul_Crime_pct$범죄.발생)
Seoul_Crime_pct$서울시.검거 <- sum(Seoul_Crime_pct$범죄.검거)

Seoul_Crime_pct$발생확률 <- paste("발생",round(Seoul_Crime_pct$범죄.발생 / Seoul_Crime_pct$서울시.발생 *100,2),"%") 
Seoul_Crime_pct$검거확률 <- paste("검거",round(Seoul_Crime_pct$범죄.검거 / Seoul_Crime_pct$서울시.검거 *100,2),"%") 

Seoul_Crime_pct$lon <- Seoul_address$lon
Seoul_Crime_pct$lat <- Seoul_address$lat
Seoul_Crime_final <- Seoul_Crime_pct %>% select("관서명","발생확률","검거확률",lon,lat)
ggmap(seoul_map)+geom_point(data= Seoul_Crime_final,
                           aes(x=lon,y=lat),
                           colour="red",
                           size=3)+
  geom_text(data=Seoul_Crime_final,aes(label=관서명,vjust=+1))+
  geom_text(data=Seoul_Crime_final,aes(label=발생확률,vjust=+2))+
  geom_text(data=Seoul_Crime_final_pct,aes(label=검거확률,vjust=+3))

str(Seoul_Crime)
