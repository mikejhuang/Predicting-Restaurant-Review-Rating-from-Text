restaurants<-business[grep("Restaurants",business$categories),'business_id']

restaurantreviews<-review[grep(restaurants[1],review$business_id),]
for(i in 2:length(restaurants)){
  restaurantreviews<-rbind(restaurantreviews,review[grep(restaurants[i],review$business_id),])
}
