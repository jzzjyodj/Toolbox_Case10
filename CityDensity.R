#read files
population <- read.csv("population.csv")
population
regionarea <- read.csv("regionarea.csv")
regionarea

#merge tables through left join
merged_table = merge(x=population,y=regionarea,by=c("Region"),
                     all.x=TRUE)

merged_table

#delete all row with values = 0
merged_table <- merged_table[apply(merged_table!=0, 1, all),]
merged_table

# create city count table
city_count = aggregate(CityProvince ~ Region, data = merged_table, FUN = function(x) length(unique(x)))
city_count
colnames(city_count)[2] <- "City_Count"
city_count

##aggregate population per each city
aggregate_table = aggregate(cbind(Population,HouseholdPopulation,Nhouseholds) ~ CityProvince + Province + Region + Area, 
                            data = merged_table, sum)
aggregate_table

##join aggregate_table and city_count table
final_merged = merge(x=aggregate_table,y=city_count,by=c("Region"),
                     all.x=TRUE)
final_merged

##Create city area column
final_merged$CityArea <- final_merged$Area / final_merged$City_Count
final_merged

#create column population density
final_merged$PopulationDensity <- final_merged$Population / final_merged$CityArea
final_merged

#get top five
ordered_table <- final_merged[with(final_merged,order(-PopulationDensity )),]

topfive <- ordered_table[1:5,]
topfive = topfive[c('CityProvince','Region','Province','Population','PopulationDensity')]
topfive

#top five by region
ordered_table <- final_merged[order(final_merged$PopulationDensity, decreasing = TRUE), ]

ordered_table <- Reduce(rbind,                                 
                        by(ordered_table,
                           ordered_table["Region"],
                           head,
                           n = 5))
ordered_table = ordered_table[c('CityProvince','Region','Province','Population','PopulationDensity')]
ordered_table
#write csv file
write.csv(ordered_table,"topfive_cityperregion.csv", row.names = FALSE)
write.csv(topfive,"topfive_city.csv", row.names = FALSE)


