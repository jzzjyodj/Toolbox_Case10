##readfiles
population <- read.csv("C:/Users/jabella/Downloads/1.0 Data Scientist Toolbox Case/population.csv", stringsAsFactors = TRUE)
population
regionarea <- read.csv("C:/Users/jabella/Downloads/1.0 Data Scientist Toolbox Case/regionarea.csv", stringsAsFactors = TRUE)
regionarea

#merge tables through left join
merged_table = merge(x=population,y=regionarea,by=c("Region"),
                 all.x=TRUE)

merged_table

#delete all row with values = 0
merged_table <- merged_table[apply(merged_table!=0, 1, all),]
merged_table

#barangay count table
barangay_count = aggregate(Barangay ~ Region, data = merged_table, FUN = function(x) length((x)))
barangay_count
colnames(barangay_count)[2] <- "Barangay_Count"
barangay_count

#join merged_table and barangay_count table
final_merged = merge(x=merged_table,y=barangay_count,by=c("Region"),
                     all=TRUE)
final_merged

#create column barangayarea
final_merged$BarangayArea <- final_merged$Area / final_merged$Barangay_Count
final_merged

#create column population density
final_merged$PopulationDensity <- final_merged$Population / final_merged$BarangayArea
final_merged


#get top five
ordered_table <- final_merged[with(final_merged,order(-PopulationDensity )),]

topfive <- ordered_table[1:5,]
topfive

#top five by region
ordered_table <- final_merged[order(final_merged$PopulationDensity, decreasing = TRUE), ]

ordered_table <- Reduce(rbind,                                 
                        by(ordered_table,
                           ordered_table["Region"],
                           head,
                           n = 5))
ordered_table
#write csv file
write.csv(ordered_table,"C:\\Users\\jabella\\Desktop\\Toolbox_Case10\\topfive_barangay.csv", row.names = FALSE)


