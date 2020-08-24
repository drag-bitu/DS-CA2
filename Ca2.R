library(dplyr)
library(readr)
library(mice)
library(VIM)

### A) To display total number of rows, the structure of the data frame,and first 10 rows of the data frame containing all of the NIPostcode data

NI_crime_data <- read.csv('D:/DS/NIPostcodes.csv', header=FALSE, stringsAsFactors=FALSE)

#total number of rows
nrow(NI_crime_data) 

#returns structure of data 
str(NI_crime_data)


# returns mentioned limit number of top records
head(NI_crime_data,10)



### b) Add a suitable title for coloumn of the data
col_names <- c("Organisation.Name", 
               "Sub-building.Name", 
               "Building.Name", 
               "Number", 
               "Primary.Thorfare",
                  "Alt.Thorfare", 
                  "Secondary.Thorfare", 
               "Locality", 
               "Townland", 
               "Town", 
               "County", 
               "Postcode", 
                  "x-coordinates", 
               "y-coordinates", 
               "Primary.Key")

#inserting colum names into data
colnames(NI_crime_data) <- col_names
str(NI_crime_data)

### c)Replace and recode all missing entries with a suitable identifier
NI_crime_data[NI_crime_data == ""] <- NA

str(NI_crime_data)

# total number of missing values
sum(is.na(NI_crime_data)) 

#mising values are displayed graphically
md.pattern(NI_crime_data)

missing_values <- aggr(NI_crime_data, prop = FALSE, numbers = TRUE) #using vim packages to display the missing values

#summary of missing data
summary(missing_values) 

### d) Total number of missing values for each column in the postcode data 
missing_values <- data.frame(sapply(NI_crime_data, function(x)sum(length(which(is.na(x))))))
missing_values <- data.frame(missing_values)
missing_values


missing_values <- aggr(NI_crime_data, prop= FALSE, numbers = TRUE) 
summary(missing_values)

### e) Move the primary key identifier to the start of the dataset.
#colums to be moved
new_NI_crime_data <- subset(NI_crime_data, select=c(15,1:14)) 

str(new_NI_crime_data)

### f) Create a new dataset called Limavady_data. Store within it only information where locality, townland and town contain the name Limavady. Count and display the number of rows. 
##Store this information in a csv file called Limavady
attach(NI_crime_data)
limavady_data <- NI_crime_data[which( NI_crime_data$Locality =="LIMAVADY"| NI_crime_data$Townland == "LIMAVADY"|NI_crime_data$Town=="LIMAVADY"),] 
limavady_data
nrow(limavady_data)
write.csv(limavady_data,"Limavady.csv")

### g) Save the modified NIPostcode dataset in a csv file called CleanNIPostcodeData.
write.csv(new_NI_crime_data, file="CleanNIPostcodeData.csv", row.names=FALSE)





##########**********************************section-2*****************************##########
#*******************************************############*************************************
  
###Using R, amalgamate all of the crime data from each csv file into one dataset. Save this dataset into a csv file called AllNICrimeData. Count and show the number of rows in the AllNICrimeData dataset. 
##Do not hard code the dataset path into your R code.  


rm(list = ls(all=TRUE))
getwd()
setwd("D:/DS/NI Crime Data")


##Question a

data_paths = list.files(path="D:/DS/NI Crime Data", pattern="*.csv", 
                       full.names=TRUE, recursive= TRUE) # list of CSV file in directory 

merge_df <- sapply(data_paths, read_csv, simplify=FALSE, col_types = cols(.default = "c")) %>%
  bind_rows() # merges all the data frames togather, and attribute type to character

data_path <- file.path('AllNICrimeData.csv')

write.csv(merge_df, file=data_path, row.names=FALSE) # save file locally

AllNICrimeData <- read.csv(data_path, header=TRUE, stringsAsFactors=FALSE)#, na.strings= c("")) # reads csv file

str(AllNICrimeData) # displays structure

nrow(AllNICrimeData) # total row count

# (b) Modify the structure of the newly created AllNICrimeData csv file and 
# remove the following attributes: CrimeID, Reported by, Falls within, LSOA code, LSOA name, last outcome and context. 
# Save this new structure and show the structure of the modified file.

str(AllNICrimeData)

delete_col <- c("Crime.ID", "Reported.by", "Falls.within", "LSOA.code", "LSOA.name", "Last.outcome.category", 
              "Context") # columns which need to be removed

new_crime_df = AllNICrimeData[, !(colnames(AllNICrimeData) %in% delete_col)] # select columns other than mentioned array

str(new_crime_df) # display structure



#####c

unique(AllNICrimeData$Crime.type)

library(dplyr)
new_crime_df=
  new_crime_df %>% mutate(Crime.type=recode_factor(Crime.type, 
                                                         'Anti-social behaviour' = 'ASBO', 
                                                         'Bicycle theft' = 'BITH',
                                                         'Burglary' = 'BURG',
                                                         'Criminal damage and arson' = 'CDAR',
                                                         'Drugs' = 'DRUG', 
                                                         'Other theft' = 'OTTH',
                                                         'Possession of weapons' = 'POFW',
                                                         'Public order' = 'PUBO',
                                                         'Robbery' = 'ROBY', 
                                                         'Shoplifting' = 'SHOP',
                                                         'Theft from the person' = 'THPR',
                                                         'Vehicle crime' = 'VECR',
                                                         'Violence and sexual offences'='VECO',
                                                         'Other crime' = 'OTCR'))
print(sample_n(new_crime_df, 10))


# (d) Using the plot() function, show a plot of each crime frequency from the crime.type field. 
# Specify relevant options such as title, axes labels, bar colours. 
# Provide a detailed discussion about what you found from this chart

crime_freq <- prop.table(table(new_crime_df$Crime.type)) # dplyr library function to calculate entire table
print(crime_freq)

barplot(crime_freq, ylab = "Frequency", xlab = "Crime Type", 
        main = "Different crime and frequency in Northern Ireland", col = rainbow(14)) # displays bar chart



# (e) Modify the AllNICrimeData dataset so that the Location attribute contains only a street name.
new_crime_df$Location <- gsub('On or near ', '', new_crime_df$Location)

print(sample_n(new_crime_df, 10))


# (f) Choose 5000 random samples of crime data from the AllNICrimeData dataset where the location attribute contains

set.seed(100) # set seed to 100 for constant records

random_crime_sample <- sample_n(new_crime_df[complete.cases(new_crime_df$Location), ], 500) # non NA 5000 records

find_a_town <- function(name) {
  new_crime_df$Town[match(toupper(new_crime_df$County), tolower(new_crime_df$Locality))] # returns town
}

print(random_crime_sample$Location) # prints random sample records



####(g) Create a function called add_town_data that examines the information from each crime record 
# in random_crime_sample and matches each record with relevant data in the VillageList.csv file. 
# Add the population attribute to the random_crime_sample data frame.

VillageList <- read.csv("D:/DS/VillageList.csv")
#Creating function `add_town_data`
add_town_data <- function(df)
{
  VillageList$POPULATION[match(tolower(df),tolower(VillageList$ï..CITY.TOWN.VILLAGE))]
}
#Calling function `add_town_data`
random_crime_sample$Population <- add_town_data(random_crime_sample$Town)


str(random_crime_sample)


######################################hhh
str(AllNICrimeData)

delete_col <- c("LSOA.code","LSOA.name","Last.outcome.category","Context","Primary.Key","Organisation.Name","Sub.building.Name",
                "Building.Name","Number","Primary.Thorfare","Alt.Thorfare","Secondary.Thorfare","Locality","Townland","Postcode",
                "x.coordinates","y.coordinates","X1")


AllNICrimeData = AllNICrimeData[, !(colnames(AllNICrimeData) %in% delete_col)]


str(AllNICrimeData)

colnames(random_crime_sample)[6] <-  "City-Town-Village"
colnames(random_crime_sample)[5] <-  "Crime type"
#Writing the  changes to  random_crime_sample csv file
write.csv(random_crime_sample,"random_crime_sample.csv", row.names=FALSE)

str(random_crime_sample)
####################################################i
  
belfastcrime <- filter(random_crime_sample,`City-Town-Village` == 'BELFAST')
#Counting the number of crimes that happened in Belfast
belfastcrimecount <- count(belfastcrime,'Crime type')
#Sorting crime according to count
sort(belfastcrimecount$n)
#Filtering and storing the crimes that happned only in Londenderry
londonderrycrime <- filter(random_crime_sample,`City-Town-Village` == 'LONDONDERRY')
#Counting the number of crimes that happened in Londenderry
londonderrycrimecount <- count(londonderrycrime,'Crime type')
#Sorting crime according to count
sort(londonderrycrimecount$n)
#Creating multi-panelled window for plotting
par(mfrow = c(2,2))
#Plotting the crimes happened in Blefast
plot(belfastcrimecount$"Crime type", belfastcrimecount$n, xlab = "Crime Type", ylab = "Crime Count", main = "Types of crime happened in Belfast with their count", cex.axis=0.35)
#Plotting the crimes happened in Londonderry
plot(londonderrycrimecount$Crime.type, londonderrycrimecount$n, xlab = "Crime Type", ylab = "Crime Count", main = "Types of crime happened in Londonderry with their count", cex.axis=0.35)


