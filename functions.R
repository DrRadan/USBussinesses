Broom<-function(table, n_naics=0, year=2014){
  
  # This function takes in a dataframe containing annual data from the Bussiness census database that have been already downloaded.
  # Arguments are:
  # table is a dataframe representing data from one year
  # n_naics : the  numebr of naics digits to include in the output. Possible values are 0, 3, 4, 5, 6 (0 = for all sectors)
  # year : the year the dataframe refers to. Is usefull in order to define the naics fields accuratelly (they have occesional changes)
  
  library(dplyr)
  library(zipcode)
  
  zip <- tbl_df(table)
  if (n_naics==0) {
    querry <- "------"
    print("Calculating data for all bussinesses.")
  }else if (n_naics>=3 && n_naics<=6){
    unit <- "[0123456789]"
    querry1 <- paste(replicate(n_naics, unit),  collapse = "")
    querry2 <- paste(replicate(6-n_naics,"/"), collapse = "")
    querry <- paste(querry1, querry2, sep="")
    print(paste("Calculating data for ",as.character(n_naics),"-digit bussiness codes.", sep=""))
  }else{
    print("You made a big mistake! I cannot process what you ask!!!!")
    break()
  }
  zip <- filter(zip, grepl(querry,zip$naics))
  
  zip$zip <- clean.zipcodes(zip$zip)
  
  if (year<=2014 && year>=2012) {
    file="~/Datasets/Census_Bussiness_bureau/NorthAmericanIndustryClassificationSystemDescriptions_2012_14.txt"
  }else if (year<=2011 && year>=2008){
    file="~/Datasets/Census_Bussiness_bureau/NorthAmericanIndustryClassificationSystemDescriptions_2008_11.txt"
  } else if (year<=2007 && year>=2003){
    "~/Datasets/Census_Bussiness_bureau/NorthAmericanIndustryClassificationSystemDescriptions_2003_07.txt"
  } else if (year<=2002 && year>=1998){
    file = "~/Datasets/Census_Bussiness_bureau/NorthAmericanIndustryClassificationSystemDescriptions_1998_2007.txt"
  }
  print(paste("Calculating table for year ", as.character(year)))
  naics <- tbl_df(read.csv(file))
  colnames(naics)<-c("naics","description")
  zip <- left_join(zip, naics, by='naics')
  zip <- zip[,c(1,2,13,3:12)]
  
  uspc <- tbl_df(read.csv("~/Datasets/AuxiliaryVarious/US_postal_codes.csv"))
  uspc <- uspc[,c(1,3,4:7)]
  colnames(uspc)<- c("zip","state_name", "state","county", "latitude", "longitude")
  uspc$zip<-clean.zipcodes(uspc$zip)
  
  zip<- left_join(zip, uspc, by='zip')
  zip <- zip[,c(1,14:18,2:13)]
  zip <- cbind(zip,polynames=as.factor(paste(tolower(zip$state_name), ",", tolower(zip$county),sep="")))
  zip <- zip[,c(1:4,19,5:18)]
}
