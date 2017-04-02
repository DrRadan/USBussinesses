cBroom<-function(table, n_naics=0, year){
  
  # This function takes in a dataframe containing annual data from the Bussiness census database that have been already downloaded.
  # Arguments are:
  # table is a dataframe representing data from one year
  # n_naics : the  numebr of naics digits to include in the output. Possible values are 0, 3, 4, 5, 6 (0 = for all sectors)
  # year : the year the dataframe refers to. Is usefull in order to define the naics fields accuratelly (they have occesional changes)
  
  library(dplyr)
  co <- tbl_df(table)
  colnames(co) <- tolower(colnames(co))
  
  # 1.  As a first step, reduce size of the table by subsetting observations by the desired naics code level via n_naics
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
    print("Please set an acceptable value for n_naics. Acceptable values are: 0 (default), 3, 4, 5, and 6.
          aborting")
    exit()
  }
  co <- filter(co, grepl(querry,co$naics))
  
  # 2.  Perofm transformations to the table that depend on year-specific reference lookups and formats. See README for more info.
  print(paste("Calculating table for year ", as.character(year)))
  if (year<=2014 & year>=2012) {
    naics <- tbl_df(read.csv("~/Datasets/Census_Bussiness_bureau/NorthAmericanIndustryClassificationSystemDescriptions_2012_14.txt"))
    colnames(naics)<-tolower(colnames(naics))
    co <- left_join(co, naics, by='naics')
    geo <- tbl_df(read.csv("~/Datasets/Census_Bussiness_bureau/geo_reference_2012_14.txt"))
    co <- left_join(co, geo, by=c('fipstate','fipscty'))
    co <- co[,c(1,2,28,27,25,26,3,11:24)]
  }else if (year<=2011 & year>=2008){
    naics <- tbl_df(read.csv("~/Datasets/Census_Bussiness_bureau/NorthAmericanIndustryClassificationSystemDescriptions_2008_11.csv", sep="."))
    colnames(naics)<-tolower(colnames(naics))
    levels(co$naics) <- levels(naics$naics)
    co <- left_join(co, naics, by='naics')
    geo <- tbl_df(read.csv("~/Datasets/Census_Bussiness_bureau/geo_reference_2002_11.txt"))
    co <- left_join(co, geo, by=c('fipstate','fipscty'))
    co <- co[,c(1,2,28,27,25,26,3,11:24)]
  } else if (year==2007){
    naics <- tbl_df(read.csv("~/Datasets/Census_Bussiness_bureau/NorthAmericanIndustryClassificationSystemDescriptions_2003_07.txt", sep="."))
    colnames(naics)<-tolower(colnames(naics))
    levels(co$naics) <- levels(naics$naics)
    co <- left_join(co, naics, by='naics')
    geo <- tbl_df(read.csv("~/Datasets/Census_Bussiness_bureau/geo_reference_2002_11.txt"))
    co <- left_join(co, geo, by=c('fipstate','fipscty'))
    co <- co[,c(1,2,28, 27, 25,26,3,11:24)]
  } else if (year<=2006 & year>=2003){
    naics <- tbl_df(read.csv("~/Datasets/Census_Bussiness_bureau/NorthAmericanIndustryClassificationSystemDescriptions_2003_07.txt", sep="."))
    colnames(naics)<-tolower(colnames(naics))
    levels(co$naics) <- levels(naics$naics)
    co <- left_join(co, naics, by='naics')
    geo <- tbl_df(read.csv("~/Datasets/Census_Bussiness_bureau/geo_reference_2002_11.txt"))
    co <- left_join(co, geo, by=c('fipstate','fipscty'))
    co <- co[,c(1,2,25,24,22,23,3,8:21)]
  } else if (year==2002){
    naics <- tbl_df(read.csv("~/Datasets/Census_Bussiness_bureau/NorthAmericanIndustryClassificationSystemDescriptions_1998_02.txt", sep="."))
    colnames(naics)<-tolower(colnames(naics))
    levels(co$naics) <- levels(naics$naics)
    co <- left_join(co, naics, by='naics')
    geo <- tbl_df(read.csv("~/Datasets/Census_Bussiness_bureau/geo_reference_2002_11.txt"))
    co <- left_join(co, geo, by=c('fipstate','fipscty'))
    co <- co[,c(1,2,25,24,22,23,3,8:21)]
  }
  else if (year<=2001 & year>=1998){
    naics <- tbl_df(read.csv("~/Datasets/Census_Bussiness_bureau/NorthAmericanIndustryClassificationSystemDescriptions_1998_02.txt", sep="."))
    colnames(naics)<-tolower(colnames(naics))
    co <- left_join(co, naics, by='naics')
    geo <- tbl_df(read.csv("~/Datasets/Census_Bussiness_bureau/geo_reference_1996_01.txt"))
    co <- left_join(co, geo, by=c('fipstate','fipscty'))
    co <- co[,c(1,2,25,24,22,23,3,8:21)]
  } else {exit()}
  return(co)
}
