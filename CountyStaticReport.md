## County Counts For 2002-2012

<!-- Point knitr to the underlying code file so it knows where to look for the chunks. -->




```r
# pathInputDirectory <-
# 'F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial/PhiFreeDatasets'
pathInputDirectory <- file.path(getwd(), "PhiFreeDatasets")
pathInputSummaryCounty <- file.path(pathInputDirectory, "CountCountyFortified.csv")

dsValueAllVariables <- read.csv(pathInputSummaryCounty, stringsAsFactors = FALSE)
```



```r
dvName <- "CountPerCapita"  #The number of victims per county population; darker counties have more victims
dsValueAllVariables$DV <- dsValueAllVariables[, dvName]
# dsValueAllVariables$DVLabel <- round(dsValueAllVariables$DV, 2) #Keeps
# leading zeros
dsValueAllVariables$DVLabel <- gsub("^0.", ".", round(dsValueAllVariables$DV, 
    2))  #Remove leading zeros.
MapCounties(dsValueAllVariables, deviceWidth = 14)
```

![plot of chunk CountPerCapita](figure/CountPerCapita.png) 



```r
dvName <- "CountPerCapitaRank"  #The county's rank for the number of victims per county population; darker counties have more victims
dsValueAllVariables$DV <- dsValueAllVariables[, dvName]
dsValueAllVariables$DVLabel <- dsValueAllVariables$DV
MapCounties(dsValueAllVariables, deviceWidth = 14)
```

![plot of chunk CountPerCapitaRank](figure/CountPerCapitaRank.png) 

