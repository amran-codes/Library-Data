# Introduction-to-Ontario-Library-Data
 
# INTRODUCTION 
 #The objective of this project is to understand how  Ontario Public Libraries can be more successful. In this analysis we will be looking at how to improve Ontario public libraries and the metric for this will mostly be revenue as a cue for success. The data set that will be used is between 2015-2017. In this analysis we will gather the data for three different insights which can be used to improve the success of Ontario public libraries. Success Criteria for this analysis includes a library being more successful when it has: Higher revenue, and more cardholders. The measured data set or columns that will be looked at include: revenue, and the number of active library cardholders. The categorical data set or columns that will be looked at include: cities or the town. The assumptions in the analysis include: Libraries with more visitors have higher revenues. Having a higher revenue makes a library more successful. To analyze this data, the analysis will gain insight from data visualized looking at tables, graphs, mean distribution, median distribution, and the quantile points of the data.


#Importing datasets
```{r}
if (!require('ggplot2')) 
{
  install.packages('ggplot2');
  library(ggplot2);
}

#importing my data sets
data2015 <- read.csv("2015_ontario_public_library_statistics_open_data_dec_2017rev.csv")

data2016 <- read.csv("2016 ontario_public_library_statistics_open_data_2016.csv")

data2017 <- read.csv("2017 ontario_public_library_statistics_open_data_july_2019_rev1.csv")

```

#Merged all files into one object
```{r}
#organizing my column names
common_columns <- Reduce(intersect, list(colnames(data2015),colnames(data2016),colnames(data2017)))

#combining my files data
datacombined <- rbind(subset(data2015, select = common_columns), subset(data2016, select = common_columns), subset(data2017, select = common_columns))


```

#Creating a new column that represents operating revenue per active card holder (total operating Revenue / number of active card holders).
```{r}

#Removing  the commas 
datacombined$B2.9..Total.Operating.Revenues <-  gsub(",", "", datacombined$B2.9..Total.Operating.Revenues)

datacombined$A1.14..No..of.Active.Library.Cardholders <- gsub(",", "", datacombined$A1.14..No..of.Active.Library.Cardholders)

#Checking the class of the columns
class(datacombined$A1.14..No..of.Active.Library.Cardholders)

class(datacombined$B2.9..Total.Operating.Revenues)

#Making NA into zero's
datacombined[is.na(datacombined)] <- 0

# Making the variables  numeric form
datacombined$A1.14..No..of.Active.Library.Cardholders <- as.numeric(datacombined$A1.14..No..of.Active.Library.Cardholders)

datacombined$B2.9..Total.Operating.Revenues <-  as.numeric(datacombined$B2.9..Total.Operating.Revenues)

#Removing the zeros from the specified columns
datacombined <- subset(datacombined, datacombined$A1.14..No..of.Active.Library.Cardholders > 0 & datacombined$B2.9..Total.Operating.Revenues > 0)

#Dividing Operating Revenue by Active Card Holder
datacombined$B2.9..Total.Operating.Revenues / datacombined$A1.14..No..of.Active.Library.Cardholders

#duplicating my dataframe
newdatacombined <- datacombined

#Creating new variable column for Operating Revenue per Active Card Holder
newdatacombined$newcolumn <- newdatacombined$B2.9..Total.Operating.Revenues / newdatacombined$A1.14..No..of.Active.Library.Cardholders

```


#INSIGHT 1: Creating a table of two columns that has the data for city names and newcolumn (total revenue per active cardholder). The insight we will gain from this data will indicate which cities have the highest operating revenue per cardholder.
```{r}
 #Making newcolumn and City/Town into one object 
twodatacombined<- dplyr::select (newdatacombined,newcolumn,A1.10.City.Town)

 #Putting the new object in ascending order by values in (newcolumn)
twodatacombined <- twodatacombined[order(twodatacombined$newcolumn),]

 #Looking at the last values in the object, this shows the highest values in (newcolumn) which shows the libraries with the highest operating revenue per active cardholer
tail(twodatacombined)
 
```


#INSIGHT 2: looking at the average and overall distribution, with plot of revenue vs cardholders. Seeing higher total revenue with higher total cardholder. Libraries with more cardholders have higher total operating revenue numbers and so the insight gained here is that we need to invest more in understanding how these libraries are able to attract more cardholders.
```{r}
#average for newcolumn, total revenue per cardholder 
mean(newdatacombined$newcolumn, na.rm = TRUE)
median(newdatacombined$newcolumn, na.rm = TRUE)

 #plot showing the distribution of operating revenue vs number of active cardholders
ggplot(data = newdatacombined, mapping = aes(x = A1.14..No..of.Active.Library.Cardholders, y = B2.9..Total.Operating.Revenues))+ geom_point(size = 5)

```


#INSIGHT 3: Looking at the quantile points of the newcolumn to understand the distribution of the newcolumn values. This data will show us the quantile points which demonstrate the five values for the minimum, the 1st quantile, the median, then mean 3rd quantile, and the maximum for every level of operating revenue per active cardholder. 
```{r}
library(tidyverse)
library(ggplot2) 

 #this shows the quantile points
quantile(twodatacombined$newcolumn, probs=c(0, 0.25, 0.5, 0.75, 1))

```

#Recommended Actions 
With the data gained from this report we’ve seen specific insights on total revenue per active cardholder. The government is advised to do further investigation on the top performing (highest revenue per cardholder) libraries logistial functioning information and their data to better understand their success and how to replicate that for other less performing libraries to improve their success..

The insights gained from looking at the median and average operating revenue per number cardholder is the knowledge of how well a typical library is expected to perform on average operating revenue per number and the median tells the exact middle value for which any other value is either below or above that number. This insight into the data allows the organization to further visualize how the data is distributed and how high or low a library's operating revenue per number value is in comparison to other libraries.

The organization should also perform more focused analysis on the libraries in the lowest and highest ends of the quantile points. This will provide the government with further information on exactly how to classify the lower end performing libraries, further research can look into the availability of resources in these areas including electronic resources which could be contributing to their lower success.




 #Fixing PDF knitting issue with package
 
install.packages("tinytex")
tinytex::install_tinytex()

 
