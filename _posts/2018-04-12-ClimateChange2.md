---
title: "Visualizing the Anthropogenic Causes of Climate Change"
author: "aaj2146 - "
date: "April 26, 2018"
output:
   html_document:
      self_contained: false
      keep_md: true
---



# 1. Introduction

### Insipiration:
In this paper we study the anthropogenic causes of climate change. Specifically, we use this visualization as an inspiration for our project [https://www.bloomberg.com/graphics/2015-whats-warming-the-world/] . In this award visualization, journalists at Bloomberg in collaboration with data scientists at NASA studied the various causes of climate change scaled to plot with global temperatures in the past 150 years since the Industrial Revolution. The format of the interactive visualization is excellent to demonstrate its point. Each factor contributing to climate change has been first plotted individually followed by their combined effect to demonstrate their net effect on global temperature. In similar vein, we chose to focus more on the anthropogenic factors that directly correlate with CO2 



### The Team: 

- **Aditya Jadhav aaj2146**    

- **Yang Qiao yq2223**

- **Simran Lamba sl4228**

As millennials who grew up in the developing world, we are first hand witnesses to the drastic effects of environmental degradation and wanton urbanization. We firmly believe that data science will be a key force that helps mitigate the impending challenges that climate change will present, not only through technological innovations but also through greater awareness and influence on public policy. An exploratory data analysis of the  climate change movement raises some interesting socio-political questions. 

In our literature survey, we came across Capitalism Vs The Climate (Naomi Klein) [https://thischangeseverything.org/book/], a book that discusses the issue of climate change in a socio political context through the late 80s to the present time. The author makes statements pertaining to the causes of climate change from a purely political standpoint. One of the key claims is that free market capitalism has directly led to a phenomenal rise in global CO~2~ emissions. The book outlines how trade deals make provisions that inhibit and stifle measures taken by various countries to adopt green technology. Another by product of these trade deals is the availability of inexpensive goods manufactured in places where environmental regulations are lax. The past three decades have seen an precedented rise in consumerism as evidenced by indicators like household credit card debt. 


We wish to verify these claims by analyzing the data in the references she provides as well as through other publicly available data. We shall get in to the details of this data in the next section. In particular, we wish to answer the following questions with Capitalsim vs The Climate as the broader theme: 




### Questions:

1. Landmark “Free-Trade” agreements seem to coincide with landmark climate negotiations. How effective are these agreements? Do these free trade negotiations actually inhibit the policies that climate agreements want to put in place?


2. How does the health of an economy affect public perception of climate change?


3. How consumerism and a unhealthy demand for cheaper material goods has contributed to global warming?


4. How climate science which is often discredited by vested interests has been perceived by the general public over the years. Is there any correlation with the aforementioned free trade deals and lesser agreeability with scientific fact?                    



------------------------

***************************




# 2. Description of Data                                                     


1. **Global Temperature**      
https://climate.nasa.gov/vital-signs/global-temperature/         
This time series plot shows the change in global surface temperature relative to 1951-1980 average temperatures. Seventeen of the 18 warmest years in the 136-year record all have occurred since 2001, with the exception of 1998. The year 2016 ranks as the warmest on record. (Source: NASA/GISS). This research is broadly consistent with similar constructions prepared by the Climatic Research Unit and the National Oceanic and Atmospheric Administration.


2. **Atmospheric CO2 content   **
http://scrippsco2.ucsd.edu/assets/data/atmospheric/stations/in_situ_co2/monthly/monthly_in_situ_co2_mlo.csv       
We took data from the Scripps Institute’s CO2 program at UC San Diego. The data we chose to work with is the monthly record of atmospheric CO2 content. 



3. **Household Credit Card Debt**
https://wallethub.com/edu/credit-card-debt/25533/#revolving-consumer-debt        
We chose to look at the yearly credit card debt per household in the United States as an estimate of consumerism. This data is sourced from the Federal Reserve by WalletHub. It is a part of WalletHub’s annual report. The data is adjusted for year on year inflation. 


4.  **Public perception and sentiment towards climate change.    **                
http://www.aei.org/publication/aei-public-opinion-study-polls-on-the-environment-energy-global-warming-and-nuclear-power-2/      
In the book the author claims that over the past three decades there has been a systematic attack on the scientific consensus about global warming in the United States. We have gathered data that tracks the year on year public sentiment about global warming in terms of how much people trust the science surrounding it. We can verify the claim made by the author in this regard.   




-------------------------------------------------

*****************************************************

# 3. Analysis of Data Quality

#####  1. Public opinion data. 

We first look at the public opinion data. This data was sourced from the AEI Public Opinion Study. It tracks the year on year public opinion about global warming. We look data for the years 1989 through 2015. The people surveyed for this data were asked the question "How much of a threat do you think Global warming poses?" The answers were classified as "Fair Amount", "Great Deal", "not at all" and "only a little"  

As we can see that there are a few missing values for the years 1992-96 and the year 2005. Also, there might be some faults in the data in terms of consistency. The data is supposed to show the percentage of each response. Thus, ideally it should sum to 100. However, in some cases it does not sum to 100. We speculate that there was an error in recording the data or perhaps some of the respondents refused to answer the question.  








```r
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(plyr)
data <- read_excel("global warming and climate change.xlsx")
new_data <- gather(data, "Perception", "Percentage", 2:5)
 graph <- ggplot() + geom_bar(aes(y = Percentage, x = Year, fill = Perception), data = new_data, stat="identity")+scale_x_continuous(breaks=seq(1989,2016,1))+
   theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 10, angle = 60, hjust =1),
        axis.text.y=element_text(colour="black", size = 10))
 graph
```

![](project_final_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

  ****************************************************
  
  
#####   2. Credit Card Debt per Household
  Here we chose to look at the credit card debt per household as an estimate of a rising consumerist lifestyle promoted by free market capitalism. We hyposthesize that the surplus of mass produced goods in the free market economy gives impetus to increased wanton consumption. This increased consumption of inexpensive material goods is correlated with a sharp rise in CO~2~ emissions. We hope to explore this relationship with this data. 
  
  An interesting observation we make is the spike in debt at the peak of the economic recession in 2008.
  


```r
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)

credit <- read_excel("credit card debt by household.xlsx")
g <- ggplot(credit, aes(Year_Q4,Per_Household_Debt))+geom_line(color = "green")+
      ggtitle("Household Credit Card Debt")+
    theme_grey(16)+xlab("Year")+ylab("Credit card debt in USD")
  theme(legend.title = element_blank())
```

```
## List of 1
##  $ legend.title: list()
##   ..- attr(*, "class")= chr [1:2] "element_blank" "element"
##  - attr(*, "class")= chr [1:2] "theme" "gg"
##  - attr(*, "complete")= logi FALSE
##  - attr(*, "validate")= logi TRUE
```

```r
g
```

![](project_final_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

*********************************************************************






********************************************************************


##### 3. Atmospheric CO~2~ content

  For this data we look at the measured concentration of CO~2~ in the earth's atmosphere for the years 1988 through 2015


```r
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)

credit <- read_excel("Emissions.xlsx")
g <- ggplot(credit, aes(Year,CO2_ppm))+geom_line(color="blue")+
      ggtitle("Atmospheric CO2 Content")+
    theme_grey(16)+
  theme(legend.title = element_blank())
g
```

![](project_final_files/figure-html/unnamed-chunk-3-1.png)<!-- -->





*************************************************************************


##### 4. Temperature Data

For the temperature plots we have taken the GLOBAL LAND-OCEAN TEMPERATURE INDEX compiled by NASA's Goddard Institute for Space Studies. It illustrates the change in global temperature relative to a certain baseline (average for the years 1951-1980).




```r
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)

credit <- read_excel("Temperature.xlsx")
g <- ggplot(credit, aes(Year,Temp))+geom_line(color="red")+
      ggtitle("Temperature Index")+
    theme_grey(16)+
  theme(legend.title = element_blank())
g
```

![](project_final_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

**********************************************************



**********************************************************



***********************************************************





# 4. Main Analysis (Exploratory Data Analysis)

A major chunk of the project was dedicated to gathering the right kind of data to answer the questions we wish to answer. To this end we ran literature survey of the references provide by Klein in her book that supposedly support her claims. 

Firstly, we would like to present a timeline beginning on 1988 that virtually marks an epoch:

- 1988 : Goddard Institute presents definitive link between rising global temperature and unfettered release of CO~2~ into the atmosphere stemming from human activity


- 1989 : IPCC (Intergovernmental Panel for Climate Change) is formed.    <br>                               Fall of the Communist Bloc

- 1992 : UN Climate Convention at Rio                                                            NAFTA is ratified

- 1994 : WTO is born

- 1997 : Kyoto Protocol is ratified

- 2008 : Housing crisis

- 2011 : Warmest year on record

- 2013 : TPP is ratified in secret

- 2014 : Green Economy Act, Canada

- 2015 : Paris Agreement 


An interactive timeline for this information is given below.



```r
library(timevis)

data <- data.frame(
  id      = 1:13,
  content = c("IPCC formed, first session", "Goddard Institute presents in Congress",
              "Fall of the Communist Bloc", "First IPCC negotiations", "UN Climate Convention in Rio", "WTO is formed", "Kyoto protocol", "Housing Crisis", "Warmest year on record", "WTO deems China's subsidy of solar panels illegal", "TPP is ratified", "Green Economy Act, Canada", "Paris Agreement"),
  start   = c("1989", "1988",
              "1989", "1989","1992","1994","1997","2008","2011","2012","2013","2014","2015"),
  end     = c(NA, NA, NA, NA,NA, NA, NA, NA,NA, NA, NA, NA,NA)
)

timevis(data)
```

<!--html_preserve--><div id="htmlwidget-b2ce4083fd4cf62fb3db" class="timevis html-widget" style="width:960px;height:480px;">
<div class="btn-group zoom-menu">
<button type="button" class="btn btn-default btn-lg zoom-in" title="Zoom in">+</button>
<button type="button" class="btn btn-default btn-lg zoom-out" title="Zoom out">-</button>
</div>
</div>
<script type="application/json" data-for="htmlwidget-b2ce4083fd4cf62fb3db">{"x":{"items":[{"id":" 1","content":"IPCC formed, first session","start":"1989"},{"id":" 2","content":"Goddard Institute presents in Congress","start":"1988"},{"id":" 3","content":"Fall of the Communist Bloc","start":"1989"},{"id":" 4","content":"First IPCC negotiations","start":"1989"},{"id":" 5","content":"UN Climate Convention in Rio","start":"1992"},{"id":" 6","content":"WTO is formed","start":"1994"},{"id":" 7","content":"Kyoto protocol","start":"1997"},{"id":" 8","content":"Housing Crisis","start":"2008"},{"id":" 9","content":"Warmest year on record","start":"2011"},{"id":"10","content":"WTO deems China's subsidy of solar panels illegal","start":"2012"},{"id":"11","content":"TPP is ratified","start":"2013"},{"id":"12","content":"Green Economy Act, Canada","start":"2014"},{"id":"13","content":"Paris Agreement","start":"2015"}],"groups":null,"showZoom":true,"zoomFactor":0.5,"fit":true,"options":[],"height":null,"api":[]},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->



We want to investigate the following claim(s) made in the book with data: 

The rise of free market capitalism at the end of the Cold War led to the proliferation of free market capitalism throughout the world. The author claims that the free market ideology which presupposes infinite growth at the cost of rising consumption is fundamentally at loggerheads with actions that combat climate change. She supports this claim by pointing out that since 1988 when the first climate negotiations began, the CO~2~ emissions have actually grown at an unprecedented rate. 


To dig deeper into the causes for this we observe that almost every landmark climate negotiation is often countered with a free trade agreement. For example, the WTO in 2012 prevented China from exporting subsidized solar panels since it amounted to "unfair trade practices". This was high on the heels of 2011 which was the warmest year on record. 


Additionally, the author claims that by outsourcing the manufacturing to the developing world (India, China, Brazil etc.) where environmental laws are lax or sometimes nonexistent has led to cheaper mass produced goods. These goods are manufactured with at a massive cost to the environment. Moreover, transporting these goods across long distances in diesel shipment trawlers contributes massively to global CO~2~ emissions. Also, the availability of such inexpensive goods on a massive scale has lead to a highly consumerist lifestyle.



To verify these claims we chose to look at household credit card debt to estimate the rising consumerism, the global surface temperature index and atmospheric CO~2~ content. On the public policy front, certain vested interests that profit from fossil fuels have been actively discrediting climate science. To gauge this we decided to look at public perception of climate change. 


Our methodology was as follows:

- We take the greenhouse effect caused by CO~2~ as a baseline starting point. Therefore, atmospheric CO~2~ content becomes the prime variable we test our hypothesis against. This means that if we find enough evidence that suggests a particular parameter causes rise in CO~2~ emissions we will accept the corresponding claim as verified.


- To this end, we first check correlations between CO~2~ emissions and the different socio-economic parameters that we are looking at. Look at their distributions

- Check the effect of each parameter individually against CO~2~ emissions 





```r
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(GGally)

data <- read_excel("raw_data.xlsx")
data<-data[c(12:28),c(2:5)]
ggpairs(data)
```

![](project_final_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
data <- read_excel("raw_data.xlsx")
new_data <- data[c(12:28),]

ggplot(data, aes(x=CO2_ppm, y=Temp)) + geom_point(color = "cyan4", size = 2)+
ggtitle("Scatterplot between Atmospheric CO2 Content & Temperature Index")+
      xlab("Atmospheric CO2 Content")+ylab("Temperature Index")+
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 12, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10))
```

![](project_final_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
ggplot(new_data, aes(x=CO2_ppm, y=Per_Household_Debt)) + geom_point(color = "cyan4", size = 2)+
ggtitle("Scatterplot between Atmospheric CO2 Content & Per_Household_Debt")+
      xlab("Atmospheric CO2 Content")+ylab("Per_Household_Debt")+
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 12, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10))
```

![](project_final_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

```r
ggplot(new_data, aes(x=CO2_ppm, y=Perception)) + geom_point(color = "cyan4", size = 2)+
ggtitle("Scatterplot between Atmospheric CO2 Content & Perception")+
      xlab("Atmospheric CO2 Content")+ylab("Perception")+
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 12, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10))
```

![](project_final_files/figure-html/unnamed-chunk-7-3.png)<!-- -->


###### Inferences

From the scatter plots we see that the strong correlation between atmospheric CO~2~ and temperature index as we had expected from our baseline assumption about the greenhouse effect. 


Counterintuitively, public opinion on the threat that climate change poses seems to have diminished against the backdrop of rising global temperatures. A larger percentage of people now believe that climate change is not such a serious threat. 


There is a fairly strong correlation between credit card debt (adjusted for inflation) which here we are using to estimate hyperconsumption and CO~2~ levels.


Finally we plot the collective effect of these three parameters on CO~2~ levels in a consolidated graph:

We scaled and normalized the data to the same standard in order to see the relative trends of each feature compared to others. We scaled the data so that the line plots of the variables will start at the same point. We substitute missing values in Preception Index with 0s.


```r
library(ggplot2)
# 1988-2015 data
#credit card debt 99-15
data2 = read.csv("Debt.csv", header =  T)
# perception 89-15
data3 = read.csv("Perception.csv", header = T)
# global temperature change  88-15
data5 = read.csv("Temperature.csv", header = T)
# CO2 Emission change
data1 = read.csv("Emissions.csv", header = T)

data = read.csv("Climate.csv", header = T)

for (i in 1:28){
  data$Temp[i] = data$Temp[i] * 700 /200
}

for (i in 1:28){
  data$CO2_ppm[i]=data$CO2_ppm[i]/200
}

for (i in 11:28){
  data$Debt[i]=data$Debt[i]/21.14/200
}

for (i in 2:28){
  data$Perception.Not.at.all.[i]=data$Perception.Not.at.all.[i]*16.3/200
}
write.csv(data, file = "climate_scaled.csv")

library(reshape2)
```

```
## 
## Attaching package: 'reshape2'
```

```
## The following object is masked from 'package:tidyr':
## 
##     smiths
```

```r
dd = melt(data, id=c("Year"))
ggplot(dd) + geom_line(aes(x=Year, y=value, colour=variable)) +
  scale_colour_manual(values=c("red","green","blue", "orange")) +ggtitle("Plot of the Four Indexes")
```

![](project_final_files/figure-html/unnamed-chunk-8-1.png)<!-- -->




# 5. Executive Summary

To summarize we first look at the raw data we have selected. Namely, public opinion on global warming, CO~2~ in the atmosphere, credit card debt per household and global surface temperature index. The plots presented earlier are tidied up. 


```r
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(plyr)
data <- read_excel("global warming and climate change.xlsx")
new_data <- gather(data, "Perception", "Percentage", 2:5)
 graph <- ggplot() + geom_bar(aes(y = Percentage, x = Year, fill = Perception), data = new_data, stat="identity")+scale_x_continuous(breaks=seq(1989,2016,1))+
   theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 10, angle = 60, hjust =1),
        axis.text.y=element_text(colour="black", size = 10))
 graph
```

![](project_final_files/figure-html/unnamed-chunk-9-1.png)<!-- -->



```r
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)

credit <- read_excel("credit card debt by household.xlsx")

g <- ggplot(credit, aes(Year_Q4,Per_Household_Debt))+geom_line(color = "cornflowerblue",lwd=1.1)+
      geom_point(color = "cornflowerblue", size = 2)+
      scale_x_continuous(breaks=seq(1999,2015,1))+
      ggtitle("Household Credit Card Debt")+
      xlab("Year")+ylab("Credit card debt in USD")+
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 10, angle = 60, hjust =1),
        axis.text.y=element_text(colour="black", size = 10))
 
g
```

![](project_final_files/figure-html/unnamed-chunk-10-1.png)<!-- -->



```r
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)

emi <- read_excel("Emissions.xlsx")

g <- ggplot(emi, aes(Year,CO2_ppm))+geom_line(color = "cornflowerblue",lwd=1.1)+
      geom_point(color = "cornflowerblue", size = 2)+
      scale_x_continuous(breaks=seq(1988,2015,1))+
      ggtitle("Atmospheric CO2 Content")+
      xlab("Year")+ylab("CO2_ppm")+
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 10, angle = 60, hjust =1),
        axis.text.y=element_text(colour="black", size = 10))
g
```

![](project_final_files/figure-html/unnamed-chunk-11-1.png)<!-- -->



```r
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)

temp <- read_excel("Temperature.xlsx")

g <- ggplot(temp, aes(Year,Temp))+geom_line(color = "cornflowerblue",lwd=1.1)+
      geom_point(color = "cornflowerblue", size = 2)+
      scale_x_continuous(breaks=seq(1988,2015,1))+
      ggtitle("Temperature Index")+
      xlab("Year")+ylab("Temperature Index")+
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 10, angle = 60, hjust =1),
        axis.text.y=element_text(colour="black", size = 10))
g
```

![](project_final_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

###### (From the main analysis section)
We want to investigate the following claim(s) made in the book with data: 

The rise of free market capitalism at the end of the Cold War led to the proliferation of free market capitalism throughout the world. The author claims that the free market ideology which presupposes infinite growth at the cost of rising consumption is fundamentally at loggerheads with actions that combat climate change. She supports this claim by pointing out that since 1988 when the first climate negotiations began, the CO~2~ emissions have actually grown at an unprecedented rate. 


To dig deeper into the causes for this we observe that almost every landmark climate negotiation is often countered with a free trade agreement. For example, the WTO in 2012 prevented China from exporting subsidized solar panels since it amounted to "unfair trade practices". This was high on the heels of 2011 which was the warmest year on record. 


Additionally, the author claims that by outsourcing the manufacturing to the developing world (India, China, Brazil etc.) where environmental laws are lax or sometimes nonexistent has led to cheaper mass produced goods. These goods are manufactured with at a massive cost to the environment. Moreover, transporting these goods across long distances in diesel shipment trawlers contributes massively to global CO~2~ emissions. Also, the availability of such inexpensive goods on a massive scale has lead to a highly consumerist lifestyle.


To verify these claims we chose to look at household credit card debt to estimate the rising consumerism, the global surface temperature index and atmospheric CO~2~ content. On the public policy front, certain vested interests that profit from fossil fuels have been actively discrediting climate science. To gauge this we decided to look at public perception of climate change. 



```r
library(ggplot2)
# 1988-2015 data
#credit card debt 99-15
data2 = read.csv("Debt.csv", header =  T)
# perception 89-15
data3 = read.csv("Perception.csv", header = T)
# global temperature change  88-15
data5 = read.csv("Temperature.csv", header = T)
# CO2 Emission change
data1 = read.csv("Emissions.csv", header = T)

data = read.csv("Climate.csv", header = T)

for (i in 1:28){
  data$Temp[i] = data$Temp[i] * 700 /200
}

for (i in 1:28){
  data$CO2_ppm[i]=data$CO2_ppm[i]/200
}

for (i in 11:28){
  data$Debt[i]=data$Debt[i]/21.14/200
}

for (i in 2:28){
  data$Perception[i]=data$Perception.Not.at.all.[i]*16.3/200
}
data = data[,c(1:4, 6)]
write.csv(data, file = "climate_scaled.csv")

library(reshape2)
dd = melt(data, id=c("Year"))

ggplot(dd) + geom_line(aes(x=Year, y=value, colour=variable)) +
  scale_colour_manual(values=c("red","green","blue", "orange")) +ggtitle("Plot of the Four Indices")+
  xlab("Year")+ylab("Data")+
      scale_x_continuous(breaks=seq(1988,2015,1))+
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 30, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 20, angle = 60, hjust =1),
        axis.text.y=element_text(colour="black", size = 20),
        axis.title.x=element_text(size=20,face="bold"),
        axis.title.y=element_text(size=20,face="bold"),
        legend.text=element_text(size=18,face="bold"),
        legend.title=element_text(size=20,face="bold"))
```

![](project_final_files/figure-html/unnamed-chunk-13-1.png)<!-- -->





# 6. Interactive Part(s)

- This is an interactive version of the consolidated time series plot. It is inspired from the Bloomberg plot where all the parameters are superimposed on the dependent variable which is the temperature index. The events we wish to highlight are annotated on the graph:

https://plot.ly/~sl4228/30/interactive-part/

- This graph can be annotated with landmark events that occurred in that 30 year window. This is achieved with this interactive timeline.  

<!--html_preserve--><div id="htmlwidget-ce809b024e61e5d0debf" class="timevis html-widget" style="width:960px;height:480px;">
<div class="btn-group zoom-menu">
<button type="button" class="btn btn-default btn-lg zoom-in" title="Zoom in">+</button>
<button type="button" class="btn btn-default btn-lg zoom-out" title="Zoom out">-</button>
</div>
</div>
<script type="application/json" data-for="htmlwidget-ce809b024e61e5d0debf">{"x":{"items":[{"id":" 1","content":"IPCC formed, first session","start":"1989"},{"id":" 2","content":"Goddard Institute presents in Congress","start":"1988"},{"id":" 3","content":"Fall of the Communist Bloc","start":"1989"},{"id":" 4","content":"First IPCC negotiations","start":"1989"},{"id":" 5","content":"UN Climate Convention in Rio","start":"1992"},{"id":" 6","content":"WTO is formed","start":"1994"},{"id":" 7","content":"Kyoto protocol","start":"1997"},{"id":" 8","content":"Housing Crisis","start":"2008"},{"id":" 9","content":"Warmest year on record","start":"2011"},{"id":"10","content":"WTO deems China's subsidy of solar panels illegal","start":"2012"},{"id":"11","content":"TPP is ratified","start":"2013"},{"id":"12","content":"Green Economy Act, Canada","start":"2014"},{"id":"13","content":"Paris Agreement","start":"2015"}],"groups":null,"showZoom":true,"zoomFactor":0.5,"fit":true,"options":[],"height":null,"api":[]},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


# 7. Conclusion 

While the author makes certain confident statements about the anthropogenic causes of climate change particularly in a socio political context, the data tells a reality that is grounded with perhaps more nuance. However, she does make compelling arguments that seem quite logical at first glance. A data driven verification of those claims is what we have attempted here. Our conclusions are as follows 

1. There is no real trend in the perception data. Even though we observe a healthy correlation between CO~2~ levels and perception data, we should not fall into the trap of correlation = causation. The change in perception could be influenced by education levels, other factors like income of the individual etc. Also, unlike temperature data which is available in abundance, there simply isn't enough comprehensive data on the public opinion available in the public domain. 

2. Given that there is a strong correlation between household credit card debt (by extension consumerism) and CO~2~ levels, and the additional knowledge about outsourced manufacturing, increased emissions from transportation of goods on a global scale etc. we can say that there is a causal link between a consumerism and CO~2~ levels.

3. The causal link between CO~2~ levels and the global temperature index has already been extensively established by various climate scientists the world over. Our attempt at visualizing that link was not entirely successful. Since we had the background knowledge that rising CO~2~ levels in the atmosphere is indeed causing warming, we tried different approaches to graphically visualize this. One of these was to plot relative change in CO~2~ levels, year on year emissions and various relative estimates of thereof. However, the trend relative to the temperature index in each case was either too weak or too strong. 


Through this project we took up a problem that we are passionate about, scanned relevant references to gather data to answer the questions we had and in the process got a taste of real life exploratory data analysis. In the future, we would like further explore the niches of this problem like public perception. In conclusion, we hope more EDAV is done in the area of climate science. The reproducibility of this project means it could be a starting point in that direction. 


