---
title: "Untitled"
author: "Siyue Wang"
date: "4/30/2022"
output:
  html_document: default
  word_document: default
  pdf_document: default
---

```{r}
library(readr)
library(readxl)



Region_CAL <- read_xlsx("~/Desktop/AREC380/AREC380 PROJ/memo2/Region_CAL.xlsx")
Region_TEX <- read_xlsx("~/Desktop/AREC380/AREC380 PROJ/memo2/Region_TEX.xlsx")
#View(Region_TEX)
#View(Region_CAL)
```


#Reshaping Electricity demend data of California ->data.1
```{r}
library(tidyverse)
library(lubridate)

Region_CAL %>%
  select(Region,`Local date`, Hour, D ) -> Cal

Cal %>%
pivot_wider(
  id_cols=`Local date`,
  names_from = Hour,
  values_from = D,
  names_prefix="DemandAtHour."
) -> Cal

Cal %>%
  mutate(Cal,
    Avg_DemandPerDay_Cal = (DemandAtHour.1+DemandAtHour.2+DemandAtHour.3+DemandAtHour.4+DemandAtHour.5+
                             DemandAtHour.6+DemandAtHour.7+DemandAtHour.8+DemandAtHour.9+DemandAtHour.10+
                             DemandAtHour.11+ DemandAtHour.12+DemandAtHour.13+DemandAtHour.14+
                             DemandAtHour.15+DemandAtHour.16+DemandAtHour.17+DemandAtHour.18+
                             DemandAtHour.19+DemandAtHour.20+DemandAtHour.21+DemandAtHour.22+
                             DemandAtHour.23+DemandAtHour.24)/24 ) -> Cal
                           
   
Cal %>%
  select(`Local date`,Avg_DemandPerDay_Cal)->Demand_Cal
#View(Demand_Cal)


```


#Reshaping Electricity demand data of Texas ->data.2
```{r}
Region_TEX %>%
  select(Region,`Local date`, Hour, D ) -> Tex

Tex %>%
pivot_wider(
  id_cols=`Local date`,
  names_from = Hour,
  values_from = D,
  names_prefix="DemandAtHour."
) -> Tex

Tex %>%
  mutate(Tex,
    Avg_DemandPerDay_Tex = (DemandAtHour.1+DemandAtHour.2+DemandAtHour.3+DemandAtHour.4+DemandAtHour.5+
                             DemandAtHour.6+DemandAtHour.7+DemandAtHour.8+DemandAtHour.9+DemandAtHour.10+
                             DemandAtHour.11+ DemandAtHour.12+DemandAtHour.13+DemandAtHour.14+
                             DemandAtHour.15+DemandAtHour.16+DemandAtHour.17+DemandAtHour.18+
                             DemandAtHour.19+DemandAtHour.20+DemandAtHour.21+DemandAtHour.22+
                             DemandAtHour.23+DemandAtHour.24)/24 ) -> Tex
                           
   
Tex %>%
  select(`Local date`,Avg_DemandPerDay_Tex)->Demand_Tex
#View(Demand_Tex)

```

#Combine data.1 and data.2 
```{r}
Demand_Tex %>%
  full_join(Demand_Cal) -> Demand

Demand %>%
  filter(year(`Local date` )> 2016 & year(`Local date`) < 2022) -> Demand

Demand %>%
  mutate(
    Date = `Local date`
  ) ->Demand

Demand %>%
  select(Date,Avg_DemandPerDay_Tex,Avg_DemandPerDay_Cal )-> Demand
#View(Demand)

```





#Reshaping AQI of Cal and Tex
```{r}
library(readr)
aqi.2017 <- read_csv("~/Desktop/AREC380/AREC380 PROJ/daily_aqi_by_cbsa_2017.csv")
aqi.2018 <- read_csv("~/Desktop/AREC380/AREC380 PROJ/daily_aqi_by_cbsa_2018.csv")
aqi.2019 <- read_csv("~/Desktop/AREC380/AREC380 PROJ/daily_aqi_by_cbsa_2019.csv")
aqi.2020 <- read_csv("~/Desktop/AREC380/AREC380 PROJ/daily_aqi_by_cbsa_2020.csv")
aqi.2021 <- read_csv("~/Desktop/AREC380/AREC380 PROJ/daily_aqi_by_cbsa_2021.csv")

#Combine them together
library(tidyverse)
library(lubridate)
bind_rows(aqi.2017,aqi.2018) ->aqi.1718
bind_rows(aqi.1718,aqi.2019) ->aqi.1719
bind_rows(aqi.1719,aqi.2020) ->aqi.1720
bind_rows(aqi.1720,aqi.2021) ->aqi.17to21


#Choose the area of Texas and California
aqi.17to21 %>%
  filter(CBSA == "Waco, TX" | CBSA == "Bakersfield, CA"
  ) ->aqi


#Plot the AQI in these two regions

aqi %>%
  select(Date,CBSA,AQI)->aqi


aqi %>%
  pivot_wider(
    id_cols=c(Date),
    names_from = CBSA,
    values_from = AQI
  ) ->AQI

AQI %>%
  mutate( 
    AQI.Cal = `Bakersfield, CA`,
    AQI.Tex = `Waco, TX`
            ) ->AQI

AQI %>%
  select(Date,AQI.Cal,AQI.Tex)->AQI
#View(AQI)

```

#join demand and aqi
```{r}
Demand %>%
  inner_join(AQI) -> new
#View(new)


```


#Tidy NO2 datasets
```{r}
library(readr)
NO2.2017 <- read_csv("~/Desktop/AREC380/AREC380 PROJ/NO2/daily_42602_2017.csv")

#year2018
NO2.2018 <- read_csv("~/Desktop/AREC380/AREC380 PROJ/NO2/daily_42602_2018.csv")

#year 2019
NO2.2019 <- read_csv("~/Desktop/AREC380/AREC380 PROJ/NO2/daily_42602_2019.csv")

#year2020
NO2.2020 <- read_csv("~/Desktop/AREC380/AREC380 PROJ/NO2/daily_42602_2020.csv")

#year2021
NO2.2021 <- read_csv("~/Desktop/AREC380/AREC380 PROJ/NO2/daily_42602_2021.csv")

##filter these datasets with effective variables
NO2.2017 %>%
  filter( `State Name` == "California" | `State Name` =="Texas") %>%
  select(`State Name`, `Date Local`, `Arithmetic Mean`, `Parameter Name`)->NO2.2017


NO2.2018 %>%
  filter( `State Name` == "California" | `State Name` =="Texas") %>%
  select(`State Name`, `Date Local`, `Arithmetic Mean`, `Parameter Name`)->NO2.2018


NO2.2019 %>%
  filter( `State Name` == "California" | `State Name` =="Texas") %>%
  select(`State Name`, `Date Local`, `Arithmetic Mean`, `Parameter Name`)->NO2.2019

NO2.2020 %>%
  filter( `State Name` == "California" | `State Name` =="Texas") %>%
  select(`State Name`, `Date Local`, `Arithmetic Mean`, `Parameter Name`)->NO2.2020

NO2.2021 %>%
  filter( `State Name` == "California" | `State Name` =="Texas") %>%
  select(`State Name`, `Date Local`, `Arithmetic Mean`, `Parameter Name`)->NO2.2021

#combine these datasets
bind_rows(NO2.2017,NO2.2018) ->f
bind_rows(f,NO2.2019) ->g
bind_rows(g,NO2.2020) ->h
bind_rows(h,NO2.2021) ->NO2


NO2 %>%
  mutate(
    NO2.level = `Arithmetic Mean`
  )->NO2

NO2 %>%
  select(`State Name`, `Date Local`, `NO2.level`) -> NO2
#View(NO2)

NO2 %>%
  group_by(`Date Local`,`State Name`) -> NO2 
#View(NO2)

```

#Calculate average NO2 per day
```{r}
NO2 %>%
  mutate(Date = `Date Local`)->NO2
NO2 %>%
  group_by(Date,`State Name`)%>%
  summarize(mean.NO2Level = mean(NO2.level)) ->NO2
  
#View(NO2)
  

```

#Reshape datasets
```{r}
NO2 %>%
  pivot_wider(
    id_cols=c(Date),
    names_from = `State Name`,
    values_from = mean.NO2Level 
  ) ->NO2

NO2 %>%
  mutate(NO2.Cal = California,
         NO2.Tex = Texas) ->NO2

NO2 %>%
  group_by(Date)

```


```{r}

NO2 %>%
  select(Date, NO2.Cal, NO2.Tex)->NO2



```

#join all variables together
```{r}
new %>%
  inner_join(
    NO2,
    by=c("Date")) -> final

final %>%
  drop_na() ->final
View(final)


```


# plot demand curves
```{r}
ggplot(data = new)+
  geom_point(mapping=aes(x=Date, y = Avg_DemandPerDay_Tex, col ="Avg_DemandPerDay_Tex"))+
  geom_point(mapping=aes(x=Date, y = Avg_DemandPerDay_Cal, col ="Avg_DemandPerDay_Cal"))+
  labs(x="Date", y = "Electricity demand")


```

# plot demand~AQI and demand~NO2
```{r}
ggplot(data=new)+
  geom_point(mapping=aes(x=Avg_DemandPerDay_Tex, y = AQI.Tex, col ="Tex"))+
  geom_point(mapping=aes(x=Avg_DemandPerDay_Cal, y = AQI.Cal, col ="Cal"))+
  labs(x="Demand", y = "AQI")

ggplot(data =final)+
  geom_point(mapping=aes(x=Avg_DemandPerDay_Tex, y = NO2.Tex, col ="Tex"))+
  geom_point(mapping=aes(x=Avg_DemandPerDay_Cal, y = NO2.Cal, col ="Cal"))+
  labs(x="Demand", y = "NO2")

ggplot(data=new)+
  geom_point(mapping=aes(x=AQI.Tex, y = Avg_DemandPerDay_Tex, col ="Tex"))+
  geom_point(mapping=aes(x=AQI.Cal, y = Avg_DemandPerDay_Cal, col ="Cal"))+
  labs(x="AQI", y = "Demand")

ggplot(data =final)+
  geom_point(mapping=aes(x=NO2.Tex, y = Avg_DemandPerDay_Tex, col ="Tex"))+
  geom_point(mapping=aes(x=NO2.Cal, y = Avg_DemandPerDay_Cal, col ="Cal"))+
  geom_smooth(mapping=aes(x=NO2.Tex, y = Avg_DemandPerDay_Tex, col ="Tex"))+
  geom_smooth(mapping=aes(x=NO2.Cal, y = Avg_DemandPerDay_Cal, col ="Cal"))+
  
  labs(x="NO2.level", y = "Demand")


```

#Build models for California
```{r}
library(modelr)
formula.1.Cal = Avg_DemandPerDay_Cal~AQI.Cal+poly(NO2.Cal,2)
formula.2.Cal = Avg_DemandPerDay_Cal~AQI.Cal+poly(NO2.Cal,3)
formula.3.Cal = Avg_DemandPerDay_Cal~AQI.Cal*poly(NO2.Cal,2)
formula.4.Cal = Avg_DemandPerDay_Cal~poly(AQI.Cal,2)*poly(NO2.Cal,3)
formula.5.Cal = Avg_DemandPerDay_Cal~poly(AQI.Cal,2)+poly(NO2.Cal,3)


lm(formula.1.Cal,data = final) ->model.ols.1.Cal
lm(formula.2.Cal,data = final) ->model.ols.2.Cal
lm(formula.3.Cal,data = final) ->model.ols.3.Cal
lm(formula.4.Cal,data = final) ->model.ols.4.Cal
lm(formula.5.Cal,data = final) ->model.ols.5.Cal


```

#Add predictions
```{r}
library(modelr)
final %>%
  add_predictions(model.ols.1.Cal,"pred.1.cal") ->final


final %>%
  add_predictions(model.ols.2.Cal,"pred.2.cal")->final


final %>%
  add_predictions(model.ols.3.Cal,"pred.3.cal")->final


final %>%
  add_predictions(model.ols.4.Cal,"pred.4.cal")->final


final %>%
  add_predictions(model.ols.5.Cal,"pred.5.cal")->final


  
```


#Train different models by cross validation
```{r}
set.seed(20010222)


final %>%
  #Create the cross-validation tibble
  crossv_kfold(k=10) %>%
  #Cross validate our models
  mutate(
    #Train the models on each fold
    cv.model.1.Cal = map(train, ~ lm(formula.1.Cal, data=.) ),
    cv.model.2.Cal = map(train, ~ lm(formula.2.Cal, data=.) ),
    cv.model.3.Cal = map(train, ~ lm(formula.3.Cal, data=.) ),
    cv.model.4.Cal = map(train, ~ lm(formula.4.Cal, data=.) ),
    cv.model.5.Cal = map(train, ~ lm(formula.5.Cal, data=.) ),
    
    #evaluate the model on the test sample
    cv.rmse.A = map2_dbl(cv.model.1.Cal,test,rmse),
    cv.rmse.B = map2_dbl(cv.model.2.Cal,test,rmse),
    cv.rmse.C = map2_dbl(cv.model.3.Cal,test,rmse),
    cv.rmse.D = map2_dbl(cv.model.4.Cal,test,rmse),
    cv.rmse.E = map2_dbl(cv.model.5.Cal,test,rmse),
  ) -> final.cv


#Compute the mean error for the models
final.cv %>%
  summarize(
    avg.loss.A = mean(cv.rmse.A),
    avg.loss.B = mean(cv.rmse.B),
    avg.loss.C = mean(cv.rmse.C),
    avg.loss.D = mean(cv.rmse.D),
    avg.loss.E = mean(cv.rmse.E)
  ) 



```

#plot the model prediction
```{r}
library(tidyverse)
library(ggplot2)
ggplot(data=final,mapping=aes(x = AQI.Cal))+
  geom_smooth(
    mapping=aes(y=Avg_DemandPerDay_Cal),
    color="black"
  ) +
  geom_smooth(
    mapping=aes(y=pred.1.cal),
    color="orange",
  )

ggplot(data=final,mapping=aes(x = AQI.Cal))+
  geom_smooth(
    mapping=aes(y=Avg_DemandPerDay_Cal),
    color="black"
  ) +
  geom_smooth(
    mapping=aes(y=pred.2.cal),
    color="orange",
  )

ggplot(data=final,mapping=aes(x = AQI.Cal))+
  geom_smooth(
    mapping=aes(y=Avg_DemandPerDay_Cal),
    color="black"
  ) +
  geom_smooth(
    mapping=aes(y=pred.3.cal),
    color="orange",
  )

##4
ggplot(data=final,mapping=aes(x = AQI.Cal))+
  geom_smooth(
    mapping=aes(y=Avg_DemandPerDay_Cal),
    color="black"
  ) +
  geom_smooth(
    mapping=aes(y=pred.4.cal),
    color="red",
  )

ggplot(data=final,mapping=aes(x = NO2.Cal))+
  geom_smooth(
    mapping=aes(y=Avg_DemandPerDay_Cal),
    color="black"
  ) +
  geom_smooth(
    mapping=aes(y=pred.4.cal),
    color="red",
  )

##5
ggplot(data=final,mapping=aes(x = AQI.Cal))+
  geom_smooth(
    mapping=aes(y=Avg_DemandPerDay_Cal),
    color="black"
  ) +
  geom_smooth(
    mapping=aes(y=pred.5.cal),
    color="blue",
  )

ggplot(data=final,mapping=aes(x = NO2.Cal))+
  geom_smooth(
    mapping=aes(y=Avg_DemandPerDay_Cal),
    color="black"
  ) +
  geom_smooth(
    mapping=aes(y=pred.5.cal),
    color="blue",
  )

```
Choose model5




#Build models for Texas
```{r}
library(modelr)
formula.1.Tex = Avg_DemandPerDay_Tex~AQI.Tex+cos(NO2.Tex)
formula.2.Tex = Avg_DemandPerDay_Tex~AQI.Tex*cos(NO2.Tex)
formula.3.Tex = Avg_DemandPerDay_Tex~AQI.Tex*poly(NO2.Tex,4)
formula.4.Tex = Avg_DemandPerDay_Tex~poly(NO2.Tex,4)
formula.5.Tex = Avg_DemandPerDay_Tex~AQI.Tex*(1/NO2.Tex)


lm(formula.1.Tex,data = final) ->model.ols.1.Tex
lm(formula.2.Tex,data = final) ->model.ols.2.Tex
lm(formula.3.Tex,data = final) ->model.ols.3.Tex
lm(formula.4.Tex,data = final) ->model.ols.4.Tex
lm(formula.5.Tex,data = final) ->model.ols.5.Tex
```

#Add prediction
```{r}
final %>%
  add_predictions(model.ols.1.Tex,"pred.1.tex") ->final


final %>%
  add_predictions(model.ols.2.Tex,"pred.2.tex")->final

final %>%
  add_predictions(model.ols.3.Tex,"pred.3.tex")->final

final %>%
  add_predictions(model.ols.4.Tex,"pred.4.tex")->final

final %>%
  add_predictions(model.ols.5.Tex,"pred.5.tex")->final

```


# train models
```{r}
set.seed(20010222)


final %>%
  #Create the cross-validation tibble
  crossv_kfold(k=10) %>%
  #Cross validate our models
  mutate(
    cv.model.1.Tex = map(train, ~ lm(formula.1.Tex, data=.) ),
    cv.model.2.Tex = map(train, ~ lm(formula.2.Tex, data=.) ),
    cv.model.3.Tex = map(train, ~ lm(formula.3.Tex, data=.) ),
    cv.model.4.Tex = map(train, ~ lm(formula.4.Tex, data=.) ),
    cv.model.5.Tex = map(train, ~ lm(formula.5.Tex, data=.) ),
    cv.rmse.1 = map2_dbl(cv.model.1.Tex,test,rmse),
    cv.rmse.2 = map2_dbl(cv.model.2.Tex,test,rmse),
    cv.rmse.3 = map2_dbl(cv.model.3.Tex,test,rmse),
    cv.rmse.4 = map2_dbl(cv.model.4.Tex,test,rmse),
    cv.rmse.5 = map2_dbl(cv.model.5.Tex,test,rmse),
  ) -> final.cv


#Compute the mean error for the models
final.cv %>%
  summarize(
    avg.loss.1 = mean(cv.rmse.1),
    avg.loss.2 = mean(cv.rmse.2),
    avg.loss.3 = mean(cv.rmse.3),
    avg.loss.4 = mean(cv.rmse.4),
    avg.loss.5 = mean(cv.rmse.5)
  ) 



```

# plot predictions
```{r}
library(ggplot2)
ggplot(data=final,mapping=aes(x =NO2.Tex))+
  geom_smooth(
    mapping=aes(y=Avg_DemandPerDay_Tex),
    color="black"
  ) +
  geom_smooth(
    mapping=aes(y=pred.1.tex),
    color="orange",
  )

ggplot(data=final,mapping=aes(x = NO2.Tex))+
  geom_smooth(
    mapping=aes(y=Avg_DemandPerDay_Tex),
    color="black"
  ) +
  geom_smooth(
    mapping=aes(y=pred.2.tex),
    color="orange",
  )


##3
ggplot(data=final,mapping=aes(x = NO2.Tex))+
  geom_smooth(
    mapping=aes(y=Avg_DemandPerDay_Tex),
    color="black"
  ) +
  geom_smooth(
    mapping=aes(y=pred.3.tex),
    color="orange",
  )

ggplot(data=final,mapping=aes(x = AQI.Tex))+
  geom_smooth(
    mapping=aes(y=Avg_DemandPerDay_Tex),
    color="blue"
  ) +
  geom_smooth(
    mapping=aes(y=pred.3.tex),
    color="orange",
  )


##4
ggplot(data=final,mapping=aes(x = NO2.Tex))+
  geom_smooth(
    mapping=aes(y=Avg_DemandPerDay_Tex),
    color="black"
  ) +
  geom_smooth(
    mapping=aes(y=pred.4.tex),
    color="orange",
  )

ggplot(data=final,mapping=aes(x = AQI.Tex))+
  geom_smooth(
    mapping=aes(y=Avg_DemandPerDay_Tex),
    color="blue"
  ) +
  geom_smooth(
    mapping=aes(y=pred.4.tex),
    color="orange",
  )



## 5
ggplot(data=final,mapping=aes(x = NO2.Tex))+
  geom_smooth(
    mapping=aes(y=Avg_DemandPerDay_Tex),
    color="black"
  ) +
  geom_smooth(
    mapping=aes(y=pred.5.tex),
    color="orange",
  )


```
