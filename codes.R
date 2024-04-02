setwd('C:/github/rocana')

library(WDI)
library(tidyverse)

## Global manufacturing exports

indi<-c(            # membuat dictionary
  "manexp"="TX.VAL.MANF.ZS.UN",
  "manex"="TX.VAL.MRCH.CD.WT",
  "gdp"="NY.GDP.MKTP.CD"
)

dat<-WDI(           # Menarik data World Bank
  country=c("WLD","IDN","CHN","VNM","MYS","THA"),
  indicator=indi,
  start=2002,end=2022
)

dat2<-WDI(           # Menarik data World Bank
  country="WLD",
  indicator=indi,
  start=2002,end=2022
)

dat$manx<-dat$manex*dat$manexp/100
dat2$manxw<-dat2$manex*dat2$manexp/100

dat2<-dat2|>select(year,manxw)

dat3<-inner_join(dat,dat2,by="year")

dat |>
  filter(country!="World" & country!="China") |>
  ggplot(aes(x=year,y=manx,color=country))+geom_line(linewidth=1.1)+theme_classic()

