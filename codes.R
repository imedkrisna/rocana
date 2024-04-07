setwd('C:/github/rocana')

library(WDI)
library(tidyverse)
library(scales)
library(readxl)

## Global manufacturing exports

indi<-c(            # membuat dictionary
  "manexp"="TX.VAL.MANF.ZS.UN",
  "manex"="TX.VAL.MRCH.CD.WT",
  "gdp"="NY.GDP.MKTP.CD"
)

dat<-WDI(           # Menarik data World Bank
  country=c("WLD","IDN","CHN","VNM","MYS","THA","IND"),
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
dat3$mx<-dat3$manx/dat3$manxw*100

dat |>
  filter(country!="World" & country!="China") |>
  ggplot(aes(x=year,y=manx,color=country))+geom_line(linewidth=1.1)+
  scale_y_continuous(labels = label_number())+theme_classic()

dat3 |>
  filter(country!="World"& country!="China") |>
  ggplot(aes(x=year,y=mx,color=country))+geom_line(linewidth=1.1)+
  labs(x="",y="% world manufactures exports",
       caption="sumber: World Development Indicators")+theme_classic()

ggsave("fig/exports.png")

read_excel("plan.xlsx",sheet="SEKI") |>
  select(tahun,fed,bi,eu,uk) |>
  pivot_longer(!tahun,names_to = "country",values_to = "rate") |>
  ggplot(aes(x=tahun,y=rate,color=country))+geom_line(linewidth=1.1)+
  scale_y_continuous(limits=c(0,8))+scale_x_continuous(breaks=seq(2010,2023,2))+
  scale_color_discrete(labels=c("IDN","USA","UK","EU"))+
  labs(x="",y="%",
       caption="sumber: SEKI")+theme_classic()
ggsave("fig/fedrate.png")

read_excel("plan.xlsx",sheet="skewed") |>
  pivot_longer(!sektor,names_to = "year",values_to="value") |>
  ggplot(aes(x=year,y=value,group=sektor,color=sektor))+geom_line(linewidth=1.1)+
  labs(x="",y="growth (%)",
       caption="sumber: BPS")+theme_classic()+theme(legend.position = "bottom")+
  guides(color=guide_legend(nrow=4, byrow=TRUE))
ggsave("fig/skewed.png",width=8,height=6)