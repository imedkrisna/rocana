library(tidyverse)
library(lubridate)
library(writexl)
library(gganimate)

setwd('C:/github/rocana')

#### FOOD MANUFACTURING

i2003<-read_csv("data/2003.csv") |> select(`...1`,IDN_C10T12)|> 
  arrange(desc(IDN_C10T12))|>rename(val=IDN_C10T12)

gabung1<-i2003 |> mutate(across(where(is.numeric), ~./.[1]*100)) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=case_when(stringr::str_detect(`...1`, "IDN")~"domestic",
                         `...1`=="VA"~"VA",
                         `...1`=="OUT"~"OUT",
                         TRUE~"foreign"))
idn1<-gabung2|> group_by(group) |> summarise(val=sum(val))
idn1$year<-2003
idn1$ctr<-"IDN"

i2003<-read_csv("data/2003.csv") |> select(`...1`,THA_C10T12)|>
  arrange(desc(THA_C10T12)) |> rename(val=THA_C10T12)

gabung1<-i2003 |> mutate(across(where(is.numeric), ~./.[1]*100)) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=case_when(stringr::str_detect(`...1`, "THA")~"domestic",
                         `...1`=="VA"~"VA",
                         `...1`=="OUT"~"OUT",
                         TRUE~"foreign"))
tha1<-gabung2|> group_by(group) |> summarise(val=sum(val))
tha1$year<-2003
tha1$ctr<-"THA"

i2003<-read_csv("data/2003.csv") |> select(`...1`,VNM_C10T12)|> 
  arrange(desc(VNM_C10T12)) |> rename(val=VNM_C10T12)

gabung1<-i2003 |> mutate(across(where(is.numeric), ~./.[1]*100)) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=case_when(stringr::str_detect(`...1`, "VNM")~"domestic",
                         `...1`=="VA"~"VA",
                         `...1`=="OUT"~"OUT",
                         TRUE~"foreign"))
vnm1<-gabung2|> group_by(group) |> summarise(val=sum(val))
vnm1$year<-2003
vnm1$ctr<-"VNM"

i2003<-read_csv("data/2003.csv") |> select(`...1`,MYS_C10T12)|> 
  arrange(desc(MYS_C10T12))|>rename(val=MYS_C10T12)

gabung1<-i2003 |> mutate(across(where(is.numeric), ~./.[1]*100)) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=case_when(stringr::str_detect(`...1`, "MYS")~"domestic",
                         `...1`=="VA"~"VA",
                         `...1`=="OUT"~"OUT",
                         TRUE~"foreign"))
mys1<-gabung2|> group_by(group) |> summarise(val=sum(val))
mys1$year<-2003
mys1$ctr<-"MYS"

i2003<-read_csv("data/2003.csv") |> select(`...1`,SGP_C10T12)|>
  arrange(desc(SGP_C10T12))|>rename(val=SGP_C10T12)

gabung1<-i2003 |> mutate(across(where(is.numeric), ~./.[1]*100)) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=case_when(stringr::str_detect(`...1`, "SGP")~"domestic",
                         `...1`=="VA"~"VA",
                         `...1`=="OUT"~"OUT",
                         TRUE~"foreign"))
sgp1<-gabung2|> group_by(group) |> summarise(val=sum(val))
sgp1$year<-2003
sgp1$ctr<-"SGP"

wew<-rbind(idn1,vnm1,tha1,sgp1,mys1)

## 2008

i2008<-read_csv("data/2008.csv") |> select(`...1`,IDN_C10T12)|> 
  arrange(desc(IDN_C10T12))|>rename(val=IDN_C10T12)

gabung1<-i2008 |> mutate(across(where(is.numeric), ~./.[1]*100)) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=case_when(stringr::str_detect(`...1`, "IDN")~"domestic",
                         `...1`=="VA"~"VA",
                         `...1`=="OUT"~"OUT",
                         TRUE~"foreign"))
idn1<-gabung2|> group_by(group) |> summarise(val=sum(val))
idn1$year<-2008
idn1$ctr<-"IDN"

i2008<-read_csv("data/2008.csv") |> select(`...1`,THA_C10T12)|>
  arrange(desc(THA_C10T12)) |> rename(val=THA_C10T12)

gabung1<-i2008 |> mutate(across(where(is.numeric), ~./.[1]*100)) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=case_when(stringr::str_detect(`...1`, "THA")~"domestic",
                         `...1`=="VA"~"VA",
                         `...1`=="OUT"~"OUT",
                         TRUE~"foreign"))
tha1<-gabung2|> group_by(group) |> summarise(val=sum(val))
tha1$year<-2008
tha1$ctr<-"THA"

i2008<-read_csv("data/2008.csv") |> select(`...1`,VNM_C10T12)|> 
  arrange(desc(VNM_C10T12)) |> rename(val=VNM_C10T12)

gabung1<-i2008 |> mutate(across(where(is.numeric), ~./.[1]*100)) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=case_when(stringr::str_detect(`...1`, "VNM")~"domestic",
                         `...1`=="VA"~"VA",
                         `...1`=="OUT"~"OUT",
                         TRUE~"foreign"))
vnm1<-gabung2|> group_by(group) |> summarise(val=sum(val))
vnm1$year<-2008
vnm1$ctr<-"VNM"

i2008<-read_csv("data/2008.csv") |> select(`...1`,MYS_C10T12)|> 
  arrange(desc(MYS_C10T12))|>rename(val=MYS_C10T12)

gabung1<-i2008 |> mutate(across(where(is.numeric), ~./.[1]*100)) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=case_when(stringr::str_detect(`...1`, "MYS")~"domestic",
                         `...1`=="VA"~"VA",
                         `...1`=="OUT"~"OUT",
                         TRUE~"foreign"))
mys1<-gabung2|> group_by(group) |> summarise(val=sum(val))
mys1$year<-2008
mys1$ctr<-"MYS"

i2008<-read_csv("data/2008.csv") |> select(`...1`,SGP_C10T12)|>
  arrange(desc(SGP_C10T12))|>rename(val=SGP_C10T12)

gabung1<-i2008 |> mutate(across(where(is.numeric), ~./.[1]*100)) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=case_when(stringr::str_detect(`...1`, "SGP")~"domestic",
                         `...1`=="VA"~"VA",
                         `...1`=="OUT"~"OUT",
                         TRUE~"foreign"))
sgp1<-gabung2|> group_by(group) |> summarise(val=sum(val))
sgp1$year<-2008
sgp1$ctr<-"SGP"

wew<-rbind(wew,idn1,vnm1,tha1,mys1,sgp1)

## 2013

i2013<-read_csv("data/2013.csv") |> select(`...1`,IDN_C10T12)|> 
  arrange(desc(IDN_C10T12))|>rename(val=IDN_C10T12)

gabung1<-i2013 |> mutate(across(where(is.numeric), ~./.[1]*100)) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=case_when(stringr::str_detect(`...1`, "IDN")~"domestic",
                         `...1`=="VA"~"VA",
                         `...1`=="OUT"~"OUT",
                         TRUE~"foreign"))
idn1<-gabung2|> group_by(group) |> summarise(val=sum(val))
idn1$year<-2013
idn1$ctr<-"IDN"

i2013<-read_csv("data/2013.csv") |> select(`...1`,THA_C10T12)|>
  arrange(desc(THA_C10T12)) |> rename(val=THA_C10T12)

gabung1<-i2013 |> mutate(across(where(is.numeric), ~./.[1]*100)) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=case_when(stringr::str_detect(`...1`, "THA")~"domestic",
                         `...1`=="VA"~"VA",
                         `...1`=="OUT"~"OUT",
                         TRUE~"foreign"))
tha1<-gabung2|> group_by(group) |> summarise(val=sum(val))
tha1$year<-2013
tha1$ctr<-"THA"

i2013<-read_csv("data/2013.csv") |> select(`...1`,VNM_C10T12)|> 
  arrange(desc(VNM_C10T12)) |> rename(val=VNM_C10T12)

gabung1<-i2013 |> mutate(across(where(is.numeric), ~./.[1]*100)) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=case_when(stringr::str_detect(`...1`, "VNM")~"domestic",
                         `...1`=="VA"~"VA",
                         `...1`=="OUT"~"OUT",
                         TRUE~"foreign"))
vnm1<-gabung2|> group_by(group) |> summarise(val=sum(val))
vnm1$year<-2013
vnm1$ctr<-"VNM"

i2013<-read_csv("data/2013.csv") |> select(`...1`,MYS_C10T12)|> 
  arrange(desc(MYS_C10T12))|>rename(val=MYS_C10T12)

gabung1<-i2013 |> mutate(across(where(is.numeric), ~./.[1]*100)) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=case_when(stringr::str_detect(`...1`, "MYS")~"domestic",
                         `...1`=="VA"~"VA",
                         `...1`=="OUT"~"OUT",
                         TRUE~"foreign"))
mys1<-gabung2|> group_by(group) |> summarise(val=sum(val))
mys1$year<-2013
mys1$ctr<-"MYS"

i2013<-read_csv("data/2013.csv") |> select(`...1`,SGP_C10T12)|>
  arrange(desc(SGP_C10T12))|>rename(val=SGP_C10T12)

gabung1<-i2013 |> mutate(across(where(is.numeric), ~./.[1]*100)) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=case_when(stringr::str_detect(`...1`, "SGP")~"domestic",
                         `...1`=="VA"~"VA",
                         `...1`=="OUT"~"OUT",
                         TRUE~"foreign"))
sgp1<-gabung2|> group_by(group) |> summarise(val=sum(val))
sgp1$year<-2013
sgp1$ctr<-"SGP"

wew<-rbind(wew,idn1,vnm1,tha1,sgp1,mys1)

## 2019

i2019<-read_csv("data/2019.csv") |> select(`...1`,IDN_C10T12)|> 
  arrange(desc(IDN_C10T12))|>rename(val=IDN_C10T12)

gabung1<-i2019 |> mutate(across(where(is.numeric), ~./.[1]*100)) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=case_when(stringr::str_detect(`...1`, "IDN")~"domestic",
                         `...1`=="VA"~"VA",
                         `...1`=="OUT"~"OUT",
                         TRUE~"foreign"))
idn1<-gabung2|> group_by(group) |> summarise(val=sum(val))
idn1$year<-2019
idn1$ctr<-"IDN"

i2019<-read_csv("data/2019.csv") |> select(`...1`,THA_C10T12)|>
  arrange(desc(THA_C10T12)) |> rename(val=THA_C10T12)

gabung1<-i2019 |> mutate(across(where(is.numeric), ~./.[1]*100)) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=case_when(stringr::str_detect(`...1`, "THA")~"domestic",
                         `...1`=="VA"~"VA",
                         `...1`=="OUT"~"OUT",
                         TRUE~"foreign"))
tha1<-gabung2|> group_by(group) |> summarise(val=sum(val))
tha1$year<-2019
tha1$ctr<-"THA"

i2019<-read_csv("data/2019.csv") |> select(`...1`,VNM_C10T12)|> 
  arrange(desc(VNM_C10T12)) |> rename(val=VNM_C10T12)

gabung1<-i2019 |> mutate(across(where(is.numeric), ~./.[1]*100)) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=case_when(stringr::str_detect(`...1`, "VNM")~"domestic",
                         `...1`=="VA"~"VA",
                         `...1`=="OUT"~"OUT",
                         TRUE~"foreign"))
vnm1<-gabung2|> group_by(group) |> summarise(val=sum(val))
vnm1$year<-2019
vnm1$ctr<-"VNM"

i2019<-read_csv("data/2019.csv") |> select(`...1`,MYS_C10T12)|> 
  arrange(desc(MYS_C10T12))|>rename(val=MYS_C10T12)

gabung1<-i2019 |> mutate(across(where(is.numeric), ~./.[1]*100)) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=case_when(stringr::str_detect(`...1`, "MYS")~"domestic",
                         `...1`=="VA"~"VA",
                         `...1`=="OUT"~"OUT",
                         TRUE~"foreign"))
mys1<-gabung2|> group_by(group) |> summarise(val=sum(val))
mys1$year<-2019
mys1$ctr<-"MYS"

i2019<-read_csv("data/2019.csv") |> select(`...1`,SGP_C10T12)|>
  arrange(desc(SGP_C10T12))|>rename(val=SGP_C10T12)

gabung1<-i2019 |> mutate(across(where(is.numeric), ~./.[1]*100)) ## Bikin persen (how to use . and ...)

gabung2<-gabung1 |>
  mutate(group=case_when(stringr::str_detect(`...1`, "SGP")~"domestic",
                         `...1`=="VA"~"VA",
                         `...1`=="OUT"~"OUT",
                         TRUE~"foreign"))
sgp1<-gabung2|> group_by(group) |> summarise(val=sum(val))
sgp1$year<-2019
sgp1$ctr<-"SGP"

wew<-rbind(wew,idn1,vnm1,tha1,sgp1,mys1)

wew|>pivot_wider(names_from = group,values_from = val)|>
  ggplot(aes(x=foreign,y=VA,color=ctr))+geom_point(aes(shape=as.factor(year)))+theme_classic()

zz<-wew|>pivot_wider(names_from = group,values_from = val)

p<-zz|>ggplot(aes(x=foreign,y=domestic,color=ctr))+geom_point()+geom_text(label=zz$ctr)+
  labs(title='Year: {closest_state}')

p<-p+transition_states(year)

animate(p, renderer = gifski_renderer("animation.gif"))

