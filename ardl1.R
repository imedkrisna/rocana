setwd('C:/github/rocana')
library(tidyverse)
library(readxl)
library(dynlm)
library(ARDL)
library(tseries)
library(vars)

dat<-read_excel('plan.xlsx',sheet='SEKI')



dat2<-dat|>dplyr::select(pdbmr,chn,usa)|>drop_na()
dat3<-dat|>dplyr::select(pdbmr,bi,fed,rate,xr)|>drop_na()
dat4<-dat|>dplyr::select(pdbmr,apbn,apbnm)|>drop_na()

dat2$lpdbmr<-log(dat2$pdbmr)
dat3$lpdbmr<-log(dat3$pdbmr)
dat3$lxr<-log(dat3$xr)
dat2$tren<-seq(1,14,1)
dat4$lapbn<-log(dat4$apbn)
dat4$lapbnm<-log(dat4$apbnm)
dat4$lpdbmr<-log(dat4$pdbmr)


lm(data=dat2,lpdbmr~usa+chn+tren)|>summary()
auto_ardl(data=dat2,lpdbmr~usa+chn,max_order=2)
ardl(data=dat2,lpdbmr~usa+chn,order=c(1,2,0))|>summary()

auto_ardl(data=dat3,lpdbmr~lxr,max_order=2)
ardl(data=dat3,lpdbmr~lxr,order=c(1,0))|>summary()

auto_ardl(data=dat3,lpdbmr~rate,max_order=2)
ardl(data=dat3,lpdbmr~rate,c(1,0))|>summary()
ardl(data=dat3,lpdbmr~bi+fed,order=c(1,0,1))|>summary()

auto_ardl(lpdbmr~lapbn+lapbnm,max_order = 2,data=dat4)
ardl(lpdbmr~lapbn+lapbnm,order = c(1,1,1),data=dat4)|>summary()
