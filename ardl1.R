setwd('C:/github/rocana')
library(tidyverse)
library(readxl)
library(dynlm)
library(ARDL)
library(tseries)
library(vars)
library("sjPlot")

dat<-read_excel('plan.xlsx',sheet='SEKI')



dat2<-dat|>dplyr::select(pdbmr,gmr,chn,usa,covid)|>drop_na()
dat3<-dat|>dplyr::select(pdbmr,gmr,bi,fed,rate,xr,gxr,covid)|>drop_na()
dat4<-dat|>dplyr::select(pdbmr,gmr,ga,apbn,apbnm,gam,covid)|>drop_na()

dat2$lpdbmr<-log(dat2$pdbmr)
dat3$lpdbmr<-log(dat3$pdbmr)
dat3$lxr<-log(dat3$xr)
dat2$tren<-seq(1,13,1) ## Switch to 14 if not using gmr
dat4$lapbn<-log(dat4$apbn)
dat4$lapbnm<-log(dat4$apbnm)
dat4$lpdbmr<-log(dat4$pdbmr)


lm(data=dat2,lpdbmr~usa+chn+tren)|>summary()
lm(data=dat2,gmr~usa+chn+tren)|>summary()
auto_ardl(data=dat2,gmr~usa+chn,max_order=2)
ardl(data=dat2,gmr~usa+chn,order=c(1,0,0))|>summary()
growth<-ardl(data=dat2,gmr~usa+chn,order=c(1,0,0))
bounds_f_test(growth,case=2)
tab_model(growth,file="reg/growth.html")

auto_ardl(data=dat3,gmr~xr,max_order=2)
ardl(data=dat3,gmr~xr,order=c(1,1))|>summary()

auto_ardl(data=dat3,gmr~rate,max_order=2)
ardl(data=dat3,gmr~rate+gxr,c(1,0,0))|>summary()
ardl(data=dat3,gmr~bi+fed+gxr,order=c(1,0,0,0))|>summary()

auto_ardl(gmr~lapbn+lapbnm,max_order = 2,data=dat4)
ardl(gmr~ga+gam,order = c(1,0,0),data=dat4)|>summary()
