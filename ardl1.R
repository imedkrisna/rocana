setwd('C:/github/rocana')
library(tidyverse)
library(readxl)
library(dynlm)
library(ARDL)
library(tseries)
library(vars)
library("sjPlot")
library(modelsummary)

dat<-read_excel('plan.xlsx',sheet='SEKI')

## Growth
datg2<-dat|>dplyr::select(gmr,chn,usa,covid)|>drop_na()
datg3<-dat|>dplyr::select(gmr,bi,fed,rate,gxr,covid)|>drop_na()
datg4<-dat|>dplyr::select(gmr,ga,gam,covid)|>drop_na()
datg5<-dat|>dplyr::select(gmr,gx,gm,covid)|>drop_na()

g1<-ardl(data=datg2,gmr~usa+chn+covid,order=c(1,0,0,0))
g2<-ardl(data=datg3,gmr~bi+fed+gxr+covid,order=c(1,0,0,0,0))
g3<-ardl(data=datg4,gmr~ga+gam+covid,order=c(1,0,0,0))
g4<-ardl(data=datg5,gmr~gx+gm+covid,order=c(1,0,0,0))

ggg<-list(
  "US China Growth"=g1,
  "monetary policy"=g2,
  "Fiscal policy"=g3,
  "Trade policy"=g4
)

modelsummary(ggg,stars=TRUE,output="reg/growthmodel.xlsx")

## Level

dat2<-dat|>dplyr::select(pdbmr,gmr,chn,usa,covid)|>drop_na()
dat3<-dat|>dplyr::select(pdbmr,gmr,bi,fed,rate,xr,gxr,covid)|>drop_na()
dat4<-dat|>dplyr::select(pdbmr,gmr,ga,apbn,apbnm,gam,covid)|>drop_na()
dat5<-dat|>dplyr::select(pdbmr,gmr,exp,imp,imp1,imp2,gx,gm,covid)|>drop_na()

dat2$lpdbmr<-log(dat2$pdbmr)
dat3$lpdbmr<-log(dat3$pdbmr)
dat3$lxr<-log(dat3$xr)
dat2$tren<-seq(1,13,1) ## Switch to 14 if not using gmr
dat4$lapbn<-log(dat4$apbn)
dat4$lapbnm<-log(dat4$apbnm)
dat4$lpdbmr<-log(dat4$pdbmr)
dat5$lx<-log(dat5$exp)
dat5$lm<-log(dat5$imp)

l1<-ardl(data=dat2,lpdbmr~usa+chn+covid,order=c(1,0,0,0))
l2<-ardl(data=dat3,lpdbmr~bi+fed+lxr+covid,order=c(1,0,0,0,0))
l3<-ardl(data=dat4,lpdbmr~lapbn+lapbnm+covid,order=c(1,0,0,0))
l4<-ardl(data=dat5,gmr~lx+lm+covid,order=c(1,0,0,0))

lll<-list(
  "US China Growth"=l1,
  "monetary policy"=l2,
  "Fiscal policy"=l3,
  "Trade policy"=l4
)

modelsummary(lll,stars=TRUE,output="reg/levelmodel.xlsx")

## Growth


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
