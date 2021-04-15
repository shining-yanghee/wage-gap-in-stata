
library(data.table)
library(readxl)
library(dplyr)
library(plm)
library(pgmm)
library(tibble)
library(tidyr)
library(pracma)
library(car)
library(pastecs)
library(stargazer)
library(remotes)
library(xts)
library(zoo)
library(lmtest)
library(gmodels)
library(vcd)
library(knitr)
library(rmarkdown)
library(Hmisc)
library(sandwich)
library(lmtest)
library(multiwayvcov)
library(texreg)
library(nlme)
library(aod)
library(xtable)
library(car)

d <- read_excel("C:/Users/Yang hee Shin/Desktop/wage data/extract_r.xlsx", sheet = "data")

nrow(d)
View(d)
summary(d)

#calculating average wage disparity for the first five years
d1 <- d %>% group_by("id") %>% filter(staff>0) %>% filter(Employees>0) %>% filter(exe_comp>=0) %>% 
  filter(exe_nr>0) %>% filter(time>"1993-12-31") %>% filter(time<"1999-03-31")
temp<-as.matrix(d1[,])
temp[is.na(temp)]<-0
d1$avg_staff <- d1$staff*1000000 / d1$Employees*1000
d1$avg_exe <- d1$exe_comp*1000 / d1$exe_nr
d1$ratio <- log10(d1$avg_exe / d1$avg_staff)
mean(d1$ratio) #-4.657: overall mean of wage disparity
D1 <- d1 %>% group_by(id) %>% group_by(N = n(), add = TRUE) %>% 
  summarise(mean=mean(ratio),na.rm=TRUE) %>% mutate(n = N - n())
r_mean <- mean(D1$mean)
r_mean #-4.72: within mean of average wage disparity


d2 <- d %>% group_by("id") %>% na.omit(time) %>% filter(time>"1993-12-31") %>% filter(time<"2018-03-31") %>%
  filter(staff>0) %>% filter(Employees>0) %>% filter(exe_comp>=0) %>% filter(exe_nr>0)

temp<-as.matrix(d2[,])
temp[is.na(temp)]<-0

pd <- pdata.frame(d2, index=c("id","time"))
pdim(pd)

pd$N <- seq(dim(pd)[1])
pd$N_t <- dim(pd)[1]
pd <- pd[order(pd$id),]
idgroup <- tapply(pd$id, pd$id, 
                  function(x) seq(1,length(x),1))
pd$idgroup <- unlist(idgroup)

View(pd)
summary(pd)
stat.desc(pd)

##depending on business character, there is a firm that doesn't report operating income, whose OI is substibuted by its net income.
##because in case of deleting missings, the time order is messed
pd$OIBD <- ifelse(is.na(pd$OIBD)==TRUE, pd$NI, pd$OIBD)
#negative values with logarithm are not possible to analyze -> adding its minimum value
#and a reference point is moved but distribution is the same
pd$equity_p <- pd$equity + 20000
pd$OIBD_p <- pd$OIBD + 20000
pd$Revenue_p <- pd$Revenue + 20000
pd$assets_p <- pd$assets + 20000
pd$cash_p <-  pd$cash + 20000
pd$debt_current_p <-  pd$debt_current + 20000
pd$debt_long_p <-  pd$debt_long + 20000
pd$OIBD_p <-  pd$OIBD + 20000
pd$PPE_p <-  pd$PPE + 20000
pd$CapEx_p <-  pd$CapEx + 20000
pd$Pension_p <-  pd$Pension + 20000

#Price level deflated and natural logarithm taken
pd$debt_sum <- pd$debt_current_p + pd$debt_long_p
pd$cpi_q <- pd$cpi_q*0.01
pd$assets_n <-log10(pd$assets_p/(1+pd$cpi_q))
pd$equity_n <- log10(pd$equity_p/(1+pd$cpi_q))
pd$cash_n <- log10(pd$cash_p/(1+pd$cpi_q))
pd$debt_current_n <- log10(pd$debt_current_p/(1+pd$cpi_q))
pd$debt_long_n <- log10(pd$debt_long_p/(1+pd$cpi_q))
pd$debt_sum_n <- log10(pd$debt_sum/(1+pd$cpi_q))
pd$OIBD_n <- log10(pd$OIBD_p/(1+pd$cpi_q))
pd$PPE_n <- log10(pd$PPE_p/(1+pd$cpi_q))
pd$Revenue_n <- log10(pd$Revenue_p/(1+pd$cpi_q))
pd$CapEx_n <- log10(pd$CapEx_p/(1+pd$cpi_q))
pd$staff_n <- log10(pd$staff/(1+pd$cpi_q))
pd$exe_comp_n <- log10(pd$exe_comp/(1+pd$cpi_q))
pd$Pension_n <- log10(pd$Pension_p/(1+pd$cpi_q))


#Peridic items of earnings and expenses are 4 prior quarters moving averaged.
#for NI, OIBD, Revenue,  staff, exe_comp, Pension
pd$OIBD_m <- movavg(pd$OIBD_n, 4, type=c("s"))
pd$Revenue_m <- movavg(pd$Revenue_n, 4, type=c("s"))
pd$staff_m <- movavg(pd$staff_n, 4, type=c("s"))
pd$exe_m <- movavg(pd$exe_comp_n, 4, type=c("s"))
pd$Pension_m <- movavg(pd$Pension_n, 4, type=c("s"))
pd$CapEx_m <- movavg(pd$CapEx_n, 4, type=c("s"))
pd$OIBD_m2 <- movavg(pd$OIBD_n, 8, type=c("s"))
#for 2 year long-term performance


#calculate wage disparity and financial ratios
#logarithmic division is subtraction
pd$avg_staff <- (pd$staff_m + log10(1000000)) - (log10(pd$Employees) + log10(1000))
pd$avg_exe <- (pd$exe_m + log10(1000)) - log10(pd$exe_nr)
pd$ratio <- pd$avg_exe - pd$avg_staff
pd$acid_r <- pd$cash_n - pd$debt_current_n
pd$capex_r <- pd$CapEx_m - pd$assets_n
pd$DtoE <- pd$debt_sum_n - pd$equity_n
pd$labor_r <-  pd$staff_m - pd$Revenue_m 
pd$pension_r <- pd$Pension_m - pd$Revenue_m
pd$PPE_r <-  pd$PPE_n - pd$assets_n
pd$parity <- (pd$exe_m + log10(1000)) - (pd$staff_m + log10(1000000))
pd$OPM <- pd$OIBD_m - pd$Revenue_m
pd$OP_perse <- (pd$OIBD_m + log10(1000000)) - (log10(pd$Employees) + log10(1000))
pd$sq_r <- pd$ratio + pd$ratio
pd$exe_r <- pd$exe_m - pd$Revenue_m


#correlation metrix
C1 <- cbind(pd$assets_n, pd$equity_n, pd$cash_n, pd$debt_current_n, pd$debt_long_n, pd$debt_sum_n, pd$PPE_n, pd$Revenue_m, pd$CapEx_m, pd$staff_m, pd$exe_m, pd$Pension_m)
R1 <- cbind(pd$ratio, pd$sq_r, pd$parity,pd$avg_staff, pd$avg_exe, pd$acid_r, pd$capex_r, pd$DtoE, pd$labor_r, pd$pension_r, pd$PPE_r, pd$exe_r)
D10 <-cbind(pd$percept_e, pd$percept_f, pd$interact)
Y1 <- cbind(pd$OIBD_m, pd$OIBD_1q, pd$OIBD_m2, pd$OIBD_1y, pd$OPM, pd$OP_perse)
M1 <- cbind(pd$crises, pd$cpi_q, pd$unemp, pd$wage_gr, pd$lf_par, pd$r_gdp)
T1 <- cbind(C1, R1, Y1, M1)
C0 <- cbind(pd$OIBD_m, pd$OP_perse, pd$assets_n, pd$equity_n, pd$cash_n, pd$debt_sum_n, pd$PPE_n, pd$Revenue_m, pd$CapEx_m, pd$staff_m, pd$exe_m, pd$Pension_m, pd$ratio, pd$sq_r, pd$parity,pd$avg_staff, pd$avg_exe, pd$acid_r, pd$capex_r, pd$DtoE, pd$labor_r, pd$pension_r, pd$PPE_r, pd$exe_r, pd$Employees, pd$cpi_q, pd$unemp, pd$wage_gr, pd$lf_par, pd$r_gdp)

cor<-round(cor(C0),4)
cor
rcorr(C0, type = c("pearson","spearman"))
print(xtable(cor), type="html")
#serial correlation test from Baltagi and Li (1995)
pbsytest(OIBD_m ~ pd$OP_perse, pd$assets_n, pd$equity_n, pd$cash_n, pd$debt_sum_n, pd$PPE_n, pd$Revenue_m, pd$CapEx_m, pd$staff_m, pd$exe_m, pd$Pension_m, pd$ratio, pd$sq_r, pd$parity,pd$avg_staff, pd$avg_exe, pd$acid_r, pd$capex_r, pd$DtoE, pd$labor_r, pd$pension_r, pd$PPE_r, pd$exe_r, pd$Employees, pd$cpi_q, pd$unemp, pd$wage_gr, pd$lf_par, pd$r_gdp, data=pd, test="j")

CrossTable(pd$gics,digits=2)
CT <- CrossTable(pd$gics,pd$local,digits=2, expected=TRUE,dnn=c("GICS","LOCAL"))
chisq.test(pd$gics, pd$local)


#for 1qr lag and 1 yr lag (within)
pd <- pd %>% group_by(id) %>% mutate(OIBD_1q = dplyr::lag(OIBD_m, n = 1, default = last(OIBD_m)))
pd <- pd %>% group_by(id) %>% mutate(OIBD_1y = dplyr::lag(OIBD_m, n = 4, default = last(OIBD_m)))
pd <- pd %>% group_by(id) %>% mutate(avg_staff_g = dplyr::lag(avg_staff, n = 1, default = last(avg_staff)))
pd <- pd %>% group_by(id) %>% mutate(avg_exe_g = dplyr::lag(avg_exe, n = 1, default = last(avg_exe)))

#perceptions of earnings growth rates for comparisions among employees, firm, and executives
pd$ch_firm <- pd$OIBD_m - pd$OIBD_1q
pd$ch_staff <- pd$avg_staff - pd$avg_staff_g
pd$ch_exe_m <- pd$avg_exe - pd$avg_exe_g
pd$percept_f <- ifelse(pd$ch_firm < pd$ch_staff, 1, 0)
pd$percept_e<- ifelse(pd$ch_exe_m < pd$ch_staff, 1, 0)
pd$interact <- pd$percept_f * pd$percept_e
stat.desc(pd$interact)

#external crises dummies of IT bubble and financial crisis
pd$crises <- recode(pd$cal_yr, "2001:2002='1'; 2008:2010='2';1994:2000='0';2003:2007='0';2011:2017='0'")
#local & industry(gics) transformed into character
pd$local <- as.factor(pd$local)
pd$gics <- as.factor(pd$gics)

#calculate group meams for dividing two groups: the above average vs. the below average
M0 <- pd %>% group_by(id) %>% group_by(N = n(), add = TRUE) %>% 
      summarise_all(funs(mean)) %>% mutate(n = N - n())
View(M0)
summary(M0)
r_mean <- mean(M0$ratio)
r_mean # 1.43 vs. -4.62 for the first 5yr before screening.
stargazer(data.frame(M0), type = "html")
stargazer(data.frame(pd), type = "html")

pd<-right_join(pd,M0,by="id")
#b_ratio: binary group of the wage disparity
hist(pd$ratio.x) #overall variations
hist(pd$ratio.y, main=paste("Histogram of within variations in wage disparity (average)")) 
pd$b_ratio <- ifelse(pd$ratio.y > 1.43, 1, 0) # -4.62 is too small, so 1.43 for the overall period is taken
stat.desc(pd)

CrossTable(pd$b_ratio,digits=2)

temp<-as.matrix(pd[,])
temp[is.na(temp)]<-0

#plm function doesn't work due to unbalanced time points! 
#regressions are operated in 'lm' with clustering standard errors
#fixed effects models are calculated with demeaned values by hand for each explanatory variables
#DV: OIBD_m (4Q moving average), OIBD_1q (1Q lag), OIBD_2y (two year outcomes), OP_perse(per-capita), OPM

#generate demeaned variables
pd$ratio.f <- pd$ratio.x - pd$ratio.y
pd$parity.f <- pd$parity.x - pd$parity.y
pd$avg_staff.f <- pd$avg_staff.x - pd$avg_staff.y
pd$avg_exe.f <- pd$avg_exe.x - pd$avg_exe.y
pd$Employees.f <- pd$Employees.x - pd$Employees.y
pd$acid_r.f <- pd$acid_r.x -pd$acid_r.y
pd$capex_r.f <- pd$capex_r.x - pd$capex_r.y
pd$DtoE.f <- pd$DtoE.x - pd$DtoE.y
pd$labor_r.f <- pd$labor_r.x - pd$labor_r.y
pd$pension_r.f <- pd$pension_r.x - pd$pension_r.y
pd$PPE_r.f <- pd$PPE_r.x - pd$PPE_r.y
pd$assets_n.f <- pd$assets_n.x - pd$assets_n.y
pd$equity_n.f <- pd$equity_n.x - pd$equity_n.y
pd$staff_m.f <- pd$staff_m.x - pd$staff_m.y
pd$exe_m.f <- pd$exe_m.x - pd$exe_m.y
pd$exe_0.f <- pd$exe_0.x - pd$exe_0.y
pd$crises.f <- pd$crises.x - pd$crises.y
pd$percept_f.f <- pd$percept_f.x - pd$percept_f.y
pd$percept_e.f <- pd$percept_e.x - pd$percept_e.y
pd$interact.f <- pd$interact.x - pd$interact.y
pd$cpi_q.f <- pd$cpi_q.x - pd$cpi_q.y
pd$unemp.f <- pd$unemp.x - pd$unemp.y
pd$wage_gr.f <- pd$wage_gr.x - pd$wage_gr.y
pd$lf_par.f <- pd$lf_par.x - pd$lf_par.y
pd$r_gdp.f <- pd$r_gdp.x - pd$r_gdp.y

pols_empty <- lm(OIBD_m.x ~ ratio.x, data = pd)
pols_empty_s <- cluster.vcov(pols_empty, cbind(pd$id, pd$time.x))
coeftest(pols_empty, pols_empty_s)
summary(pols_empty, cluster=c("id","time.x"))

pols_basic <- lm(OIBD_m.x ~ ratio.x+parity.x+avg_staff.x+avg_exe.x+Employees.x+acid_r.x+capex_r.x+DtoE.x+labor_r.x+pension_r.x+PPE_r.x+assets_n.x+equity_n.x+staff_m.x+exe_m.x+exe_0.x+as.factor(crises.x)+as.factor(gics.x)+as.factor(local.x)+as.factor(percept_f.x)+as.factor(percept_e.x)+as.factor(interact.x)+cpi_q.x+unemp.x+wage_gr.x+lf_par.x+r_gdp.x, data = pd)
pols_basic_s <- cluster.vcov(pols_basic, cbind(pd$id, pd$time.x))
coeftest(pols_basic, pols_basic_s)
summary(pols_basic, cluster=c("id","time.x"))

hybrid <- lm((OIBD_m.x-OIBD_1q.x) ~ ratio.f+parity.f+avg_staff.f+avg_exe.f+Employees.f+acid_r.f+capex_r.f+DtoE.f+labor_r.f+pension_r.f+PPE_r.f+assets_n.x+equity_n.x+staff_m.x+exe_m.x+exe_0.x+as.factor(crises.x)+as.factor(gics.x)+as.factor(local.x) + percept_f.f + percept_e.f + interact.f + cpi_q.f + unemp.f + wage_gr.f + lf_par.f + r_gdp.f, data = pd)
hybrid_s <- cluster.vcov(hybrid, cbind(pd$id, pd$time.x))
coeftest(hybrid, hybrid_s)
summary(hybrid, cluster=c("id","time.x"))

fe_basic <- lm((OIBD_m.x-OIBD_1q.x) ~ ratio.f + parity.f + avg_staff.f + avg_exe.f + Employees.f + acid_r.f + capex_r.f + DtoE.f + labor_r.f + pension_r.f + PPE_r.f + assets_n.f + equity_n.f + staff_m.f + exe_m.f + crises.f + percept_f.f + percept_e.f + interact.f + cpi_q.f + unemp.f + wage_gr.f + lf_par.f + r_gdp.f, data = pd)
fe_basic_s <- cluster.vcov(fe_basic, cbind(pd$id, pd$time.x))
coeftest(fe_basic, fe_basic_s)
summary(fe_basic, cluster=c("id","time.x"))
pFtest(pols_basic, fe_basic)

#the above wage disparity group (high) vs. the below wage disparity (low)
fe_high <- lm((OIBD_m.x-OIBD_1q.x) ~ ratio.f + parity.f + avg_staff.f + avg_exe.f + Employees.f + acid_r.f + capex_r.f + DtoE.f + labor_r.f + pension_r.f + PPE_r.f + assets_n.f + equity_n.f + staff_m.f + exe_m.f + crises.f + percept_f.f + percept_e.f + interact.f + cpi_q.f + unemp.f + wage_gr.f + lf_par.f + r_gdp.f, data = pd, subset=(b_ratio==1))
summary(fe_high, cluster=c("id","time.x"))
fe_low <- lm((OIBD_m.x-OIBD_1q.x) ~ ratio.f + parity.f + avg_staff.f + avg_exe.f + Employees.f + acid_r.f + capex_r.f + DtoE.f + labor_r.f + pension_r.f + PPE_r.f + assets_n.f + equity_n.f + staff_m.f + exe_m.f + crises.f + percept_f.f + percept_e.f + interact.f + cpi_q.f + unemp.f + wage_gr.f + lf_par.f + r_gdp.f, data = pd, subset=(b_ratio==0))
summary(fe_low, cluster=c("id","time.x"))


pd <- pd %>% group_by(id) %>% mutate(OIBD_1q.z = dplyr::lag(OIBD_1q.x, n = 1, default = last(OIBD_1q.x)))
pd$OIBD_1q.f <-pd$OIBD_1q.x -pd$OIBD_1q.z
#fixed effects with 1 quarter lag
fe_lag <- lm(OIBD_1q.f ~ ratio.f + parity.f + avg_staff.f + avg_exe.f + Employees.f + acid_r.f + capex_r.f + DtoE.f + labor_r.f + pension_r.f + PPE_r.f + assets_n.f + equity_n.f + staff_m.f + exe_m.f + crises.f + percept_f.f + percept_e.f +interact.f + cpi_q.f + unemp.f + wage_gr.f + lf_par.f + r_gdp.f, data = pd)
fe_lag_s <- cluster.vcov(fe_lag, cbind(pd$id, pd$time.x))
coeftest(fe_lag, fe_lag_s)
summary(fe_lag, cluster=c("id","time.x"))

pd <- pd %>% group_by(id) %>% mutate(OIBD_m2.z = dplyr::lag(OIBD_m2.x, n = 1, default = last(OIBD_m2.x)))
pd$OIBD_m2.f <-pd$OIBD_m2.x -pd$OIBD_m2.z
#fixed effects with 2 year moving average DV
fe_2y <- lm(OIBD_m2.f ~ ratio.f + parity.f + avg_staff.f + avg_exe.f + Employees.f + acid_r.f + capex_r.f + DtoE.f + labor_r.f + pension_r.f + PPE_r.f + assets_n.f + equity_n.f + staff_m.f + exe_m.f + crises.f + percept_f.f + percept_e.f +interact.f + cpi_q.f + unemp.f + wage_gr.f + lf_par.f + r_gdp.f, data = pd)
fe_2y_s <- cluster.vcov(fe_2y, cbind(pd$id, pd$time.x))
coeftest(fe_2y, fe_2y_s)
summary(fe_2y, cluster=c("id","time.x"))


pd <- pd %>% group_by(id) %>% mutate(OPM.z = dplyr::lag(OPM.x, n = 1, default = last(OPM.x)))
pd$OPM.f <-pd$OPM.x -pd$OPM.z
#operating income margin
fe_opm <- lm(OPM.f ~ ratio.f + parity.f + avg_staff.f + avg_exe.f + Employees.f + acid_r.f + capex_r.f + DtoE.f + labor_r.f + pension_r.f + PPE_r.f + assets_n.f + equity_n.f + staff_m.f + exe_m.f + crises.f + percept_f.f + percept_e.f +interact.f + cpi_q.f + unemp.f + wage_gr.f + lf_par.f + r_gdp.f, data = pd)
fe_opm_s <- cluster.vcov(fe_opm, cbind(pd$id, pd$time.x))
coeftest(fe_opm, fe_opm_s)
summary(fe_opm, cluster=c("id","time.x"))

pd <- pd %>% group_by(id) %>% mutate(OP_perse.z = dplyr::lag(OP_perse.x, n = 1, default = last(OPM.x)))
pd$OP_perse.f <-pd$OP_perse.x -pd$OP_perse.z
#operating income per se
fe_ops <- lm(OP_perse.f ~ ratio.f + parity.f + avg_staff.f + avg_exe.f + Employees.f + acid_r.f + capex_r.f + DtoE.f + labor_r.f + pension_r.f + PPE_r.f + assets_n.f + equity_n.f + staff_m.f + exe_m.f + crises.f + percept_f.f + percept_e.f +interact.f + cpi_q.f + unemp.f + wage_gr.f + lf_par.f + r_gdp.f, data = pd)
fe_ops_s <- cluster.vcov(fe_ops, cbind(pd$id, pd$time.x))
coeftest(fe_ops, fe_ops_s)
summary(fe_ops, cluster=c("id","time.x"))

#export
stargazer(list(pols_empty,pols_basic,hybrid,fe_basic,fe_high,fe_low,fe_lag,fe_2y,fe_opm,fe_ops), type = "html", no.space = TRUE, single.row = TRUE)
stargazer(list(hybrid,fe_basic,fe_high,fe_low,fe_lag,fe_2y,fe_opm,fe_ops), type = "html", no.space = TRUE, single.row = TRUE)

#graph
plot(pd$gics.x, pd$assets_n.x, main="Distribution of assets by industry", 
     xlab="GICS ", ylab="Assets ", pch=19)
plot(pd$time.x,pd$OIBD_m.x, main="Operating income trends by wage disparity",
     ylab="Operating income with natural logarithm ", pch=1)

ggplot(pd, aes(x = time.x, y = OIBD_m.x)) +
  geom_point(aes(colour = ratio.x)) +
  geom_smooth(method = "lm") +
  labs(title = "Operating income trends by wage disparity") +
  labs(x="time(1Q1994-4Q2017)") +
  labs(y="operating income by firm with natural logarithm")
  
