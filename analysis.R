library(tidyverse)
library(ggplot2)
library(reshape2)
library(Metrics)

# Reading in Brown data ---------------------------------------------------

brown.data <- read.table("Brown 2006 data.csv", header=TRUE, sep=",")

att <- brown.data$Attenuated
vir <- brown.data$Virulent

vir_log_normal <- log(vir[1:20]/sum(vir[1:20]))
att_log_normal <- log(att[1:20]/sum(att[1:20]))


# Reading in simulated data -----------------------------------------------

output_vir_stochastic <- read.table("output_vir_stochastic.txt",sep=' ') %>%
  rename(Time = V1,
         Bacteria = V2,
         Macrophage = V3,
         M1 = V4,
         M2 = V5,
         M3 = V6,
         M4 = V7,
         M5 = V8,
         M6 = V9,
         M7 = V10,
         M8 = V11,
         M9 = V12,
         M10 = V13,
         M11 = V14,
         M12 = V15,
         M13 = V16,
         M14 = V17,
         M15 = V18,
         M16 = V19,
         M17 = V20,
         M18 = V21,
         M19 = V22,
         M20 = V23,
         M21 = V24,
         M22 = V25,
         M23 = V26,
         M24 = V27,
         M25 = V28,
         M26 = V29,
         M27 = V30,
         M28 = V31,
         M29 = V32,
         M30 = V33) %>%
  rowwise() %>% 
  mutate(TotalBac = Bacteria + 1*M1 + 2*M2 + 3*M3 + 4*M4 + 5*M5 + 6*M6 + 7*M7 + 8*M8 + 9*M9 + 10*M10 + 
           11*M11 + 12*M12 + 13*M13 + 14*M14 + 15*M15 + 16*M16 + 17*M17 + 18*M18 + 19*M19 + 20*M20 +
           21*M21 + 22*M22 + 23*M23 + 24*M24 + 25*M25 + 26*M26 + 27*M27 + 28*M28 + 29*M29 + 30*M30)

output_att_stochastic <- read.table("output_att_stochastic.txt",sep=' ') %>%
  rename(Time = V1,
         Bacteria = V2,
         Macrophage = V3,
         M1 = V4,
         M2 = V5,
         M3 = V6,
         M4 = V7,
         M5 = V8,
         M6 = V9,
         M7 = V10,
         M8 = V11,
         M9 = V12,
         M10 = V13,
         M11 = V14,
         M12 = V15,
         M13 = V16,
         M14 = V17,
         M15 = V18,
         M16 = V19,
         M17 = V20,
         M18 = V21,
         M19 = V22,
         M20 = V23,
         M21 = V24,
         M22 = V25,
         M23 = V26,
         M24 = V27,
         M25 = V28,
         M26 = V29,
         M27 = V30,
         M28 = V31,
         M29 = V32,
         M30 = V33) %>%
  rowwise() %>% 
  mutate(TotalBac = Bacteria + 1*M1 + 2*M2 + 3*M3 + 4*M4 + 5*M5 + 6*M6 + 7*M7 + 8*M8 + 9*M9 + 10*M10 + 
           11*M11 + 12*M12 + 13*M13 + 14*M14 + 15*M15 + 16*M16 + 17*M17 + 18*M18 + 19*M19 + 20*M20 +
           21*M21 + 22*M22 + 23*M23 + 24*M24 + 25*M25 + 26*M26 + 27*M27 + 28*M28 + 29*M29 + 30*M30)

output_vir_gamma <- read.table("output_vir_gamma.txt",sep=' ') %>%
  rename(Time = V1,
         Bacteria = V2,
         Macrophage = V3,
         M1 = V4,
         M2 = V5,
         M3 = V6,
         M4 = V7,
         M5 = V8,
         M6 = V9,
         M7 = V10,
         M8 = V11,
         M9 = V12,
         M10 = V13,
         M11 = V14,
         M12 = V15,
         M13 = V16,
         M14 = V17,
         M15 = V18,
         M16 = V19,
         M17 = V20,
         M18 = V21,
         M19 = V22,
         M20 = V23,
         M21 = V24,
         M22 = V25,
         M23 = V26,
         M24 = V27,
         M25 = V28,
         M26 = V29,
         M27 = V30,
         M28 = V31,
         M29 = V32,
         M30 = V33) %>%
  rowwise() %>% 
  mutate(TotalBac = Bacteria + 1*M1 + 2*M2 + 3*M3 + 4*M4 + 5*M5 + 6*M6 + 7*M7 + 8*M8 + 9*M9 + 10*M10 + 
           11*M11 + 12*M12 + 13*M13 + 14*M14 + 15*M15 + 16*M16 + 17*M17 + 18*M18 + 19*M19 + 20*M20 +
           21*M21 + 22*M22 + 23*M23 + 24*M24 + 25*M25 + 26*M26 + 27*M27 + 28*M28 + 29*M29 + 30*M30)

output_att_gamma <- read.table("output_att_gamma.txt",sep=' ') %>%
  rename(Time = V1,
         Bacteria = V2,
         Macrophage = V3,
         M1 = V4,
         M2 = V5,
         M3 = V6,
         M4 = V7,
         M5 = V8,
         M6 = V9,
         M7 = V10,
         M8 = V11,
         M9 = V12,
         M10 = V13,
         M11 = V14,
         M12 = V15,
         M13 = V16,
         M14 = V17,
         M15 = V18,
         M16 = V19,
         M17 = V20,
         M18 = V21,
         M19 = V22,
         M20 = V23,
         M21 = V24,
         M22 = V25,
         M23 = V26,
         M24 = V27,
         M25 = V28,
         M26 = V29,
         M27 = V30,
         M28 = V31,
         M29 = V32,
         M30 = V33) %>%
  rowwise() %>% 
  mutate(TotalBac = Bacteria + 1*M1 + 2*M2 + 3*M3 + 4*M4 + 5*M5 + 6*M6 + 7*M7 + 8*M8 + 9*M9 + 10*M10 + 
           11*M11 + 12*M12 + 13*M13 + 14*M14 + 15*M15 + 16*M16 + 17*M17 + 18*M18 + 19*M19 + 20*M20 +
           21*M21 + 22*M22 + 23*M23 + 24*M24 + 25*M25 + 26*M26 + 27*M27 + 28*M28 + 29*M29 + 30*M30)

mphi_vir_stochastic <- output_vir_stochastic %>% 
  select(Time, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30) %>% 
  melt(id.vars="Time")

mphi_att_stochastic <- output_att_stochastic %>% 
  select(Time, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30) %>% 
  melt(id.vars="Time")

mphi_vir_gamma <- output_vir_gamma %>% 
  select(Time, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30) %>% 
  melt(id.vars="Time")

mphi_att_gamma <- output_att_gamma %>% 
  select(Time, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30) %>% 
  melt(id.vars="Time")

ggplot(mphi_vir_stochastic, aes(x = Time, y = value, col = variable)) + geom_line()
ggplot(mphi_att_stochastic, aes(x = Time, y = value, col = variable)) + geom_line()
ggplot(mphi_vir_gamma, aes(x = Time, y = value, col = variable)) + geom_line()
ggplot(mphi_att_gamma, aes(x = Time, y = value, col = variable)) + geom_line()

q_vir_stochastic <- output_vir_stochastic %>%
  filter(Time == 1000.00) %>%
  select(Time, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30) %>%
  pivot_longer(cols = M1:M30, names_to = "Bacteria", names_prefix = "M", values_to = "Number")

q_att_stochastic <- output_att_stochastic %>%
  filter(Time == 1000.00) %>%
  select(Time, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30) %>%
  pivot_longer(cols = M1:M30, names_to = "Bacteria", names_prefix = "M", values_to = "Number")

q_vir_gamma <- output_vir_gamma %>%
  filter(Time == 200.00) %>%
  select(Time, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30) %>%
  pivot_longer(cols = M1:M30, names_to = "Bacteria", names_prefix = "M", values_to = "Number")

q_att_gamma <- output_att_gamma %>%
  filter(Time == 200.00) %>%
  select(Time, M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30) %>%
  pivot_longer(cols = M1:M30, names_to = "Bacteria", names_prefix = "M", values_to = "Number")

# Expected steady-state distributions of non-spatial models ---------------

Gamma.q <- function(n,a,b)
{
  sum((n:200)^(a-1)*exp(-b*(n:200)))/(n*(n+1))
}

Gamma.dist <- function(a,b)
{
  q<-sapply(1:30,Gamma.q,a,b)
  q/sum(q)
}

print(Gamma.dist(9.27,0.499))
print(Gamma.dist(0.463,0.098))

Stochastic.dist <- function(alpha_e, gamma, mu){
  q <- numeric(20)
  q[1]<-1
  for (i in 2:20) {
    q[i] <- q[i-1]*(exp(alpha_e*(i-1))*(i-1))/(gamma + mu + (i*exp(alpha_e*i)))
  }
  return(q/sum(q))
}

print(Stochastic.dist(-0.101,0.604,0.353))
print(Stochastic.dist(-0.057,0.82,0.939))

# Plotting results --------------------------------------------------------

par(mfrow=c(1,2))

plot(1:20,vir_log_normal,
     col = "red",
     xlab = "Bacteria",
     ylab = "Frequency (log normalised)",
     main = "Virulent")
points(1:20,log(q_vir_stochastic$Number[1:20]/sum(q_vir_stochastic$Number[1:20])),col="darkred",pch=2)
lines(1:20,log(Stochastic.dist(-0.101,0.604,0.353)),col="darkred")
legend(x = "topright", c("Real","Expected","Simulated"), col=c("red","darkred","darkred"), pch=c(1,NA,2),lty=c(NA,1,NA))

plot(1:20,att_log_normal,
     col = "blue",
     xlab = "Bacteria",
     ylab = "Frequency (log normalised)",
     main = "Virulent")
points(1:20,log(q_att_stochastic$Number[1:20]/sum(q_att_stochastic$Number[1:20])),col="darkblue",pch=2)
lines(1:20,log(Stochastic.dist(-0.057,0.82,0.939)),col="darkblue")
legend(x = "topright", c("Real","Expected","Simulated"), col=c("blue","darkblue","darkblue"), pch=c(1,NA,2),lty=c(NA,1,NA))

plot(1:20,vir_log_normal,
     col = "red",
     xlab = "Bacteria",
     ylab = "Frequency (log normalised)",
     main = "Virulent")
points(1:20,log(q_vir_gamma$Number[1:20]/sum(q_vir_gamma$Number[1:20])),col="darkred",pch=2)
lines(1:20,log(Gamma.dist(9.27,0.499)[1:20]),col="darkred")
legend(x = "topright", c("Real","Expected","Simulated"), col=c("red","darkred","darkred"), pch=c(1,NA,2),lty=c(NA,1,NA))

plot(1:20,att_log_normal,
     col = "blue",
     xlab = "Bacteria",
     ylab = "Frequency (log normalised)",
     main = "Virulent")
points(1:20,log(q_att_gamma$Number[1:20]/sum(q_att_gamma$Number[1:20])),col="darkblue",pch=2)
lines(1:20,log(Gamma.dist(0.463,0.098)[1:20]),col="darkblue")
legend(x = "topright", c("Real","Expected","Simulated"), col=c("blue","darkblue","darkblue"), pch=c(1,NA,2),lty=c(NA,1,NA))

# RMSE analysis -----------------------------------------------------------

rmse(att_log_normal[1:15], log(q_vir_stochastic$Number[1:15]/sum(q_vir_stochastic$Number[1:15])))
rmse(vir_log_normal[1:15], log(q_att_stochastic$Number[1:15]/sum(q_att_stochastic$Number[1:15])))
rmse(vir_log_normal[1:15], log(q_vir_gamma$Number[1:15]/sum(q_vir_gamma$Number[1:7])))
rmse(att_log_normal[1:15], log(q_att_gamma$Number[1:15]/sum(q_att_gamma$Number[1:9])))
