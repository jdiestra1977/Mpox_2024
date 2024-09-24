library(tidyverse)
library(hrbrthemes)

remove(list = ls())

setwd("~/Documents/GitHub/Mpox_2024/")

files=Sys.glob("Data/Simulations/LastRunsJuly18/*")
f=1
AllResultsTogether<-NULL
for(f in 1:length(files)){
  uno<-read_csv(files[f])
  AllResultsTogether<-rbind(AllResultsTogether,uno)
}

allBridge<-AllResultsTogether %>% pull(bridge) %>% unique() %>% sort()
allRed<-AllResultsTogether %>% pull(red) %>% unique() %>% sort()

estimateCIs<-function(x){
  sample.mean <- mean(x)
#  print(sample.mean)
  
  sample.n <- length(x)
  sample.sd <- sd(x)
  sample.se <- sample.sd/sqrt(sample.n)
#  print(sample.se)
  
  alpha = 0.05
  degrees.freedom = sample.n - 1
  t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
 # print(t.score)
  
  margin.error <- t.score * sample.se
  lower.bound <- sample.mean - margin.error
  upper.bound <- sample.mean + margin.error
  list(lower.bound,upper.bound)
}

#Calculating confidence intervals by separate
CI_variables<-NULL
for(b in 1:length(allBridge)){
  for(r in 1:length(allRed)){
    ver0<-AllResultsTogether %>% 
      select(-"...1",-"strat",-country,-red_HET,-R0) %>%
      filter(red==allRed[r],bridge==allBridge[b])
    ver1<-data.frame(Lower_MSM_recov=estimateCIs(ver0 %>% pull(MSM_recov))[[1]],Upper_MSM_recov=estimateCIs(ver0 %>% pull(MSM_recov))[[2]])
    ver2<-data.frame(Lower_HET_recov=estimateCIs(ver0 %>% pull(HET_recov))[[1]],Upper_HET_recov=estimateCIs(ver0 %>% pull(HET_recov))[[2]])
    ver3<-cbind(ver1,ver2) %>% mutate(red=allRed[r],bridge=allBridge[b])
    CI_variables<-rbind(CI_variables,ver3)
  }
}

meanResults<-AllResultsTogether %>% 
  select(-"...1",-"strat",-country,-red_HET) %>% 
  group_by(R0,red,bridge) %>%
  summarise_each(list(Mean=mean,SD=sd)) %>% 
  left_join(CI_variables)

redValues<-c(0.00,0.10,0.30,0.50,0.70,0.95)
bridgeValues<-c(0.00,0.30,0.50,0.70)

#I add manually one point that was missing in the simulation by calculating
#the mean value of neighboring parameters
meanResults %>% filter(red==0.75) %>%
  filter(bridge %in% c(0.5,0.55,0.6)) %>% select(-bridge) %>%
  summarise_each(mean) %>% glimpse()
missingPoint <- data.frame(R0=1.5,red=0.75,bridge=0.55,MSM_recov_Mean=217.51,
                          HET_recov_Mean=49363.66,TOT_recov_Mean=49581.17,
                          outbreak_time_Mean=124.0025,MSM_recov_SD=5.250906,
                          HET_recov_SD=22.56119,TOT_recov_SD=23.13312,
                          outbreak_time_SD=7.153744,Lower_MSM_recov=216.6355,
                          Upper_MSM_recov=218.3845,Lower_HET_recov=49359.81,
                          Upper_HET_recov=49367.52)

meanResults1<-rbind(meanResults,missingPoint)

# Effect of bridges in MSM for some red and bridge parameters
maxMSM=meanResults1$MSM_recov_Mean %>% unique() %>% max()

meanResults1 %>%
  filter(red %in% c(0.5,0.75,0.95)) %>% mutate(red=100*red) %>%
  filter(bridge %in% c(0,0.05,0.3,0.5)) %>% mutate(bridge=100*bridge) %>%
  ggplot(aes(x=as.factor(red),y=MSM_recov_Mean/maxMSM,fill=as.factor(bridge))) + theme_bw() +
  geom_col(position = position_dodge(0.7),alpha=0.7) +
  geom_errorbar(aes(ymin=(MSM_recov_Mean-MSM_recov_SD)/maxMSM,ymax=(MSM_recov_Mean+MSM_recov_SD)/maxMSM),
                position = position_dodge(0.7),width=0.3) + labs(fill="% of bridges") + 
  theme(legend.position = c(0.75,0.85),text=element_text(size=18)) +
  xlab("Reduction of risky behavior (%)") + ylab("Fraction infected in MSM")

meanResults1 %>%
  filter(red %in% c(0.5,0.75,0.95)) %>% mutate(red=100*red) %>%
  filter(bridge %in% c(0,0.05,0.3,0.5)) %>% mutate(bridge=100*bridge) %>%
  ggplot(aes(x=as.factor(red),y=MSM_recov_Mean/maxMSM,fill=as.factor(bridge))) + theme_bw() +
  geom_col(position = position_dodge(0.7),alpha=0.7) +
  geom_errorbar(aes(ymin=Lower_MSM_recov/maxMSM,ymax=Upper_MSM_recov/maxMSM),
                position = position_dodge(0.7),width=0.3) + labs(fill="% of bridges") + 
  theme(legend.position = c(0.75,0.85),text=element_text(size=18)) + ylim(0,1)+
  xlab("Reduction of risky behavior (%)") + ylab("Fraction infected in MSM")

#Effect of bridges in outbreaks in HET
maxHET=meanResults1$HET_recov_Mean %>% unique() %>% max()

meanResults1 %>%
  filter(red %in% c(0.5,0.85,0.95)) %>% mutate(red=100*red) %>%
  filter(bridge %in% c(0.05,0.1,0.3,0.5)) %>% mutate(bridge=100*bridge) %>%
  ggplot(aes(x=as.factor(red),y=HET_recov_Mean/maxHET,fill=as.factor(bridge))) + theme_bw() +
  geom_col(position = position_dodge(0.7),alpha=0.7) +
  geom_errorbar(aes(ymin=(HET_recov_Mean-HET_recov_SD)/maxHET,ymax=(HET_recov_Mean+HET_recov_SD)/maxHET),
                position = position_dodge(0.7),width=0.3) + labs(fill="% of bridges") + 
  theme(legend.position = c(0.75,0.85),text=element_text(size=18)) +
  xlab("Reduction of risky behavior (%)") + ylab("Fraction infected in HET")

meanResults1 %>%
  filter(red %in% c(0.5,0.85,0.95)) %>% mutate(red=100*red) %>%
#  filter(bridge %in% c(0,0.05,0.3,0.5,0.7)) %>% mutate(bridge=100*bridge) %>%
  filter(bridge %in% c(0.05,0.1,0.3,0.5)) %>% mutate(bridge=100*bridge) %>%
  ggplot(aes(x=as.factor(red),y=HET_recov_Mean/maxHET,fill=as.factor(bridge))) + theme_bw() +
  geom_col(position = position_dodge(0.7),alpha=0.7) +
  geom_errorbar(aes(ymin=Lower_HET_recov/maxHET,ymax=Upper_HET_recov/maxHET),
                position = position_dodge(0.7),width=0.3) + labs(fill="% of bridges") + 
  theme(legend.position = c(0.75,0.85),text=element_text(size=18)) +
  xlab("Reduction of risky behavior (%)") + ylab("Fraction infected in MSM")

meanResults1 %>% #filter(red!=0.95) %>%
  filter(red %in% redValues) %>%
  ggplot(aes(x=bridge,y=MSM_recov_Mean,group=red,color=as.factor(red)))+
  geom_line() + geom_point() + #scale_x_log10() + scale_y_log10() +
#  geom_errorbar(aes(ymin=MSM_recov_Mean-MSM_recov_SD,ymax=MSM_recov_Mean+MSM_recov_SD))+
  geom_errorbar(aes(ymin=Lower_MSM_recov,ymax=Upper_MSM_recov))

#Heat map reduction - bridge nodes

ver<-meanResults1 %>% mutate(redLog=log(red)) %>%
  filter(red>=0.5) %>% filter(bridge<=0.5) %>%
  filter(bridge %in% c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5))

p<-ggplot(ver,aes(red, bridge, z= MSM_recov_Mean)) + theme_bw() +
  #  ggplot(aes(as.factor(red), as.factor(bridge), fill= MSM_recov_Mean/maxMSM)) + 
  geom_raster(aes(fill=MSM_recov_Mean/maxMSM),interpolate = T) +
  scale_fill_gradient(low="lightyellow", high="slateblue4")+
  #  scale_fill_gradient(low = "blue", high = "red")
  geom_contour(color="red")

pd<-ggplot_build(p)
pd$data[[2]] %>% pull(order) %>% unique()
# order in 20  40  60  80 100 120 140 160 180 200 220
contours<-pd$data[[2]] %>% filter(order==120) %>%
  select(red=x,bridge=y,MSM_recov_Mean=weight)

ggplot(ver,aes(red, bridge, z= MSM_recov_Mean)) + theme_bw() +
  #  ggplot(aes(as.factor(red), as.factor(bridge), fill= MSM_recov_Mean/maxMSM)) + 
  geom_raster(aes(fill=MSM_recov_Mean/maxMSM),interpolate = T) +
  scale_fill_gradient(low="lightyellow", high="slateblue4")+
  geom_line(data=contours,aes(red,bridge),linetype=2,color="red",linewidth=1.5) +
  labs(fill="Frac. Inf. in MSM") 

# For reduction in contact of bridges in HET layer

files1=Sys.glob("Data/Simulations/RedInHETSep/*")
f=1
AllResultsTogetherHET<-NULL
for(f in 1:length(files1)){
  uno<-read_csv(files1[f])
  AllResultsTogetherHET<-rbind(AllResultsTogetherHET,uno)
}

AllResultsTogetherHET$red %>% unique()
AllResultsTogetherHET$bridge %>% unique()
AllResultsTogetherHET$red_HET %>% unique()

meanResults_HET<-AllResultsTogetherHET %>% 
  select(MSM_recov,HET_recov,red_HET,TOT_recov,bridge,outbreak_time) %>%
  group_by(red_HET,bridge) %>% summarise_each(list(Mean=mean,Sd=sd))

allBridgeH<-AllResultsTogetherHET$bridge %>% unique() %>% sort()
allRedH<-AllResultsTogetherHET$red_HET %>% unique() %>% sort()

#Calculating confidence intervals by separate
CI_variables_H<-NULL
for(b in 1:length(allBridgeH)){
  for(r in 1:length(allRedH)){
    ver0<-meanResults_HET<-AllResultsTogetherHET %>% 
      select(MSM_recov,HET_recov,red_HET,TOT_recov,bridge,outbreak_time) %>%
      filter(red_HET==allRedH[r],bridge==allBridgeH[b])
    ver1<-data.frame(Lower_MSM_recov=estimateCIs(ver0 %>% pull(MSM_recov))[[1]],Upper_MSM_recov=estimateCIs(ver0 %>% pull(MSM_recov))[[2]])
    ver2<-data.frame(Lower_HET_recov=estimateCIs(ver0 %>% pull(HET_recov))[[1]],Upper_HET_recov=estimateCIs(ver0 %>% pull(HET_recov))[[2]])
    ver3<-cbind(ver1,ver2) %>% mutate(red_HET=allRedH[r],bridge=allBridgeH[b])
    CI_variables_H<-rbind(CI_variables_H,ver3)
  }
}

meanResults_HET<-AllResultsTogetherHET %>% 
  select(MSM_recov,HET_recov,red_HET,TOT_recov,bridge,outbreak_time) %>%
  group_by(red_HET,bridge) %>% summarise_each(list(Mean=mean,Sd=sd)) %>% 
  left_join(CI_variables_H)

meanResults_HET %>%
  ggplot(aes(x=as.factor(bridge),y=MSM_recov_Mean/max(MSM_recov_Mean),fill=as.factor(red_HET)))+
  geom_col(position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin=(MSM_recov_Mean-MSM_recov_Sd)/max(MSM_recov_Mean),ymax=(MSM_recov_Mean+MSM_recov_Sd)/max(MSM_recov_Mean)),
                position = position_dodge(0.7),width=0.3)

meanResults_HET %>%
  ggplot(aes(x=as.factor(bridge),y=HET_recov_Mean/max(HET_recov_Mean),fill=as.factor(red_HET)))+
  geom_col(position = position_dodge(0.7))+
  geom_errorbar(aes(ymin=(HET_recov_Mean-MSM_recov_Sd)/max(HET_recov_Mean),ymax=(HET_recov_Mean+MSM_recov_Sd)/max(HET_recov_Mean)),
                position = position_dodge(0.7),width=0.3)




