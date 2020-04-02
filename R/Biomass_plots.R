library(data.table)
library(tidyverse)

#strattrawlableunits <- fread("data/strattrawlableunits.txt")
#rv_data <- fread("data/Halibut_2018_RV.csv")
  JustHalibut <- fread("data/HalibutwithQcorrectedindices.csv")
  load("data/GDD.RData")
  


##Summarise mean for biomass and abudnance terms
  strata.mean<-JustHalibut%>%
    group_by(YEAR,STRAT,stratUnits)%>% 
    summarise(stratbm.mean=mean(BIOMASS),stratQbm.mean=mean(QBIOMASS),
              stratabd.mean=mean(ABUNDANCE),stratQabd.mean=mean(QABUNDANCE))

#Annual Weighted Mean by Strata, to get change for BOF,WSS and ESS

library(SDMTools)


wt.mean <- function(x,wt) {
  
  s = which(is.finite(x*wt)); wt = wt[s]; x = x[s] #remove NA info
  
  return( sum(wt * x)/sum(wt) ) #return the mean
  
}

wt.sd <- function(x,wt) {
  
  return( sqrt(wt.var(x,wt)) ) #return the standard deviation
  
}

wt.var <- function(x,wt) {
  
  s = which(is.finite(x + wt)); wt = wt[s]; x = x[s] #remove NA info
  
  xbar = wt.mean(x,wt) #get the weighted mean
  
  return( sum(wt *(x-xbar)^2)*(sum(wt)/(sum(wt)^2-sum(wt^2))) ) #return the variance
  
}



#For whole shelf improved  verison

Annual.Mean<-strata.mean%>% group_by(YEAR)%>%
  
  summarise(Strat.bm=wt.mean(stratbm.mean,stratUnits),Strat.bmSD=wt.sd(stratbm.mean,stratUnits),
            
            Strat.abd=wt.mean(stratabd.mean,stratUnits),Strat.abdSD=wt.sd(stratabd.mean,stratUnits))

Annual.Mean <- rename(Annual.Mean, Year = YEAR)

bm <- ggplot(Annual.Mean, mapping = aes(x = Year, y = Strat.bm))+geom_smooth(colour = "black")+geom_point()+
  theme_bw()+ scale_y_continuous(limits = c(0,4))+ scale_x_continuous()+
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        text = element_text(size=17), axis.text.x = element_text(color = "grey20", size = 14, vjust = .5),
        axis.text.y = element_text(color = "grey20", size = 14, vjust = .5))+
  labs(x = "\nYear", y = "q-corrected Stratified Mean Wt per tow (kg)\n");bm

ggsave("output2/biomass.png",bm,dpi=300,width=8,height=6,units="in")

abd <- ggplot(Annual.Mean, mapping = aes(x = Year, y = Strat.abd))+geom_smooth(colour = "black")+geom_point()+
  theme_bw()+ scale_x_continuous()+
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        text = element_text(size=17), axis.text.x = element_text(color = "grey20", size = 14, vjust = .5),
        axis.text.y = element_text(color = "grey20", size = 14, vjust = .5))+
  labs(x = "\nYear", y = "q-corrected Stratified Mean Abundance per tow\n");abd

ggsave("output2/abundance.png",abd,dpi=300,width=8,height=6,units="in")



GDD <- filter(GDD, grepl("4X|4W|4Vs|4Vn", GDD$ZONE))
## Merge dataframe to polot against eachother

bm_gdd <- merge(Annual.Mean, GDD %>%
                  group_by(Year) %>% 
                  summarise(GDD = mean(sGDD)), ) %>%
  select(Year, Strat.bm, GDD)

## Plot 
p1 <- ggplot(bm_gdd, aes(x = GDD, y = Strat.bm))+ 
    geom_point()+ 
    geom_smooth(method = "lm", se = FALSE, colour = "black")+
    scale_x_continuous(breaks = seq(0, 2100, by = 200))+
    scale_y_continuous(breaks = seq(0, 4, by = 1), limits = c(0,4), expand = c(0,0.25))+
    theme_bw()+
    theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(),
          text = element_text(size=17), axis.text.x = element_text(color = "grey20", size = 14, angle = 90, vjust = .5),
          axis.text.y = element_text(color = "grey20", size = 14, vjust = .5))+
    labs(y = "\nq-corrected Stratified Mean Wt per tow (Kg)\n", x = "\nMean Growing Degree Days\n");p1# /n creates a line break which is simply easier/faster than useing theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) to specify distance of labels
  
ggsave("output/bm-gdd.tiff",p1,dpi=600,width=8,height=6,units="in")



p3 <- ggplot(filter(prop_hab, Habitat == "Preferred", !is.na(Stock)), 
             aes(x = Year, y = Proportion,  colour = ZONE))+ 
  geom_line()+ 
  geom_smooth(method = "lm", aes(colour = ZONE), se = FALSE)+
  facet_wrap(~Stock)+
  scale_colour_viridis(discrete = TRUE, option = "D")+ 
  theme_bw()+
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        legend.box.background = element_rect(colour = "black"), legend.background = element_blank(),
        strip.background =element_rect(fill="#f0f0f0"))+#a6bddb  #f0f0f0
  guides(color = guide_legend(reverse = TRUE))+
  labs(colour = "NAFO Zone", y = "Proportion of Preferred Habitat");p3

ggsave("output/Proportion_prefhab.pdf",p3,dpi=600,width=8,height=6,units="in")



#q-corrected plot
ggplot(bm_gdd, aes(x = Year, y = Annual.Mean))