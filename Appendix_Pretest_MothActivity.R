# Deleting R's memory
rm(list=ls())

#  add-on packages
library(readr)
library(dplyr)
library(ggplot2)
library (stringr) #str_detect
library (tidyr) #complete
library(png)  #read in png
library (grid) #rasterGrob
library (ggpubr) #ggarrange
library (lme4) #mixed model
library (lmerTest) #mixed model
library(lubridate)

# Now read in the data
# Laptop
MothActivityDataset <- read.csv("C:/Users/Konto/OneDrive - Universität Zürich UZH/Masterthesis/6_OvipositionBioassays/1_ExperimentalSetup/Pretests/Pretest_MothActivity.csv")
colnames(MothActivityDataset)[1] <- "Trial"
colnames(MothActivityDataset)[10] <- "Not Found"
MothActivityDataset$Date <- as.Date(dmy(MothActivityDataset$Date))
MothActivityDataset$DateTime  <- paste(MothActivityDataset$Date, MothActivityDataset$X)
MothActivityDataset$DateTime<- as.POSIXct(MothActivityDataset$DateTime, format="%Y-%m-%d %I:%M:%S %p")

gg <- pivot_longer (MothActivityDataset, cols = c("Mating", "Plant", "Active", "Inactive", "Unchanged", "Not Found"), names_to = "activity")

gg$activity <- factor(gg$activity, levels=c("Mating", "Plant", "Active", "Inactive", "Unchanged", "Not Found"))
gg$DateTime <- strftime(gg$DateTime, format="%Y-%m-%d %I:%M %p")
gg <-gg[order(as.Date(gg$DateTime, format="%Y-%m-%d %I:%M %p")),]
order <- unique(gg$DateTime)
gg$DateTime <- factor(gg$DateTime, levels = order)

ggplot(data = gg, aes (x = DateTime, y= value, fill = activity))+
  geom_bar(position="fill", stat="identity", )+
  facet_grid(~ Trial, scale="free_x", space = "free_x")+
  labs (y = "Count", x = "Date & Time [year-month-day hour:minute]")+
  labs(fill = "Activity")+
  theme_bw()+
  scale_fill_manual(values = c("Not Found" = "lightgrey", "Unchanged" = "darkgrey", "Inactive" = "#b1d4e0","Active" = "#2e8bc0", "Mating" = "#0c2d48", "Plant" = "#145da0"), drop = FALSE)+
  theme(axis.title= element_text(size = 9), text=element_text(size=8), plot.margin = margin(0.2,0.5,0,0.8, "cm"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_discrete(breaks=gg$DateTime[gg$SumCheck!=0])

ggsave(file="Pretest_Supp_MothActivity.pdf", width = 17.8, units = "cm", dpi=700) 


ggplot(data = gg, aes (x = DateTime, y= value, color = activity))+
  geom_point()+
  facet_wrap(~ Trial, scale="free_x")+
  labs (y = "Count", x = "Time")+
  labs(color = "Activity")+
  theme_bw()+
  scale_color_manual(values = c("NF" = "lightgrey", "U" = "darkgrey", "I" = "#b1d4e0","A" = "#2e8bc0", "M" = "#0c2d48", "P" = "#145da0"), drop = FALSE)+
  theme(axis.title= element_text(size = 9), text=element_text(size=8), plot.margin = margin(0.2,0.5,0,0.8, "cm"))


