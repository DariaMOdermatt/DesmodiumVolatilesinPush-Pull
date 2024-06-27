##Windtunnel Bioassays

# Authors: Daria M. Odermatt, University of Zurich (Paragraph 1-6), Berhard Schmid, University of Zurich (paragraph 6)

# The data file contains the summarized raw data of all trials and is analyzed in the following steps:
## 1. Reading & Data Organization
## 2. Cutting data at 5 min
## 3. Segment Changes / Activity of the moths
## 4. Visualization Control & Treatments
## 5. Visualization of Settlement Position
## 6. Statistics 

##################################################################################'
# 1. Reading & Data Organization ----
##################################################################################'

#clear R's memory
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


# Read in the data
WindtunnelDataset <- read.csv("03_Windtunnel_ObservedData.csv")

names(WindtunnelDataset) <- c("Original.Location", "Location", "Clock", "Date", "Treatment", "Repetition", "Treatment.Left", "Treatment.Right", "Responding", "Commment", "Video.Save.Time", "Determiantion.via.Video")

#Renaming Treatments & Adjusting data
WindtunnelDataset$Treatment <- gsub("Intortum", "D. intortum", WindtunnelDataset$Treatment)
WindtunnelDataset$Treatment <- gsub("Incanum", "D. incanum", WindtunnelDataset$Treatment)
WindtunnelDataset$Clock <- strptime(WindtunnelDataset$Clock, format = "%m/%d/%Y %H:%M:%S")

# Removing entries with two times the same segment in a row (which was in few cases done, when movement but no segment change was observed)
Filter_DF <- WindtunnelDataset %>% filter(Location != lag(Location, default = "x"))

#Calculating Duration 
Filter_DF$Duration <- Filter_DF$Clock[-1] - Filter_DF$Clock
units (Filter_DF$Duration) <- "mins"
Filter_DF$Duration <- as.numeric(Filter_DF$Duration)

#Filtering out all Rows with the last timestamp (duration is already noted in the entry before)
Filter_DF <- filter(Filter_DF, Location != "End")

# Exclusion of the repetitions Control B4 & D.incanum B12, due to faulty execution of the repetition
Filter_DF <- filter(Filter_DF, Treatment != "Control" | Repetition != "B4")
Filter_DF <- filter(Filter_DF, Treatment != "D. incanum" | Repetition != "B12")

# Number of Repetitions per treatment
Filter_DF %>%
  group_by(Treatment) %>%
  summarise(count = n_distinct(Repetition))

##################################################################################'
# 2. Cutting data at 5 min ----
##################################################################################'

# Duration per Treatment & Repetition
agg_TotalTime <- aggregate(Duration ~Treatment + Repetition, data = Filter_DF, FUN=sum)
names(agg_TotalTime) <- c("Treatment", "Repetition", "TotalDuration_min")

# Determination of the Start time stamp
Filter_DF <- Filter_DF %>%
  group_by (Treatment, Repetition) %>%
  mutate(StartTime = min (Clock))

Filter_DF$StartTime <- strptime(Filter_DF$StartTime, format = "%Y-%m-%d %H:%M:%S")

# Accumulated Time in the experiment
Filter_DF <- mutate(Filter_DF, AccStartTime = difftime(Clock, StartTime, units = c("mins")))
Filter_DF <- mutate(Filter_DF, AccEndTime = AccStartTime + Duration)

# Filtering all Entries that started after 5 min experiment & Reducing Duration if Endtime is >5
Filter_DF <- filter (Filter_DF, AccStartTime < 5)
Filter_DF$CutDuration <- ifelse (Filter_DF$AccEndTime >5, Filter_DF$Duration - (Filter_DF$AccEndTime-5), Filter_DF$Duration)

# Adjusted duration times
agg_TotalTime <- aggregate(CutDuration ~Treatment + Repetition, data = Filter_DF, FUN=sum)
ggplot(agg_TotalTime, aes (x = Treatment, y = CutDuration))+
  geom_point()+
  labs (y = "Total Duration per Repetition [min]")

## Comment: 5 repetitions are below 5 min:
##  Three repetitions have 5s or less difference and therefore are accepted.
## Two repetitions miss approx. 20s, but were still accepted due to no segment changes in the last 2.5 min and the assumption that the moths were already settled

##################################################################################'
# 3. Segment Changes / Activity of the moths ----
##################################################################################'

# 1) Activity per treatment

# Number of rows (= segment changes) of each treatment & repetition
NTreatRep <- Filter_DF %>%
  group_by(Treatment, Repetition) %>%
  summarise(n = n()) 

ggplot(data = NTreatRep, aes (x = Treatment, y=n))+
  theme_bw()+
  geom_violin(trim=FALSE, fill="grey90", color= NA)+
  labs (y = "Number of Segment Changes", x= "")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1)+
  scale_y_continuous(breaks = seq(0, 80, by = 10))+
  theme(text=element_text(size=9))

# 2) Activity over time
# Categorisation
Filter_DF$Segment <- ifelse (Filter_DF$AccStartTime <=1, '0-1 min',
                          ifelse (1<Filter_DF$AccStartTime  & Filter_DF$AccStartTime <=2, '1-2 min',
                               ifelse (2<Filter_DF$AccStartTime & Filter_DF$AccStartTime <=3, '2-3 min',
                                    ifelse (3<Filter_DF$AccStartTime & Filter_DF$AccStartTime <=4, '3-4 min',
                                         ifelse (4<Filter_DF$AccStartTime & Filter_DF$AccStartTime <=5, '4-5 min', NA)))))

# Summarising segment changes (First two changes are subtracted as the Start time stamp and the first position are no active segment changes)
NSegment <- Filter_DF %>%
  group_by(Treatment, Repetition, Segment) %>%
  summarise(n = n())
NSegment$n <- ifelse(NSegment$Segment == '0-1 min', NSegment$n -1, NSegment$n)
NSegment <- NSegment %>% ungroup %>%
  complete(Segment, Repetition, Treatment,  fill = list(n = 0)) #Adding zero hits
NSegment  <- filter(NSegment, (Treatment != "Control" | Repetition != "B4")&(Treatment != "D. incanum" | Repetition != "B12")) #Filtering excluded samples again

# Categorising moths according to their activity
NSegment<- NSegment %>% 
  pivot_wider(names_from = Segment, values_from = n)

NSegment$Settled <- 
  ifelse (NSegment$'2-3 min' ==0&NSegment$'3-4 min' ==0&NSegment$'4-5 min' ==0, "Settled after 2 min",
         ifelse(NSegment$'3-4 min' ==0&NSegment$'4-5 min' ==0, "Settled after 3 min", 
                ifelse((NSegment$'2-3 min'+NSegment$'3-4 min'+NSegment$'4-5 min')<3, "Little activity", "High activity")))

NSegment$Settled <- factor(NSegment$Settled , levels=c("High activity","Little activity","Settled after 3 min",  "Settled after 2 min"))
plotAct1 <- ggplot(NSegment, aes(x= Treatment, fill = Settled))+
  geom_bar(position="fill")+
  scale_fill_brewer(palette="Spectral")+
  geom_text(stat ="count", aes(label = after_stat (count)), position = position_fill(vjust = 0.5), color = 'black')+
  theme_bw()+
  labs(y="Frequency", x="", fill = "")+
  theme(legend.position="bottom")+
  theme(text=element_text(size=9))
plotAct1

 # Plot to display Activity vs. time including line widths
NLineWidth <- dplyr::select (NSegment, -Settled)
NLineWidth$'0-1 min' <- interaction(NLineWidth$`0-1 min`, NLineWidth$`1-2 min`, sep = '_')
NLineWidth$'1-2 min' <- interaction(NLineWidth$`1-2 min`, NLineWidth$`2-3 min`, sep = '_')
NLineWidth$'2-3 min' <- interaction(NLineWidth$`2-3 min`, NLineWidth$`3-4 min`, sep = '_')
NLineWidth$'3-4 min' <- interaction(NLineWidth$`3-4 min`, NLineWidth$`4-5 min`, sep = '_')
NLineWidth$'4-5 min' <- as.factor(NLineWidth$'3-4 min') #Width of the segments 3-4 min until 4-5 min are the same to prevent NAs in the LineWidth table

NLineWidth <- pivot_longer(NLineWidth, cols = c('0-1 min', '1-2 min', '2-3 min', '3-4 min', '4-5 min'))
names(NLineWidth) <- c("Repetition", "Treatment","Segment", "Line")

Temp<- NLineWidth %>%
  group_by (Treatment, Segment, Line)%>%
  summarise(n = n())
NLineWidth <- merge(Temp, NLineWidth, by = c("Treatment", "Segment", "Line"))

Temp <- pivot_longer(NSegment, cols = c('0-1 min','1-2 min','2-3 min', '3-4 min', '4-5 min'), names_to='Segment', values_to='NSegChanges')
NLineWidth <- merge(Temp, NLineWidth, by = c("Treatment", "Segment", "Repetition"), all.x = TRUE)
names(NLineWidth) <- c("Treatment", "Segment", "Repetition", "Settled", "NSegChanges", "Line", "LineWidth")

#Adding a floating mean
Temp<- NLineWidth %>%
  group_by (Treatment, Segment)%>%
  summarise(NSegChanges = mean(NSegChanges))
Temp$Settled <- "Mean"
Temp$Repetition <- NA
Temp$Line <- NA
Temp$LineWidth <- 1
NLineWidth <- rbind(NLineWidth, Temp)

#Line graph
NLineWidth$Treatment <- factor(NLineWidth$Treatment, levels=c("Control", "D. incanum","D. intortum"))
NLineWidth$Settled <- factor(NLineWidth$Settled, levels=c("Settled after 2 min", "Settled after 3 min","Little activity", "High activity", "Mean"))

plotAct2 <- ggplot(data = NLineWidth, aes (x = Segment, y=NSegChanges, group = Repetition, color = Settled, linetype = Settled))+
  geom_line(data = subset(NLineWidth, Settled == "High activity"),aes(linewidth=LineWidth), lineend = "round") +
  geom_line(data = subset(NLineWidth, Settled == "Little activity"),aes(linewidth=LineWidth), lineend = "round") +
  geom_line(data = subset(NLineWidth, Settled == "Settled after 3 min"),aes(linewidth=LineWidth), lineend = "round") +
  geom_line(data = subset(NLineWidth, Settled == "Settled after 2 min"),aes(linewidth=LineWidth), lineend = "round") +
  geom_line(data = subset(NLineWidth, Settled == "Mean"),aes(linewidth=LineWidth), lineend = "round", linetype = "dashed") +
  scale_linewidth(range = c(0.5, 3))+
  facet_wrap(~ Treatment)+
  labs (y = "Number of segment changes", x = "Time segment")+
  labs(linewidth = "Number of \n overlaying lines", color = "Activity", linetype = "Activity")+
  guides(linewidth = FALSE)+
  scale_color_manual(values = c("Settled after 2 min" = "#2E75B6", "Settled after 3 min" = "#A9D18E", "Little activity" = "#F19E65","High activity" = "#F50000", "Mean" = "black"), drop = FALSE)+
  scale_linetype_manual(values = c("Settled after 2 min" = "solid", "Settled after 3 min" = "solid", "Little activity" = "solid","High activity" = "solid", "Mean" = "dashed"), drop = FALSE)+
  theme_bw()+
  theme(axis.title= element_text(size = 9), text=element_text(size=8), plot.margin = margin(0.2,0.5,1,0.8, "cm"), legend.position=c(0.5, -0.22), legend.direction = "horizontal", legend.background = element_rect(fill = "transparent"))
plotAct2

##################################################################################'
# 4. Visualization Control & Treatments----
##################################################################################'
# Summarise stays in each location
agg_Location <- aggregate(Filter_DF$CutDuration, by=list(Filter_DF$Original.Location, Filter_DF$Location, Filter_DF$Repetition, Filter_DF$Treatment,Filter_DF$Treatment.Left, Filter_DF$Date), FUN=sum)

names(agg_Location) <- c("OriginalLocation", "Location", "Repetition", "Treatment", "Treatment_Left", "Date", "CutDuration")
agg_Location$OriginalLocation <- factor(agg_Location$OriginalLocation , levels=c("Left (50cm-30cm)", "Close Left (30cm-10cm)", "Middle (10cm-10cm)", "Close Right (10cm-30cm)","Right (30cm-50cm)"))

#Combining the data (Close Left & Left and Close Right & Right, respectively)
agg_Location$LocationUnited <- ifelse(agg_Location$Location == "Left (30cm-50cm)"|agg_Location$Location == "Close Left (10cm-30cm)", 'Left (10cm-50cm)',
                                      ifelse(agg_Location$Location == "Close Right (10cm-30cm)"|agg_Location$Location == "Right (30cm-50cm)", 'Right (10cm-50cm)',
                                             ifelse(agg_Location$Location == "Middle (10cm-10cm)", 'Middle (10cm-10cm)',
                                                    ifelse(agg_Location$Location == "Maize (30cm-50cm)"|agg_Location$Location == "Close Maize (10cm-30cm)", 'Maize (10cm-50cm)',
                                                           ifelse(agg_Location$Location == "Close Des (10cm-30cm)"|agg_Location$Location == "Des (30cm-50cm)", 'Desmodium (10cm-50cm)',NA)))))

agg_Location <- aggregate(agg_Location$CutDuration, by=list(agg_Location$LocationUnited, agg_Location$Repetition, agg_Location$Treatment, agg_Location$Treatment_Left, agg_Location$Date), FUN=sum)
names(agg_Location) <- c("LocationUnited", "Repetition", "Treatment", "Treatment_Left", "Date", "CutDuration")

# Adding zero values
agg_Location <- agg_Location %>%
  group_by(Treatment, Treatment_Left)%>%
  complete(LocationUnited, Repetition, fill = list(CutDuration = 0))

agg_Location <- agg_Location %>%
  group_by(Treatment, Repetition)%>%
  fill(Date, .direction = "updown")

#inserting mean
Temp<- agg_Location %>%
  group_by (Treatment, LocationUnited)%>%
  summarise(CutDuration = mean(CutDuration))
Temp$Treatment_Left <- NA
Temp$Repetition <- "mean"
Temp$Date <- NA
agg_Location <- rbind (agg_Location, Temp)
agg_Location$color <- ifelse (agg_Location$Repetition == "mean", "mean", "repetition")
agg_Location$LocationUnited <- factor(agg_Location$LocationUnited , levels=c("Left (10cm-50cm)", "Maize (10cm-50cm)", "Middle (10cm-10cm)", "Desmodium (10cm-50cm)" , "Right (10cm-50cm)"))
agg_Location$color <- factor (agg_Location$color, levels = c("repetition", "mean"))

plotTreat <- agg_Location %>%
  ggplot(aes(x = LocationUnited, y = CutDuration, fill = color, color = color)) + 
  geom_violin(trim=FALSE, fill="grey90", color= NA)+
  geom_dotplot (binaxis='y', stackdir='center', dotsize=0.7) +
  scale_color_manual (values = c("mean" = 'red', "repetition" = "black"))+
  scale_fill_manual (values = c("mean" = 'red', "repetition" = "black"))+
  labs(y= "Duration of stay per segment [min]", x= "", color = "", fill = "")+
  theme_bw()+
  ylim(0,6.5)+
  facet_wrap(~Treatment, scale="free_x")+
  theme(text=element_text(size=9), plot.margin = margin(0.2,0.5,0.5,0.8, "cm"), legend.position=c(0.5, -0.275), legend.direction = "horizontal", legend.background = element_rect(fill = "transparent"))+
  scale_x_discrete( guide = guide_axis(n.dodge = 2), labels=c("Left (10cm-50cm)"="Left (10- 50cm)","Middle (10cm-10cm)"="Middle (10 - 10cm)", "Right (10cm-50cm)"="Right (10 - 50cm)", "Maize (10cm-50cm)"="Maize (10 - 50cm)","Desmodium (10cm-50cm)"="Des (10 - 50cm)"))
plotTreat

##################################################################################'
# 5. Visualization of Settlement Position ----
##################################################################################'

#Merging Last Position with the Acitvity of the moths that stayed in the same (merged) Segment for the last 2 min of the experiment 
#All 'High activity' moth showed changes in the last two minutes, as well as the repetitions B13 and B15 of D. intortum.
Settlement <- merge(NSegment, Filter_DF, by = c("Repetition", "Treatment"))
Settlement <- filter(Settlement, Settled == "Settled after 2 min" | Settled == "Settled after 3 min" | Settled == "Little activity")
Settlement <- filter(Settlement, (Treatment != "D. intortum" | Repetition != "B13") & (Treatment != "D. intortum" | Repetition != "B15"))

#Number of remaining repetitions per treatment
Settlement %>%
  group_by(Treatment) %>%
  summarise(count = n_distinct(Repetition))

#Only keeping the last time stamp and removing all zero values, as sometime two time stamps were made in the same second
Settlement <- Settlement %>% 
  group_by(Repetition, Treatment) %>%
  filter(Clock == max(Clock))
Settlement <- filter(Settlement, CutDuration != 0)

#Combining the data (Close Left & Left and Close Right & Right, respectively)
Settlement$LocationUnited <- ifelse(Settlement$Location == "Left (30cm-50cm)"|Settlement$Location == "Close Left (10cm-30cm)", 'Left (10cm-50cm)',
                                    ifelse(Settlement$Location == "Close Right (10cm-30cm)"|Settlement$Location == "Right (30cm-50cm)", 'Right (10cm-50cm)',
                                           ifelse(Settlement$Location == "Middle (10cm-10cm)", 'Middle (10cm-10cm)',
                                                  ifelse(Settlement$Location == "Maize (30cm-50cm)"|Settlement$Location == "Close Maize (10cm-30cm)", 'Maize (10cm-50cm)',
                                                         ifelse(Settlement$Location == "Close Des (10cm-30cm)"|Settlement$Location == "Des (30cm-50cm)", 'Desmodium (10cm-50cm)',NA)))))
Settlement$LocationUnited <- factor(Settlement$LocationUnited , levels=c("Left (10cm-50cm)", "Maize (10cm-50cm)", "Middle (10cm-10cm)", "Desmodium (10cm-50cm)" , "Right (10cm-50cm)"))

# Moths per Location visualized
plotSett <- Settlement %>%
  ggplot(aes(x = LocationUnited)) + 
  geom_bar()+
  labs(y= "Count", x= "Last Location in Wind Tunnel")+
  theme_bw()+
  facet_wrap(~Treatment,  scales ="free_x")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme(text=element_text(size=8))+
  geom_text(stat='count', aes(label=..count..),color = 'white', vjust= +2)
plotSett

aggSett <- Settlement %>% #Count of Area entries per substance and desmodium field species
  group_by(Treatment, LocationUnited) %>%
  summarise(Total_count=n())

plotSett <- aggSett %>%
  ggplot(aes(x = LocationUnited, y = Treatment, fill = Total_count)) + 
  geom_tile()+
  labs(y= "", x= "Count of last location in wind tunnel of settled moths")+
  theme_bw()+
  scale_fill_gradient(low = "white", high = "#255d92", limits = c(0,8))+
  facet_wrap(~Treatment,  scales ="free")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme(text=element_text(size=8), plot.margin = margin(0,0.5,0.2,1.1, "cm"), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), strip.background = element_blank(), strip.text.x = element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), panel.border=element_blank(), legend.position = 'none')+
  geom_text(aes(label=Total_count),color = 'white')
plotSett

imgTreatment <- readPNG("03_Windtunnel_Setup_margins.png")
imgTreatment <- rasterGrob(imgTreatment, interpolate=TRUE)


# Final Graph
plot <- ggarrange(plotAct2, imgTreatment, plotTreat, plotSett,
          heights = c(1.2,0.6, 1, 0.2),
          labels = c("A", "B", "C", ""),
          ncol = 1, nrow = 4)
plot

ggsave("Wind_Res_Activity_Treat.pdf", width = 17.8, height = 19, units = "cm", dpi=700)

##################################################################################'
# 6. Statistics ----
##################################################################################'

# Creating seperate datafile
StatisticsDF <- filter(agg_Location, Repetition != "mean" & (Treatment != "D. incanum" | Repetition != "B8")&(Treatment != "D. intortum" | Repetition != "B9"))

# Creating the columns for comparing 'maize vs. desmodium treatments' and left vs. right position of the maize treatment
StatisticsDF$ContrvsTreat <- ifelse(StatisticsDF$Treatment == "D. intortum" | StatisticsDF$Treatment == "D. incanum", "Desmodium Treatments", "Control")
StatisticsDF$PosMaize <- ifelse(StatisticsDF$Treatment_Left == "Maize", "Left", "Right")
StatisticsDF$PosMaize <- ifelse(StatisticsDF$Treatment == "Control"&StatisticsDF$LocationUnited == "Right (10cm-50cm)", "Right", StatisticsDF$PosMaize)
StatisticsDF <- filter(StatisticsDF, LocationUnited != "Middle (10cm-10cm)") # Removal of the "Middle" position

# Factorising All Variables
StatisticsDF$Date <- factor(StatisticsDF$Date)
StatisticsDF$ContrvsTreat <- factor(StatisticsDF$ContrvsTreat)
StatisticsDF$PosMaize <- factor(StatisticsDF$PosMaize)
StatisticsDF$Treatment <- factor(StatisticsDF$Treatment)
StatisticsDF$Rep <- factor(paste(StatisticsDF$Treatment,StatisticsDF$Repetition,sep="_"))
StatisticsDF$RelDuration <- StatisticsDF$CutDuration/5 #convert time to proportion of total time

# Checking different distributions
hist(StatisticsDF$RelDuration,n=20) # original data

# Angular transformation
StatisticsDF$y_ang <- asin(sqrt(StatisticsDF$RelDuration))
hist(StatisticsDF$y_ang,n=20)

# Logit transformation
StatisticsDF$y_logit <- log(StatisticsDF$RelDuration/(1-StatisticsDF$RelDuration)) #logit transformation (similar to angular)
StatisticsDF$y_logit[!is.finite(StatisticsDF$y_logit)] <- -6 #setting infinite values as -6 for display in the graph
hist(StatisticsDF$y_logit,n=20)
## Comment: Leads to the most normal distribution among the data point and best fit for the QQ-plot, which however still shows a slight sigmoid function.

# wt1: The side of maize is tested (including both sides of the control), whereby the hypothesis "Do moths behave differently on the side of maize?" can be answered.
# wt2: The side of desmodium is tested (including both sides of the control), whereby the hypothesis "Do moths behave differently on the side of desmodium?" can be answered.
StatisticsDF$wt1 <- as.numeric((StatisticsDF$LocationUnited=="Maize (10cm-50cm)")
                               |(StatisticsDF$LocationUnited=="Left (10cm-50cm)")
                               |(StatisticsDF$LocationUnited=="Right (10cm-50cm)"))
StatisticsDF$wt2 <- as.numeric((StatisticsDF$LocationUnited=="Desmodium (10cm-50cm)")
                               |(StatisticsDF$LocationUnited=="Left (10cm-50cm)")
                               |(StatisticsDF$LocationUnited=="Right (10cm-50cm)"))

# Explaination Variables:
## Date: Date of the execution of the experiment
## ContrvsTreat: Differences between the control & the treatments (D. intortum & D. incanum)
## Treatment: Differences between the two treatments and therefore the two desmodium species
## PosMaize: The Maize plant could be placed on the left or on the right side. For the control treatment (in which both sides consisted of maize), for each treatment both sides were inserted, which led to an artificial addition of repetitions.
## Rep: ID of each repetition, because control treatments were inserted twice.

# Linear model
lm1 <- lm(terms(y_logit~Date+ContrvsTreat+Treatment+PosMaize+Rep,keep.order=T)
          ,weights=wt1, data=StatisticsDF)
anova(lm1)
par(mfrow=c(2,2))
plot(lm1)

lm2 <- lm(terms(y_logit~Date+ContrvsTreat+Treatment+PosMaize+PosMaize:(ContrvsTreat+Treatment)+Rep,keep.order=T)
          ,weights=wt2, data=StatisticsDF)
anova(lm2)
plot(lm2)

# Anova
aov1 <- aov(y_logit~Date + ContrvsTreat+Treatment+PosMaize+Error(Rep)
            , data=filter(StatisticsDF, wt1 == 1))
summary(aov1)
aov2 <- aov(y_logit~Date+ContrvsTreat+Treatment+PosMaize+ Error(Rep)
            , data=filter(StatisticsDF, wt2 == 1))
summary(aov2) 

# Mixed model
lmer1 <- lmer(y_logit~ ContrvsTreat+Treatment+PosMaize+(1|Date)+(1|Rep),data=filter(StatisticsDF, wt1 == 1))
anova(lmer1,type=1)
plot(lmer1)
lmer1 # Variation of Date would have to be negative


lmer2 <- lmer(y_logit~ContrvsTreat+Treatment+PosMaize+(1|Date)+(1|Rep),data=filter(StatisticsDF, wt2 == 1))
anova (lmer2,type=1) 
plot(lmer2)
lmer2 # Variation of Date would have to be negative

##Comment: A significant effect could not be determined in any of the models tested.
