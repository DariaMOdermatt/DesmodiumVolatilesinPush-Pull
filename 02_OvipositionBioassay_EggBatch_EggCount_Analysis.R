##Oviposition Bioassays

# author: Daria M. Odermatt, University of Zurich

# Two data files contain the summarized parameters of all trials and the counted egg batches, as well as the exact egg count numbers, that were counted on all photographs. The parameters are described in the file "Oviposition Raw Data Description"
# Sections:
## 1. Reading & Organizing Data
## 2. Summarizing EggCountFile & Merging the Tables
## 3. Quality control
## 4. Vizualisation of the basic Parameters
## 5. Vizualisation of the EggCount on the different Positions
## 6. Statistics 

# Treatment Abbreviations
##Control: Maize vs. Maize
##M-GL: Maize vs. D. intortum (greenleaf desmodium)
##M-AD: Maize vs. D. incanum (African desmodium)
##M-M(GL): Maize vs. Maize (both inside the cage) + D. intortum (greenleaf desmodium) outside the cage close to one of the maize plants
##M-M(AD): Maize vs. Maize (both inside the cage) + D. incanum (African desmodium) outside the cage close to one of the maize plants
##For the two last treatments (indirect treatments), the maize plant which was close to the desmodium plant was called 'desmodium treatment'

##################################################################################'
# Library ----
##################################################################################'

#clear R's memory
rm(list=ls())

#  add-on package
library(readr)
library(dplyr)
library(ggplot2)
library (data.table) #melt
library(tidyverse)#pivot_longer
library (stringr) #str_split_fixed
library (tidyr) #complete
library (ggpubr) #ggarrange
library(png)  #read in png
library (grid) #rasterGrob
library(ggh4x) #Arranging facets
library (lme4) #mixed model
library (lmerTest) #mixed model


##################################################################################'
# 1. Reading & Organizing Data ----
##################################################################################'

# Read in the data
EggBatchDataset <- read.csv("C:/Users/Konto/OneDrive - Universität Zürich UZH/Masterthesis/11_SuplementaryData/DesmodiumVolatilesinPushPull/2_OvipositionBioassay/AnalysisR_Oviposition-1-EggBatches-Summary.csv")
# EggBatchDataset contains all counted Egg batches, all dead moths, comments and Treatments
EggCountDataset <- read.csv("C:/Users/Konto/OneDrive - Universität Zürich UZH/Masterthesis/11_SuplementaryData/DesmodiumVolatilesinPushPull/2_OvipositionBioassay//AnalysisR_Oviposition-2-EggCount-Summary.csv")
# EggCountDataset contains the eggs per picture

# Renaming of the headers
names(EggBatchDataset) <- c("Start.Date", "Person", "Cage", "Treatment", "Position", "Moths.Alive", "Moths.Dead", "EggBatches", "Comment", "Included.or.Excluded")
EggCountDataset <- rename(EggCountDataset, "End.Date" = "Date")

# Adjustment of the format of the dates
EggBatchDataset$Start.Date<-as.Date(EggBatchDataset$Start.Date, "%m/%d/%Y")
EggCountDataset$End.Date<-as.Date(as.character(EggCountDataset$End.Date), "%Y%m%d")

# Adjustment of the format of the positions
EggCountDataset <- mutate(EggCountDataset, Position = recode(Position, Maize = 'On maize plant', Des = 'On Des plant', CloseMaize = 'Close to maize plant', CloseDes = 'Close to Des plant', NoDecision = 'No decision'))
EggCountDataset <- mutate(EggCountDataset, Position = recode(Position, Left = 'On left plant', Right = 'On right plant', CloseLeft = 'Close to left plant', CloseRight = 'Close to right plant', NoDecision = 'No decision'))
EggBatchDataset$Position <- str_to_sentence(EggBatchDataset$Position)
EggBatchDataset <- mutate(EggBatchDataset, Position = recode(Position, 'On des plant' = 'On Des plant', 'Close to des plant' = 'Close to Des plant'))

# Adding the Start & End Dates
EggBatchDataset$End.Date <-EggBatchDataset$Start.Date +3
EggCountDataset$Start.Date <- EggCountDataset$End.Date -3

# Displaying the reasons for exclusion of repetitions in the oviposition dataset
unique(subset(summarise(EggBatchDataset, End.Date, Cage, Treatment, Included.or.Excluded, Comment), Included.or.Excluded=="Excluded"))
n_distinct(subset(summarise(EggBatchDataset, End.Date, Cage, Included.or.Excluded), Included.or.Excluded=="Excluded"))

# Counting Batches in Photos with photographs that contain several egg batches (e.g. 1-2)
# This Function created with the help of ChatGPT
calculate_sequence_length <- function (sequence_str){
  sequence <- as.numeric(unlist(str_split_fixed(sequence_str, "-",2))) 
  lenght_of_sequence <-abs(sequence[2]-sequence[1])+1
  return (lenght_of_sequence)
}

## Counting the No of Batches
# Pictures were named with a or b if two photographs were necessary for one batch
# Pictures with two batches, that weren't consecutive No for some reasons e.g. 7 & 9, contained the symbol "&"
# Pictures with no count indicate that there was only one batch per repetition per treatment
## Comment: Files with no file number indicate that there was only one batch for this repetition and this position
EggCountDataset$BatchCount <- sapply(EggCountDataset$BatchNo, calculate_sequence_length)
EggCountDataset$BatchCount <- ifelse (grepl("a|b", EggCountDataset$BatchNo), "0.5", EggCountDataset$BatchCount) 
EggCountDataset$BatchCount <- ifelse (grepl("&", EggCountDataset$BatchNo), "2", EggCountDataset$BatchCount)
EggCountDataset$BatchCount <- ifelse (is.na(EggCountDataset$BatchCount), "1" , EggCountDataset$BatchCount)
EggCountDataset$BatchCount <- as.numeric(EggCountDataset$BatchCount)

# Check of unique values of the Datasets
sort(unique(EggCountDataset$End.Date))
sort(unique(EggCountDataset$Cage))
sort(unique(EggCountDataset$BatchNo))

##################################################################################'
# 2. Summarizing EggCountFile & Merging the Tables ----
##################################################################################'

EggCountDataset <- arrange(EggCountDataset,Start.Date, Cage,Position, BatchNo)
Sum <- summarise(group_by (EggCountDataset, End.Date, Cage, Position),SumEggs = sum(EggNo), BatchCount = sum(as.numeric(BatchCount)), Files = paste(BatchNo, collapse = ","))

# Merging tables + Rearrangement of the columns
Merged <- merge(Sum, EggBatchDataset, by = c("Position", "Cage", "End.Date"), all.y = TRUE)
Merged <- subset(Merged, select = c(Start.Date, End.Date, Cage, Position, Treatment, SumEggs, EggBatches, BatchCount, Files, Included.or.Excluded, Person))
Merged$BatchCount <- ifelse (is.na(Merged$BatchCount), "0" , Merged$BatchCount)
Merged$SumEggs <- as.numeric(ifelse (is.na(Merged$SumEggs)&Merged$EggBatches==0, "0" , Merged$SumEggs))

#Changing the name of the indirect desmodium treatments as "On Des plant" can be confusing because moths could only lay eggs on a maize plant close to a desmodium and not directly on a desmodium plant
Merged$Position <- ifelse(Merged$Treatment == "M-M(AD)"| Merged$Treatment == "M-M(GL)", gsub("On Des plant", "On Des treat",Merged$Position), Merged$Position)

# Check if files got lost
Control <- merge(Sum, filter(EggBatchDataset, Included.or.Excluded == "Included"), by = c("Position", "Cage", "End.Date"), all.x = TRUE)
Control <- filter(Control, is.na(Start.Date))  #Entry from 2023-10-10 is a test data set that should be displayed now and excluded for further analysis
# Comments: Four entries in the EggCountDataset aren't merged, in case of discrepancy between the noted egg batch number and the amount of batches that are photographed, the number of photographs is considered as correct number of egg batches.
#      1) Repetitions 2023-05-26 G2 was excluded due to larval feeding, while 2023-05-22 K3 was excluded due to misplacement of Desmodium plant. These repetitions won't be included into the real analysis.
#      2) Repetitions 2023-07-06 G2 & 2023-07-06 E1 were originally excluded because of the note of no oviposition. They will be included again, because egg batches were found & photographed.
Merged$Included.or.Excluded <- ifelse(Merged$End.Date == "2023-07-06"& Merged$Cage =="G2", "Included", 
                               ifelse(Merged$End.Date == "2023-07-06"& Merged$Cage == "E1", "Included", Merged$Included.or.Excluded))

# Filtering for included Datasets
Merged <- filter(Merged, Included.or.Excluded == "Included")

##################################################################################'
# 3. Quality control ----
##################################################################################'
# This section serves to compare if egg batch photographs with the egg batches that were note and allow exclusion in case of big discrepancy. Additionally, the uncertainties of the egg count on pictures are reviewed.

# Controlling the EggBatches with the Number of files
# Number of files should match the number of Eggbatches
Merged$Check <- ifelse(Merged$EggBatches == Merged$BatchCount, "Pass", "Failed")
Merged$Check <- ifelse(is.na(Merged$SumEggs)&Merged$EggBatches!=0, "Failed", Merged$Check)

# Quality check for missmatch in the egg batches are classified in the following order
# 1) CO and Audrey didn't report if there were multiple batches per tapes in the trials of 20230519-22, 20230526-29 & 20230530-0602, therefore a lower number of photographs than reported egg batches is accepted
Merged$ReasonFail <- ifelse(Merged$EggBatches > Merged$BatchCount & Merged$Person == "CO" & Merged$BatchCount != "0" | Merged$EggBatches > Merged$BatchCount & Merged$Person == "Audrey" & Merged$BatchCount != "0" , "No listing if several batches were on one tape", NA)

# 2) In case of more Egg batches available then expected the photographs are considered as more reliable, as it is more likely to report a wrong number of batches on the paper sheet
Merged$ReasonFail <- ifelse(!is.na(Merged$ReasonFail), Merged$ReasonFail, ifelse(Merged$EggBatches < Merged$BatchCount, "More Egg batches available", Merged$ReasonFail))

# 3) Eggs laid in the 'No decision' area are not considered as the focus of the study and therefore repetitions with missing NoDecision eggs are accepted no matter how many batches there are missing
Merged$ReasonFail <- ifelse(!is.na(Merged$ReasonFail), Merged$ReasonFail, ifelse(Merged$Position == "No decision", "NoDecision is missing", Merged$ReasonFail))

# 4) Up to two missing egg batches were accepted per treatment, in case of large numbers of egg batches up to 25% loss was accepted
Merged$ReasonFail <- ifelse(!is.na(Merged$ReasonFail), Merged$ReasonFail, ifelse(Merged$EggBatches<=as.numeric(Merged$BatchCount)+2, "1-2 Egg Batch missing", Merged$ReasonFail))
Merged$ReasonFail <- ifelse(!is.na(Merged$ReasonFail), Merged$ReasonFail, ifelse(as.numeric(Merged$BatchCount)/as.numeric(Merged$EggBatches) > 0.75, "Less than 25% of the Egg Batches missing", Merged$ReasonFail))

# 5) Inclusion of the following repetition due to noting mistake
Merged$ReasonFail <- ifelse(Merged$End.Date == "2023-06-07"& Merged$Cage =="C1"&Merged$Position == "Close to maize plant", "Listing error", Merged$ReasonFail)
## Comment: 20230607 C1 was supposed to have 12 eggbatches 'Close to the Maize' but instead  12 batches were named with 'On the Maize plant'. Therefore, it is assumed that these batches aren't missing, but were noted wrong on the paper sheet

# 6) All remaining errors are eventually excluded
Merged$ReasonFail <- ifelse(!is.na(Merged$ReasonFail)&Merged$Check == "Failed", Merged$ReasonFail, "Exclusion")
Merged$ReasonFail <- ifelse(Merged$Check == "Pass", NA, Merged$ReasonFail) #Removing comments from the pass samples
Merged <- arrange(Merged, End.Date, Cage, Position)
unique(filter(Merged, Check== "Failed",ReasonFail== "Exclusion"))

# Exclusion of the following four repetitions
Merged$Included.or.Excluded <- ifelse(Merged$End.Date == "2023-05-22"& Merged$Cage =="C1", "Excluded", Merged$Included.or.Excluded)
Merged$Included.or.Excluded <- ifelse(Merged$End.Date == "2023-05-22"& Merged$Cage =="C2", "Excluded", Merged$Included.or.Excluded)
Merged$Included.or.Excluded <- ifelse(Merged$End.Date == "2023-05-22"& Merged$Cage =="G2", "Excluded", Merged$Included.or.Excluded)
Merged$Included.or.Excluded <- ifelse(Merged$End.Date == "2023-05-22"& Merged$Cage =="J2", "Excluded", Merged$Included.or.Excluded)

# Listing of the ReasonFail (= why the photographed egg batches didn't match the listed egg batches)
Merged <- filter(Merged, Included.or.Excluded == "Included")
table(Merged[ , c("ReasonFail", "Treatment")])

# Check for Uncertainties
# Comment: During the egg count several difficulties influencing the accuracy of egg count were noticed and categorized.

# Categories of the uncertainties
CheckUncertainties <- merge(EggCountDataset, EggBatchDataset, by = c("Position", "Cage", "End.Date"))

# Merging of big & small layer and big & small clusters as the rating was subjectively
CheckUncertainties$Uncertainty <- ifelse (grepl( "Big Layer|Small Layer", CheckUncertainties$Uncertainty), 'Layer',
                                  ifelse (grepl( "Big Cluster|Small Cluster", CheckUncertainties$Uncertainty), 'Cluster',                  
                                                                                  CheckUncertainties$Uncertainty ))

# Comment: Pictures with Larvae hatched, Count differences or dried eggs still have a high certainty in the count data and therefore are renamed in 'High certainty'. So are rows with no comment.
CheckUncertainties$Uncertainty <- recode(CheckUncertainties$Uncertainty, 'Larvae Hatched' = 'High Certainty', 'Count Difference' = 'High Certainty', 'Eggs Dried' = 'High Certainty')
CheckUncertainties$Uncertainty <- ifelse(CheckUncertainties$Uncertainty =="", "High Certainty", CheckUncertainties$Uncertainty)

# Plot Uncertainties
CheckUncertainties$Uncertainty <- str_to_sentence(CheckUncertainties$Uncertainty)
CheckUncertainties$Uncertainty <- factor(CheckUncertainties$Uncertainty, levels = c("Cluster", "Faint cluster", "High certainty", "Particles interfering", "Layer", "Faint particles"))
CheckUncertainties$Position <- factor(CheckUncertainties$Position, levels = c("On maize plant", "On left plant", "Close to maize plant", "Close to left plant" , "No decision", "Close to Des plant", "Close to right plant" ,"On Des plant", "On right plant"))

#Renaming Treatments
CheckUncertainties$Treatment <- ifelse(CheckUncertainties$Treatment == "M-AD", "D. incanum direct",
                                    ifelse (CheckUncertainties$Treatment == "M-GL", "D. intortum direct", 
                                        ifelse(CheckUncertainties$Treatment == "M-M(AD)", "D. incanum indirect", 
                                            ifelse (CheckUncertainties$Treatment == "M-M(GL)", "D. intortum indirect", CheckUncertainties$Treatment))))


ggplot(CheckUncertainties, aes(x=Position, fill=Uncertainty)) + 
  geom_bar(position="fill")+
  ylab("Percentage")+
  facet_wrap(~Treatment, scales ="free_x")+
  theme_bw()+
  theme(legend.position = c(0.85, 0.2), text = element_text(size = 13))+
  scale_fill_brewer(palette="Spectral")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  labs(x= "", y = "Frequency of the uncertainty categories")+
  geom_text(stat ="count", aes(label = after_stat (count)), position = position_fill(vjust = 0.5), color = '#494747', size = 2)+
  theme(axis.title = element_text (size= 9), text=element_text(size=6.9))
##Comment: In comparison of the two most extreme and relevant positions for the statitstics (On the left, right, maize or desmodium plant), no big differences can be identified and therefore no further measures are made. 

# Count of the remaining repetitions
Merged %>%
  group_by(Treatment) %>% 
  summarise(unique = n_distinct(End.Date, Cage))

##################################################################################'
# 4.Vizualisation of the fundamental Parameters ----
##################################################################################'
#Renaming Treatments
Merged$Treatment <- ifelse(Merged$Treatment == "M-AD", "D. incanum direct",
                           ifelse (Merged$Treatment == "M-GL", "D. intortum direct", 
                                   ifelse(Merged$Treatment == "M-M(AD)", "D. incanum indirect", 
                                          ifelse (Merged$Treatment == "M-M(GL)", "D. intortum indirect", Merged$Treatment))))

#Setting all empty cells to 0
Merged$SumEggs <-  if_else(is.na(Merged$SumEggs), 0, Merged$SumEggs)

# 1) Total of laid eggs per treatment
EggsTreatment <- Merged %>%
  group_by (End.Date, Cage, Treatment) %>%
  summarise(SumEggsTreat = sum(SumEggs))

# Scatterplot Sum of eggs per Repetition
EggsTreatment$Treatment <- factor(EggsTreatment$Treatment, levels = c("Control", "D. incanum direct", "D. intortum direct", "D. incanum indirect", "D. intortum indirect"))
EggsPerTreatment <- ggplot(data = EggsTreatment, aes(x= Treatment, y = SumEggsTreat))+
  theme_bw()+
  geom_jitter(col = 'grey', width =0.1)+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  geom_point(stat = "summary", fun = "mean", colour = 'black', size = 3, shape =18)+
  labs(x = "", y = "Sum eggs per repetition")+
  theme(text=element_text(size=9), plot.margin = margin(0.2,4,0.2,4, "cm"))
EggsPerTreatment

# 2) Ratio Egg count per batch
# Percentage of batches that weren't collected seperately
Bigger1 <- filter(EggCountDataset,BatchCount >1)
sum(Bigger1$BatchCount)/sum(EggCountDataset$BatchCount) #Percentage of Egg Batches that were counted in groups of several batches and not seperately.

# Ratio Eggs per batch
Merged$BatchCount <- as.numeric(Merged$BatchCount)
Merged$EggsPerBatch <-Merged$SumEggs/ Merged$BatchCount

Merged$Position <- factor(Merged$Position, levels = c("On maize plant", "On left plant", "Close to maize plant", "Close to left plant" , "No decision", "Close to Des plant", "Close to right plant" ,"On Des plant", "On right plant", "On Des treat"))

# Box + Dot plot
EggsPerBatch <- ggplot(data= Merged, aes(x=Position, y = EggsPerBatch))+
  geom_boxplot(alpha = 0, color = 'black', lwd = 0.2) +
  geom_dotplot (binaxis='y', stackdir='center', dotsize=0.7, fill = 'grey70')+
  facet_wrap(~Treatment, scales = "free_x")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  ylim(0, 830)+
  labs(x = "", y = "Eggs per batch")+
  theme_bw()+
  theme(axis.title = element_text (size = 9),text=element_text(size=7.3),plot.margin = margin(0.2,0.5,0.2,0.6, "cm"))
EggsPerBatch

plotSupp <- ggarrange(EggsPerTreatment, EggsPerBatch,
                  widths = c(0.5,1),
                  labels = c("A", "B"),
                  ncol = 1, nrow = 2)
plotSupp
ggsave("Ovi_Suppl_EggsPerTreatBatch.pdf", width = 17.8, height = 20, units = "cm", dpi=700)

##################################################################################'
# 5. Vizualisation of the EggCount on the different Positions ----
##################################################################################'
Merged$Position <- factor(Merged$Position, levels = c("On maize plant", "On left plant", "Close to maize plant", "Close to left plant" , "No decision", "Close to Des plant", "Close to right plant" ,"On Des plant", "On right plant", "On Des treat"))
Merged$Treatment <- factor(Merged$Treatment, levels = c("Control","D. incanum direct", "D. intortum direct", "D. incanum indirect", "D. intortum indirect"))

# Relative number of laid eggs/ batches per position
Merged <- Merged %>%
  group_by(End.Date, Cage) %>% 
  mutate(SumEggsRep = sum(SumEggs))
Merged$EggsRel <- Merged$SumEggs/Merged$SumEggsRep

Merged$BatchCount <- as.numeric(Merged$BatchCount)
Merged <- Merged %>%
  group_by(End.Date, Cage) %>% 
  mutate(SumBatchRep = sum(BatchCount))
Merged$BatchRel <- Merged$BatchCount/Merged$SumBatchRep

# Graph EggCount & Egg Batches combined
# Second step is needed since ggplot doesn't provide a second y axis option. Therefore the values are adjusted to each other and an y axis is adjusted manually
Merged <- melt(data.table(Merged), measure.vars=c("EggsRel", "BatchRel"), value.name="Rel.NoEggs_Batches")
Merged$variable <- ifelse (Merged$variable == "EggsRel", "Rel. no. of eggs", 
                              ifelse (Merged$variable == "BatchRel", "Rel. no. of egg batches", NA))



#Graph with setup panels
imgControl <- readPNG("C:/Users/Konto/OneDrive - Universität Zürich UZH/Masterthesis/6_OvipositionBioassays/3_DataAnalysis_Statistics/PicturesSetup/Oviposition_Control.png")
imgControl <- rasterGrob(imgControl, interpolate=TRUE)
imgDirect <- readPNG("C:/Users/Konto/OneDrive - Universität Zürich UZH/Masterthesis/6_OvipositionBioassays/3_DataAnalysis_Statistics/PicturesSetup/Oviposition_DirectTreatment.png")
imgDirect <- rasterGrob(imgDirect, interpolate=TRUE)
imgIndirect <- readPNG("C:/Users/Konto/OneDrive - Universität Zürich UZH/Masterthesis/6_OvipositionBioassays/3_DataAnalysis_Statistics/PicturesSetup/Oviposition_IndirectTreatment.png")
imgIndirect <- rasterGrob(imgIndirect, interpolate=TRUE)

Merged$Treatment <- factor(Merged$Treatment, levels = c("Control","D. incanum direct",  "D. incanum indirect","D. intortum direct", "D. intortum indirect"))
design = "
ABC
#DE
"

plot <- ggplot(data= Merged, aes(x=Position, y = as.numeric(Rel.NoEggs_Batches), fill = variable))+
  geom_boxplot(lwd = 0.2, outlier.size = 0.7)+ 
  facet_wrap(~Treatment, scales = "free_x")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  labs(y= "Relative amount of eggs or batches per position", x="", fill = "")+
  theme_bw()+
  scale_fill_brewer(palette="Greys")+
  theme(legend.position = c(0.15, 0.3), text = element_text(size = 13))+
  theme(axis.title = element_text (size = 9),text=element_text(size=8.1), plot.margin = margin(0.2,0.1,0,0.2, "cm"))+
  ggh4x::facet_manual(~Treatment, design = design, scales = "free_x")

imgSetups <- ggarrange(ggplot() + theme_void(), imgControl, ggplot() + theme_void(), imgDirect,  ggplot() + theme_void(), imgIndirect,  ggplot() + theme_void(), widths = c(0.125,0.157,0.146,0.157,0.151,0.206,0.049), labels = c("","A","", "B", "", "C", ""), ncol = 7, nrow = 1, hjust = 1.5)

ggarrange(imgSetups, plot, heights = c(0.4, 1), labels = c("", "D"), ncol = 1, nrow = 2)
ggsave(file="Ovi_Res_Treatments.pdf", width = 17.8, height = 13, units = "cm", dpi=700)  

# Effect size
EffectSize <- Merged %>%
  group_by (Treatment, Position) %>%
  summarise(MeanEggs = mean(Rel.NoEggs_Batches))
EffectSize <- pivot_wider(EffectSize, names_from = Position, values_from = MeanEggs)
EffectSize <- filter(EffectSize, Treatment != "Control")
EffectSize$MaizevsDes <- EffectSize$`On maize plant`/ EffectSize$`On Des plant`
EffectSize$MaizevsDes <- ifelse(is.na(EffectSize$MaizevsDes), EffectSize$`On maize plant`/ EffectSize$`On Des treat`, EffectSize$MaizevsDes)

##################################################################################'
# 6. Statistics ----
##################################################################################'

#Creating a data file reserved for statistics
StatisticsDF <- filter(Merged, variable == "Rel. No of Eggs")
StatisticsDF <- select(StatisticsDF, Start.Date, Cage, Position, Treatment, SumEggs, Rel.NoEggs_Batches)
colnames(StatisticsDF)[6] <- c("RelEggNo")

# Creating the columns for comparing 'Control vs. treatments', 'indirect vs. direct treatment', the two desmodium species and the greenhouses + prepering variables
StatisticsDF$Treatment <- factor(StatisticsDF$Treatment)
StatisticsDF$Start.Date <- factor(StatisticsDF$Start.Date)
StatisticsDF$Position <- factor (StatisticsDF$Position)
StatisticsDF$ContrvsTreat <- factor(ifelse (StatisticsDF$Treatment == "Control", "Control", "Treatment"))
StatisticsDF$IndvsDir <- factor(ifelse (StatisticsDF$Treatment == "Control", "0", ifelse(StatisticsDF$Treatment == "D. incanum direct" |StatisticsDF$Treatment == "D. intortum direct", "Direct", "Indirect")))
StatisticsDF$IncvsInt <- factor(ifelse (StatisticsDF$Treatment == "Control", "0", ifelse(StatisticsDF$Treatment == "D. incanum direct" |StatisticsDF$Treatment == "D. incanum indirect", "D. incanum", "D. intortum")))
StatisticsDF$Greenhouse <- factor(ifelse (grepl("A|B|C|D|E", StatisticsDF$Cage), "3B", "1B"))

# Separation of the repetitions into groups and numbers 
StatisticsDF$Rep <- factor(paste(StatisticsDF$Start.Date,StatisticsDF$Cage,sep="_"))
StatisticsDF$Group <- factor(substr(StatisticsDF$Cage, 1, 1))
StatisticsDF$CageNo<- factor(substr(StatisticsDF$Cage, 2, 2))
hist(StatisticsDF$RelEggNo)

#Adding both sides for the control variable
StatisticsDF$PosControl <- ifelse(StatisticsDF$Position == "On left plant", 'Left',
                                  ifelse(StatisticsDF$Position == "On right plant", 'Right', ''))


##wt1 Hypothesis: Do FAW moths lay eggs on maize differently depending if a maize, a desmodium plant or a maize plant with a desmodium plant in proximity is placed as second choice?
##wt2 Hypothesis: Do FAW moths lay eggs on a maize plant, a desmodium plant or a maize plant with a desmodium plant in proximity differently?
StatisticsDF$wt1 <- as.numeric((StatisticsDF$Position=="On maize plant")
                               |(StatisticsDF$Position=="On left plant"
                                 |(StatisticsDF$Position=="On right plant")))

StatisticsDF$wt2 <- as.numeric((StatisticsDF$Position=="On Des plant")
                               |(StatisticsDF$Position=="On Des treat")
                                |(StatisticsDF$Position=="On left plant")
                                 |(StatisticsDF$Position=="On right plant"))

# Linear models
# Comment: As the repetitions are included as a normal term, the true p-values would have to be calculated dividing the Variaion of the groups through the variation of the Repetition term. 
lm1 <- lm(RelEggNo ~ Start.Date + Greenhouse + Group/CageNo + ContrvsTreat +  IndvsDir +  IncvsInt + Treatment  + Rep
         , data=filter(StatisticsDF, wt1 == 1))
anova(lm1)
par(mfrow=c(2,2))
plot(lm1) 

lm2 <- lm(RelEggNo ~ Start.Date + Greenhouse + Group/CageNo + ContrvsTreat +  IndvsDir +  IncvsInt + Treatment  + Rep
          , data=filter(StatisticsDF, wt2 == 1))
anova(lm2)
par(mfrow=c(2,2))
plot(lm2) 

# Anova
aov1 <- aov(RelEggNo ~ Start.Date + Greenhouse + Group/CageNo + ContrvsTreat +  IndvsDir +  IncvsInt + Treatment + Error(Rep)
           ,data=filter(StatisticsDF, wt1 == 1))
summary(aov1)

aov2 <- aov(RelEggNo ~ Start.Date + Greenhouse + Group/CageNo + ContrvsTreat +  IndvsDir +  IncvsInt + Treatment + Error(Rep)
            ,data=filter(StatisticsDF, wt2 == 1))
summary(aov2)

# mixed model
lmer1 <- lmer(RelEggNo~ ContrvsTreat + IndvsDir + IncvsInt + Treatment + (1| Greenhouse) + (1| Group:CageNo) + (1|Start.Date) + (1|Rep), data=filter(StatisticsDF, wt1 == 1))
plot(lmer1)
anova(lmer1,type=1) #Rep assumed to have 0 variance component, but should be negative

lmer2 <- lmer(RelEggNo~ ContrvsTreat + IndvsDir + IncvsInt + Treatment + (1|Greenhouse) + (1| Group:CageNo) + (1|Start.Date) + (1|Rep), data=filter(StatisticsDF, wt2 == 1))
plot(lmer2)
anova(lmer2,type=1) #Rep assumed to have 0 variance component, but should be negative


## Comments:
# A difference is called significant with a p-value <0.05
# Linear model: The linear model testing eggs laid direct On maize plants and eggs laid direct on desmodium plants, shows a significant difference for both in control vs. treatment and the comparison between the indirect vs. direct treatment. No significant differences occur for the start date, greenhouse, group, the comparison between the two desmodium species and the last treatment variable (which displays differences between the indirect, direct treatment AND the two desmodium species). 
# ANOVA: Testing directly an ANOVA leads as in the linear model to a significant difference for the control vs. treatments and the comparison between the indirect and direct treatment. In both cases, a significant difference is identified between the control and the treatments and the indirect and the direct treatment. Additionally, testing on the maize side lead to a p-value <0.5 for the group, which was decisive for the position of the different treatments, while the anova test of the Desmodium side led to an significant effect for the start date, as the trials were carried in five cycles with five different start times.
## Mixed model: In the mixed models of both sides, again a significant differences for the terms control vs. treatment and Indirect vs. direct can be seen.