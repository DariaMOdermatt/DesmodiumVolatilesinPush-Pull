#Data analysis of the Volatile Sampling with the Tenax Tubes

# author: Daria M. Odermatt, University of Zurich

# Two data files contain the summarized parameters of sampling parameters and locations, as well as the area, similarity index (SI) and height of all identified features from the untarget analysis.
## 1. Reading & Organizing Data
## 2. Merging the sampling parameters with the untarget data 
## 3. Quality control & Filtering of non-relevant substances 
## 4. Desmodium Field Samples
## 5. Fields Samples vs. Pot Samples
## 6. Desmodium vs. Maize
## 7. Statistics & PCA

##################################################################################'
# 1. Reading & Organizing Data ----
##################################################################################'

#clear R's memory
rm(list=ls())

#  add-on package
library(readr)
library (tidyr)  #for reshaping the columns
library(dplyr)
library(ggplot2)
library (stringr) #str_split
library (ggfortify) #PCA plots
library (ggrepel)
library (ggpubr) #ggarrange

# Read in the data
SamplingDF <- read.csv("TenaxSamplingDF.csv", sep=";")
# SamplingDF contains all parameters relevant for the sampling
UntargetDF <-  read.csv("20240425_Insight_ManualIntegrations_CombinedLists.CSV")
# UntargetDF contains the areas, similarity indices (SI) and height of the features identified in the untarget analysis

# Adding number for each target volatile & Rearrangement/ Reduction of the columns
colnames(UntargetDF)[1] <- "ID"
colnames(UntargetDF) <- gsub('...', "__", colnames(UntargetDF), fixed = TRUE)
Rearr_UntargetDF <- pivot_longer(UntargetDF, cols=c(-1, -2, -3, -4), names_pattern = "(.*)__(.+$)", names_to = c("Sample", "parameter"))
Rearr_UntargetDF <- pivot_wider(Rearr_UntargetDF, names_from = parameter, values_from = value)
Rearr_UntargetDF <- dplyr::select(Rearr_UntargetDF, -Flags)
Rearr_UntargetDF <- rename(Rearr_UntargetDF, "Substance" = "Name")

# Specifying Maize to "Maize Untreated"
SamplingDF$Species <- ifelse(SamplingDF$Species == "Maize", "Maize untreated",SamplingDF$Species)

# Extraction of Sample information
Rearr_UntargetDF$Label <- str_split(Rearr_UntargetDF$Sample, "__", simplify=T)[,1]

# Reformating the substance names
Rearr_UntargetDF$Substance <- gsub('MZ: ', "", Rearr_UntargetDF$Substance)
Rearr_UntargetDF$Substance <- gsub('\\(MZ): ', "", Rearr_UntargetDF$Substance) #\\ is needed before brackets
Rearr_UntargetDF$Substance <- str_to_title(Rearr_UntargetDF$Substance) 
Rearr_UntargetDF$Substance <- gsub('.Alpha.', "\U03B1", Rearr_UntargetDF$Substance)
Rearr_UntargetDF$Substance <- gsub('Alpha', "\U03B1", Rearr_UntargetDF$Substance)
Rearr_UntargetDF$Substance <- gsub('.Beta.', "\U03B2", Rearr_UntargetDF$Substance)
Rearr_UntargetDF$Substance <- gsub('.Gamma.', "\U03B3", Rearr_UntargetDF$Substance)
Rearr_UntargetDF$Substance <- gsub('.Delta.', "\U03B4", Rearr_UntargetDF$Substance)
Rearr_UntargetDF$Substance <- gsub("\\(e)", "(E)", Rearr_UntargetDF$Substance) #italics missing
Rearr_UntargetDF$Substance <- gsub("\\(z)", "(Z)", Rearr_UntargetDF$Substance) #italics missing
Rearr_UntargetDF$Substance <- gsub("Derivative", "derivative", Rearr_UntargetDF$Substance)
Rearr_UntargetDF$Substance <- gsub("^(.*), (.*)$", "\\2-\\1", Rearr_UntargetDF$Substance) # (E)- & (Z)- nach vorne schieben
Rearr_UntargetDF$Substance <- gsub("--", "-", Rearr_UntargetDF$Substance) 
Rearr_UntargetDF$Substance <- gsub("germacrene d", "germacrene D", Rearr_UntargetDF$Substance)
Rearr_UntargetDF$Substance <- gsub("-Ol", "-ol", Rearr_UntargetDF$Substance)
Rearr_UntargetDF$Substance <- gsub("Acetate", "acetate", Rearr_UntargetDF$Substance) 


# Exchanging TMTT and DMNT with their Abbreviations
Rearr_UntargetDF$Substance[Rearr_UntargetDF$Substance == '(3e,7e)-4,8,12-Trimethyltrideca-1,3,7,11-Tetraene'] <- 'TMTT'
Rearr_UntargetDF$Substance[Rearr_UntargetDF$Substance == '(E)-4,8-Dimethylnona-1,3,7-Triene'] <- 'DMNT'

##################################################################################'
# 2. Merging the sampling parameters with the untarget data ----
##################################################################################'

# Merging the two datasets
Merged <- merge(Rearr_UntargetDF, SamplingDF, by = c("Label"), all.y = TRUE, all.x = TRUE)

# Assign Category of StorageControl, which is not included in the SampleDF
Merged$Species <- ifelse (grepl( "ControlStorage", Merged$Sample), 'StorageControl', Merged$Species)

# Removal two excess columns and the rows of the 5 samples that weren't measured
Merged <- filter(Merged, (!is.na(Merged$Substance)))

# Removal of the faulty samples in Rusinga & Siaya
Merged <- filter(Merged, Merged$Label != "A3" & Merged$Label != "A4" & Merged$Label != "A13" & Merged$Label != "A14")

##################################################################################'
# 3. Quality control & Filtering of non-relevant substances ----
##################################################################################'

# Number of rows of each treatment & repetition
Merged %>%
  group_by(Species, Location) %>% 
  filter(!is.na(Area))%>% 
  summarise(unique = n_distinct(Sample))

# List of Sample names
unique(Merged$Sample)
unique(Merged$Substance)

# Distribution of all area data
ggplot(data=Merged, aes(x = Area)) +
  geom_histogram()+
  theme(text=element_text(family="Calibri", size=9))

ggplot(data=Merged, aes(x = log10(Area))) +
  geom_histogram()+
  theme(text=element_text(family="Calibri", size=9))
## Comment: Close to normal distribution is achieved after log10-transformation

Merged$LogArea <- log10(Merged$Area)
Merged$LogArea <- sub("-Inf", "0", Merged$LogArea)
Merged$LogArea <- as.numeric(Merged$LogArea)

# Filter for substances that occur in at least 2/3 of one of the desmodium filed samples and not more than 2/5 of the controls
agg <- Merged
agg$Species <- gsub ("StorageControl|Control D. incanum|Control D. intortum", "Control", agg$Species) #Summarising all controls

agg <- agg %>% #Count of Area entries per substance and desmodium field species
  filter (Location == "Field"| is.na(Location))%>% #StorageControl had no location
  filter (LogArea != 0) %>%
  group_by(Substance, Species) %>%
  summarise(Total_count=n(),.groups = 'drop')

agg <- pivot_wider(agg, names_from = Species, values_from = Total_count)
agg <- filter(agg, (agg$`D. incanum`>=9 | agg$`D. intortum`>=7) & (agg$Control<=2|is.na(agg$Control)))
n_distinct(agg$Substance) #Count of the remaining substances are left in Total
SubstancesOfInterest <- agg$Substance 
Filter_Merged <- filter(Merged, Substance %in% SubstancesOfInterest)

# Heatmap: Samples occuring in the Ambient Controls & StorageControl
Filter_Merged$Substance <- factor(Filter_Merged$Substance, levels = rev(levels(factor(Filter_Merged$Substance))))
Filter_Merged %>%
  filter (Species == "Control D. incanum" | Species == "Control D. intortum" | Species == "StorageControl")%>%
  filter (LogArea != 0) %>%
  ggplot(aes(x = interaction(Sample, Species, sep = "\n"), y = Substance, fill = LogArea)) +
  geom_tile()+
  scale_fill_viridis_c()+ # colour blind palette
  labs( y ="Target Substance", x = "", legend= "log(Area)", fill = "log(Area)")+
  theme_bw()
## Comment: None of the substances of interest were found in Control1 or the StorageControl, whereby most of the 14/17 substances were present in Control3.

#Combining Ambient Controls to one category & Removing the Storage Control (which contains no target volatiles)
Filter_Merged<- filter (Filter_Merged, Label != "ControlStorage")
Filter_Merged$Species <- ifelse (grepl( "Control", Filter_Merged$Species), 'Ambient Control', Filter_Merged$Species)

##################################################################################'
# 4. Desmodium Field Samples ----
##################################################################################'

#Count of number of detected substances per desmodium species & similarities tested
aggregate(Substance ~ Species, filter(Filter_Merged, Area!=0 & Location == "Field"), function(x) length(unique(x)))
Temp <- distinct(filter(Filter_Merged, Area!=0 & Species == "D. intortum" & Location == "Field"), Substance)
SubstancesIntortum <- Temp$Substance
Temp <- distinct(filter(Filter_Merged, Area!=0 & Species == "D. incanum" & Location == "Field"), Substance)
SubstancesIncanum<- Temp$Substance
sum(SubstancesIntortum %in% SubstancesIncanum)
sort(SubstancesIncanum)
sort(SubstancesIntortum)
# (E)-alpha-bergamotene only occurs in D. incanum and feature 21 only occured in D. intortum

#Display of the variance between of the desmodium species
Filter_Merged$Substance <- factor(Filter_Merged$Substance, levels = rev(levels(factor(Filter_Merged$Substance))))
Filter_Merged %>%
  filter(Species == "D. incanum"| Species =="D. intortum"| Species == "Ambient Control") %>%
  filter (Location == "Field") %>%
  ggplot (aes(x = LogArea, y = Substance))+
  facet_wrap(~Species)+
  geom_point(colour = 'darkgrey')+
  geom_point(stat = "summary", fun = "mean", colour = 'black', size = 3, shape =18)+
  theme_bw()+
  labs(y = "Target Substance", x = "log10(Area)")+
  theme(text=element_text(size=9))

# Qualitative Data
# Summarising the count of entries per species and substance
Qual <- Filter_Merged %>%
  filter (LogArea != 0) %>%
  group_by(Substance, Species, Location) %>%
  summarise(Total_count=n(), mean_logArea_without0 = mean(LogArea),.groups = 'drop')

Qual <- Filter_Merged %>%
  group_by(Species, Location) %>%
  summarise(Total_count=n()/n_distinct(Substance),.groups = 'drop') %>%
  merge(., Qual, by = c("Species", "Location"), all.y = TRUE, all.x = TRUE)

names(Qual) <- c("Species", "Location", "Max_n", "Substance", "Total_count", "Mean_LogArea")

# Including zero hits
Qual <- Qual %>% 
  group_by(Species) %>%
  complete(Location, Substance, fill = list(Mean_LogArea = 0, Total_count = 0))

Qual <- Qual %>% 
  group_by(Location) %>%
  complete(Species, Substance, fill = list(Mean_LogArea = 0, Total_count = 0))

# Quantitative Comparison
Filter_Merged %>%
  filter(Species == "D. incanum"| Species =="D. intortum"| Species == "Ambient Control") %>%
  filter (Location == "Field") %>%
  filter (LogArea != 0) %>%
  ggplot(aes(x = Species, y = Substance, fill = LogArea)) +
    geom_tile()+
    scale_fill_viridis_c()+ # colour blind palette
    theme_minimal()+
    labs(x="", y= "Target Substances", fill = "log(Area)")

##################################################################################'
# 5. Desmodium Fields vs. Pot  ----
##################################################################################'
# Several Graph displaying a comparison of the qualitative abundance of the substances in pots vs. field
Qual %>%
  filter (Species == "D. intortum" | Species == "D. incanum") %>%
  ggplot(aes(x = Location, y = Substance, fill = (Total_count/Max_n))) +
    geom_tile()+
    scale_fill_viridis_c()+ # colour blind palette
    theme_minimal()+
    facet_wrap(~Species)+
    labs(x="", y= "Target Substance", fill = "rel. \nQualitative Abundance")

Filter_Merged %>%
  filter (Species == "D. intortum" | Species == "D. incanum") %>%
  ggplot(aes(x = Location, y = Substance, fill = LogArea)) +
    geom_tile()+
    scale_fill_viridis_c()+ # colour blind palette
    theme_minimal()+
    facet_wrap(~Species)+
    labs(x="", y= "Target Substance", fill = "log(Area)")

Filter_Merged %>%
  filter(Species == "D. incanum"| Species == "D. intortum") %>%
  ggplot (aes(x = LogArea, y = Substance))+
  facet_wrap(~Species)+
  geom_point(aes(color = Location))+
  geom_point(stat = "summary", fun = "mean", size = 3, shape =18, aes(color=paste("Mean", Location)))+
  theme_bw()+
  labs(y = "Target Substance", x = "log(Area)")+
  scale_color_manual(values = c("Mean Field" = "#0041C2", "Mean Pot" = "black", "Field" = "lightblue", "Pot" = "grey"))

# Several Graph displaying a comparison of the qualitative abundance of the substances in all species
Qual%>%
  ggplot(aes(x = Species, y = Substance, fill = (Total_count/Max_n))) +
  geom_tile()+
  scale_fill_viridis_c(na.value = NA)+ # colour blind palette
  theme_minimal()+
  labs(x="", y= "Target Substance", fill = "rel. \nQualitative Abundance")

##################################################################################'
# 6. Desmodium vs. Maize ----
##############################################################################
Filter_Merged$Substance <- factor(Filter_Merged$Substance, levels = rev(levels(factor(Filter_Merged$Substance))))
#Filter_Merged$[match("\\(E)", Filter_Merged)] = expression("\\("*italic("E")*"\\)")
Filter_Merged %>%
  mutate(Species_Location = paste(Species, Location)) %>%
  ggplot(aes(x = Species_Location, y = Substance, fill = LogArea)) +
  geom_tile()+
  scale_fill_viridis_c(na.value = NA)+ # colour blind palette
  theme_bw()+
  labs(x="", y= "Target Substance", fill = "log(Area)")+  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+  
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5) )+
  theme(text=element_text(size=12, family = "sans"), legend.key.size = unit(0.5, 'cm'), legend.position = c(-.5, -.20), legend.direction = "horizontal", plot.margin = margin(0,0,0,0, "cm"))

# Marking all substances that occured in the Desmodium field samples but in fewer than 2/3 of the samples in grey, as they would have been excluded due to the exclusion criteria and are only included as they occur in higher abundance in the other Desmodium species.
# for the potted samples all substances are displayed that occur in at least one sample as there was a smaller number of samples taken (n = 3-5).
Heatmap <- Filter_Merged %>% 
  filter (LogArea != 0) %>%
  group_by(Species,Location,Substance) %>% 
  summarise(MeanLogArea=mean(LogArea),Total_count=n(), .groups= 'drop')
Heatmap$alpha <- ""  

Heatmap$MeanLogAreaAdj <- ifelse (Heatmap$Location == "Field",
                        ifelse (Heatmap$Species == "D. intortum",ifelse (Heatmap$Total_count <7,NA,
                              Heatmap$MeanLogArea),Heatmap$MeanLogArea),Heatmap$MeanLogArea)

Heatmap$MeanLogAreaAdj <-   ifelse (Heatmap$Location == "Field",
                           ifelse (Heatmap$Species == "D. incanum",ifelse (Heatmap$Total_count <9,NA,
                               Heatmap$MeanLogAreaAdj), Heatmap$MeanLogAreaAdj), Heatmap$MeanLogAreaAdj)

Heatmap %>%
  mutate(Species_Location = paste(Species, Location)) %>%
  ggplot(aes(x = Species_Location, y = Substance, fill = MeanLogAreaAdj)) +
  geom_tile()+
  scale_fill_viridis_c(na.value = 'grey')+ # colour blind palette
  labs(x="", y= "Target Substance", fill = "log(Area)")+  
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+  
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5) )+
  theme(text=element_text(size=11, family = "sans"), legend.key.size = unit(0.5, 'cm'), legend.position = c(-.4, -.20), legend.direction = "horizontal", plot.margin = margin(0,0,0,0, "cm"))+
  geom_point(aes(size="NA"), shape =NA, colour = "grey")+ # for the grey color in the legend
  guides(size=guide_legend("Found in fewer than \n 2/3 of the samples", override.aes=list(shape=15, size = 7), ))


ggsave(file="Volatiles_Res_Heatmap.pdf", width = 8.7, height = 13, units = "cm", dpi=700)  


# Heatmap for supporting information displays all hits in all samples
Filter_Merged %>%
  mutate(Species_Location = paste(Species,"\n", Location)) %>%
  ggplot(aes(x = Sample, y = Substance, fill = LogArea)) +
  facet_grid(~Species_Location, scales="free_x", space = 'free', labeller = label_wrap_gen(10))+
  geom_tile()+
  scale_fill_viridis_c(na.value = NA)+ # colour blind palette
  theme_bw()+
  labs(x="", y= "Target Substance", fill = "log(Area)")+  
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())+  
  theme(text=element_text(size=8, family = "sans"), legend.key.size = unit(0.5, 'cm'), legend.position = "bottom", legend.direction = "horizontal", plot.margin = margin(0,0,0,0, "cm"))

ggsave(file="Volatiles_Supp_Heatmap.pdf", width = 17.8, height = 13, units = "cm", dpi=700)  

##################################################################################'
# 7. Statistics & PCA ----
##################################################################################'
## With help from https://www.datacamp.com/tutorial/pca-analysis-r

library (corrr)
library (ggcorrplot)
library (FactoMineR)
library(factoextra)

#Removing Ambient Control
Matrix_PCA <- select(filter(Filter_Merged, Species != "Ambient Control"), NameAnalysisFile, Substance, LogArea)

#Rearranging Dataset + Replacing Emppty fields to 0
Matrix_PCA <- pivot_wider(Matrix_PCA, names_from= Substance, values_from = LogArea)
Matrix_PCA <- data.frame(Matrix_PCA, row.names = 1, check.names = FALSE)
Matrix_PCA <- mutate_all(Matrix_PCA, function(x) as.numeric(as.character(x)))
Matrix_PCA[is.na(Matrix_PCA)] <- 0

#Sort Columnnames
Matrix_PCA <- Matrix_PCA[,sort(colnames(Matrix_PCA))]

#1) Normalization of the Data
Norm_PCA <- scale(Matrix_PCA)
Norm_PCA_Category <- as.data.frame(Norm_PCA)
Norm_PCA_Category$Category <- c("D. incanum / Field", "D. incanum / Field", "D. incanum / Field", "D. incanum / Field", "D. incanum / Field", "D. incanum / Field", "D. incanum / Field", "D. incanum / Field", "D. incanum / Field", "D. incanum / Field", "D. incanum / Field", "D. incanum / Field", "D. incanum / Field", "D. incanum / Field", "D. intortum / Field", "D. intortum / Field", "D. intortum / Field", "D. intortum / Field", "D. intortum / Field", "D. intortum / Field", "D. intortum / Field", "D. intortum / Field", "D. intortum / Field", "D. intortum / Field", "D. intortum / Field", "D. incanum / Pot", "D. incanum / Pot", "D. incanum / Pot", "D. intortum / Pot", "D. intortum / Pot", "D. intortum / Pot", "D. intortum / Pot", "D. intortum / Pot", "Maize Infested / Pot", "Maize Infested / Pot", "Maize Infested / Pot", "Maize Infested / Pot", "Maize Healthy / Pot", "Maize Healthy / Pot", "Maize Healthy / Pot", "Maize Healthy / Pot")
head(Norm_PCA)

#2) Correlation matrix
corr_matrix <- cor(Norm_PCA)
ggcorrplot(corr_matrix)

#3) Applying PCA
Data_PCA <- prcomp(Norm_PCA)
summary(Data_PCA)
PCALoadings <- as.data.frame(Data_PCA$rotation)

PCAPlot<- autoplot(Data_PCA, data = Norm_PCA_Category, colour = 'Category', label = FALSE, frame = TRUE)+
  scale_color_manual(values=c("#009900", "#93c400", "#0066cc", "#12B4E9", "#ffcc00", "#ff6600"))+
  scale_fill_manual(values=c("#009900", "#93c400", "#0066cc", "#12B4E9", "#ffcc00", "#ff6600"))+  
  theme(text=element_text(size=9, family = "sans"), legend.position = "bottom", plot.margin = margin(0.2,0.5,0.2,0.8, "cm"))
PCAPlot

#4) Visualization of the principal components
fviz_eig(Data_PCA, addlabels = TRUE)
Loadings<- fviz_pca_var(Data_PCA, col.var = "cos2", labelsize = 2, arrowsize = 0.1, ##The darker an arrow the higher cos2, which displays the representation of a variable on the components
             gradient.cols = c("lightgrey", "darkgrey", "black"),
             repel = TRUE, col.circle = NA)+
             theme(text=element_text(size=9, family = "sans"), legend.position = "bottom")+
             labs(title = "")
Loadings
fviz_cos2(Data_PCA, choice = "var", axes = 1:3)
fviz_pca_biplot(Data_PCA)
loadfonts(device = "win")
windowsFonts()

#5) Clustering
kmeans_PCA <- kmeans(Norm_PCA, 3, nstart=10)
fviz_cluster(kmeans_PCA, Matrix_PCA)

ggarrange(PCAPlot, Loadings, 
                  widths = c(1,1),
                  labels = c("A", "B"),
                  ncol = 2, nrow = 1,
                  align = "h")
ggsave("Volatiles_Res_PCA.pdf", width = 17.8, height = 11, units = "cm", dpi=700)

