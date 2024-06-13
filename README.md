This data repository contains all raw data and relevant data sets for the preprint / publication "Desmodium Volatiles in ``Push-Pull'' Agriculture and Protection Against the Fall Armyworm, Spodoptera frugiperda" written by Daria M. Odermatt, Frank Chidawanyika, Daniel M. Mutyambai, Bernhard Schmid, Luiz A. Domeignoz-Horta, Amanual Tamiru and Meredith C. Schuman. The statistical graphs were created in R with minor graphical adjustents (such as conversion the words Desmodium, D. intortum or D. incanum into italics) done in a graphical programm.

The data files are subdivided in the three parts of the experiments: 
1) Volatile sampling of Desmodium intortum, D. incanum and maize headspaces.
2) Oviposition Bioassays with comparison of the fall armyworm moth oviposition on maize vs. Desmodium in direct and indirect treatments
3) Wind tunnel Bioassay with comparison of the fall armyworm moth behaviour in abundance of maize vs. maize + Desmodium volatiles. 

In the section 1_VolatileSampling all raw datafiles of the TD-GC-MS measurements can be found, as well as relevant parameters for the feature detection by MZmine (Version 3.9.0, https://doi.org/10.1038/s41587-023-01690-2) and the peak integration by LabSolutions Insight (Shimadzu corporation, Kyoto, Japan).

In the section 2_OvipositionBioassay all notes about the egg batch acquisition and all processed and unprocessed picures of the collected eggs can be found. Additionally, the code and a guide for the use of a semi-automatic egg count process with ImageJ (Version 1.54f, National Institutes of Health, USA) is provided. 

In the section 3_WindtunnelBioassay all observations of the wind tunnel assays can be found in an summarised csv-file, as well as the codes for the excel macros used for data arrangement. 
The videos recurings of the wind tunnel experiments can be made available on request (daria.odermatt@bluewin.ch).

The parameter descriptions off the files used for the statistical analysis can be found below:

01_VolatileSampling_Insight_ManualIntegrations_CombinedLists.csv:
All substances are listed in the rows, while the area, similarity index with the reference spectra (SI) and height of the peaks of each sample are provided in the columns with a separation of ":" (e.g. A1 - Rusinga - 05.05.2023_662023_5 : Height).
The sample treatments can be identified by the label letter (A = D. incanum field, B = D. intortum field, C = D. incanum pot, D = D. intortum pot, I = Maize infested, N = Maize not infested) and further sampling parameters can be found in the document "01_VolatileSamplingTenaxSamplingDF.csv".

02_Oviposition-1-EggBatches-Summary.csv:
Start Date = Release date of the moths
Person = Person who collected the eggs
Cage = The name of the used cage was composed from letters A - K (no letter I) in which cages of the same treatment were groued (for a plan of the setup, see supporting information) and a numbers 1-3
Treatment = The treatment varied between "Control", which consisted of two maize plants inside of the cage, "M-AD", which consisted of one maize plant and one D. incanum plant inside of the cage (in the publication called "D. incanum direct"), "M-GL", which consisted of one maize plant and one D. intortum plant inside of the cage (in the publication called "D. intortum direct"), "M-M(AD)", which consisted of two maize plants inside the cage and one D. incanum plant placed outside of the cage close to one maize plant (in the publication called "D. incanum indirect") and "M-M(GL)", which consisted of two maize plants inside the cage and one D. intortum plant placed outside of the cage close to one maize plant (in the publication called "D. intortum indirect").
Position = Position where egg batches were found were categorized in "On of the Maize plant", "Close of the Maize plant" (in distance of max 20cm of the Maize leaves), "Close of the Desmodium plant"  (in distance of max 20cm of the Desmodium leaves), "On of the Desmodium plant" and "No Decision", if none of the other positions fitted. For the control instead of "maize" and "desmodium" the terms "left" and "right" were used.
Moths alive = Number of moths that were found alive in the cage at the corresponding position
Moths dead = Total number of moths that were found dead in the cage at the corresponding position
Egg Batches = Number of egg batches found that were found dead in the cage at the corresponding position
Comments = Comments about irregularities
Included.or.Excluded = Exclusion of repetition in case of experimental irregularities or no oviposition

02_Oviposition-2-EggCount-Summary.csv:
File.name = Full file name from the photographed eggbatch
Comment.y.n = Pictures with irregularities observerd, such as counting difficulties, were marked with the prefix "Comment", whereas all other pictures contained the prefix "NoComment".
Date = Date of egg batch collection and therefore last date of the oviposition trial
Cage = The name of the used cage was composed from letters A - K (no letter I) in which cages of the same treatment were groued (for a plan of the setup, see supporting information) and a numbers 1-3
Position = Position where egg batches were found (categories are listed above)
BatchNo = Number of the batch in the photograph. Multiple batches collected on one tape were note with "-" (e.g. 1-2), while one batch on multiple pictures was named with letters (e.g. 2a & 2b).
Day = Optional, Day of oviposition collection, this parameter was only measured for the first set of repetitions, as a few batches already after one or two days instead of after the full three dass of the experiment. 
EggNo = Number of counted eggs with ImageJ
File = Format of the file (all pictures were saved in either tif or jpeg format)
Uncertainty  = Categorisation of factors that could have influenced the number of eggs counted in a picture. This parameter was determined by rechecking all pictures with comments once again manually.
Comments = Comments about irregularities in the egg count

