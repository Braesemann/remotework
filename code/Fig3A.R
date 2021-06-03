#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# EGOLM
# 2020-12-02
# ONET skills analysis
# Fabian Braesemann 
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

#%#%#%#%#%#%#%#%#%#%
# Loading packages
#%#%#%#%#%#%#%#%#%#%
library(tidyverse)    # numerous data wrangling packages
library(data.table)   # quick data loadinglibrary(lubridate)
library(lubridate)    # Working with dates
library(RColorBrewer) # Fancy colours
'%!in%' <- function(x,y)!('%in%'(x,y)) # opposite of %in% command
library("openxlsx")
options(stringsAsFactors = FALSE)
library("pheatmap")

# Read requirements data
skills <- read.xlsx("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/official_statistics/bls_data/Skills.xlsx")
skills$table <- "skills"
abilities <- read.xlsx("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/official_statistics/bls_data/Abilities.xlsx")
abilities$table <- "abilities"
knowledge <- read.xlsx("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/official_statistics/bls_data/Knowledge.xlsx")
knowledge$table <- "knowledge"

# Aggregate
requirements <- rbind(skills, abilities, knowledge)

# Pre-process data
requirements$`O*NET-SOC.Code` <- sapply(requirements$`O*NET-SOC.Code`, function(x) strsplit(x,"\\.")[[1]][1])

# As their are no drafters, all other in the dataset, I calculate mean values for the occupation...
drafters <- requirements %>% filter(str_detect(`O*NET-SOC.Code`, "17-301"), Scale.Name == "Importance") 
drafters <- drafters %>% group_by(Element.Name) %>% dplyr::summarise(Data.Value = mean(Data.Value))
drafters$`O*NET-SOC.Code` <- "17-3019"
drafters <- drafters %>% dplyr::select(`O*NET-SOC.Code`, Element.Name, Data.Value)
drafters <- data.frame(drafters)

# Filter and select relevant variables
requirements <- requirements %>% filter(Scale.Name == "Importance") %>%
  dplyr::select(`O*NET-SOC.Code`, Element.Name, Data.Value) #%>%
#filter(`O*NET-SOC.Code` %in% c(occupations$project_SOC4))

# Long to wide for clustering
requirements <- requirements %>% group_by(`O*NET-SOC.Code`, Element.Name) %>% dplyr::summarise(Data.Value = mean(Data.Value))
requirements <- data.frame(requirements)
# ... and add the occupation back to the requirements data frame prior to spreading
requirements <- rbind(requirements, drafters)

######

occupations3 <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/olm_data/OLM_Occupation_Data_aggregated.csv", sep = ";")

#------
# Skill clustering

important_skills <- requirements %>% group_by(Element.Name) %>% dplyr::summarise(importance = mean(Data.Value)) %>% arrange(desc(importance)) %>% print(n = 119) %>% filter(importance >= 2.5)

requirements <- requirements %>% filter(Element.Name %in% c(important_skills$Element.Name))

occ_labels <- occupations3 %>% dplyr::select(short_name, SOC4_onet) %>% arrange(short_name)

requirements <- merge(requirements, occ_labels, by.x = "O.NET.SOC.Code", by.y = "SOC4_onet", all.y = T)
requirements <- requirements %>% dplyr::select(-`O.NET.SOC.Code`)

r_wide <- spread(requirements,  short_name, Data.Value, fill = 0)

pheatmap(r_wide[,2:47], cutree_cols = 6, cutree_rows = 9, labels_row = r_wide$Element.Name, fontsize =  9,
         color = colorRampPalette(brewer.pal(n = 9, name =
                                               "Blues"), bias = 0.65)(100))
