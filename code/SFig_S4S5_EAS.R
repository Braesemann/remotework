#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# EGOLM
# 2020-11-30
# Adding BLS data to occupations
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
library(scales)
'%!in%' <- function(x,y)!('%in%'(x,y)) # opposite of %in% command
options(stringsAsFactors = FALSE)

# Load data

jobTypes <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/official_statistics/bls_data/SOC_job_mapping.csv")

occupations <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/olm_data/OLM_Occupation_Data.csv")

soc_codes <- occupations %>% group_by(SOC4) %>% summarise(count = n())

bls_data <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/official_statistics/bls_data/SOC_education.csv")

bls_data <- bls_data %>% filter(soc_code %in% soc_codes$SOC4)

colnames(bls_data)

bls_data <- bls_data %>% mutate(L1 = Less_than_high_school_diploma * 1,
                                          L2 = High_school_diploma * 2,
                                          L3 = Some_college_no_degree * 3,
                                          L4 = Associate_degree * 4,
                                          L5 = Bachelor_degree * 5,
                                          L6 = Master_degree * 6,
                                          L7 = Doctoral_degree * 7, L_Total = (L1 +L2 + L3 + L4 +L5 +L6 +L7)/7)

bls_data2 <- bls_data %>% dplyr::select(soc_code, L_Total)

#########################
# INTERLUDE 2021-04-07 Histogram of educational score for supplment

bls_data <- bls_data %>% mutate(occ_group = sapply(soc_code, function(x) strsplit(x,"-")[[1]][1]))

bls_data <- bls_data %>% mutate(occ_group = ifelse(occ_group == "13", "Business & Finance",
                                                     ifelse(occ_group == "15", "Computer & Math",
                                                            ifelse(occ_group == "17", "Architecture & Engineering",
                                                                   ifelse(occ_group == "19", "Sciences",
                                                                          ifelse(occ_group == "23", "Legal",
                                                                                 ifelse(occ_group == "27", "Design & Media",
                                                                                        ifelse(occ_group == "41", "Sales",
                                                                                               ifelse(occ_group == "43", "Admin Support",NA)))))))))

bls_data <- bls_data %>% mutate(short_occ = ifelse(soc_code == "13-2011", "Accountants",
                                                     ifelse(soc_code == "15-1257", "Web Developers",
                                                            ifelse(soc_code == "17-3019", "Drafters",
                                                                   ifelse(soc_code == "19-2099", "Physical Scientists",
                                                                          ifelse(soc_code == "23-2011", "Paralegals",
                                                                                 ifelse(soc_code == "27-1024", "Graphic Designers",
                                                                                        ifelse(soc_code == "41-9041", "Telemarketers",
                                                                                              ifelse(soc_code == "43-9021", "Data Entry Keyers",NA)))))))))

bls_data3 <- bls_data %>% filter(!is.na(short_occ))

# Order the variables by degree and occupation

bls_data3 <- bls_data3 %>% dplyr::select(-L1, -L2, -L3, -L4, -L5, -L6, -L7, -L_Total)

bls_data3 <- bls_data3 %>% gather(key, value, -job_title, -soc_code, -occ_group, - short_occ)

bls_data3 <- bls_data3 %>% mutate(key = ifelse(key == "Less_than_high_school_diploma", "No High school",
                                               ifelse(key == "High_school_diploma", "High school",
                                                      ifelse(key == "Some_college_no_degree", "Some College",
                                                             ifelse(key == "Associate_degree", "Asscociate",
                                                                    ifelse(key == "Bachelor_degree", "Bachelor",
                                                                           ifelse(key == "Master_degree", "Master",
                                                                                  ifelse(key == "Doctoral_degree", "Doctorate",NA))))))))

bls_data3 <- bls_data3 %>% mutate(key = factor(key, levels = c("No High school", "High school","Some College",
                                                               "Asscociate", "Bachelor", "Master","Doctorate")))

bls_data3 <- bls_data3 %>% mutate(short_occ = factor(short_occ, levels = c("Data Entry Keyers", "Telemarketers", "Drafters",
                                                                           "Paralegals", "Graphic Designers", "Web Developers", "Accountants", "Physical Scientists")))

bls_data3 <- bls_data3 %>% mutate(occ_group = factor(occ_group, levels = c("Admin Support", "Sales", "Architecture & Engineering",  "Legal",
                                                                           "Design & Media" , "Computer & Math", "Business & Finance","Sciences")))


#%#%#%#%#%#%
# EDUCATIONAL ATTAINMENTS SCORES FIGURE (Fig S4)
#%#%#%#%#%#%

bls_data3 %>%
ggplot(aes(x = key, y = value, fill = occ_group)) + geom_bar(stat="identity", alpha  = 0.8, col = "black", lwd = 0.2) + 
  facet_wrap(~short_occ, ncol = 4) + 
  scale_fill_brewer(palette = "RdBu") + 
  scale_y_continuous(breaks = c(0,10,20,30,40,50)) +
  labs(x = "", y = "Educational attainment\n(% of individuals with degree)", fill = "") +
  theme_bw() +
  theme(legend.position = "bottom", legend.text = element_text(size = 11), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 12),
        axis.text = element_text(size = 12), strip.text = element_text(size = 10), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()) + guides(fill = guide_legend(ncol = 4))

# Figure on differences between groups
bls_data <- bls_data %>% mutate(occ_group = factor(occ_group, levels = c("Admin Support", "Sales", "Architecture & Engineering",  "Legal",
                                                                         "Design & Media" , "Computer & Math", "Business & Finance","Sciences")))

bls_data <- bls_data %>% mutate(occ_group = ifelse(occ_group == "Architecture & Engineering", "Architecture\n& Engineering",
                                                   ifelse(occ_group == "Business & Finance", "Business\n& Finance",
                                                          ifelse(occ_group == "Computer & Math", "Computer\n& Math",as.character(occ_group)))))

bls_data <- bls_data %>% mutate(occ_group = factor(occ_group, levels = c("Admin Support", "Sales", "Architecture\n& Engineering",  "Legal",
                                                                           "Design & Media" , "Computer\n& Math", "Business\n& Finance","Sciences")))

bls_data %>% group_by(occ_group) %>% summarise(median(L_Total))

bls_data <- bls_data %>% mutate(L_Total_z = (L_Total - mean(L_Total)) / sd(L_Total))

bls_data %>% group_by(occ_group) %>% summarise(median(L_Total_z))

#%#%#%#%#%#%
# DIFFERENCES BETWEEN EDUCATIONAL ATTAINMENT SCORES (Fig S5)
#%#%#%#%#%#%

bls_data %>%
  ggplot(aes(x = occ_group, y = L_Total_z, fill = occ_group, colour = occ_group)) + 
  geom_point(position = position_jitter(0.2,1), stroke = 0.35, size = 3, colour = "black", shape = 21) + geom_boxplot(fill = NA) +
  scale_fill_brewer(palette = "RdBu") + 
  scale_colour_brewer(palette = "RdBu") + 
  theme_bw() +
  labs(x = "", y = "Educational attainment score") +
  theme(legend.position = "none", text = element_text(size = 14), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 12))


#%#%#%#%#%#%