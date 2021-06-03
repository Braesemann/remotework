#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# EGOLM
# 2020-12-01
# Occupation regressions
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
library(ggrepel)      # Fancy labels in plot
library(scales)       # Log-transformed axis labels
library(ggpubr)       # Arranging several ggplots
library(stargazer)    # LaTeX regression tables
'%!in%' <- function(x,y)!('%in%'(x,y)) # opposite of %in% command
options(stringsAsFactors = FALSE)

#%#%#%#%#%#%
# Load data
#%#%#%#%#%#%

occupations3 <- read.csv("https://raw.githubusercontent.com/Braesemann/EGOLM/main/data/olm_data/OLM_Occupation_Data_aggregated.csv", sep = ";")

occupations3 <- occupations3 %>% mutate(L_Total_z = (L_Total - mean(L_Total)) / sd(L_Total))

occupations3$soc_groups <- ifelse(occupations3$short_soc1_name %in% c("Admin Support",
                                                                      "Sales",
                                                                      "Design & Media",
                                                                      "Legal"), "Non-Tech", "Tech")



# Conduct regression on wages
lmWage <- lm(log(mean_wage) ~ L_Total_z + log(mean_numApplicants) + log(project_count), data = occupations3)

# Conduct regression on experience gradient
lmRs <- lm(log(Rs) ~ log(mean_wage) + log(project_count) + log(mean_numApplicants) + L_Total_z, data = occupations3)

# Output to LaTeX
stargazer(lmWage, lmRs, digits = 2, type = "text")

# Arrange short occupation names according to average wage
occupations3$short_soc1_name <- factor(occupations3$short_soc1_name, levels = c("Admin Support",
                                                                                "Sales",
                                                                                "Design & Media",
                                                                                "Legal",
                                                                                "Business & Finance",
                                                                                "Computer & Math", "Architecture & Engineering","Sciences"))



occupations3$soc_groups2 <- "All occupations"

# Select one occupation per group for the plots
occupations3$short_name2 <- ifelse(occupations3$short_name %in% c("Paralegals", "Telemarketers", "Data Entry Keyers", "Data Scientists",
                                                                  "Physical Scientists", "Graphic Designers", 
                                                                  "Accountants", "Architectural Drafters"), occupations3$short_name, NA)

# Plot 1: Educational attainment score and wage
plot1 <- occupations3 %>%
  ggplot(aes(x = L_Total_z, y = mean_wage, col = short_soc1_name, group = 1)) + 
  geom_point(aes(fill = short_soc1_name), shape = 21, size = 4.5, alpha = 0.8, colour = "black", stroke = 0.3) + scale_y_log10() + 
  geom_smooth(method = "lm", colour = "black", lwd = 0.5, aes(group = 1), show.legend = F, se = F, formula = y ~ x) +
  annotation_logticks(sides = "lr", colour = "grey") + theme_bw() +
  scale_fill_brewer(palette = "RdBu") +
  facet_wrap(~soc_groups) +
  labs(x = "Educational attainment score", y = "Average wage (USD)", fill = "") +
  geom_text_repel(aes(label = short_name2), size = 3.5, nudge_x = 0, colour = "black", alpha = 0.8) +
  theme(text = element_text(size = 17), panel.grid = element_blank(), legend.position = "bottom") +
  guides(size=FALSE)

# Plot 2: Number of applicants per project and wage
plot2 <- occupations3 %>%
  ggplot(aes(x = mean_numApplicants, y = mean_wage, col = short_soc1_name, group = 1)) + 
  geom_point(aes(fill = short_soc1_name), shape = 21, size = 4.5, alpha = 0.8, colour = "black", stroke = 0.3) + scale_y_log10() + 
  scale_x_log10() + 
  geom_smooth(method = "lm", colour = "black", lwd = 0.5, aes(group = 1), show.legend = F, se = F) +
  annotation_logticks(sides = "lbtr", colour = "grey") + theme_bw() +
  scale_fill_brewer(palette = "RdBu") +
  facet_wrap(~soc_groups) +
  labs(x = "Number of applicants per project", y = "", fill = "") +
  geom_text_repel(aes(label = short_name2), size = 3.5, nudge_x = 0, colour = "black", alpha = 0.8) +
  theme(text = element_text(size = 17), panel.grid = element_blank(), legend.position = "bottom",
        axis.text.y = element_blank(), plot.margin=unit(c(0.25,1.4,0.125,0),"cm")) +
  guides(size=FALSE)

# Plot 3: wage and experience gradient (outlier removed)
plot3 <- occupations3 %>%
  ggplot(aes(x = mean_wage, y = Rs*100, group = soc_groups)) + 
  geom_point(aes(fill = short_soc1_name), shape = 21, size = 4, alpha = 0.8, colour = "black", stroke = 0.3) + 
  scale_x_log10() + 
  geom_smooth(method = "lm", lwd = 0.5, colour = "black", aes(group = soc_groups), show.legend = F, se = F) +
  facet_wrap(~soc_groups) +
  annotation_logticks(sides = "bt", colour = "grey") + theme_bw() +
  scale_fill_brewer(palette = "RdBu") +
  labs(x = "Averge wage (USD)", y = 'Experience gradient', fill = "") +
  geom_text_repel(aes(label = short_name2), size = 3.5, colour = "black", alpha = 0.8) +
  theme(text = element_text(size = 17), panel.grid = element_blank(), legend.position = "bottom",
        axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0))) +
  guides(size=FALSE)

# Plot 4: Market size and experience gradient (outlier removed)
plot4 <- occupations3 %>% #filter(Rs > 0.005) %>%
  ggplot(aes(x = project_count, y = Rs, group = soc_groups)) + 
  geom_point(aes(fill = short_soc1_name), shape = 21, size = 4, alpha = 0.8, colour = "black", stroke = 0.3) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 3),                 # 10^x formatted log-scale
                labels = trans_format("log10", math_format(10^.x))) +
  geom_smooth(method = "lm", lwd = 0.5, colour = "black", aes(group = soc_groups), show.legend = F, se = F) +
  facet_wrap(~soc_groups) +
  annotation_logticks(sides = "bt", colour = "grey") + theme_bw() +
  scale_fill_brewer(palette = "RdBu") +
  labs(x = "Market size", y = "", fill = "", col = "Tech") +
  geom_text_repel(aes(label = short_name2), size = 3.5, colour = "black", alpha = 0.8) +
  theme(text = element_text(size = 17), panel.grid = element_blank(), legend.position = "bottom",
        axis.text.y = element_blank(),
        plot.margin=unit(c(0.25,1.4,0.125,0),"cm")) +
  guides(size=FALSE)

ggarrange(plot1, plot2, plot3, plot4, nrow = 2,  ncol = 2, common.legend = TRUE, legend = "none")

#%#%#%#%#%#%#%#%#%#%#%#%#%