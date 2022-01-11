
#####################################################
############### 0) Import Packages ##################
#####################################################

if(!require("stats")) { install.packages("stats") }; library("stats")
if(!require("Matrix")) { install.packages("Matrix") }; library("Matrix")
if(!require("tidyverse")) { install.packages("tidyverse") }; library("tidyverse")
if(!require("tidytext")) { install.packages("tidytext") }; library("tidytext")
if(!require("dplyr")) { install.packages("dplyr") }; library("dplyr")
if(!require("readxl")) { install.packages("readxl") }; library("readxl")
if(!require("openxlsx")) { install.packages("openxlsx") }; library("openxlsx")
if(!require("WriteXLS")) { install.packages("WriteXLS") }; library("WriteXLS")
if(!require("car")) { install.packages("car") }; library("car")
if(!require("DescTools")) { install.packages("DescTools") }; library("DescTools")
if(!require("MASS")) { install.packages("MASS") }; library("MASS")
if(!require("data.table")) { install.packages("data.table") }; library("data.table")
if(!require("zoo")) { install.packages("zoo") }; library("zoo")
if(!require("factoextra")) { install.packages("factoextra") }; library("factoextra")
if(!require("grid")) { install.packages("grid") }; library("grid")
if(!require("cowplot")) { install.packages("cowplot") }; library("cowplot")
if(!require("gridExtra")) { install.packages("gridExtra") }; library("gridExtra")




#####################################################
############### 1) Global Inputs ####################
#####################################################

# base directory
base <- "/Users/matthewobrien/Documents/Projects/Draftkings"

# datasets
BATTERS_LOG <- read.csv(paste(base, "/Datasets/BATTERS.csv", sep = ""))
PITCHERS_LOG <- read.csv(paste(base, "/Datasets/PITCHERS.csv", sep = ""))

#####################################################
################# 2) PCA Features ###################
#####################################################

# The goal here is to reduce the dimensionality of  all the features we have 
# and compress into fewer, more meaningful features. For batters, we combine 
# batting average, woba, slugging %, on base %, on base plus slugging, and 
# isolated power. For pitchers we have woba, whip, strikeouts / walks, and era. 

###################### BATTERS #######################

bat_opp_list <- c("", "_VH")
bat_period_list <- c("", "_L10", "_L5", "_L3")

# iterate through list of periods, creating a new pca_'period'_quality var for each
# and writing plots for investigation

# Open pdf 
pdf(file=paste(base, "/Plots/PCA_batters.pdf", sep = ""), width = 11, height = 8, paper = "a4r")


for (opp in bat_opp_list) {
  
  # initiate empty list of to fill with plot
  grobs <- list()
  
  for (period in bat_period_list) {
    
    # make lifetime tag as nothing and generate feature list
    if (period==""){
      bat_feat_list <- c("B_AVG", "B_WOBA", "B_SLG", "B_OBP", "B_OPS", "B_ISO")
      title = "_LIFE"
    } else {
      bat_feat_list <- c("B_AVG", "B_WOBA", "B_SLG", "B_OBP", "B_OPS", "B_ISO")
      title <- period
    }
    
    feature_list <- paste0(bat_feat_list, period, opp, sep = "")
    
    # conduct pca
    bat.feat <- BATTERS_LOG[BATTERS_LOG$person.index>=10, feature_list]
    bat.feat <- bat.feat[complete.cases(bat.feat),]
    bat.pca <- prcomp(bat.feat, scale = TRUE)
    
    # plot variable components for first two dimentions
    grobs[[period]] <- fviz_pca_var(bat.pca, col.var = "contrib", repel = TRUE, title = paste("batter.quality: ", title, sep = ""))
    
    # Bind new pca predition to BATTERS_LOG
    prediction <- predict(bat.pca, newdata = BATTERS_LOG)[,1]
    newvarname <- paste("batter.quality", title, opp, sep = "")
    BATTERS_LOG[[newvarname]] <- prediction
  
    
  }
  
  
  # arrange running list of plots in a grid
  grid.arrange(grobs = grobs, nrow = 2, top = textGrob(paste0("PCA: Batters ",opp), gp=gpar(fontsize=15,font=1)))

}

dev.off()


###################### PITCHERS #######################

pit_opp_list <- c("")
pit_period_list <- c("","_L10","_L5", "_L3")

# iterate through list of periods, creating a new pca_'period'_quality var for each
# and writing plots for investigation

# Open pdf 
pdf(file=paste(base, "/Plots/PCA_pitchers.pdf", sep = ""), width = 11, height = 8, paper = "a4r")


for (opp in pit_opp_list) {
  
  # initiate empty list of to fill with plot
  grobs <- list()
  
  for (period in pit_period_list) {
    
    # make lifetime tag as nothing and generate feature list
    if (period==""){
      pit_feat_list <- c("P_WOBA", "P_ERA", "P_WHIP", "P_SW")
      title = "_LIFE"
    } else {
      pit_feat_list <- c("P_WOBA", "P_ERA", "P_WHIP", "P_SW")
      title <- period
    }
    
    feature_list <- paste0(pit_feat_list, period, opp, sep = "")
    
    # conduct pca
    pit.feat <- PITCHERS_LOG[PITCHERS_LOG$person.index>=3, feature_list]
    pit.feat <- pit.feat[complete.cases(pit.feat),]*ifelse(period=="_L10", -1, 1)
    pit.pca <- prcomp(pit.feat, scale = TRUE, center = TRUE)
    print(pit.pca)
    
    # plot variable components for first two dimentions
    grobs[[period]] <- fviz_pca_var(pit.pca, col.var = "contrib", repel = TRUE, title = paste("pitcher.quality: ", title, sep = ""))
    
    # Bind new pca predition to BATTERS_LOG
    prediction <- predict(pit.pca, newdata = PITCHERS_LOG)[,1]
    newvarname <- paste("pitcher.quality", title, opp, sep = "")
    PITCHERS_LOG[[newvarname]] <- prediction
    
    
  }
  
  
  # arrange running list of plots in a grid
  grid.arrange(grobs = grobs, nrow = 2, top = textGrob(paste0("PCA: Pitchers ",opp), gp=gpar(fontsize=15,font=1)))
  
}

dev.off()



#####################################################
########## 3) Merging Final Dataset #################
#####################################################

##################### TEAM LOGS ####################

# Teams Game Logs
TEAMS_LOG <- read.csv(paste(base, "/retrosplits/daybyday/teams-2018.csv", sep = "")) %>%
  mutate(game.date = as.Date(game.date, "%Y-%m-%d")) %>%
  mutate(year = as.character(substr(game.date, 0, 4))) %>%
  group_by(team.key) %>%
  mutate(team.index = 1:n()) %>%
  mutate(B_1B = B_H - B_2B - B_3B - B_HR) %>%
  mutate(B_TEAM_FP = 3*(B_H - B_2B - B_3B - B_HR) + 5*(B_2B) + 8*(B_3B) + 10*(B_HR) + 2*(B_R) + 2*(B_RBI) + 2*(B_BB) + 2*(B_HP) + 5*(B_SB)) %>%
  mutate(B_TEAM_FP_AVG = lag(rollapplyr(B_TEAM_FP, team.index, mean, fill = NA))) %>%
  mutate(B_TEAM_FP_L3 = rollapplyr(B_TEAM_FP,list(-(3:1)),mean,fill=NA)) %>%
  mutate(B_TEAM_FP_L5 = rollapplyr(B_TEAM_FP,list(-(5:1)),mean,fill=NA)) %>%
  mutate(B_TEAM_FP_L10 = rollapplyr(B_TEAM_FP,list(-(10:1)),mean,fill=NA))
