
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

# Lists for column filtering
keep <- c("game.key", "game.date", "site.key", "team.alignment", "opponent.key", "person.key", "slot", "seq")


#####################################################
############### 2) Helper Functions #################
#####################################################


# Merge all files located in a folder

MergeFun = function(path, key){
  filenames=list.files(path=path, full.names=TRUE)
  filenames=filenames[grep(key, filenames)]  
  rbindlist(lapply(filenames, fread))
}


#####################################################
############### 2) Import and Clean #################
#####################################################

##################### HANDIDNESS ####################

# Batter Handedness
BATTERS_HAND <- MergeFun(path = paste(base, "/retrosplits/splits/",sep = ""), key = "batting-platoon") %>%
  dplyr::select(1,3:4) %>%
  group_by(YEAR, RESP_BAT_ID) %>%
  distinct(YEAR, RESP_BAT_ID, RESP_BAT_HAND_CD) %>%
  mutate(EXISTS = 1) %>%
  spread(RESP_BAT_HAND_CD, EXISTS) %>%
  mutate(B_HAND = ifelse(is.na(R) & L==1, "L", ifelse(is.na(L) & R==1, "R", "S"))) %>%
  rename(year = YEAR, person.key = RESP_BAT_ID) %>%
  mutate(year = as.character(year)) %>%
  dplyr::select(year, person.key, B_HAND)


# Pitchers Handedness
PITCHERS_HAND <- MergeFun(path = paste(base, "/retrosplits/splits/",sep = ""), key = "pitching-platoon") %>%
  dplyr::select(1,3:4) %>%
  group_by(YEAR, RESP_PIT_ID) %>%
  distinct(YEAR, RESP_PIT_ID, RESP_PIT_HAND_CD) %>%
  mutate(EXISTS = 1) %>%
  spread(RESP_PIT_HAND_CD, EXISTS) %>%
  mutate(P_HAND = ifelse(is.na(R) & L==1, "L", ifelse(is.na(L) & R==1, "R", "S"))) %>%
  rename(year = YEAR, person.key = RESP_PIT_ID) %>%
  mutate(year = as.character(year)) %>%
  dplyr::select(year, person.key, P_HAND)

# FLA to MIA, MON TO WAS

################# STARTING_PITCHERS ################

PITCHER_STARTS <- MergeFun(path = paste(base, "/retrosplits/daybyday",sep = ""), key = "playing-") %>%
  mutate(year = as.character(substr(game.date, 0, 4))) %>%
  mutate(P_START = P_GS) %>%
  filter(P_START==1, P_G==1) %>%
  mutate(game.date = as.Date(game.date, "%Y-%m-%d")) %>%
  left_join(PITCHERS_HAND, by = c("person.key", "year")) %>%
  group_by(year, game.key) %>%
  mutate(team.key=c(last(opponent.key), first(opponent.key))) %>%
  rename(person.key_pitcher = person.key) %>%
  dplyr::select(year, game.date, game.key, team.key, opponent.key, person.key_pitcher, P_HAND)


##################### GAME LOGS ####################

# Batters Game Logs
BATTERS_LOG <- MergeFun(path = paste(base, "/retrosplits/daybyday",sep = ""), key = "playing-") %>%
  mutate(B_START = ifelse(seq==1, 1, 0)) %>%
  filter(P_G==0, B_START==1) %>%
  mutate(B_POS = case_when(F_C_GS==1 ~ "C", F_1B_GS==1 ~ "1B", F_2B_GS==1 ~ "2B", F_3B_GS==1 ~ "3B", F_SS_GS==1~ "SS", F_LF_GS==1~ "OF", F_CF_GS==1~ "OF", F_RF_GS==1~ "OF")) %>%
  mutate(B_POS = ifelse(is.na(B_POS) & B_START==1, "DH", B_POS)) %>%
  rename(B_HOME = team.alignment) %>%
  dplyr::select(any_of(keep), starts_with("B_"), -B_GW) %>%
  mutate(game.date = as.Date(game.date, "%Y-%m-%d")) %>%
  mutate(year = as.character(substr(game.date, 0, 4))) %>%
  group_by(person.key) %>%
  left_join(BATTERS_HAND, by = c("person.key", "year")) %>%
  left_join(PITCHER_STARTS[,c("year", "game.key", "team.key", "person.key_pitcher", "P_HAND")], by = c("year", "game.key", "opponent.key" = "team.key")) %>%
  mutate(person.index = 1:n()) %>%
  mutate(B_1B = B_H - B_2B - B_3B - B_HR) %>%
  mutate(B_FP = 3*(B_H - B_2B - B_3B - B_HR) + 5*(B_2B) + 8*(B_3B) + 10*(B_HR) + 2*(B_R) + 2*(B_RBI) + 2*(B_BB) + 2*(B_HP) + 5*(B_SB)) %>%
  mutate(B_FP_AVG = lag(rollapplyr(B_FP, person.index, mean, fill = NA))) %>%
  mutate(B_AVG = lag(rollapplyr(B_H, person.index, sum, fill = NA)) / lag(rollapplyr(B_AB, person.index, sum, fill = NA))) %>%
  mutate(B_WOBA = (0.69*lag(rollapplyr(B_BB, person.index, sum, fill = NA)) + 0.719*lag(rollapplyr(B_HP, person.index, sum, fill = NA)) + 0.87*lag(rollapplyr(B_1B, person.index, sum, fill = NA)) + 1.217*lag(rollapplyr(B_2B, person.index, sum, fill = NA)) + 1.529*lag(rollapplyr(B_3B, person.index, sum, fill = NA)) + 1.94*lag(rollapplyr(B_HR, person.index, sum, fill = NA)))/ (lag(rollapplyr(B_AB, person.index, sum, fill = NA)) + lag(rollapplyr(B_BB, person.index, sum, fill = NA)) - lag(rollapplyr(B_IBB, person.index, sum, fill = NA)) + lag(rollapplyr(B_SF, person.index, sum, fill = NA)) + lag(rollapplyr(B_HP, person.index, sum, fill = NA)))) %>%
  mutate(B_SLG = lag(rollapplyr(B_TB, person.index, sum, fill = NA)) / lag(rollapplyr(B_AB, person.index, sum, fill = NA))) %>%
  mutate(B_OBP = (lag(rollapplyr(B_H, person.index, sum, fill = NA)) + lag(rollapplyr(B_BB, person.index, sum, fill = NA)) + lag(rollapplyr(B_HP, person.index, sum, fill = NA)))/ (lag(rollapplyr(B_AB, person.index, sum, fill = NA)) + lag(rollapplyr(B_BB, person.index, sum, fill = NA)) + lag(rollapplyr(B_SF, person.index, sum, fill = NA)) + lag(rollapplyr(B_HP, person.index, sum, fill = NA)))) %>%
  mutate(B_OPS = B_OBP + B_SLG) %>%
  mutate(B_ISO = B_SLG - B_AVG) %>%
  mutate(B_FP_L3 = rollapplyr(B_FP,list(-(3:1)),mean,fill=NA)) %>%
  mutate(B_FP_L5 = rollapplyr(B_FP,list(-(5:1)),mean,fill=NA)) %>%
  mutate(B_FP_L10 = rollapplyr(B_FP,list(-(10:1)),mean,fill=NA)) %>%
  mutate(B_AVG_L3 = rollapplyr(B_H, list(-(3:1)), sum, fill = NA) / rollapplyr(B_AB, list(-(3:1)), sum, fill = NA)) %>%
  mutate(B_AVG_L5 = rollapplyr(B_H, list(-(5:1)), sum, fill = NA) / rollapplyr(B_AB, list(-(5:1)), sum, fill = NA)) %>%
  mutate(B_AVG_L10 = rollapplyr(B_H, list(-(10:1)), sum, fill = NA) / rollapplyr(B_AB, list(-(10:1)), sum, fill = NA)) %>%
  mutate(B_WOBA_L3 = (0.69*rollapplyr(B_BB, list(-(3:1)), sum, fill = NA) + 0.719*rollapplyr(B_HP, list(-(3:1)), sum, fill = NA) + 0.87*rollapplyr(B_1B, list(-(3:1)), sum, fill = NA) + 1.217*rollapplyr(B_2B, list(-(3:1)), sum, fill = NA) + 1.529*rollapplyr(B_3B, list(-(3:1)), sum, fill = NA) + 1.94*rollapplyr(B_HR, list(-(3:1)), sum, fill = NA)) / (rollapplyr(B_AB, list(-(3:1)), sum, fill = NA) + rollapplyr(B_BB, list(-(3:1)), sum, fill = NA) - rollapplyr(B_IBB, list(-(3:1)), sum, fill = NA) + rollapplyr(B_SF, list(-(3:1)), sum, fill = NA) + rollapplyr(B_HP, list(-(3:1)), sum, fill = NA))) %>%
  mutate(B_WOBA_L5 = (0.69*rollapplyr(B_BB, list(-(5:1)), sum, fill = NA) + 0.719*rollapplyr(B_HP, list(-(5:1)), sum, fill = NA) + 0.87*rollapplyr(B_1B, list(-(5:1)), sum, fill = NA) + 1.217*rollapplyr(B_2B, list(-(5:1)), sum, fill = NA) + 1.529*rollapplyr(B_3B, list(-(5:1)), sum, fill = NA) + 1.94*rollapplyr(B_HR, list(-(5:1)), sum, fill = NA)) / (rollapplyr(B_AB, list(-(5:1)), sum, fill = NA) + rollapplyr(B_BB, list(-(5:1)), sum, fill = NA) - rollapplyr(B_IBB, list(-(5:1)), sum, fill = NA) + rollapplyr(B_SF, list(-(5:1)), sum, fill = NA) + rollapplyr(B_HP, list(-(5:1)), sum, fill = NA))) %>%
  mutate(B_WOBA_L10 = (0.69*rollapplyr(B_BB, list(-(10:1)), sum, fill = NA) + 0.719*rollapplyr(B_HP, list(-(10:1)), sum, fill = NA) + 0.87*rollapplyr(B_1B, list(-(10:1)), sum, fill = NA) + 1.217*rollapplyr(B_2B, list(-(10:1)), sum, fill = NA) + 1.529*rollapplyr(B_3B, list(-(10:1)), sum, fill = NA) + 1.94*rollapplyr(B_HR, list(-(10:1)), sum, fill = NA)) / (rollapplyr(B_AB, list(-(10:1)), sum, fill = NA) + rollapplyr(B_BB, list(-(10:1)), sum, fill = NA) - rollapplyr(B_IBB, list(-(10:1)), sum, fill = NA) + rollapplyr(B_SF, list(-(10:1)), sum, fill = NA) + rollapplyr(B_HP, list(-(10:1)), sum, fill = NA))) %>%
  mutate(B_SLG_L3 = rollapplyr(B_TB, list(-(3:1)), sum, fill = NA) / rollapplyr(B_AB, list(-(3:1)), sum, fill = NA)) %>%
  mutate(B_SLG_L5 = rollapplyr(B_TB, list(-(5:1)), sum, fill = NA) / rollapplyr(B_AB, list(-(5:1)), sum, fill = NA)) %>%
  mutate(B_SLG_L10 = rollapplyr(B_TB, list(-(10:1)), sum, fill = NA) / rollapplyr(B_AB, list(-(10:1)), sum, fill = NA)) %>%
  mutate(B_OBP_L3 = (rollapplyr(B_H, list(-(3:1)), sum, fill = NA) + rollapplyr(B_BB, list(-(3:1)), sum, fill = NA) + rollapplyr(B_HP, list(-(3:1)), sum, fill = NA))/ (rollapplyr(B_AB, list(-(3:1)), sum, fill = NA) + rollapplyr(B_BB, list(-(3:1)), sum, fill = NA) + rollapplyr(B_SF, list(-(3:1)), sum, fill = NA) + rollapplyr(B_HP, list(-(3:1)), sum, fill = NA))) %>%
  mutate(B_OBP_L5 = (rollapplyr(B_H, list(-(5:1)), sum, fill = NA) + rollapplyr(B_BB, list(-(5:1)), sum, fill = NA) + rollapplyr(B_HP, list(-(5:1)), sum, fill = NA))/ (rollapplyr(B_AB, list(-(5:1)), sum, fill = NA) + rollapplyr(B_BB, list(-(5:1)), sum, fill = NA) + rollapplyr(B_SF, list(-(5:1)), sum, fill = NA) + rollapplyr(B_HP, list(-(5:1)), sum, fill = NA))) %>%
  mutate(B_OBP_L10 = (rollapplyr(B_H, list(-(10:1)), sum, fill = NA) + rollapplyr(B_BB, list(-(10:1)), sum, fill = NA) + rollapplyr(B_HP, list(-(10:1)), sum, fill = NA))/ (rollapplyr(B_AB, list(-(10:1)), sum, fill = NA) + rollapplyr(B_BB, list(-(10:1)), sum, fill = NA) + rollapplyr(B_SF, list(-(10:1)), sum, fill = NA) + rollapplyr(B_HP, list(-(10:1)), sum, fill = NA))) %>%
  mutate(B_OPS_L3 = B_OBP_L3 + B_SLG_L3) %>%
  mutate(B_OPS_L5 = B_OBP_L5 + B_SLG_L5) %>%
  mutate(B_OPS_L10 = B_OBP_L10 + B_SLG_L10) %>%
  mutate(B_ISO_L3 = B_SLG_L3 - B_AVG_L3) %>%
  mutate(B_ISO_L5 = B_SLG_L5 - B_AVG_L5) %>%
  mutate(B_ISO_L10 = B_SLG_L10 - B_AVG_L10) %>%
  group_by(person.key, P_HAND) %>%
  mutate(person.index = 1:n()) %>%
  mutate(B_FP_AVG_VH = lag(rollapplyr(B_FP, person.index, mean, fill = NA))) %>%
  mutate(B_AVG_VH = lag(rollapplyr(B_H, person.index, sum, fill = NA)) / lag(rollapplyr(B_AB, person.index, sum, fill = NA))) %>%
  mutate(B_WOBA_VH = (0.69*lag(rollapplyr(B_BB, person.index, sum, fill = NA)) + 0.719*lag(rollapplyr(B_HP, person.index, sum, fill = NA)) + 0.87*lag(rollapplyr(B_1B, person.index, sum, fill = NA)) + 1.217*lag(rollapplyr(B_2B, person.index, sum, fill = NA)) + 1.529*lag(rollapplyr(B_3B, person.index, sum, fill = NA)) + 1.94*lag(rollapplyr(B_HR, person.index, sum, fill = NA)))/ (lag(rollapplyr(B_AB, person.index, sum, fill = NA)) + lag(rollapplyr(B_BB, person.index, sum, fill = NA)) - lag(rollapplyr(B_IBB, person.index, sum, fill = NA)) + lag(rollapplyr(B_SF, person.index, sum, fill = NA)) + lag(rollapplyr(B_HP, person.index, sum, fill = NA)))) %>%
  mutate(B_SLG_VH = lag(rollapplyr(B_TB, person.index, sum, fill = NA)) / lag(rollapplyr(B_AB, person.index, sum, fill = NA))) %>%
  mutate(B_OBP_VH = (lag(rollapplyr(B_H, person.index, sum, fill = NA)) + lag(rollapplyr(B_BB, person.index, sum, fill = NA)) + lag(rollapplyr(B_HP, person.index, sum, fill = NA)))/ (lag(rollapplyr(B_AB, person.index, sum, fill = NA)) + lag(rollapplyr(B_BB, person.index, sum, fill = NA)) + lag(rollapplyr(B_SF, person.index, sum, fill = NA)) + lag(rollapplyr(B_HP, person.index, sum, fill = NA)))) %>%
  mutate(B_OPS_VH = B_OBP + B_SLG) %>%
  mutate(B_ISO_VH = B_SLG - B_AVG) %>%
  mutate(B_FP_L3_VH = rollapplyr(B_FP,list(-(3:1)),mean,fill=NA)) %>%
  mutate(B_FP_L5_VH = rollapplyr(B_FP,list(-(5:1)),mean,fill=NA)) %>%
  mutate(B_FP_L10_VH = rollapplyr(B_FP,list(-(10:1)),mean,fill=NA)) %>%
  mutate(B_AVG_L3_VH = rollapplyr(B_H, list(-(3:1)), sum, fill = NA) / rollapplyr(B_AB, list(-(3:1)), sum, fill = NA)) %>%
  mutate(B_AVG_L5_VH = rollapplyr(B_H, list(-(5:1)), sum, fill = NA) / rollapplyr(B_AB, list(-(5:1)), sum, fill = NA)) %>%
  mutate(B_AVG_L10_VH = rollapplyr(B_H, list(-(10:1)), sum, fill = NA) / rollapplyr(B_AB, list(-(10:1)), sum, fill = NA)) %>%
  mutate(B_WOBA_L3_VH = (0.69*rollapplyr(B_BB, list(-(3:1)), sum, fill = NA) + 0.719*rollapplyr(B_HP, list(-(3:1)), sum, fill = NA) + 0.87*rollapplyr(B_1B, list(-(3:1)), sum, fill = NA) + 1.217*rollapplyr(B_2B, list(-(3:1)), sum, fill = NA) + 1.529*rollapplyr(B_3B, list(-(3:1)), sum, fill = NA) + 1.94*rollapplyr(B_HR, list(-(3:1)), sum, fill = NA)) / (rollapplyr(B_AB, list(-(3:1)), sum, fill = NA) + rollapplyr(B_BB, list(-(3:1)), sum, fill = NA) - rollapplyr(B_IBB, list(-(3:1)), sum, fill = NA) + rollapplyr(B_SF, list(-(3:1)), sum, fill = NA) + rollapplyr(B_HP, list(-(3:1)), sum, fill = NA))) %>%
  mutate(B_WOBA_L5_VH = (0.69*rollapplyr(B_BB, list(-(5:1)), sum, fill = NA) + 0.719*rollapplyr(B_HP, list(-(5:1)), sum, fill = NA) + 0.87*rollapplyr(B_1B, list(-(5:1)), sum, fill = NA) + 1.217*rollapplyr(B_2B, list(-(5:1)), sum, fill = NA) + 1.529*rollapplyr(B_3B, list(-(5:1)), sum, fill = NA) + 1.94*rollapplyr(B_HR, list(-(5:1)), sum, fill = NA)) / (rollapplyr(B_AB, list(-(5:1)), sum, fill = NA) + rollapplyr(B_BB, list(-(5:1)), sum, fill = NA) - rollapplyr(B_IBB, list(-(5:1)), sum, fill = NA) + rollapplyr(B_SF, list(-(5:1)), sum, fill = NA) + rollapplyr(B_HP, list(-(5:1)), sum, fill = NA))) %>%
  mutate(B_WOBA_L10_VH = (0.69*rollapplyr(B_BB, list(-(10:1)), sum, fill = NA) + 0.719*rollapplyr(B_HP, list(-(10:1)), sum, fill = NA) + 0.87*rollapplyr(B_1B, list(-(10:1)), sum, fill = NA) + 1.217*rollapplyr(B_2B, list(-(10:1)), sum, fill = NA) + 1.529*rollapplyr(B_3B, list(-(10:1)), sum, fill = NA) + 1.94*rollapplyr(B_HR, list(-(10:1)), sum, fill = NA)) / (rollapplyr(B_AB, list(-(10:1)), sum, fill = NA) + rollapplyr(B_BB, list(-(10:1)), sum, fill = NA) - rollapplyr(B_IBB, list(-(10:1)), sum, fill = NA) + rollapplyr(B_SF, list(-(10:1)), sum, fill = NA) + rollapplyr(B_HP, list(-(10:1)), sum, fill = NA))) %>%
  mutate(B_SLG_L3_VH = rollapplyr(B_TB, list(-(3:1)), sum, fill = NA) / rollapplyr(B_AB, list(-(3:1)), sum, fill = NA)) %>%
  mutate(B_SLG_L5_VH = rollapplyr(B_TB, list(-(5:1)), sum, fill = NA) / rollapplyr(B_AB, list(-(5:1)), sum, fill = NA)) %>%
  mutate(B_SLG_L10_VH = rollapplyr(B_TB, list(-(10:1)), sum, fill = NA) / rollapplyr(B_AB, list(-(10:1)), sum, fill = NA)) %>%
  mutate(B_OBP_L3_VH = (rollapplyr(B_H, list(-(3:1)), sum, fill = NA) + rollapplyr(B_BB, list(-(3:1)), sum, fill = NA) + rollapplyr(B_HP, list(-(3:1)), sum, fill = NA))/ (rollapplyr(B_AB, list(-(3:1)), sum, fill = NA) + rollapplyr(B_BB, list(-(3:1)), sum, fill = NA) + rollapplyr(B_SF, list(-(3:1)), sum, fill = NA) + rollapplyr(B_HP, list(-(3:1)), sum, fill = NA))) %>%
  mutate(B_OBP_L5_VH = (rollapplyr(B_H, list(-(5:1)), sum, fill = NA) + rollapplyr(B_BB, list(-(5:1)), sum, fill = NA) + rollapplyr(B_HP, list(-(5:1)), sum, fill = NA))/ (rollapplyr(B_AB, list(-(5:1)), sum, fill = NA) + rollapplyr(B_BB, list(-(5:1)), sum, fill = NA) + rollapplyr(B_SF, list(-(5:1)), sum, fill = NA) + rollapplyr(B_HP, list(-(5:1)), sum, fill = NA))) %>%
  mutate(B_OBP_L10_VH = (rollapplyr(B_H, list(-(10:1)), sum, fill = NA) + rollapplyr(B_BB, list(-(10:1)), sum, fill = NA) + rollapplyr(B_HP, list(-(10:1)), sum, fill = NA))/ (rollapplyr(B_AB, list(-(10:1)), sum, fill = NA) + rollapplyr(B_BB, list(-(10:1)), sum, fill = NA) + rollapplyr(B_SF, list(-(10:1)), sum, fill = NA) + rollapplyr(B_HP, list(-(10:1)), sum, fill = NA))) %>%
  mutate(B_OPS_L3_VH = B_OBP_L3 + B_SLG_L3) %>%
  mutate(B_OPS_L5_VH = B_OBP_L5 + B_SLG_L5) %>%
  mutate(B_OPS_L10_VH = B_OBP_L10 + B_SLG_L10) %>%
  mutate(B_ISO_L3_VH = B_SLG_L3 - B_AVG_L3) %>%
  mutate(B_ISO_L5_VH = B_SLG_L5 - B_AVG_L5) %>%
  mutate(B_ISO_L10_VH = B_SLG_L10 - B_AVG_L10) %>%
  mutate_if(is.numeric, list(~na_if(., Inf))) %>%
  group_by(person.key) %>%
  mutate(person.index = 1:n()) %>%
  dplyr::select(game.key, game.date, year, site.key, opponent.key, person.key, person.index, slot, seq, B_HOME, B_HAND, B_START,
                person.key_pitcher, P_HAND, B_FP_AVG, B_AVG, B_WOBA, B_SLG, B_OBP, B_OPS, B_ISO, B_FP_L3, B_FP_L5, B_FP_L10,
                B_AVG_L3, B_AVG_L5, B_AVG_L10, B_WOBA_L3, B_WOBA_L5, B_WOBA_L10, B_SLG_L3, B_SLG_L5, B_SLG_L10, B_OBP_L3, B_OBP_L5, B_OBP_L10, B_OPS_L3,
                B_OPS_L5, B_OPS_L10, B_ISO_L3, B_ISO_L5, B_ISO_L10, B_FP_AVG_VH, B_AVG_VH, B_WOBA_VH, B_SLG_VH, B_OBP_VH, B_OPS_VH,
                B_ISO_VH, B_FP_L3_VH, B_FP_L5_VH, B_FP_L10_VH, B_FP_L10_VH, B_AVG_L3_VH, B_AVG_L5_VH, B_AVG_L10_VH, B_WOBA_L3_VH, 
                B_WOBA_L5_VH, B_WOBA_L10_VH, B_SLG_L3_VH, B_SLG_L5_VH, B_SLG_L10_VH, B_OBP_L3_VH, B_OBP_L5_VH, B_OBP_L10_VH, 
                B_OPS_L3_VH, B_OPS_L5_VH, B_OPS_L10_VH, B_ISO_L3_VH, B_ISO_L5_VH, B_ISO_L10_VH)
  


# Pitchers Game Logs
PITCHERS_LOG <- MergeFun(path = paste(base, "/retrosplits/daybyday",sep = ""), key = "playing-") %>%
  mutate(P_START = F_P_GS) %>%
  filter(P_G==1, P_START ==1) %>%
  mutate(P_POS = "P") %>%
  rename(P_HOME = team.alignment) %>%
  dplyr::select(any_of(keep), starts_with("P_"), F_P_GS) %>%
  mutate(game.date = as.Date(game.date, "%Y-%m-%d")) %>%
  mutate(year = as.character(substr(game.date, 0, 4))) %>%
  group_by(person.key) %>%
  left_join(PITCHERS_HAND, by = c("person.key", "year")) %>%
  mutate(person.index = 1:n()) %>%
  mutate(P_ER_9I = P_ER*(27/P_OUT)) %>%
  mutate(P_ER_9I = ifelse(is.infinite(P_ER_9I), NA, P_ER_9I)) %>%
  mutate(P_1B = P_H - P_2B - P_3B - P_HR) %>%
  mutate(P_CG = ifelse(P_OUT==27, 1, 0)) %>%
  mutate(P_CGSO = ifelse(P_OUT==27 & P_ER==0, 1, 0)) %>%
  mutate(P_NH = ifelse(P_OUT==27 & P_H==0, 1, 0)) %>%
  mutate(P_FP = 2.25*(P_OUT/3) + 2*(P_SO) + 4*(P_W) - 2*(P_ER) - 0.6*(P_H) - 0.6*(P_BB) - 0.6*(P_HP) + 2.5*(P_CG) + 2.5*(P_CGSO) + 5*(P_NH)) %>%
  mutate(P_FP_AVG = lag(rollapplyr(P_FP, person.index, mean, fill = NA))) %>%
  mutate(P_FP_L3 = rollapplyr(P_FP,list(-(3:1)),mean,fill=NA)) %>%
  mutate(P_FP_L5 = rollapplyr(P_FP,list(-(5:1)),mean,fill=NA)) %>%
  mutate(P_FP_L10 = rollapplyr(P_FP,list(-(10:1)),mean,fill=NA)) %>%
  mutate(P_WOBA = (0.69*lag(rollapplyr(P_BB, person.index, sum, fill = NA)) + 0.719*lag(rollapplyr(P_HP, person.index, sum, fill = NA)) + 0.87*lag(rollapplyr(P_1B, person.index, sum, fill = NA)) + 1.217*lag(rollapplyr(P_2B, person.index, sum, fill = NA)) + 1.529*lag(rollapplyr(P_3B, person.index, sum, fill = NA)) + 1.94*lag(rollapplyr(P_HR, person.index, sum, fill = NA)))/ (lag(rollapplyr(P_AB, person.index, sum, fill = NA)) + lag(rollapplyr(P_BB, person.index, sum, fill = NA)) - lag(rollapplyr(P_IBB, person.index, sum, fill = NA)) + lag(rollapplyr(P_SF, person.index, sum, fill = NA)) + lag(rollapplyr(P_HP, person.index, sum, fill = NA)))) %>%
  mutate(P_WOBA_L3 = (0.69*rollapplyr(P_BB, list(-(3:1)), sum, fill = NA) + 0.719*rollapplyr(P_HP, list(-(3:1)), sum, fill = NA) + 0.87*rollapplyr(P_1B, list(-(3:1)), sum, fill = NA) + 1.217*rollapplyr(P_2B, list(-(3:1)), sum, fill = NA) + 1.529*rollapplyr(P_3B, list(-(3:1)), sum, fill = NA) + 1.94*rollapplyr(P_HR, list(-(3:1)), sum, fill = NA)) / (rollapplyr(P_AB, list(-(3:1)), sum, fill = NA) + rollapplyr(P_BB, list(-(3:1)), sum, fill = NA) - rollapplyr(P_IBB, list(-(3:1)), sum, fill = NA) + rollapplyr(P_SF, list(-(3:1)), sum, fill = NA) + rollapplyr(P_HP, list(-(3:1)), sum, fill = NA))) %>%
  mutate(P_WOBA_L5 = (0.69*rollapplyr(P_BB, list(-(5:1)), sum, fill = NA) + 0.719*rollapplyr(P_HP, list(-(5:1)), sum, fill = NA) + 0.87*rollapplyr(P_1B, list(-(5:1)), sum, fill = NA) + 1.217*rollapplyr(P_2B, list(-(5:1)), sum, fill = NA) + 1.529*rollapplyr(P_3B, list(-(5:1)), sum, fill = NA) + 1.94*rollapplyr(P_HR, list(-(5:1)), sum, fill = NA)) / (rollapplyr(P_AB, list(-(5:1)), sum, fill = NA) + rollapplyr(P_BB, list(-(5:1)), sum, fill = NA) - rollapplyr(P_IBB, list(-(5:1)), sum, fill = NA) + rollapplyr(P_SF, list(-(5:1)), sum, fill = NA) + rollapplyr(P_HP, list(-(5:1)), sum, fill = NA))) %>%
  mutate(P_WOBA_L10 = (0.69*rollapplyr(P_BB, list(-(10:1)), sum, fill = NA) + 0.719*rollapplyr(P_HP, list(-(10:1)), sum, fill = NA) + 0.87*rollapplyr(P_1B, list(-(10:1)), sum, fill = NA) + 1.217*rollapplyr(P_2B, list(-(10:1)), sum, fill = NA) + 1.529*rollapplyr(P_3B, list(-(10:1)), sum, fill = NA) + 1.94*rollapplyr(P_HR, list(-(10:1)), sum, fill = NA)) / (rollapplyr(P_AB, list(-(10:1)), sum, fill = NA) + rollapplyr(P_BB, list(-(10:1)), sum, fill = NA) - rollapplyr(P_IBB, list(-(10:1)), sum, fill = NA) + rollapplyr(P_SF, list(-(10:1)), sum, fill = NA) + rollapplyr(P_HP, list(-(10:1)), sum, fill = NA))) %>%
  mutate(P_WOBA = ifelse(is.infinite(P_WOBA), NA, P_WOBA)) %>%
  mutate(P_ERA = lag(rollapplyr(P_ER_9I, person.index, mean, fill = NA))) %>%
  mutate(P_ERA_L3 = rollapplyr(P_ER_9I, list(-(3:1)), mean, fill = NA)) %>%
  mutate(P_ERA_L5 = rollapplyr(P_ER_9I, list(-(5:1)), mean, fill = NA)) %>%
  mutate(P_ERA_L10 = rollapplyr(P_ER_9I, list(-(10:1)), mean, fill = NA)) %>%
  mutate(P_WHIP =  (lag(rollapplyr(P_BB, person.index, sum, fill = NA)) + lag(rollapplyr(P_H, person.index, sum, fill = NA))) / lag(rollapplyr(P_OUT/3, person.index, sum, fill = NA))) %>%
  mutate(P_WHIP_L3 =  (rollapplyr(P_BB, list(-(3:1)), sum, fill = NA) + rollapplyr(P_H, list(-(3:1)), sum, fill = NA)) / rollapplyr(P_OUT/3, list(-(3:1)), sum, fill = NA)) %>%
  mutate(P_WHIP_L5 =  (rollapplyr(P_BB, list(-(5:1)), sum, fill = NA) + rollapplyr(P_H, list(-(5:1)), sum, fill = NA)) / rollapplyr(P_OUT/3, list(-(5:1)), sum, fill = NA)) %>%
  mutate(P_WHIP_L10 =  (rollapplyr(P_BB, list(-(10:1)), sum, fill = NA) + rollapplyr(P_H, list(-(10:1)), sum, fill = NA)) / rollapplyr(P_OUT/3, list(-(10:1)), sum, fill = NA)) %>%
  mutate(P_SW =  lag(rollapplyr(P_SO, person.index, sum, fill = NA)) / lag(rollapplyr(P_BB, person.index, sum, fill = NA))) %>%
  mutate(P_SW_L3 =  rollapplyr(P_SO, list(-(3:1)), sum, fill = NA) / rollapplyr(P_BB, list(-(3:1)), sum, fill = NA)) %>%
  mutate(P_SW_L5 =  rollapplyr(P_SO, list(-(5:1)), sum, fill = NA) / rollapplyr(P_BB, list(-(5:1)), sum, fill = NA)) %>%
  mutate(P_SW_L10 =  rollapplyr(P_SO, list(-(10:1)), sum, fill = NA) / rollapplyr(P_BB, list(-(10:1)), sum, fill = NA)) %>%
  mutate_if(is.numeric, list(~na_if(., Inf))) %>%
  dplyr::select(game.key, game.date, year, site.key, opponent.key, person.key, person.index, P_HOME, P_START, P_HAND,
         P_FP_AVG, P_FP_L3, P_FP_L5, P_FP_L10, P_WOBA, P_WOBA_L3, P_WOBA_L5, P_WOBA_L10, P_ERA, P_ERA_L3, P_ERA_L5, 
         P_ERA_L10, P_WHIP, P_WHIP_L3, P_WHIP_L5, P_WHIP_L10, P_SW, P_SW_L3, P_SW_L5, P_SW_L10)
  

# writing batter and pitcher dataframes
write.csv(BATTERS_LOG, paste(base, "/Datasets/BATTERS.csv", sep = ""), row.names = F)
write.csv(PITCHERS_LOG, paste(base, "/Datasets/PITCHERS.csv", sep = ""), row.names = F)

