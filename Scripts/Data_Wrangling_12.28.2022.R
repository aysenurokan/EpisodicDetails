library(dplyr)
library(tidyverse)
library(tidyr)

#Since the data wrangling on 12/27 messed up MPlus analyses, I will add episodic details to the 12/22 df via left_join

#Load episodic details
Episodic <- read.csv("~/Desktop/Past/MSc Thesis/Thesis/Episodic Detail Coding - Final Sample Int_Ext.csv")
Episodic <- Episodic %>% pivot_longer(!ID, names_to = "Cue_Word", values_to = "value") %>% separate(Cue_Word, into = c("Cue_Word", "Detail"), sep="_") %>%
  distinct(ID, Cue_Word, Detail, .keep_all = T)
Episodic <- Episodic %>% pivot_wider(names_from = "Detail", values_from = "value")

#Group by valance
neg <- c("Keyifsiz", "Kizgin", "Sikinti", "Korku", "Kaba", "Perisan")
pos <- c("Nese", "Basari", "Mutlu", "Yetenek", "Huzur", "Durust")
Episodic <- Episodic %>% mutate(Valance = case_when((Cue_Word %in% neg) ~ "Negative",
                                            (Cue_Word %in% pos) ~ "Positive"))

Episodic <- Episodic %>% mutate(trial = case_when(
  Cue_Word == "Nese" | Cue_Word == "Keyifsiz" ~ 1,
  Cue_Word == "Basari" | Cue_Word == "Kizgin" ~ 2,
  Cue_Word == "Mutlu" | Cue_Word == "Sikinti" ~3,
  Cue_Word == "Yetenek" | Cue_Word == "Korku" ~4,
  Cue_Word == "Huzur" | Cue_Word == "Kaba" ~5,
  Cue_Word == "Durust" | Cue_Word == "Perisan" ~6))

Episodic$Valance <- factor(as.character(Episodic$Valance), levels = c("Negative", "Positive"), labels = c("Neg", "Pos"))

#Pivot Wider
Episodic <- Episodic %>% select(-Cue_Word) %>% pivot_wider(names_from = "Valance", values_from = c(Internal, External))

#Rename vars
Episodic <- Episodic %>% rename("Int_Neg" = Internal_Neg, "Int_Pos" = Internal_Pos, "Ext_Neg" = External_Neg, "Ext_Pos" = External_Pos)

#check var types --> lapply turned df into list
sapply(Episodic, class) 
Episodic$ID <- as.numeric(Episodic$ID)
Episodic$Int_Pos <- as.numeric(Episodic$Int_Pos)
Episodic$Int_Neg <- as.numeric(Episodic$Int_Neg)
Episodic$Ext_Pos <- as.numeric(Episodic$Ext_Pos)
Episodic$Ext_Neg <- as.numeric(Episodic$Ext_Neg)

#save Episodic as a separate df
saveRDS(Episodic, file = "~/Desktop/Past/MSc Thesis/Thesis/Episodic_Details.rds")

#merge with 12/22 df
setwd("~/Desktop/Episodic Details Manuscript")
df_22 <- readRDS("df_Mplus_12_22.rds")
Episodic$ID <- factor(as.character(Episodic$ID))
Episodic <- readRDS("~/Desktop/Past/MSc Thesis/Thesis/Episodic_Details.rds")
df_12_28 <- left_join(df_22, Episodic, by = c("ID", "trial"))

#correct for missing data not being 0
for(r in 1:nrow(df_12_28)){
  if(is.na(df_12_28$VP_Pos[r])){
    df_12_28$Int_Pos[r] <- NA
  }
}

for(r in 1:nrow(df_12_28)){
  if(is.na(df_12_28$VP_Pos[r])){
    df_12_28$Ext_Pos[r] <- NA
  }
}

for(r in 1:nrow(df_12_28)){
  if(is.na(df_12_28$VP_Neg[r])){
    df_12_28$Int_Neg[r] <- NA
  }
}

for(r in 1:nrow(df_12_28)){
  if(is.na(df_12_28$VP_Neg[r])){
    df_12_28$Ext_Neg[r] <- NA
  }
}

#save 12/28 df
saveRDS(df_12_28, file = "~/Desktop/Episodic Details Manuscript/df_12_28.rds")

#save df as .dat file
library(MplusAutomation)
setwd("~/Desktop/Episodic Details Manuscript")
df <- readRDS("df_12_28.rds")
#add z-scores 
df <- df %>% mutate(DERS_sc = (DERS - mean(DERS))/sd(DERS)) 
df <- df %>% mutate (Avo_sc = (Avo - mean(Avo, na.rm=T))/sd(Avo, na.rm = T))
df$LNB_sc <- (df$LNB_MEFF - mean(df$LNB_MEFF, na.rm=T))/sd(df$LNB_MEFF, na.rm=T)

#add sqrt transformation for non-scaled values
df$DERS_sq <- sqrt(df$DERS)
df$Avo_sq <- sqrt(df$Avo)
df$LNB_sq <- sqrt(df$LNB_MEFF)

df <- df %>% select(-LNB_log, -AMT_Pos, -AMT_Neg)

prepareMplusData(df, filename = "df_MPlus_12_29.dat", inpfile = FALSE)

#run models
library(MplusAutomation)
setwd("~/Desktop/Episodic Details Manuscript/MSEMs")
runModels(getwd(), replaceOutfile = "always", logFile = "log.txt")


