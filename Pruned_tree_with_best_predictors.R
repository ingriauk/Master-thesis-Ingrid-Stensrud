library(party)
library(partykit)
# data <- readRDS("ompleted_military_training_data.rds")

# Change the names and order of levels
levels(data$ONSKER_TJENESTE_I_FORSVARET) <- c("Agree", "Somewhat disagree", "Somewhat agree", "Disagree", "Don't know")

# Reorder the levels
data$ONSKER_TJENESTE_I_FORSVARET <- factor(data$ONSKER_TJENESTE_I_FORSVARET, 
                                           levels = c("Disagree", "Somewhat disagree", "Don't know", "Somewhat agree", "Agree"))
levels(data$ONSKER_TJENESTE_I_FORSVARET) <- c("D", "SD", "DK", "SA", "A")
names(data)[names(data) == "ONSKER_TJENESTE_I_FORSVARET"] <- "Motivated"

# Change the names and order of levels
levels(data$PASSER_TIL_TJENESTE) <- c("Agree", "Somewhat disagree", "Somewhat agree", "Disagree", "Don't know")

# Reorder the levels
data$PASSER_TIL_TJENESTE <- factor(data$PASSER_TIL_TJENESTE, 
                                           levels = c("Disagree", "Somewhat disagree", "Don't know", "Somewhat agree", "Agree"))
levels(data$PASSER_TIL_TJENESTE) <- c("D", "SD", "DK", "SA", "A")

# Change name on variable 
names(data)[names(data) == "PASSER_TIL_TJENESTE"] <- "Suitable"

# Change name on variable 
names(data)[names(data) == "SKOLEPOENG_BEREGNET2"] <- "GPA"

# Make GPA actual value
data$GPA <- round(data$GPA / 10, 2)

# Change the names and order of levels
levels(data$HVOR_OFTE_TRENER_DU) <- c("1-2", "3-4", "5 =<", "<1")

# Reorder the levels
data$HVOR_OFTE_TRENER_DU <- factor(data$HVOR_OFTE_TRENER_DU, 
                                   levels = c("<1", "1-2", "3-4", "5 =<"))

# Change name on variable 
names(data)[names(data) == "HVOR_OFTE_TRENER_DU"] <- "Weekly_workout_frequency"

# Specify the formula
formula <- FULLFORT_RS ~ Motivated + 
  Suitable + 
  HVOR_OFTE_FRILUFTSLIV + 
  Weekly_workout_frequency + 
  GPA + 
  SCORE_SOSIAL + 
  PROBLEMATISK_AA_SOVE_BORTE + 
  KARAKTER_STYRKETEST_EGENERKLARING + 
  HEMMENDE_ANGST_URO

# Specify controls
controls <- ctree_control(maxdepth = 3,
                          testtype = "Bonferroni")

# Make tree
tree1 <- ctree(formula, data = data, control = controls)

# Make plot
plot(tree1, type = "extended",
     inner_panel = node_inner(tree1, id = FALSE, gp = gpar(fontsize = 11)))