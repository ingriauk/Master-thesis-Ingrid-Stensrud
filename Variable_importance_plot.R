library(ggplot2)
library(reshape2)

# VARIMP <-readRDS("varimp.rds")
# data <- readRDS("Completed_military_training_data.rds")

var_names <- colnames(data) # Get names in data
var_names <- var_names[var_names != "FULLFORT_RS"] # Remove the response variable from var_names
rownames(VARIMP) <- var_names # Assign these names to the rows of VARIMP matrix
varimp_df_median <- as.data.frame(VARIMP) # Make dataframe
names(varimp_df_median) <- paste0("Fold", 1:ncol(VARIMP))
varimp_df_median$Variable <- rownames(VARIMP)
varimp_df_median$MedianImportance <- apply(varimp_df_median, 1, median, na.rm = TRUE) # Make column with median variable importance
varimp_df_median$MedianImportance <- as.numeric(varimp_df_median$MedianImportance)
varimp_df_median <- varimp_df_median[order(-varimp_df_median$MedianImportance), ] # Decreasing order

# Make table of variable importance
varimp_table <- varimp_df_median[, c("Variable", "MedianImportance")] # Create new dataframe with only variable names and median importance
varimp_table$MedianImportance <- round(varimp_table$MedianImportance, 6)
write.xlsx(varimp_table, "varimp_table.xlsx", rowNames = FALSE) # Write the data frame to an Excel file

# Reshape the data to long format
varimp_long <- melt(varimp_df_median, id.vars = "Variable", variable.name = "Fold", value.name = "Importance")

##### Variable Importance plot with variables sorted by the median ####
# Plot without everything below HAR_BLITT_OPERERT (surgery)

# ADHD_ADD is next in line after HAR_BLITT_OPERERT, so remove ADHD_ADD and everything below.
which(varimp_long$Variable == 'ADHD_ADD')
varimp_long[1056,] # = 0.0003211485. To find the MedianImportance of ADHD_ADD and later remove everything below

varimp_long$Fold <- as.character(varimp_long$Fold)
delete <- varimp_long[varimp_long$Fold == "MedianImportance" & varimp_long$Importance <= 0.0003211485,]$Variable # Deleting everything below surgery
varimp_long_surgery <- varimp_long[!varimp_long$Variable %in% delete,]

##### Change name on variables to english ####
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'ONSKER_TJENESTE_I_FORSVARET'] <- 'Motivation'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'PASSER_TIL_TJENESTE'] <- 'Self-perceived suitability'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'HVOR_OFTE_FRILUFTSLIV'] <- 'Annual frequency of outdoor activities'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'HVOR_OFTE_TRENER_DU'] <- 'Weekly workout frequency'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'SKOLEPOENG_BEREGNET2'] <- 'GPA'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'SCORE_SOSIAL'] <- 'Social life'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'PROBLEMATISK_AA_SOVE_BORTE'] <- 'Has trouble sleeping in new places'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'KARAKTER_STYRKETEST_EGENERKLARING'] <- 'Physical strenght'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'HEMMENDE_ANGST_URO'] <- 'Inhibiting anxiety or depression'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'KARAKTER_3000_M_EGENERKLARING'] <- 'Endurance 3000 meters'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'estability'] <- 'Neuroticism'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'agreeableness'] <- 'Agreeableness'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'sex'] <- 'Gender'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'KOMMER_GODT_OVERENS_MED_ANDRE'] <- 'Gets along with others'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'conscientiousness'] <- 'Conscientiousness'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'TRIVES_MED_MEDELEVER'] <- 'Enjoys being with classmates'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'HAANDTERER_STRESS_OG_FRISTER_BRA'] <- 'Handles stress and deadlines well'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'c7'] <- 'Always meets prepared'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'HUDSYKDOMMER'] <- 'Skin diseases'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'a10'] <- 'Trusts others'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'cekstraledd1'] <- 'Completes tasks halfheartedly'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'n2'] <- 'Worries a lot'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'o2'] <- 'Wants to selfdevelope'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'n9'] <- 'Feels tense often'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'ER_DU_BLITT_OPERERT'] <- 'Has undergone surgery'
varimp_long_surgery$Variable[varimp_long_surgery$Variable == 'ADHD_ADD'] <- 'ADHD or ADD'

##### Plot ####
# Create red line 
#most_negative_value <- min(varimp_long$Importance)  # If using rule of thumb for variable importance by Strobl (2009)
#most_negative_value_flipped <- abs(most_negative_value)

p <- ggplot(varimp_long_surgery, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_boxplot(color = "black", fill = "grey", alpha = 0.7) +
  labs(x = " ", y = " ") +
  coord_flip() +
  ylim(0.00,0.003) +
  geom_vline(aes(xintercept=0), color="black") +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(colour = "black", size = 12),
        axis.text.y = element_text(colour = "black", size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey", linewidth = 0.2),
        panel.grid.minor = element_line(colour = "grey", linewidth = 0.5),
        axis.line = element_line(colour = "black", linewidth = 0.2))


# Print the plot
print(p)