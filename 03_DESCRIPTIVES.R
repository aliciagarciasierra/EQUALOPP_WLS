
###################################################################################
###################### DESCRIPTIVES  ###########################################
###################################################################################

####################################################################
################## FIGURE 1: HEXBIN CHART ##############################
####################################################################

# Reshape to wide at the family level
wide<-reshape(as.data.frame(siblings),direction="wide", idvar="familyID", timevar="withinID")

# PGI Variables
pgis <- c("pgi_education", "pgi_cognitive", "pgi_math_exam", "pgi_math_ability",  
          "pgi_depression", "pgi_well_being", "pgi_neuroticism")

# Loop to generate the plots
sapply(pgis, function(pgi){
  # Dynamically create the column names for sibling 1 and sibling 2
  x_var <- paste0(pgi, ".1")
  y_var <- paste0(pgi, ".2")
  
  # Create the hexbin plot for each PGI variable
  hexbin <- ggplot(wide, aes_string(x=x_var, y=y_var)) +
    geom_hex(bins = 80) +
    geom_abline(color="darkgoldenrod1", linetype="dashed", size=0.3) +
    theme_bw() +
    scale_x_continuous(breaks=c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4)) +
    scale_y_continuous(breaks=c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4)) +
    scale_fill_continuous(type = "viridis") +
    xlab("Sibling 1") +
    ylab("Sibling 2") +
    theme(axis.title.y = element_text(size = 11, angle = 90)) +
    theme(axis.title.x = element_text(size = 11, angle = 00)) +
    theme(legend.title = element_text(color = "black", size = 8),
          legend.text = element_text(color = "black", size = 7)) +
    ggtitle(paste("Hexbin chart: differences in the", gsub("pgi_", "", pgi), "PGI between siblings")) +
    theme(plot.title = element_text(size=12, face="bold"))
  
  # Display the plot with the correct fill label
  print(hexbin + labs(fill = "Count"))
})




