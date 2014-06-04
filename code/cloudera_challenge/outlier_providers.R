library(reshape2)
library(ggplot2)
library(plyr)

load_prov_proc_data <- function()
{
  inpatient <- read.csv("/Users/blahiri/healthcare/data/cloudera_challenge/Inpatient_Data_2012_CSV/Medicare_Provider_Charge_Inpatient_DRG100_FY2012.csv")
  inpatient <- inpatient[, c("DRG.Definition", "Provider.Id", "Provider.Name", "Average.Covered.Charges")]
  colnames(inpatient) <- c("proc", "prov_id", "prov_name", "avg_charge")
  
  outpatient <- read.csv("/Users/blahiri/healthcare/data/cloudera_challenge/Outpatient_Data_2012_CSV/Medicare_Provider_Charge_Outpatient_APC30_CY2012.csv")
  outpatient <- outpatient[, c("APC", "Provider.Id", "Provider.Name", "Average..Estimated.Submitted.Charges")]
  colnames(outpatient) <- c("proc", "prov_id", "prov_name", "avg_charge")

  #Each combination of provider and procedure occurs exactly once. 3317 providers, 130 procedures. The median number of procedures per provider is 59.
  all_data <- rbind(inpatient, outpatient)
  cat(paste("nrow(inpatient) = ", nrow(inpatient), ", nrow(outpatient) = ", nrow(outpatient), ", nrow(all_data) = ", nrow(all_data), "\n", sep = ""))

  #aggdata <- table(all_data$prov_id)
  data.wide <- dcast(all_data, prov_id + prov_name ~ proc, value.var = "avg_charge")
  data.wide[is.na(data.wide)] <- 0
  prov_ids <- data.wide$prov_id
  prov_id_and_names <- data.wide[, c("prov_id", "prov_name")]
  data.wide <- data.wide[,!(names(data.wide) %in% c("prov_id", "prov_name"))]
  rownames(data.wide) <- prov_ids
  #print(data.wide[1:5, ])
  
  pc <- prcomp(data.wide, scale = TRUE)
  projected <- pc$x[, c("PC1", "PC2")]

  png("./figures/providers_first_two_pc.png",  width = 600, height = 480, units = "px")
  projected <- data.frame(projected)
  p <- ggplot(projected, aes(x = PC1, y = PC2)) + geom_point(size = 2) + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold')) + 
         ggtitle("Projections along first two PCs for providers")
  print(p)
  dev.off()
  pc

  #This gives 16 providers who are located in CA, NJ and PA
  outliers <- subset(projected, (PC1 <= -30))
  outliers$prov_id <- rownames(outliers)
  outliers <- merge(x = outliers, y = prov_id_and_names, all.x = TRUE)
}
