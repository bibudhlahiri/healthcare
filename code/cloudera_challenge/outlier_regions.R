library(reshape2)
library(ggplot2)
library(plyr)

create_data <- function()
{
  inpatient <- read.csv("/Users/blahiri/healthcare/data/cloudera_challenge/Inpatient_Data_2012_CSV/Medicare_Provider_Charge_Inpatient_DRG100_FY2012.csv")
  inpatient <- inpatient[, c("DRG.Definition", "Provider.Id", "Provider.Name", "Hospital.Referral.Region..HRR..Description", "Average.Covered.Charges")]
  colnames(inpatient) <- c("proc", "prov_id", "prov_name", "region", "avg_charge")
  
  outpatient <- read.csv("/Users/blahiri/healthcare/data/cloudera_challenge/Outpatient_Data_2012_CSV/Medicare_Provider_Charge_Outpatient_APC30_CY2012.csv")
  outpatient <- outpatient[, c("APC", "Provider.Id", "Provider.Name", "Hospital.Referral.Region..HRR..Description", "Average..Estimated.Submitted.Charges")]
  colnames(outpatient) <- c("proc", "prov_id", "prov_name", "region", "avg_charge")

  #Each combination of provider and procedure occurs exactly once. Group by region so that each combination of region and procedure occurs exactly once.
  all_data <- rbind(inpatient, outpatient)
  cat(paste("nrow(inpatient) = ", nrow(inpatient), ", nrow(outpatient) = ", nrow(outpatient), ", nrow(all_data) = ", nrow(all_data), "\n", sep = ""))

  aggdata <- aggregate(x = all_data$avg_charge, by = list(all_data$region, all_data$proc), FUN = mean, na.rm = TRUE) 
  colnames(aggdata) <- c("region", "proc", "avg_charge")

  data.wide <- dcast(aggdata, region ~ proc, value.var = "avg_charge")
  data.wide[is.na(data.wide)] <- 0
  data.wide
}

principal_component <- function()
{
  data.wide <- create_data()
  regions <- data.wide$region
  data.wide <- data.wide[,!(names(data.wide) %in% c("region"))]
  rownames(data.wide) <- regions
  
  pc <- prcomp(data.wide, scale = TRUE)
  projected <- pc$x[, c("PC1", "PC2")]

  png("./figures/regions_first_two_pc.png",  width = 600, height = 480, units = "px")
  projected <- data.frame(projected)
  p <- ggplot(projected, aes(x = PC1, y = PC2)) + geom_point(size = 2) + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold')) + 
         ggtitle("Projections along first two PCs for regions")
  print(p)
  dev.off()

  #This gives 3 regions
  outliers <- subset(projected, (PC1 <= -30))
  print(outliers)
  pc
}


compare_outliers_by_pc_with_rest <- function(outlier = "CA - San Jose")
{
  #Compare the average charge for different procedures by the outlier to the 
  #median average charge taken over all the remanining regions for the different procedures

  data.wide <- create_data()
  regions <- data.wide$region
  data.wide <- data.wide[,!(names(data.wide) %in% c("region"))]
  rownames(data.wide) <- regions

  rem_data <- data.wide[-which(rownames(data.wide) == outlier), ]
  medians_of_rem_regions <- apply(rem_data, 2, median) 

  data_for_plots <- rbind(data.frame(procedures = names(medians_of_rem_regions), charge = as.numeric(medians_of_rem_regions), region = "remaining"), 
                          data.frame(procedures = names(data.wide[outlier, ]), charge = as.numeric(data.wide[outlier, ]), region = "outlier"))
  data_for_plots$procedures <- substr(data_for_plots$procedures, 1, as.numeric(regexpr("-", data_for_plots$procedures)) - 2)
  data_for_plots$procedures <- factor(data_for_plots$procedures, 
                              levels = data_for_plots$procedures,
                              ordered = TRUE)

  filename <- paste("./figures/outlier_", outlier, "_vs_rest.png", sep = "")
  png(file = filename, width = 1700, height = 600)
  p <- ggplot(data_for_plots, aes(x = procedures, y = charge, fill = region)) + geom_bar(position="dodge", stat = "identity") + 
         theme(axis.text = element_text(colour = 'blue', size = 10, face = 'bold')) +
         theme(axis.text.x = element_text(angle = 90)) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
  print(p)
  aux <- dev.off()
}

#Angle-based outlier degree (Kriegel et al. 2008)
ABOD_naive <- function()
{
  data.wide <- create_data()
  regions <- data.wide$region
  data.wide <- data.wide[,!(names(data.wide) %in% c("region"))]
  rownames(data.wide) <- regions
  abod_values <- data.frame(matrix(0, ncol = 2, nrow = length(regions)))
  abod_values$regions <- regions
  loopc <- 0

  for (p in regions)
  {
    angles_between_pairs <- c()
    for (x in regions[regions != p])
    {
      for (y in regions[!regions %in% c(p, x)])
      {
        px <- data.wide[x, ] - data.wide[p, ]
        py <- data.wide[y, ] - data.wide[p, ]
        theta <- acos(sum(px*py)/(sqrt(sum(px*px))*sqrt(sum(py*py))))
        cat(paste("p = ", p, ", x = ", x, ", y = ", y, ", theta = ", theta, "\n", sep = ""))
        angles_between_pairs <- c(angles_between_pairs, theta)
      }
    }
    loopc <- loopc + 1
    #if (loopc %% 5 == 0)
    #{
      cat(paste("loopc = ", loopc, ", ", Sys.time(), "\n", sep = ""))
    #}
    abod_values[loopc, "abod"] <- var(angles_between_pairs)
  }
  abod_values
}

