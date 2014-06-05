library(reshape2)
library(ggplot2)
library(plyr)

create_data <- function()
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
  data.wide
}

principal_component <- function()
{
  data.wide <- create_data()
  prov_ids <- data.wide$prov_id
  prov_id_and_names <- data.wide[, c("prov_id", "prov_name")]
  data.wide <- data.wide[,!(names(data.wide) %in% c("prov_id", "prov_name"))]
  rownames(data.wide) <- prov_ids
  
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

  #This gives 16 providers who are located in CA, NJ and PA
  outliers <- subset(projected, (PC1 <= -30))
  outliers$prov_id <- rownames(outliers)
  outliers <- merge(x = outliers, y = prov_id_and_names, all.x = TRUE)
  print(outliers)
  
  #Look into the rotation matrix (loadings) to see why are the outliers outliers
  #For provider 50441, score on PC1 is -46.326151. Why?
  data.wide <- scale(data.wide)
  prov_50441 <- data.wide["50441", ]
  first_pc <- pc$rotation[, "PC1"]
  cat("\nprov_50441 is\n")
  print(prov_50441)
  #print(as.numeric(prov_50441) %*% as.numeric(first_pc))  #matches -46.326151

  cat("\nfirst_pc in sorted order is\n")
  sorted_first_pc <- sort(first_pc)
  print(sorted_first_pc)
  #Get the numbers from prov_50441 in the order given by sorted_first_pc
  cat("\nprov_50441 in the order given by sorted_first_pc is\n")
  ordered_prov_50441 <- prov_50441[names(sorted_first_pc)]
  print(ordered_prov_50441)
 
  data_for_plot <- data.frame(matrix(0, ncol = 2, nrow = length(ordered_prov_50441)))
  data_for_plot$procedure <- names(ordered_prov_50441)
  data_for_plot$procedure <- substr(data_for_plot$procedure, 1, as.numeric(regexpr("-", data_for_plot$procedure)) - 2) 
  data_for_plot$scaled_avg_expense <- as.numeric(ordered_prov_50441)

  data_for_plot$procedure <- factor(data_for_plot$procedure, 
                              levels = data_for_plot$procedure,
                              ordered = TRUE)

  png(file = "./figures/prov_50441.png", width = 800, height = 600)
  p <- ggplot(data_for_plot, aes(x = procedure, y = scaled_avg_expense)) + geom_bar() + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.text.x = element_text(angle = 90)) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold'))
  print(p)
  aux <- dev.off()
 
  ordered_prov_50441
}

#Find the distance with the k-th nearest neighbor for each point (provider). List the ones for which this distance is highest.
compute_knn_distances <- function()
{
  library(foreach)
  library(doMC)
  registerDoMC(8)
  library(multicore)

  all_data <- create_data()
  prov_ids <- all_data$prov_id
  all_data <- all_data[,!(names(all_data) %in% c("prov_id", "prov_name"))]
  rownames(all_data) <- prov_ids

  cat(sprintf('Running with %d worker(s)\n', getDoParWorkers()))
  (name <- getDoParName())
  (ver <- getDoParVersion())
  if (getDoParRegistered())
   cat(sprintf('Currently using %s [%s]\n', name, ver))

  data.dist = dist(all_data, method = "euclidean")

  n <- attr(data.dist, "Size")
  cat(paste("n = ", n, "\n", sep = ""))
  dist_to_kNN <- data.frame()
  labels <- attr(data.dist, "Labels")
  
  #for (i in 1:n)
  dist_to_kNN <- foreach (i=1:n, .combine = rbind) %dopar%
  {
    #Get the distances to all other points
    cat(paste("i = ", i, ", time = ", Sys.time(), "\n", sep = ""))
    distances <- c()
    for (j in 1:n)
    {
      if (j != i)
      {
        if (j > i)
        {
          #The original formula is meant to give correct result for i > j
          dist <- data.dist[n*(i-1) - i*(i-1)/2 + j-i]
        }
        else if (j < i)
        {
          dist <- data.dist[n*(j-1) - j*(j-1)/2 + i-j]
        }
        distances <- append(distances, dist)
      }
    }
    distances <- sort(distances)
    #Have to retrieve the provider IDs. Currently, values in dist_to_kNN$id are between 1 and n.
    dist_to_kNN[i, "id"] <- labels[i] #i
    for (k in 1:20)
    {
      column <- paste("distance_", k, sep = "")
      dist_to_kNN[i, column] <- distances[k]
    }
    if (i %% 10 == 0)
    {
      cat(paste("i = ", i, ", time = ", Sys.time(), "\n", sep = ""))
    }
    dist_to_kNN[i, ]
  } #end for (i in 1:n)
  write.csv(dist_to_kNN, "/Users/blahiri/healthcare/documents/cloudera_challenge/dist_to_kNN.csv")
}


outliers_by_knn <- function(n_potential_outliers = 100)
{
  dist_to_kNN <- read.csv("/Users/blahiri/healthcare/documents/cloudera_challenge/dist_to_kNN.csv")
  dist_to_kNN <- dist_to_kNN[,!(names(dist_to_kNN) %in% c("X"))]
  potential_outliers <- data.frame(matrix(nrow = n_potential_outliers, ncol = 0))
  for (k in 1:20)
  {
    column <- paste("distance_", k, sep = "")
    dist_to_kNN <- dist_to_kNN[order(-dist_to_kNN[, column]),]
    potential_outliers[, column] <- dist_to_kNN[1:n_potential_outliers, "id"]
  }
  write.csv(potential_outliers, "/Users/blahiri/healthcare/documents/cloudera_challenge/potential_outliers.csv")
  outliers <- intersect(potential_outliers$distance_1, potential_outliers$distance_2)
  for (k in 3:20)
  {
    column <- paste("distance_", k, sep = "")
    outliers <- intersect(outliers, potential_outliers[, column])
  }
  outliers <- sort(outliers)
}

find_intersection_of_two_methods <- function()
{
  outliers_by_pc <- principal_component()
  outliers_by_nn <- outliers_by_knn()
  cat("Outliers by the overlap of two methods are\n")
  print(intersect(outliers_by_pc$prov_id, outliers_by_nn))
}


