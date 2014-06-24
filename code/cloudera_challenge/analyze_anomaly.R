library(rpart)
library(RPostgreSQL)
library(reshape2)
library(e1071)
library(ggplot2)
library(plyr)


prepare_data <- function()
{
  con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "cloudera_challenge")
  statement <- paste("select p.patient_id, p.age_group, p.gender, p.income_range, 
                             pp.procedure_id, p.is_anomalous
                      from patients p, patient_procedure pp
                      where p.patient_id = pp.patient_id", sep = "")
  res <- dbSendQuery(con, statement)
  pat_proc <- fetch(res, n = -1)
  cat(paste("nrow(pat_proc) = ", nrow(pat_proc), "\n", sep = ""))
  dbDisconnect(con)
  dummy <- rep(1, nrow(pat_proc))
  pat_proc <- cbind(pat_proc, dummy)
  data.wide <- dcast(pat_proc, patient_id + age_group + gender + income_range + is_anomalous ~ procedure_id, value.var = "dummy")
  data.wide[is.na(data.wide)] <- 0
  write.csv(data.wide, "/Users/blahiri/healthcare/data/cloudera_challenge/pat_proc.csv")
  data.wide
}

train_validate_test_rpart <- function()
 {
   set.seed(1)
   pat_proc <- read.csv("/Users/blahiri/healthcare/data/cloudera_challenge/pat_proc.csv")
   for (column in colnames(pat_proc))
   {
     if (column != 'patient_id' & column != 'X')
     {
       pat_proc[, column] <- as.factor(pat_proc[, column])
     }
   }

   #Tried 1 and 0 in 2:1 ratio: the nodes labeled 1 still have =0 on the incoming edges 
   #Tried 1 and 0 in 1:1 ratio: the nodes labeled 1 still have =0 on the incoming edges 
   anom <- subset(pat_proc, (is_anomalous == '1'))
   benign <- subset(pat_proc, (is_anomalous == '0'))
   n_benign <- nrow(benign)
   sample_from_benign <- sample(1:n_benign, n_benign/2)
   pat_proc <- rbind(anom, benign[sample_from_benign, ])
   cat(paste("After sampling, nrow(pat_proc) = ", nrow(pat_proc), ", and distn\n", sep = ""))
   print(table(pat_proc$is_anomalous))

   x <- pat_proc[,!(names(pat_proc) %in% c("patient_id", "is_anomalous", "X"))]
   y <- pat_proc[, "is_anomalous"]
   
   train = sample(1:nrow(pat_proc), 0.5*nrow(pat_proc))
   test = (-train)
   cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(pat_proc) - length(train)), "\n", sep = ""))

   #if (FALSE)
   #{
   str_formula <- "is_anomalous ~ "
   for (column in colnames(x))
   {
     str_formula <- paste(str_formula, column, " + ", sep = "")
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)
   #}
   #str_formula <- "is_anomalous ~ age_group + gender + income_range + X470 + X871 + X291 + X247 + X460 + X853 + X292 + X194 + X329 + X392 + X193 + X190 + X208 + X682 + X287 + 
   #                               X0605 + X0604 + X0606 + X0269 + X0368 + X0607 + X0690 + X0013 + X0015"
   #print(str_formula)
   model <- rpart(as.formula(str_formula), data = pat_proc[train, ])

   #Variables used in the decision tree were: 0605 - Level 2 Hospital Clinic Visits, 0604 - Level 1 Hospital Clinic Visits, 
   #0606 - Level 3 Hospital Clinic Visits, 0269 - Level II Echocardiogram Without Contrast, 0368 - Level II Pulmonary Tests,
   #0607 - Level 4 Hospital Clinic Visits, 0690 - Level I Electronic Analysis of Devices, 0013 - Level II Debridement & Destruction,
   #0015 - Level III Debridement & Destruction. These are the features that make the classification highly accurate. However, these
   #are not procedures that anomalous patients have done most frequently. 
   pred <- predict(model, newdata = pat_proc[test,], type = "prob")
   pat_proc[test, "predicted_prob_anomalous"] <- pred[, "1"]
   pat_proc[test, "predicted_is_anomalous"] <- ifelse(pat_proc[test, "predicted_prob_anomalous"] >= 0.5, '1', '0')
   print(table(pat_proc[test,"is_anomalous"], pat_proc[test, "predicted_is_anomalous"], dnn = list('actual', 'predicted')))
   #pred
   model
 }

anom_with_lr <- function()
{
  set.seed(1)
  pat_proc <- read.csv("/Users/blahiri/healthcare/data/cloudera_challenge/pat_proc.csv")
  for (column in colnames(pat_proc))
  {
    if (column != 'patient_id' & column != 'X')
     {
       pat_proc[, column] <- as.factor(pat_proc[, column])
     }
  }
  pat_proc <- pat_proc[,!(names(pat_proc) %in% c("X"))]

  anom <- subset(pat_proc, (is_anomalous == '1'))
  benign <- subset(pat_proc, (is_anomalous == '0'))
  n_benign <- nrow(benign)
  
  #Take a random sample of 50K from the unlabeled 100K
  into_model <- sample(1:n_benign, n_benign/2)
  for_modeling <- rbind(anom, benign[into_model, ])
  #Try to pull this from a much larger sample, or, the entire data, because the ones with lowest proabilities, among
  #the selected 10,000, have probabilities around 0.05
  for_finding_more <- benign[-into_model, ]
  
  train = sample(1:nrow(for_modeling), 0.5*nrow(for_modeling))
  test = (-train)
  cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(for_modeling) - length(train)), "\n", sep = ""))

  str_formula <- "is_anomalous ~ "
  for (column in colnames(pat_proc))
  {
    if (column != 'is_anomalous' & column != 'patient_id')
    {
      str_formula <- paste(str_formula, column, " + ", sep = "")
    }
  }
  str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)

  logr <- glm(as.formula(str_formula), family = binomial("logit"), data = for_modeling[train, ] #, weights = weights
             )

  for_modeling[test, "predicted_prob_anomalous"] <- predict(logr, newdata = for_modeling[test,], type = "response")
  for_modeling[test, "predicted_is_anomalous"] <- ifelse(for_modeling[test, "predicted_prob_anomalous"] >= 0.5, '1', '0')
  print(table(for_modeling[test,"is_anomalous"], for_modeling[test, "predicted_is_anomalous"], dnn = list('actual', 'predicted')))

  #The features in decreasing order of absolute value of coefficient are: 0604, 0605, 0013, 0690, 0606, 0012, 0608, 0369,...
  for_finding_more[, "predicted_prob_anomalous"] <- predict(logr, newdata = for_finding_more, type = "response")
  for_finding_more <- for_finding_more[order(-for_finding_more[, "predicted_prob_anomalous"]),]
  write.csv(for_finding_more[1:10000, ], "/Users/blahiri/healthcare/data/cloudera_challenge/additional_10000.csv")
  return(list("model" = logr, "ffm" = for_finding_more)) 
}

principal_component <- function()
{
  set.seed(1)
  pat_proc <- read.csv("/Users/blahiri/healthcare/data/cloudera_challenge/pat_proc.csv")
  n <- nrow(pat_proc)
  sampled <- sample(1:n, 3000)
  pat_proc <- pat_proc[sampled, ]
  cat(paste("nrow(pat_proc) = ", nrow(pat_proc), ", distribution of is_anomalous is\n", sep = ""))
  print(table(pat_proc$is_anomalous))

  is_anomalous <- pat_proc$is_anomalous
  pat_proc <- pat_proc[,!(names(pat_proc) %in% c("X", "patient_id", "is_anomalous", "age_group", "gender", "income_range"))]
  #Drop columns with variance 0 as that presents a problem in scaling
  pat_proc <- pat_proc[, apply(pat_proc, 2, var, na.rm=TRUE) != 0]
  
  pc <- prcomp(pat_proc, scale = TRUE)
  #Sum of eigenvalues (sum(pc$sdev) = 129.1221) for 130 variables, so PCA is done on the correlation matrix. 
  projected <- as.data.frame(pc$x[, c("PC1", "PC2")])
  projected$is_anomalous <- is_anomalous
  projected$is_anomalous <- as.factor(projected$is_anomalous)

  #Anomalous on the right, benign on the left
  png("./figures/patients_first_two_pc.png",  width = 600, height = 480, units = "px")
  projected <- data.frame(projected)
  p <- ggplot(projected, aes(x = PC1, y = PC2)) + geom_point(aes(colour = is_anomalous), size = 2) + 
         theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold')) + 
         ggtitle("Projections along first two PCs for patients")
  print(p)
  dev.off()
  
  anom <- subset(projected, (is_anomalous == '1'))
  benign <- subset(projected, (is_anomalous == '0'))
  cat("Five num for anom is\n")
  print(fivenum(anom$PC1))
  cat("Five num for benign is\n")
  print(fivenum(benign$PC1))
  #Five num for anom is
  # -0.6200888  1.0228486  1.7112295  2.3853555  7.6313867
  #Five num for benign is
  # -4.1364377 -1.3995730 -0.8463755 -0.2611927  3.4011001
  #However, first two PCs explain only 3% of total variance, first PC explains only 1.87% of variance.
  projected
}



