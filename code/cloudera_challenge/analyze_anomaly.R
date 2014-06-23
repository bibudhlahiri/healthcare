library(rpart)
library(RPostgreSQL)
library(reshape2)
library(e1071)

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

   #pat_proc <- create_balanced_sample(pat_proc)
   #pat_proc <- create_bs_by_over_and_undersampling(pat_proc)
   x <- pat_proc[,!(names(pat_proc) %in% c("patient_id", "is_anomalous", "X"))]
   y <- pat_proc[, "is_anomalous"]
   
   train = sample(1:nrow(pat_proc), 0.5*nrow(pat_proc))
   test = (-train)
   cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(pat_proc) - length(train)), "\n", sep = ""))

   str_formula <- "is_anomalous ~ "
   for (column in colnames(x))
   {
     str_formula <- paste(str_formula, column, " + ", sep = "")
   }
   str_formula <- substring(str_formula, 1, nchar(str_formula) - 2)
   
   model <- rpart(as.formula(str_formula), data = pat_proc[train, ])

   pred <- predict(model, newdata = pat_proc[test,], type = "prob")
   pat_proc[test, "predicted_prob_anomalous"] <- (predict(model, newdata = pat_proc[test,], type = "prob"))[, "1"]
   pat_proc[test, "predicted_is_anomalous"] <- ifelse(pat_proc[test, "predicted_prob_anomalous"] >= 0.5, '1', '0')
   print(table(pat_proc[test,"is_anomalous"], pat_proc[test, "predicted_is_anomalous"], dnn = list('actual', 'predicted')))
   model
 }

