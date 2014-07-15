setwd("/Users/blahiri/healthcare/documents/recommendation_system/")
library(data.table)
beneficiaries <- fread("beneficiary_summary_2008_2009.csv")
beneficiaries <- as.data.frame(beneficiaries)
#converting to numeric matrix
beneficiaries[, 3:24] <- sapply(beneficiaries[, 3:24], as.numeric)

#storing numeric columns temporarily into diff object
beneficiaries1 <- beneficiaries[,1:2]
beneficiaries2 <- beneficiaries[,3:24]

#changing 2 to 0
beneficiaries2[beneficiaries2 ==2 ] <- 0

#recreating benefeciaries object and removing unwanted objects
beneficiaries <- cbind(beneficiaries1,beneficiaries2)
rm(beneficiaries1,beneficiaries2)
gc(T)

dense_matrix <- beneficiaries



require(bit64) #For reading in the claim IDs properly
tcdc <- fread("transformed_claim_diagnosis_codes.csv")
tcdc <- as.data.frame(tcdc)
temp <- tcdc[,c("desynpuf_id","dgns_cd")] 
#converting to wide format
system.time(converted  <-  dcast(temp,desynpuf_id~dgns_cd))
#merging with benefeciaries data (this one takes max time among all steps.)
combined <- merge(beneficiaries,converted, by = "desynpuf_id",all.x = T)
