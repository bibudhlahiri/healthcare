#Do the following in cluster (not local machine)
s = scan("PCDR1101.ADT", what = "character")
s1 = strsplit(s, "\036")
#class(s1) = "list"
#length(s1[[1]]) = 24,157,944
#class(s1[[1]]) = "character"
#s1[[1]][1] = "20110120, 071159871, 0096, ," -- Date, patient ID (to be looked up in XML), procedure code (matched against in and out-patient spreadsheets)
#s1[[1]][2] = "20110114, 198158662, 0604, ,"

s2 = strsplit(s1[[1]], "\037")
#REVIEW.TXT has 50,045 lines. One number per line and numbers look like patient IDs. Matches found quickly in XML for 860434019 and 864155362.
#length(s2) = 24,157,944
#class(s2) = "list"
#s2[[1]] gives
#[1] "20110120"  "071159871" "0096"      ""
#With 12 files, PCDR11*.ADT files should give about 289,895,328 rows.
#grep -c "id" PNTSDUMP.XML gives 100,001,224 -- 100 million patients.
#s3 = do.call(rbind.data.frame, s2) --Slow
#s2 is a list of vectors. Convert each vector in it to a list.
s3 <- lapply(s2, as.list)
library(data.table)
DT <- rbindlist(s3)

#What if we create Hive tables and write them back as csv files?
#cut -c-50 PCDR1101.ADT
