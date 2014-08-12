library(RPostgreSQL)

trim_comma <- function(x)
{
  if (substring(x, nchar(x), nchar(x)) == ',')
  {
    return(substring(x, 1, nchar(x)-1))
  }
  x
}

trim.leading <- function (x)  sub("^\\s+", "", x)

longest_common_subseq <- function(x, y)
{
  x <- sapply(x, trim_comma)
  y <- sapply(y, trim_comma)
  m <- length(x)
  n <- length(y)
  if ((m == "") | (n == ""))
  {
    return("")
  }
  c <- matrix(nrow = m+1, ncol = n+1)
  b <- matrix(nrow = m+1, ncol = n+1)
  c[2:(m+1), 1] = 0
  c[1, 1:(n+1)] = 0
  for (i in 1:m) 
  {
    for (j in 1:n)
    {
      #cat(paste("i = ", i, ", x[i] = ", x[i], ", j = ", j, ", y[j] = ", y[j], "\n", sep = ""))
      if (x[i] == y[j])
      {
        c[i+1, j+1] = c[i, j] + 1;
        b[i+1, j+1] = 'D'; #D for diagonal
      }
      else if (c[i, j+1] >= c[i+1, j])
      {
        c[i+1, j+1] = c[i, j+1];
        b[i+1, j+1] = 'N'; #N for north
      }
      else 
      {
        c[i+1, j+1] = c[i+1, j];
        b[i+1, j+1] = 'W'; #W for west
      }
    } 
  }
  #print(c)
  #print(b)
  lcs <<- ""
  print_lcs(b, x, m+1, n+1, " ")
  ncalls <<- ncalls + 1
  if (FALSE & ncalls %% 1000 == 0)
  {
    cat(paste("ncalls = ", ncalls, ", time = ", Sys.time(), "\n", sep = ""))
  }
  lcs
}

print_lcs <- function(b, x, i, j, seperator)
{
  #cat(paste("i = ", i, ", j = ", j, ", b[i, j] = ", b[i, j], "\n", sep = ""))
  if ((i == 1) | (j == 1))
  {
    return()
  }
  if (b[i, j] == 'D')
  {
    print_lcs(b, x, i-1, j-1, seperator)
    #print(x[i-1])
    lcs <<- paste(lcs, x[i-1], sep = seperator)
  }
  else if (b[i, j] == 'N')
  {
    print_lcs(b, x, i-1, j, seperator)
  }
  else
  {
    print_lcs(b, x, i, j-1, seperator)
  }
}

summarize_diag_codes <- function()
 {
   con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
   #There are 14,562 distinct values of long_desc
   statement <- paste("select a.diagnosis_code as diagnosis_code_1, a.long_desc as long_desc_1, 
                       b.diagnosis_code as diagnosis_code_2, b.long_desc long_desc_2
                       from diagnosis_codes a, diagnosis_codes b
                       where substring(a.diagnosis_code from 1 for 4) = substring(b.diagnosis_code from 1 for 4)
                       and a.diagnosis_code < b.diagnosis_code
                       ", sep = "")
  res <- dbSendQuery(con, statement)
  df <- fetch(res, n = -1)
  #df <- df[1:500, ]
  ncalls <<- 0
  df$lcs <- apply(df, 1, function(row)longest_common_subseq(unlist(strsplit(row["long_desc_1"], " ")), unlist(strsplit(row["long_desc_2"], " "))))
  #print(df$lcs)
  dbDisconnect(con)
  #length(unique(df$lcs)) = 5797. So the first step cuts down by 60%.
  df
}

summarize_drug_names <- function()
 {
   con <- dbConnect(PostgreSQL(), user="postgres", password = "impetus123",  
                   host = "localhost", port="5432", dbname = "DE-SynPUF")
   #There are 22,749 distinct values of long_desc
   statement <- paste("select lower(n1.proprietaryname) pname_1, lower(n2.proprietaryname) pname_2
                       from ndc_codes n1, ndc_codes n2
                       where n1.pharm_classes = n2.pharm_classes
                       and n1.producttypename = n2.producttypename
                       and n1.nonproprietaryname = n2.nonproprietaryname
                       and n1.routename = n2.routename
                       and n1.labelername = n2.labelername
                       and n1.substancename = n2.substancename
                       and n1.productid < n2.productid
                       order by n1.proprietaryname, n2.proprietaryname
                       ", sep = "")
  res <- dbSendQuery(con, statement)
  df <- fetch(res, n = -1)
  ncalls <<- 0
  df$lcs <- apply(df, 1, function(row)longest_common_subseq(unlist(strsplit(row["pname_1"], " ")), unlist(strsplit(row["pname_2"], " "))))
  #print(df$lcs)
  dbDisconnect(con)
  #length(unique(df$lcs)) = 2384. So the first step cuts down by 90%.
  #df
  unique_lcss <- sort(unique(df$lcs))
  #summarize_further(unique_lcss)
  return(unique_lcss)
}

summarize_further <- function(unique_lcss)
{
  n_unique_lcss <- length(unique_lcss)
  sim_scores <- matrix(0, nrow = n_unique_lcss, ncol = n_unique_lcss)
  for (i in 1:n_unique_lcss)
  {
    for (j in 1:n_unique_lcss)
    {
      #cat(paste("i = ", i, ", j = ", j, ", unique_lcss[i] = ", unique_lcss[i], ", unique_lcss[j] = ", unique_lcss[j], "\n", sep = ""))
      if ((unique_lcss[i] == "") | (unique_lcss[j] == "")) 
      {
        sim_scores[i, j] <- 0
      }
      else
      {
        lcs <- longest_common_subseq(unlist(strsplit(unique_lcss[i], " ")), unlist(strsplit(unique_lcss[j], " ")))
        lcs <- trim.leading(lcs)
        sim_scores[i, j] <- ifelse((lcs == " "), 0, length(unlist(strsplit(lcs, " "))))
      }
      if (sim_scores[i, j] > 2)
      {
        cat(paste("i = ", i, ", j = ", j, ", lcs = ", lcs, ", sim_scores[i, j] = ", sim_scores[i, j], "\n", sep = ""))
      }
    }
  }
  sim_scores
}

extract_summaries <- function(sim_scores)
{
  square_size <- 3
  n_unique_lcss <- nrow(sim_scores)
  for (i in 1:(n_unique_lcss - square_size + 1))
  {
    regional_sum <- sum(sim_scores[i:(i+square_size-1), i:(i+square_size-1)])
    if (regional_sum > 1.5*(square_size^2))
    {
      cat(paste("start of square = ", i, ", end of square = ", (i+square_size-1), ", regional_sum = ", regional_sum, "\n", sep = ""))
      cat("Corresponding names are\n")
      print(unique_lcss[i:(i+square_size-1)])
    }
  }
}






