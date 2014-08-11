library(RPostgreSQL)

trim_comma <- function(x)
{
  if (substring(x, nchar(x), nchar(x)) == ',')
  {
    return(substring(x, 1, nchar(x)-1))
  }
  x
}

longest_common_subseq <- function(x, y)
{
  x <- sapply(x, trim_comma)
  y <- sapply(y, trim_comma)
  m <- length(x)
  n <- length(y)
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
   statement <- paste("select a.diagnosis_code as diagnosis_code_1, a.long_desc as long_desc_1, 
                       b.diagnosis_code as diagnosis_code_2, b.long_desc long_desc_2
                       from diagnosis_codes a, diagnosis_codes b
                       where substring(a.diagnosis_code from 1 for 3) = substring(b.diagnosis_code from 1 for 3)
                       and a.diagnosis_code <> b.diagnosis_code
                       --and a.long_desc like 'Malignant neoplasm%'
                       --and b.long_desc like 'Malignant neoplasm%'
                       ", sep = "")
  res <- dbSendQuery(con, statement)
  df <- fetch(res, n = -1)
  df <- df[1:500, ]
  df$lcs <- apply(df, 1, function(row)longest_common_subseq(unlist(strsplit(row["long_desc_1"], " ")), unlist(strsplit(row["long_desc_2"], " "))))
  print(df$lcs)
  dbDisconnect(con)
}


