longest_common_subseq <- function(x, y)
{
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
  lcs <- ""
  print_lcs(lcs, b, x, m+1, n+1)
  print(lcs)
}

print_lcs <- function(lcs, b, x, i, j)
{
  #cat(paste("i = ", i, ", j = ", j, ", b[i, j] = ", b[i, j], "\n", sep = ""))
  if ((i == 1) | (j == 1))
  {
    return()
  }
  if (b[i, j] == 'D')
  {
    print_lcs(lcs, b, x, i-1, j-1)
    print(x[i-1])
    eval.parent(substitute(lcs <- paste(lcs, x[i-1], "", sep = "")))
  }
  else if (b[i, j] == 'N')
  {
    print_lcs(lcs, b, x, i-1, j)
  }
  else
  {
    print_lcs(lcs, b, x, i, j-1)
  }
}
