library(data.table)
library(ggplot2)

plot_reliability_curve <- function()
{
  filename <- "C:\\Users\\blahiri\\healthcare\\data\\cloudera_challenge\\reliability_curve_lr.csv"
  reli_data <- fread(filename, header = TRUE, sep = ",", stringsAsFactors = FALSE, showProgress = TRUE, 
                      colClasses = c("numeric", "numeric", "numeric", "numeric", "numeric"),
                      data.table = TRUE)
  reli_data[, midpoint := (start + end)/2]
  image_file <- "C:\\Users\\blahiri\\healthcare\\code\\cloudera_challenge\\figures\\reliability_curve_lr.png"
  png(image_file, width = 800, height = 800)
  p <- ggplot(reli_data, aes(x = midpoint, y = fraction, group = 1)) + geom_line(colour="red") + geom_point() + geom_abline(intercept = 0, slope = 1)
  print(p)
  aux <- dev.off()
}

plot_reliability_curve()

