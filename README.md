# N-Of-1 (Single Subject Design)

This repo was created primarily to serve as an analysis tool for the PCORI project. For public use, please see nof1.pdf for more details. You can run Bayesian linear regression, ordinal/logistic regression, and poisson regression using this package.

# To Install and Load the package

```{r}
library(devtools)
install_github("jiabei-yang/nof1", force = TRUE)
library(nof1)
```

# PCORI Code

```{r}
library(jsonlite)

##### PRODUCE
json.file <- fromJSON("sample input.json")
#json.file <- fromJSON("sample input-small.json")
#json.file <- fromJSON("sample input-no mscd.json")
#json.file <- fromJSON("sample input-no scd.json")
result <- do.call(wrap, json.file)
output <- toJSON(result, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output

######## afib study
json.file2 <- fromJSON("afib sample input.json")
result2 <- do.call(wrap2, json.file2)
output2 <- toJSON(result2, pretty = TRUE, UTC = TRUE, auto_unbox = TRUE, na = NULL)
output2
```

# MINES code

```{r}
library(ggplot2)
library(scales)

# Read in dataset
# exercise <- read.csv("dummy_data_HYH.csv", header = TRUE, sep = ",", na.strings=c("","NA"))
exercise <- read.csv("dummy_data_HYH.csv", 
                     header     = TRUE, 
                     sep        = ",", 
                     na.strings = c("","NA"))

#######################################################################################################
######################################## energy time-series plot ######################################
#######################################################################################################
nof1 <- nof1.data(exercise$energy,                  # outcome
                  exercise$Treat,                   # treatment 
                  baseline = "Usual routine",       # what is the control/baseline group
                  response = "normal")              # what is the type of the outcome

# timestamp: time of the nof1 event occurring
# timestamp.format: format of the timestamp
# normal.response.range: the range of the outcome if continuous; a vector of minimum and maximum
# y.name: label for the y axis
# x.name: label for the x axis; if not specified, the output will not give label on the x axis
# title: title for the plot
energy_plot <- time_series_plot(nof1, 
                                timestamp             = exercise$date_start, 
                                timestamp.format      = "%m/%d/%y", 
                                normal.response.range = c(0, 10),
                                y.name                = "Self-reported energy",
                                title                 = "Your energy on days you maintained your usual routine vs. days you did deep breathing meditation")

energy_plot # This is the raw plot that the package can help you produce, 
            # if you want to make it nicer, you can customize using the following code as you did before

# This produces a figure in pdf version, which can then be inserted into word document
pdf("energy.pdf", height = 4, width = 8.5)
energy_plot + 
  theme(legend.title = element_blank(), 
        plot.title   = element_text(color  = "#666666", 
                                    face   = "bold", 
                                    size   = 12, 
                                    hjust  = 0.2)) + 
  scale_x_date(labels = date_format("%b-%d"), 
               breaks = '4 days')
dev.off()

#######################################################################################################
############################################ test NA in energy ########################################
#######################################################################################################
# manually set some of the energy's to NA
exercise$energy[c(3, 14)] <- NA

# proceed the same as before
nof1 <- nof1.data(exercise$energy,                  # outcome
                  exercise$Treat,                   # treatment 
                  baseline = "Usual routine",       # what is the control/baseline group
                  response = "normal")              # what is the type of the outcome

# timestamp: time of the nof1 event occurring
# timestamp.format: format of the timestamp
# normal.response.range: the range of the outcome if continuous; a vector of minimum and maximum
# y.name: label for the y axis
# x.name: label for the x axis; if not specified, the output will not give label on the x axis
# title: title for the plot
energy_plot <- time_series_plot(nof1, 
                                timestamp             = exercise$date_start, 
                                timestamp.format      = "%m/%d/%y", 
                                normal.response.range = c(0, 10),
                                y.name                = "Self-reported energy",
                                title                 = "Your energy on days you maintained your usual routine vs. days you did deep breathing meditation")

energy_plot # There may be a warning message here, but it comes from the NA's so it is fine

# This produces a figure in pdf version, which can then be inserted into word document
pdf("energy_withna.pdf", height = 4, width = 8.5)
energy_plot + 
  theme(legend.title = element_blank(), 
        plot.title   = element_text(color  = "#666666", 
                                    face   = "bold", 
                                    size   = 12, 
                                    hjust  = 0.2)) + 
  scale_x_date(labels = date_format("%b-%d"), 
               breaks = '4 days')
dev.off()

#######################################################################################################
######################################### focus time-series plot ######################################
#######################################################################################################
nof1 <- nof1.data(exercise$focus,                 
                  exercise$Treat,                   
                  baseline = "Usual routine",       
                  response = "normal")              

focus_plot <- time_series_plot(nof1, 
                                timestamp             = exercise$date_start, 
                                timestamp.format      = "%m/%d/%y", 
                                normal.response.range = c(0, 10),
                                y.name                = "Self-reported focus",
                                title                 = "Your focus on days you maintained your usual routine vs. days you did deep breathing meditation")

focus_plot 

pdf("focus.pdf", height = 4, width = 8.5)
focus_plot + 
  theme(legend.title = element_blank(), 
        plot.title   = element_text(color  = "#666666", 
                                    face   = "bold", 
                                    size   = 12, 
                                    hjust  = 0.2)) + 
  scale_x_date(labels = date_format("%b-%d"), 
               breaks = '4 days')
dev.off()

#######################################################################################################
##################################### happiness time-series plot ######################################
#######################################################################################################
nof1 <- nof1.data(exercise$happiness,                 
                  exercise$Treat,                   
                  baseline = "Usual routine",       
                  response = "normal")              

happiness_plot <- time_series_plot(nof1, 
                               timestamp             = exercise$date_start, 
                               timestamp.format      = "%m/%d/%y", 
                               normal.response.range = c(0, 10),
                               y.name                = "Self-reported happiness",
                               title                 = "Your happiness on days you maintained your usual routine vs. days you did deep breathing meditation")

happiness_plot 

pdf("happiness.pdf", height = 4, width = 8.5)
happiness_plot + 
  theme(legend.title = element_blank(), 
        plot.title   = element_text(color  = "#666666", 
                                    face   = "bold", 
                                    size   = 12, 
                                    hjust  = 0.2)) + 
  scale_x_date(labels = date_format("%b-%d"), 
               breaks = '4 days')
dev.off()
```



Thank you,    
Michael Seo, Jiabei Yang
