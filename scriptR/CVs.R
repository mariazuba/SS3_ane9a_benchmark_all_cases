
# Coefficients of Variation

run_esc <- "boot/data/run/"
list.files(run_esc, full.names = TRUE)
esc <- readLines(paste0(run_esc, "Esc.txt"))

data <- "data/run/"
run_data <- paste0(data, esc)
load(paste0(run_data, "/inputData.RData"))

# Load libraries
library(ggplot2)
library(dplyr)

# Apply the analysis for each 'index' value again
CPUEs <- dat$dat$CPUE
# Calculate the logarithm of the observed values (obs)
CPUEs$log_obs <- log(CPUEs$obs)

# Create a function that assigns a different span value according to the index
get_span_by_index <- function(index) {
  # Define different span values for each index
  if (index == 2) {
    return(0.3)
  } else if (index == 3) {
    return(0.6)
  } else if (index == 4) {
    return(0.72)
  } else if (index == 5) {
    return(0.6)
  } else {
    return(0.6)  # Default value in case no match is found
  }
}

# Adjust the span value to improve LOESS fitting on real values with variable span
calcular_cv_por_index <- function(sub_data) {
  # Get the current index (assuming there is a single index per group)
  current_index <- unique(sub_data$index)
  
  # Get the corresponding span for this index
  span_value <- get_span_by_index(current_index)
  
  # Fit LOESS with the corresponding span
  loess_fit <- loess(obs ~ year, data = sub_data, span = span_value)
  
  # Fitted predictions
  sub_data$loess_fitted <- predict(loess_fit)
  
  # Calculate residuals
  sub_data$residuals <- sub_data$obs - sub_data$loess_fitted
  
  # Avoid division by zero
  if (mean(sub_data$loess_fitted) != 0) {
    # Calculate the CV for real values in percentage
    cv <- (sd(sub_data$residuals) / mean(sub_data$loess_fitted)) * 100
  } else {
    cv <- NA  # Handle cases where the fitted mean is zero
  }
  
  return(cv)
}

# Split the data into groups by index
CPUEs_split <- CPUEs %>% group_split(index)

# Calculate the CV for each group
cv_results <- lapply(CPUEs_split, calcular_cv_por_index)

# Combine the results into a dataframe
cv_por_index <- data.frame(
  index = sapply(CPUEs_split, function(x) unique(x$index)),
  CV = unlist(cv_results)
)

# Convert the CV to text for each index
#cv_text <- paste("CV =", round(cv_por_index$CV, 1))
cv_text <- paste0("CV = ", round(cv_por_index$CV, 0), "%")


# Create plots with ggplot for each 'index', showing the LOESS fit on real values and including the CV values
fig1<-ggplot(CPUEs, aes(x = year, y = obs)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.72, se = FALSE) +
  facet_wrap(~ index, scales = "free_y",as.table = TRUE, strip.position = "top",
             labeller = labeller(index = c("2" = "PELAGO", 
                                          "3" = "ECOCADIZ",
                                          "4" = "BOCADEVA", 
                                          "5" = "ECOCADIZ-RECLUTAS"))) +  # Separate by each index
  labs(title = "LOESS Fit for Values of Each Index", 
       x = "Year", 
       y = "Observed Index", 
       color = "Index") +
  theme(panel.background = element_rect(fill ="gray80")) +
  theme(panel.grid=element_line(color=NA)) +
  ggtitle('')+
  theme(plot.title = element_text(size =5),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 6),
        strip.text = element_text(size = 6),
        panel.background = element_rect(colour="gray",fill = "gray99"),
        strip.background = element_rect(colour = "gray", fill = "gray99"),
        legend.title = element_text(size = 6, face = "bold"), 
        legend.text = element_text(size = 6)) +
  # Add CV values to each facet
  geom_text(data = cv_por_index, aes(x = Inf, y = Inf, label = cv_text ),
            hjust = 2.5, vjust = 2.5, inherit.aes = FALSE, size = 3)
ggsave(file.path(paste0("report/run/comparison","/fig_CVs_Loess.png")), fig1,  width=8, height=5)

