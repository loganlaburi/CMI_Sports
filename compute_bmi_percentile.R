# Generate the BMI Percentile 
bmi_chart <- read.csv('bmi_chart.csv')

df$PhysicalSex <- replace(df$PhysicalSex, 
                          df$PhysicalSex == "F",
                          "2")
df$PhysicalSex <- replace(df$PhysicalSex, 
                          df$PhysicalSex == "M",
                          "1")

# Find the match in the CDC chart
df$age_months = 12 * as.numeric(df$PhysicalAge)
bmi_m <- df$age_months
bmi_l <- df$age_months
bmi_s <- df$age_months

for (i in 1:length(df$age_months)) {
  a = match(x = interaction(df$age_months[i], df$PhysicalSex[i]), 
            table = interaction(bmi_chart$Agemos, bmi_chart$Sex))
  # If we find something that is exactly match from the BMI Chart
  if (!is.na(a)) {
    bmi_m[i] <- bmi_chart[a, ][,'M']
    bmi_l[i] <- bmi_chart[a, ][,'L']
    bmi_s[i] <- bmi_chart[a, ][,'S']
    # If the dataset of the age (months) is "NA"
  } else if (is.na(df$age_months[i])){
    bmi_m[i] <- 'NA'
    bmi_l[i] <- 'NA'
    bmi_s[i] <- 'NA'   
    # If the datasest of the age (month) is smaller than the next value
  } else if (df$age_months[i] < (floor(df$age_months[i]) + 0.5)) {
    a = match(x = interaction((floor(df$age_months[i]) - 0.5), df$PhysicalSex[i]), 
              table = interaction(bmi_chart$Agemos, bmi_chart$Sex))
    bmi_m[i] <- bmi_chart[a, ][,'M']
    bmi_l[i] <- bmi_chart[a, ][,'L']
    bmi_s[i] <- bmi_chart[a, ][,'S']
    # If the datasest of the age (month) is bigger than the next value
  } else if (df$age_months[i] > (floor(df$age_months[i]) + 0.5)) {
    a = match(x = interaction((floor(df$age_months[i]) + 0.5), df$PhysicalSex[i]), 
              table = interaction(bmi_chart$Agemos, bmi_chart$Sex))
    bmi_m[i] <- bmi_chart[a, ][,'M']
    bmi_l[i] <- bmi_chart[a, ][,'L']
    bmi_s[i] <- bmi_chart[a, ][,'S']
    # Error message if all conditions are failed
  } else {
    print(i)
    print('This is the index of the dataset that give you an error. \n\n
    Your code does not have NA or any other information. Check with Michelle')
  }
}

df$bmi_m = bmi_m
df$bmi_l = bmi_l
df$bmi_s = bmi_s

# Calculate Z (USE LMS Method from CDC)
df$bmi_zscore = ((as.numeric(df$PhysicalBMI)/as.numeric(df$bmi_m))^as.numeric(df$bmi_l) - 1) / (as.numeric(df$bmi_l)*as.numeric(df$bmi_s))

# Calculate BMI percentile
df$bmi_percentile <- pnorm(df$bmi_zscore)
df$bmi_percentile <- df$bmi_percentile * 100


df$bmi_percentile <- replace(df$bmi_percentile, 
                             df$bmi_percentile < 1,
                             1.00)
