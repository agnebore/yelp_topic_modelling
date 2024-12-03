#-----------------
# STEP 1: Set Directories and Import Libraries 
#-----------------

#install.packages("stm", dependencies = TRUE)
#install.packages("textmineR")
library(stm)
library(textmineR)
library(ggplot2)
library(stargazer)
library(lubridate)
library(knitr)

#Set working directory
setwd("~/Desktop/R/Results")

#Set data directory
dir <- "~/Desktop/R/Data/"

#-----------------
# STEP 2. Import Data and Fetch Flagged Reviews
#-----------------

#Import data
yelp_reviews <- read.csv(file=paste0(dir, "Modified_cleaned_ReviewsIncludingDates.csv"),
                         stringsAsFactors=FALSE, sep= ",")

#Flagged reviews dataset
df <- subset(yelp_reviews, yelp_reviews$recommended == "False")


#Create Date Variables
df$year <- substr(df$review_date, 1, 4)
df$review_date <- as.Date(df$review_date)
df$day <- as.numeric(df$review_date - min(df$review_date, na.rm = TRUE))

#Create inState Variable
df$in_state = as.integer(grepl(", OH", df$reviewer_location))

#Create isWeekday Variable
df$is_weekday <- as.integer(!weekdays(date(df$review_date)) %in% c("Saturday", "Sunday"))

# Assuming df$review_date is in "YYYY-MM-DD" format or similar
df$is_weekday <- as.integer(!weekdays(as.Date(df$review_date)) %in% c("Saturday", "Sunday"))

#Create isWeekend Variable 
df$is_weekend<- as.integer(!df$is_weekday)

#Drop Unused Variables
df <- subset(df, select = -c(name_business, 
                             url, 
                             recommended, 
                             username, 
                             reviewer_location))

---------------------------------
  ## 2.1 Export Cleaned Dataset For Submission 
---------------------------------

write.csv(df, "cleaned_dataset.csv", row.names = FALSE)

#-----------------
# STEP 3. Data Pre-Processing
#-----------------

#Preprocess the raw text data
processed_text <- textProcessor(df$review_text, 
                                metadata = df) 
#Organize pre-processed data for format suitable for stm
processed_text <- prepDocuments(processed_text$documents, 
                                processed_text$vocab, 
                                processed_text$meta)

#Store documents, vocabulary and metadata in separate lists/data elements
docs <- processed_text$documents
vocab <- processed_text$vocab
meta <- processed_text$meta

#-----------------
# STEP 4. Running and Inspecting the Topic Model
#-----------------

---------------------------------
  ## 4.1 Topic Modelling, Top-Down Iteration 
---------------------------------

topic_model <- stm(documents = docs, vocab = vocab,
                   K =12, prevalence =~ rating + s(day), #K for iteration here
                   max.em.its = 75, data = meta,
                   init.type = "Spectral", verbose = FALSE)

---------------------------------
  ## 4.2 Topic Exploration
---------------------------------

# Finding Topic Prevalence
plot(topic_model) #Plots topics in order of prevalence
topic_model$prevalence <- colSums(topic_model$theta) / sum(topic_model$theta) * 100 #Get the prevalence of each topic in all corpora

topic_prevalence <- cbind(paste0("topic_", 1:12), topic_model$prevalence) #Create Prevalence Dataframe
round(topic_model$prevalence,3)
topic_prevalence <- topic_prevalence[order(-topic_model$prevalence), ] #Sort by Prevalence

colnames(topic_prevalence) <- c("Topic","Prevalence") #Create column names
html_table <- knitr::kable(topic_prevalence, format = "html")
writeLines(html_table, "topic_prevalence.html") #Export as html table

#Display most important words
summary(topic_model)

# Finding Document-Topic Probabilities (Graphic Topic Prevalence)
topic_hist <- plot(topic_model, 
                   type = "hist", 
                   xlim = c(0, 0.8), 
                   ylim = c(0, 900))

# Finding Overview of Words in Topics
topic_labels <- plot(topic_model, type = "labels") 

#Finding Sample Texts
sample_texts <- findThoughts(topic_model, 
                             texts = meta$review_text, 
                             n = 5)

sample_texts <- as.character(sample_texts)

# Write the output to a text file
writeLines(sample_texts, "sample_texts.txt")

# Generate a word cloud for a topic
cloud(topic_model, topic = 1)

#-----------------
# STEP 5. Create Cohen's Kappa Export for Manual Coders
#-----------------
#Create a dataframe with review text and document-topic probability of each document
model_df <- make.dt(topic_model, meta)

kappa_matrix <- model_df[, c("review_text", 
                             "Topic1","Topic2", 
                             "Topic3", "Topic4", 
                             "Topic5", "Topic6", 
                             "Topic7", "Topic8", 
                             "Topic9", "Topic10", 
                             "Topic11", "Topic12")]
 
topic_columns <- kappa_matrix[, -1] #Subset columns with topic probabilities
kappa_matrix$dominant_topic <- max.col(topic_columns) #Find the number of the dominant topic
kappa_matrix$is_t1 <- ifelse(kappa_matrix$dominant_topic == 1, TRUE, FALSE) #Create a dichotomous variable
kappa_matrix <- kappa_matrix[, c("review_text", "is_t1")] #Subset with only 1 and 0 for Topic 1

#Get sample of 300 reviews for manual coders
sampled_indices <- sample(1:nrow(kappa_matrix), 300, replace = FALSE)
kappa_matrix_export <- kappa_matrix[sampled_indices, ]

#Export to CSV
write.csv(kappa_matrix_export, "kappa_matrix_export.csv", sep = ";" ,row.names = FALSE)

#-----------------
# STEP 6. Descriptive Statistics
#-----------------

---------------------------------
  ## 6.1 Extracting Random Text Samples Including Rating 
---------------------------------
  
# Identify indices of documents classified as belonging to Topic 1
topic_one_indices <- which(meta$binary_is_t1 == 1)

# Randomly select three indices from those classified as belonging to Topic 1
random_three_indices <- sample(topic_one_indices, min(3, length(topic_one_indices)), replace = FALSE)

# Extract the selected reviews' metadata, including review texts and ratings
random_three_reviews_topic_one <- meta[random_three_indices, c("review_text", "rating")]

# Print the extracted reviews and their ratings
print(random_three_reviews_topic_one)

---------------------------------
  ## 6.2 Percentage Distribution of Star Ratings for Topic 1
---------------------------------
  
# Filter `meta` for reviews classified as belonging to Topic 1
topic1_reviews <- meta[meta$is_t1_binary == 1,]

# Ensure all rating levels are included, even if some are absent in the filtered data
rating_levels <- 1:5

# Calculate the distribution of star ratings for Topic 1 reviews
rating_distribution <- table(factor(topic1_reviews$rating, levels = rating_levels))

# Calculate the percentage distribution of these ratings
rating_percentage <- prop.table(rating_distribution) * 100

# Print the percentage distribution
cat("Percentage Distribution for Topic 1 Reviews:\n")
print(rating_percentage)

# Plotting the percentage distribution
barplot_heights <- barplot(rating_percentage, 
                           main = "Percentage Distribution of Star Ratings for Service Quality Reviews", 
                           xlab = "Rating", 
                           ylab = "Percentage of Service Quality Reviews", 
                           col = "steelblue",
                           ylim = c(0, max(rating_percentage) + 10), # Adjust Y-axis to fit labels
                           names.arg = c("1 Star", "2 Stars", "3 Stars", "4 Stars", "5 Stars"))

# Adding percentages on top of the bars
text(x = barplot_heights, # Bar midpoints
     y = rating_percentage + 1, # Slightly above the bars
     labels = paste0(sprintf("%.1f", rating_percentage), "%"), # Format percentages
     cex = 0.8) # Font size

---------------------------------
  #### 6.3 Get number of times Topic 1 was identified in data set
---------------------------------
  
# Assuming kappa_matrix$is_t1 exists and corresponds to each row in meta
meta$is_t1_binary <- ifelse(kappa_matrix$is_t1, 1, 0)

# Count how many times Topic 1 is the primary topic
topic1_count <- sum(meta$is_t1_binary == 1)

# Print the count
print(topic1_count)


---------------------------------
  ### 6.4 Review Length and Service Quality 
---------------------------------
  
# Ensure topic1_reviews is correctly defined and includes review_text
topic1_reviews$review_length <- sapply(strsplit(as.character(topic1_reviews$review_text), " "), length)

# Categorize review lengths
topic1_reviews$length_category <- cut(topic1_reviews$review_length,
                                      breaks = c(-Inf, 50, 250, Inf),
                                      labels = c("Short", "Medium", "Long"),
                                      right = FALSE)

# Calculate percentage distribution
length_distribution <- table(topic1_reviews$length_category)
length_percentage <- prop.table(length_distribution) * 100

# Convert the table to a data frame for ggplot
length_df <- as.data.frame(length_percentage)
names(length_df) <- c("LengthCategory", "Percentage")
length_df$LengthCategory <- factor(length_df$LengthCategory, levels = c("Short", "Medium", "Long"))

# Plotting with specified fill colors using scale_fill_manual
library(ggplot2)

ggplot(length_df, aes(x = LengthCategory, y = Percentage, fill = LengthCategory)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Short" = "lightblue", "Medium" = "steelblue", "Long" = "darkblue")) +
  labs(title = "Distribution of Review Lengths for Service Quality Reviews",
       x = "Review Length Category",
       y = "Percentage of Reviews (%)") +
  theme_minimal() +
  theme(legend.position = "none") +  # This removes the legend
  geom_text(aes(label = sprintf("%.1f%%", Percentage), 
                y = ifelse(Percentage > 0, Percentage + 2, 1)), size = 4.5, vjust = 0)


-----------------------------------------
  ### 6.5 Service Quality Evolution over Time
------------------------------------------

### 6.5.1 In Percentage Distribution ###

# Aggregate Total and Topic 1 Reviews by Year
total_reviews_by_year <- aggregate(x = list(TotalCount = meta$review_date), 
                                   by = list(Year = meta$year), 
                                   FUN = length)

topic1_reviews_by_year <- aggregate(x = list(Topic1Count = topic1_reviews_up_to_2023$review_date), 
                                    by = list(Year = topic1_reviews_up_to_2023$year), 
                                    FUN = length)

# Merge Dataframes on Year and Calculate Percentages
merged_yearly_data <- merge(total_reviews_by_year, topic1_reviews_by_year, by = "Year")
merged_yearly_data$Percentage <- (merged_yearly_data$Topic1Count / merged_yearly_data$TotalCount) * 100

# Ensure Year is treated as a numerical value for proper line plotting
merged_yearly_data$Year <- as.numeric(as.character(merged_yearly_data$Year))

# Plot the Percentage Trend with a Line
ggplot(merged_yearly_data, aes(x = Year, y = Percentage)) +
  geom_line(color = "steelblue", size = 1) + 
  geom_point(color = "lightblue", size = 3) +
  theme_minimal() +
  labs(title = "Distribution of Service Quality Reviews over Time",
       x = "Year", 
       y = "Total Reviews in Time Period (%)") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


### Expected Topic Proportion over Time ###

predict_topics <- estimateEffect(1 ~ rating + day, topic_model, metadata = meta, uncertainty = "Global")

plot(predict_topics, "day", method = "continuous", topics = 1,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Time")

monthseq <- seq(from = min(df$review_date),
                to = max(df$review_date), by = "year")

monthnames <- format(monthseq, "%Y")

axis(1, at = as.numeric(monthseq) - min(as.numeric(monthseq)),
     labels = monthnames)


#-----------------
# STEP 7. Regressions
#-----------------

# Dichotomous Variables to Factors 
model_df$numeric_instate <- factor(model_df$in_state)
model_df$numeric_isweekday <- factor(model_df$is_weekday)
model_df$numeric_isweekend <- factor(model_df$is_weekend)

# Star Ratings to Dichotomous Variables
model_df$rating_1 <- factor(ifelse(model_df$rating == 1, 1, 0))
model_df$rating_2 <- factor(ifelse(model_df$rating == 2, 1, 0))
model_df$rating_3 <- factor(ifelse(model_df$rating == 3, 1, 0))
model_df$rating_4 <- factor(ifelse(model_df$rating == 4, 1, 0))
model_df$rating_5 <- factor(ifelse(model_df$rating == 5, 1, 0))


#Preparing Linear Regression Formulas
mdlA <- ReviewLength ~ Topic1

#Preparing Logistic Regression Formulas
mdlB <- Topic1 ~ numeric_instate
mdlC <- Topic1 ~ numeric_isweekday
mdlD <- Topic1 ~ numeric_isweekend
mdlE <- Topic1 ~ reviewer_friends
mdlF <- Topic1 ~ reviewer_reviews 

mdlG <- rating_1 ~ Topic1
mdlH <- rating_2 ~ Topic1
mdlI <- rating_3 ~ Topic1
mdlJ <- rating_4 ~ Topic1
mdlK <- rating_5 ~ Topic1

#Store Regression Results 
rstl.mdlA <- lm(mdlA , data = model_df)

rstl.mdlB <- glm(mdlB, family = binomial(link = 'logit'),
                 data = model_df)
rstl.mdlC <- glm(mdlC, family = binomial(link = 'logit'),
                 data = model_df)
rstl.mdlD <- glm(mdlD, family = binomial(link = 'logit'),
                 data = model_df)
rstl.mdlE <- glm(mdlE, family = binomial(link = 'logit'),
                 data = model_df)
rstl.mdlF <- glm(mdlF, family = binomial(link = 'logit'),
                 data = model_df)
rstl.mdlG <- glm(mdlG, family = binomial(link = 'logit'),
                 data = model_df)
rstl.mdlH <- glm(mdlH, family = binomial(link = 'logit'),
                 data = model_df)
rstl.mdlI <- glm(mdlI, family = binomial(link = 'logit'),
                 data = model_df)
rstl.mdlJ <- glm(mdlJ, family = binomial(link = 'logit'),
                 data = model_df)
rstl.mdlK <- glm(mdlK, family = binomial(link = 'logit'),
                 data = model_df)


#Print Linear Regression Results - Topic Independent
stargazer(rstl.mdlA, 
          title = "Linear Regressions, Topic Modelling",
          align = TRUE, 
          no.space = TRUE, 
          intercept.bottom = FALSE, 
          type = "text",
          out = "wwma2_linear_regression_ti.html")

#Print Logistic Regression Results - Topic Dependent
stargazer(rstl.mdlB, rstl.mdlC, rstl.mdlD, rstl.mdlE, rstl.mdlF,
          title = "Linear Regressions, Topic Modelling",
          align = TRUE, 
          no.space = TRUE, 
          intercept.bottom = FALSE, 
          type = "text",
          out = "wwma2_linear_regression_td.html")

#Print Logistic Regression Results
stargazer(rstl.mdlG, rstl.mdlH, rstl.mdlI, rstl.mdlJ, rstl.mdlK,
          title = "Logistic Regressions, Topic Modelling",
          align = TRUE, 
          no.space = TRUE, 
          intercept.bottom = FALSE, 
          type = "text",
          out = "wwma2_log_regression.html")


### Calculate Log Likelihood 
exp(rstl.mdlK$coefficients)
exp(rstl.mdlG$coefficients)
exp(rstl.mdlH$coefficients)
exp(rstl.mdlI$coefficients)
exp(rstl.mdlJ$coefficients)