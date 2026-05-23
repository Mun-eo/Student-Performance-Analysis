# =======================================================================
# STUDENT PERFORMANCE ANALYSIS ----
# =======================================================================
# Purpose: Frank Johnson High school wants to understand how variables 
#          such as "Study Hours", "Smartphone usage", "Subject", and 
#          "Learning Method affects the exam scores of their students. 
#          This would help the school determine what interventions or 
#          programs they should develop to continue support their students'
#          development.
#
#
# Variables examined:
#   Study Hours/Day             - The amount of study hours per student
#   Attendance %                - Attendance percentage per student
#   Exam Score                  - General exam scores per student (Doesn't specify a subject) 
#   Favorite subject            - Students favorite subjects   
#   Smartphone Usage            - The amount of hours students use their smartphones
#   Learning Method             - The method students attend school (e.g. Hybrid, On-site, Online)
#
# Analysis Goals:
# 1. Is there a relationship between smart phone usage and student exam scores? (Correlation Test)
# 2. Is there a significant mean difference in exam scores between STEM & Non-STEM Majors? (T-Test)
# 3. Can attendance predict student exam scores? (Linear Regression)
# 4. Do students who learn On-site perform better on exams than students who are online? (T-Test)
#
# Hypotheses:
# H1: There will be a strong negative relationship between student smart phone usage and student exam scores.
# H2: Students in STEM Majors will have a significantly higher mean in exam scores 
# compared to Non-STEM Majors.
# H3: Student attendance will be a strong positive predictor of student exam scores.
# H4: Students who attend school in person will have a significantly higher mean in exam scores
# compared to students who learn online
# =======================================================================



# 1. INSTALL & LOAD PACKAGES ----

# Run the install lines once if the packages aren't already on your machine.
# install.packages(c("tidyverse", "stats", "effectsize", "ggtext"))
my_packages <- c("tidyverse", "stats", "effectsize", "ggtext")
lapply(my_packages, require, character.only = TRUE)



# 2. LOAD, VIEW DATA, & CHECK FOR NA'S ----
students_df <- read_csv("education_dataset.csv")
glimpse(students_df)
summary(students_df)

colSums(is.na(students_df)) # Checks for NA values in each column
# ==============================================================


# ==============================================================
# 3. DATA CLEANING FOR STATISTICAL TEST 
# ==============================================================
# --------------------------------------------------------------
# STEP 3a. Set up for correlation and linear regression test
# --------------------------------------------------------------
# a. Select relevant columns for analysis 
relevant_cols <- c("Study Hours/Day", 
                   "Attendance %",
                   "Exam Score",
                   "Favorite Subject",
                   "Smartphone Usage (hrs/day)",
                   "Learning Method"
                   )

analyzed_students_df <- students_df %>% 
  select(all_of(relevant_cols)) # Creates new data set with relevant columns

# b. View new data set
glimpse(analyzed_students_df)

# --------------------------------------------------------------
# STEP 3b. Set up for Independent Samples T-Test
# --------------------------------------------------------------
# a. Create STEM column for STEM vs. Non-STEM T-Test
analyzed_students_df <- analyzed_students_df %>% 
  mutate(
    STEM = case_when(
      `Favorite Subject` == "Math" ~ 1,
      `Favorite Subject` == "Computer" ~ 1,
      `Favorite Subject` == "Science" ~ 1,
      `Favorite Subject` == "English" ~ 0,
      `Favorite Subject` == "History" ~ 0
    ) %>% factor(
      levels = c(0,1),
      labels = c("Non-STEM", "STEM")
    )
  )

# b. Create Learning Groups for Onsite vs. Online T-Test
analyzed_students_df %>% count(`Learning Method`) %>% view()

Lmethod_t_test <- analyzed_students_df %>% 
  filter(`Learning Method` %in% c("Online", "Offline")) %>% 
  mutate(
    Learning_Group = case_when(
      `Learning Method` == "Online" ~ 1,
      `Learning Method` == "Offline" ~ 0
    ) %>% 
      factor(
        levels = c(1,0),
        labels = c("Online", "Onsite")
      )
  )


# c. Confirm the major & learning groups is balanced enough for a t-test
analyzed_students_df %>% count(STEM) 
Lmethod_t_test %>% count(Learning_Group)
# ==============================================================


# ==============================================================
# 4. EXPLORATORY DATA ANALYSIS (VISUALIZATIONS)
# =============================================================
# --------------------------------------------------------------
# STEP 4a. Visualize the distribution for each single variable being analyzed 
# --------------------------------------------------------------
# a. Create histogram to visualize the data spread for student study hours
ggplot(analyzed_students_df, aes(x = `Study Hours/Day`)
       ) + 
  geom_histogram(color = "black", fill = "pink")
  
# b. Create histogram to visualize the data spread for student attendance
ggplot(analyzed_students_df, aes(x = `Attendance %`)
       ) +
  geom_histogram(bins = 40, color = "black", fill = "pink") 

# c. Create histogram to visualize the data spread for student exam scores
ggplot(analyzed_students_df, aes(x = `Exam Score`)
       ) +
  geom_histogram(color = "black", fill = "pink") 

# d. Create histogram to visualize the data spread for student smart phone usage
ggplot(analyzed_students_df, aes(x = `Smartphone Usage (hrs/day)`)
       ) + 
  geom_histogram(color = "black", fill = "pink") 
  
# --------------------------------------------------------------
# STEP 4b. Visualize scatter plots for H1 & H3
# --------------------------------------------------------------
# a. Create scatter plot to visualize relationship between 
#    student smart phone usage and student exam scores.
ggplot(analyzed_students_df, aes(x = `Smartphone Usage (hrs/day)`,
                                 y = `Exam Score`)
       ) +
  geom_point(alpha = 0.5, color = "black", size = 2) +
  geom_smooth(method = "lm", color = "red", linewidth = 1, se = TRUE) +
  labs(x = "Students Smart Phone Usage (Hrs)",
       y = "Students Exam Score")

# b. Create scatter plot to visualize student 
#    attendance as a predictor of student exam scores
ggplot(analyzed_students_df, aes(x = `Attendance %`,
                                 y = `Exam Score`)
       ) + 
  geom_point(color = "black", size = 3) + 
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) + 
  labs(x = "Students Attendance",
       y = "Students Exam Score") 

# --------------------------------------------------------------
# STEP 4c. Visualize T-Test for H2 & H4
# --------------------------------------------------------------
# a. Create box plot to visualize mean difference 
#    in exam scores between STEM & Non-STEM Majors?
ggplot(analyzed_students_df, aes(x = STEM,
                                 y = `Exam Score`)
       ) + 
  geom_boxplot(aes(fill = STEM))


# b. Create box plot to visualize mean difference 
#    in exam scores between On-line & Onsite students?
ggplot(Lmethod_t_test, aes(x = Learning_Group,
                           y = `Exam Score`)) +
  geom_boxplot()
# ==============================================================


# ==============================================================
# 5. STATISTICAL TEST FOR ANALYSIS  
# ==============================================================
# --------------------------------------------------------------
# STEP 5a. Conduct Correlation Statistical Test
# --------------------------------------------------------------
# a. Run correlation test to assess relationship strength and direction
#    between smart phone usage and student exam scores?
cor.test(analyzed_students_df$`Smartphone Usage (hrs/day)`, 
         analyzed_students_df$`Exam Score`,
         alternative = "less",
         method = "pearson",
         conf.level = 0.95)
# Finding: While there was a statistically significant finding between student smartphone usage 
#          & student exam score, the effect size was very low(r = -0.07). Meaning that there 
#          is no practical important relationship between student smartphone usage & student exam 
#          score, the significant result can be mostly due to the large sample size 
#          rather than real world impact.


# --------------------------------------------------------------
# STEP 5b. Conduct Linear Regression Statistical Test
# --------------------------------------------------------------
# a. Run linear regression test to assess predictive strength and direction
#    of attendance on student exam scores?
regression_model <- lm(`Exam Score` ~ `Attendance %`,
                       data = analyzed_students_df)

summary(regression_model)
# Finding: There was not a statistically significant finding between student attendance 
#          & student exam scores. The predictive strength and direction was found to be 
#          very weak(b = -0.08). Student attendance could not be used an as a variable 
#          to predict student exam performance (r2 = 0.00). 


# --------------------------------------------------------------
# STEP 5b. Conduct Independent Sample T-Test Statistical Tests
# --------------------------------------------------------------
# a. Run independent samples t-test to test if there is a mean difference 
#    in exam scores between STEM & Non-STEM Majors?
t.test(`Exam Score` ~ STEM, 
       var.equal = FALSE, 
       conf.level = 0.95,
       data = analyzed_students_df)

# Generate descriptive stats & mean difference, cohens d for independent samples t-test
descrpitive_stats <- analyzed_students_df %>% 
  group_by(STEM) %>% 
  summarise(
    mean = mean(`Exam Score`),
    sd = sd(`Exam Score`),
    n = n()
  )

mean_difference <- descrpitive_stats[2, 2] - descrpitive_stats[1, 2]

cohens_d <- cohens_d(`Exam Score` ~ STEM,
                     data = analyzed_students_df)


# View outputs from independent samples t-test objects
descrpitive_stats
mean_difference
cohens_d
# Finding: There was a statistically significant mean difference in exam scores 
#          between STEM & Non-STEM majors (p < 0.05). Although STEM majors scored 
#          an average of 4.31 points higher in exam scores compared to 
#          Non-STEM majors, the effect size was small (d = 0.23). This means that 
#          the difference in exam scores between both groups overlap a lot.

# --------------------------------------------------------------

# b. Run independent samples t-test to test if there is a mean difference 
#    in exam scores between On-line & Onsite students
glimpse(Lmethod_t_test)

t.test(`Exam Score` ~ Learning_Group,
       var.equal = FALSE,
       data = Lmethod_t_test,
       conf.level = 0.95)

# Generate descriptive stats for Learning Groups cohens d for independent samples t-test
lm_descrpitive_stats <- Lmethod_t_test %>% 
  group_by(Learning_Group) %>% 
  summarise(
    mean = mean(`Exam Score`),
    sd = sd(`Exam Score`),
    N = n()
  )

# Generate mean difference between Learning Groups
lm_mean_difference <- lm_descrpitive_stats[2, 2] - lm_descrpitive_stats[1, 2]


# Calculate cohens d for Learning Groups
lm_cohen_d <- cohens_d(`Exam Score` ~ Learning_Group,
         data = Lmethod_t_test)


# View outputs from independent samples t-test objects
lm_descrpitive_stats
lm_mean_difference
lm_cohen_d


# Finding: There was not a statistically significant mean difference in exam score 
#          between Online & On-Site students (p > 0.09). Although the Onsite students 
#          scored an average of 3.63 points higher in exam score compared to Online students,
#          the effect size was small. Implying that the difference in exam scores between 
#          both groups over lap.
# ==============================================================


# ==============================================================
# 6. FINAL VISUALIZATIONS  
# ==============================================================
# a. Correlation visualization of student smart phone usage and exam score
ggplot(analyzed_students_df, aes(x = `Smartphone Usage (hrs/day)`,
                                 y = `Exam Score`)
       ) +
  geom_point(color = "#6200EE", size = 2, alpha = 0.5) + 
  geom_smooth(method = "lm", color = "#03DAC5", se = TRUE, linewidth = 1.5) +
  labs(
    title = "No Practical Relationship between Student Smartphone Usage and Exam Scores",
    subtitle = "Relationship found statistically significant (*p* < 0.05) but, the effect size is low (*r* = -0.07). Significance susceptible to large sample size instead of real world effects.",
    x = "Smartphone Usage (Hours)",
    y = "Exam Scores") + 
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_textbox_simple(vjust = .7),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title = element_text(size = 11),
    axis.line = element_line(linetype = "dashed"),
    axis.text = element_text(size = 10)) 


?element_line
# b. Linear regression visualization of student attendance and exam scores?



# Independent samples t-test visualization of mean differences between STEM & Non-STEM Majors?


# Independent samples t-test visualization of mean differences between Online & Onsite Students?





