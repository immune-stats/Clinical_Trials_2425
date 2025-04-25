library(dplyr)

data <- read.csv('Project_3_data.csv')

data <- data %>%
  mutate(
    BMI = Weight / ((Height / 100)^2),
    Center = as.factor(Center),
    Sex = factor(Sex, levels = c(1, 2), labels = c("Female", "Male")),
    Disease_duration = factor(Disease_duration, levels = c(1, 2, 3),
                              labels = c("2-5 years", "6-9 years", ">10 years")),
    treatment_group = factor(treatment_group, levels = c(0, 1),
                             labels = c("Placebo", "Rituximab")),
    Infection_trigger = factor(Infection_trigger, levels = c(0, 1),
                               labels = c("No", "Yes")),
    response = as.numeric(response)
  )
data$Height[is.na(data$Height)] <- mean(data$Height, na.rm = TRUE)
data_filtered <- data %>%
  filter(Infection_trigger == "Yes")


model <- glm(response ~ treatment_group + Age + Sex + Disease_duration + BMI + Center,
             data = data_filtered,
             family = binomial(link = 'logit'))
summary(model)

model_final <- glm(formula = response ~ treatment_group + Age + Sex, family = binomial(link = "logit"), 
                   data = data_filtered)
summary(model_final)


#######################

model_final_final <- glm(formula = response ~ treatment_group + Age + Sex, family = binomial(link = "logit"), 
                   data = data)
summary(model_final_final)

model_final_final <- glm(formula = response ~ . -Weight -Height, family = binomial(link = "logit"), 
                         data = data)
summary(model_final_final)
exp(-0.852832) # ok 60%
########################

# Analysis of proportion

73 / 77
binom.test(73, 77)

# placebo
66 / 74
binom.test(66, 74)$conf.int

prop.test( c(73, 66), c(77, 74))





