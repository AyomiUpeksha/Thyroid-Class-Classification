#------------------------------------------------------------------------
#  (5) EDA 

# 5.1 Investigating the pairwise Qulitative variables with Thyroid class
glimpse(balnced_rose)

# Converting to numerical variables
balnced_rose$patient_age <- as.numeric(balnced_rose$patient_age)
balnced_rose$TSH_reading <- as.numeric(balnced_rose$TSH_reading)
balnced_rose$T3_reading <- as.numeric(balnced_rose$T3_reading)
balnced_rose$T4_reading <- as.numeric(balnced_rose$T4_reading)
balnced_rose$thyrox_util_rate_T4U_reading <- as.numeric(balnced_rose$thyrox_util_rate_T4U_reading)
balnced_rose$FTI_reading <- as.numeric(balnced_rose$FTI_reading)

p3 <- balnced_rose %>% filter(is.na(ThryroidClass) == FALSE) %>%
  ggpairs(columns = c("patient_age","TSH_reading","T3_reading","T4_reading","thyrox_util_rate_T4U_reading", "FTI_reading"),
          mapping = aes(color = ThryroidClass))

p3

# no Multicoliniariaty effects

# 5.2 One quantitative variable with Thyroid class


p4 <- balnced_rose %>% ggplot(aes(x= patient_age, y =ThryroidClass, fill = ThryroidClass)) + 
  geom_boxplot() 

p5 <- balnced_rose %>% ggplot(aes(x= TSH_reading, y =ThryroidClass, fill = ThryroidClass)) + 
  geom_boxplot() 

p5 <- balnced_rose %>% ggplot(aes(x= T3_reading, y =ThryroidClass, fill = ThryroidClass)) + 
  geom_boxplot() 

p6 <- balnced_rose %>% ggplot(aes(x= T4_reading, y =ThryroidClass, fill = ThryroidClass)) + 
  geom_boxplot() 

p7 <- balnced_rose %>% ggplot(aes(x= thyrox_util_rate_T4U_reading, y =ThryroidClass, fill = ThryroidClass)) + 
  geom_boxplot() 

p8 <- balnced_rose %>% ggplot(aes(x= FTI_reading, y =ThryroidClass, fill = ThryroidClass)) + 
  geom_boxplot() 


# All variables have some impaction to the thyroid class

# 5.3 One qualitative variables with thyroid class
# Pair wise chi-square test
set.seed(3333)

p14 <- ggbarstats( # p = 2.28e-07
  data = df_12,
  x = sick,
  y = ThryroidClass
) + labs(caption = NULL) # remove caption

p15 <- ggbarstats( #p=0.05
  data = df_12,
  x = pregnant,
  y = ThryroidClass
) + labs(caption = NULL) # remove caption

p28 <- ggbarstats( # p = 3.32e-46
  data = df_12,
  x = ref_src,
  y = ThryroidClass
) + labs(caption = NULL) # remove caption

p13 <- ggbarstats( # p =0.20
  data = df_12,
  x = patient_gender,
  y = ThryroidClass
) + labs(caption = NULL) # remove caption


plot1 <- p14 + p15 + p28 + p13

plot1 +
  plot_layout(guide = 'collect') +
  plot_annotation(
    title = 'Figure 3.2.1 Distribution of Thyroid class with Sikness, Referral source,pregnant & gender',
    theme = theme(plot.title = element_text(hjust = 0.5, size = 10)))


p16 <- ggbarstats( # p =0.02
  data = df_12,
  x = presc_thyroxine,
  y = ThryroidClass
) + labs(caption = NULL) # remove caption

p21 <- ggbarstats( # p = 1.29e-05
  data = df_12,
  x = query_hypothyroid,
  y = ThryroidClass
) + labs(caption = NULL) 

p18 <- ggbarstats( # p = 0.09
  data = df_12,
  x = presc_anthyroid_meds,
  y = ThryroidClass
) + labs(caption = NULL) 

p19 <- ggbarstats( # p = 0.08
  data = df_12,
  x = thyroid_surgery,
  y = ThryroidClass
) + labs(caption = NULL) 

p20 <- ggbarstats( # p = 0.16
  data = df_12,
  x = radioactive_iodine_therapyI131,
  y = ThryroidClass
) + labs(caption = NULL) 

p22 <- ggbarstats( # p=0.06
  data = df_12,
  x = query_hyperthyroid,
  y = ThryroidClass
) + labs(caption = NULL) 

p25 <- ggbarstats( # p= 0.18
  data = df_12,
  x = tumor,
  y = ThryroidClass
) + labs(caption = NULL) 

p27 <- ggbarstats( # p= 0.02
  data = df_12,
  x = psych_condition,
  y = ThryroidClass
) + labs(caption = NULL) 

p23 <- ggbarstats( # p=0.91
  data = df_12,
  x = lithium,
  y = ThryroidClass
) + labs(caption = NULL) 

p24 <- ggbarstats( # p=0.62
  data = df_12,
  x = goitre,
  y = ThryroidClass
) + labs(caption = NULL) 

p26 <- ggbarstats( # p=6.72e-04
  data = df_12,
  x = hypopituitarism,
  y = ThryroidClass
) + labs(caption = NULL) 

17 <- ggbarstats( # p= 0.41
  data = df_12,
  x = queried_why_on_thyroxine,
  y = ThryroidClass
) + labs(caption = NULL) 

plot2 <- p17

plot2 +
  plot_layout(guide = 'collect') +
  plot_annotation(
    title = 'Figure 3.2.2 Distribution of Thyroid class with Hypopituitarism and Action status of Quirey',
    theme = theme(plot.title = element_text(hjust = 0.5, size = 10)))

#queried_why_on_thyroxine have no effect to the thyroid class

df_13 <- df_12 %>% select(., -(queried_why_on_thyroxine))


































