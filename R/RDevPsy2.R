library(lme4)
library(tidyverse)
library(eyetrackingR)
library(DHARMa)
library(performance)

data(cbpp) #bovine pleuropneumonia

# make proportion
cbpp2 <- cbpp %>% 
  mutate(prop = incidence / size)

hist(cbpp2$prop)

cbpp_lm <- lm(prop ~ period, data = cbpp2)
summary(cbpp_lm)

plot(cbpp_lm)
qqnorm(resid(cbpp_lm))
check_model(cbpp_lm)
simulateResiduals(cbpp_lm, plot = T)


cbpp_glm <- glm(cbind(incidence, size - incidence) ~ period, 
                family = binomial,
                data = cbpp2)
summary(cbpp_glm)
simulateResiduals(cbpp_glm, plot = T)

cbpp2 %>% 
  ggplot(aes(x = as.numeric(period), y = prop)) +
  geom_point() +
  stat_smooth(method = 'lm')

cbpp2$predict <- predict(cbpp_glm, type = 'response')

cbpp2 %>% 
  ggplot(aes(x = as.numeric(period), y = prop)) +
  geom_point() +
  stat_summary(aes(y = predict), fun = 'mean', geom = 'line', colour = 'blue')

# plots
cbpp2 %>% 
  ggplot(aes(x = as.numeric(period), y = prop, colour = herd)) +
  geom_point() +
  geom_line(alpha = 0.4)

# random int
cbpp_lmer <- lmer(prop ~ period +
                    (1|herd), 
                  data = cbpp2)
summary(cbpp_lmer)

check_model(cbpp_lmer)
simulateResiduals(cbpp_lmer, plot = T)

cbpp2$predict_lmer <- predict(cbpp_lmer, type = 'response')

cbpp2 %>% 
  ggplot(aes(x = period, y = prop, group = herd)) +
  geom_point() +
  geom_line(aes(y = predict_lmer),  colour = 'blue')


 # ---------------------------------------------------
data("sleepstudy")

sleepstudy %>% 
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point()

lm.0 <- lm(Reaction ~ 1, data = sleepstudy)
# complete pooling
lm.1 <- lm(Reaction ~ Days, data = sleepstudy)
#no pooling
lm.2 <- lm(Reaction ~ Days + Subject, data = sleepstudy)
#partial pooling
lme.1 <- lmer(Reaction ~ Days + (1|Subject), data = sleepstudy)


lme.3 <- lmer ( Reaction ~ Days + ( Days | Subject ) , data = sleepstudy )
# -----------------------------------------------------

etdata <- read_csv('data/ET_data.csv')

etdata2 <- etdata %>% 
  group_by(ParticipantName, Target) %>% 
  summarise(Prop = mean(Prop, na.rm = T)) %>% 
  ungroup()


# -----------------------------------------------------
data("word_recognition")

data <- make_eyetrackingr_data(word_recognition, 
                               participant_column = "ParticipantName",
                               trial_column = "Trial",
                               time_column = "TimeFromTrialOnset",
                               trackloss_column = "TrackLoss",
                               aoi_columns = c('Animate','Inanimate'),
                               treat_non_aoi_looks_as_missing = TRUE
)

# data <- subset_by_window(data, window_start_msg = "TrialStart", msg_col = "Message", rezero= TRUE)

response_window <- subset_by_window(data, 
                                    window_start_time = 15500, 
                                    window_end_time = 21000, 
                                    rezero = FALSE)

response_window_clean <- clean_by_trackloss(data = response_window, 
                                            trial_prop_thresh = .25)

response_window_clean$Target <- as.factor( ifelse(test = grepl('(Spoon|Bottle)', response_window_clean$Trial), 
                                                  yes = 'Inanimate', 
                                                  no  = 'Animate') )



response_window_agg_by_sub <- make_time_window_data(response_window_clean, 
                                                    aois='Animate',
                                                    predictor_columns=c('Target','Age','MCDI_Total'),
                                                    summarize_by = "ParticipantName")


plot(response_window_agg_by_sub, predictor_columns="Target", dv = "Prop")

t.test(Prop ~ Target, data= response_window_agg_by_sub, paired=TRUE)

# add trial level
response_window_agg <- make_time_window_data(response_window_clean, 
                                             aois='Animate',
                                             predictor_columns=c('Target','Age','MCDI_Total'))

model_time_window <- lmer(Prop ~ TargetC + (1 + TargetC | Trial) + (1 | ParticipantName), 
                          data = response_window_agg)

model_time_window_add_predictors <- lmer(Prop ~ TargetC*AgeC*MCDI_TotalC + (1 + TargetC | Trial) + (1 | ParticipantName), 
                                         data = response_window_agg)
# -----------------------------------------
response_time <- make_time_sequence_data(response_window_clean, time_bin_size = 100, 
                                         predictor_columns = c("Target"),
                                         aois = "Animate"
)


plot(response_time, predictor_column = "Target") 

# polynomial
timecodes <- unique(response_time[, c('ot1','ot2','ot3','ot4','ot5','ot6','ot7')])
timecodes$num <- 1:nrow(timecodes)

ggplot(timecodes, aes(x=num, y=ot1)) +
  geom_line() +
  geom_line(aes(y=ot2), color='red') +    # quadratic
  geom_line(aes(y=ot3), color='blue') +   # cubic
  geom_line(aes(y=ot4), color='green') +  # quartic
  geom_line(aes(y=ot5), color='purple') + # quintic 
  geom_line(aes(y=ot6), color='yellow') + # sextic
  geom_line(aes(y=ot7), color='pink') +   # septic
  scale_x_continuous(name="") +
  scale_y_continuous(name="")

# gca ------------------------
response_time$TargetC <- ifelse(response_time$Target == 'Animate', .5, -.5)
response_time$TargetC <- as.numeric(scale(response_time$TargetC, center=TRUE, scale=FALSE))

# Construct model
model_time_sequence <- lmer(Prop ~ TargetC*(ot1) + (1 + ot1 | Trial) + (1 + ot1 | ParticipantName), 
                            data = response_time, REML = FALSE)


plot(response_time, predictor_column = "Target", dv = "Elog", model = model_time_sequence) +
  theme_light()

model_time_sequence3 <- lmer(Prop ~ TargetC*(ot1 + ot2 + ot3 + ot4) + (1 | Trial) + (1 | ParticipantName), 
                            data = response_time, REML = FALSE)
