library(lme4)
library(glmmTMB)
library(MASS)
library(tidyverse)
library(DHARMa)
library(performance)
library(faraway)
library(hnp)
library(eyetrackingR)
library(PupillometryR)

data(cbpp)

cbpp2 <- cbpp %>% 
  mutate(prop = incidence / size)

hist(cbpp2$incidence)
hist(cbpp2$prop)

cbpp_lm <- lm(prop ~ period, data = cbpp2)
summary(cbpp_lm)

qqnorm(resid(cbpp_lm)); qqline(resid(cbpp_lm), col = 2, lty = 2)
plot(cbpp_lm)

cbpp_lm2 <- lm(incidence ~ period, data = cbpp2)
summary(cbpp_lm2)

qqnorm(resid(cbpp_lm2)); qqline(resid(cbpp_lm2), col = 2, lty = 2)

cbpp_glm <- glm(incidence ~ period, 
                family = poisson(link = 'log'),
                data = cbpp2)
summary(cbpp_glm)

check_model(cbpp_glm)
check_posterior_predictions(cbpp_glm)

cbpp_glm2 <- glm(cbind(incidence, size - incidence) ~ period, 
                family = binomial,
                data = cbpp2)
summary(cbpp_glm2)

check_model(cbpp_glm2)
simulateResiduals(cbpp_glm2, plot = T)
halfnorm(resid(cbpp_glm2))
hnp(cbpp_glm2)

cbpp2 %>% 
  ggplot(aes(x = period, y = prop)) +
  geom_point() +
  facet_wrap(~herd)

cbpp2$predict_lm <- predict(cbpp_lm)

cbpp2 %>% 
  ggplot(aes(x = as.numeric(period), y = prop)) +
  geom_point() +
  geom_line(aes(y = predict_lm), colour = 'red') +
  facet_wrap(~herd)

cbpp2$predict_glm <- predict(cbpp_glm2, type = 'response')

cbpp2 %>% 
  ggplot(aes(x = as.numeric(period), y = prop)) +
  geom_point() +
  geom_line(aes(y = predict_lm), colour = 'red') +
  geom_line(aes(y = predict_glm), colour = 'blue') +
  facet_wrap(~herd)

cbpp_glmer <- glmer(cbind(incidence, size - incidence) ~ period +
                      (1|herd), 
                 family = binomial,
                 data = cbpp2)
summary(cbpp_glmer)

# cbpp_glmer2 <- glmer(cbind(incidence, size - incidence) ~ period +
#                       (period|herd), 
#                     family = binomial,
#                     data = cbpp2)
# summary(cbpp_glmer2)

# sleep ----------------------------------
data("sleepstudy")

sleepstudy %>% 
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point() +
  facet_wrap(~Subject)

lm.0 <- lm(Reaction ~ 1, data = sleepstudy)
summary(lm.0)

sleepstudy$predict0 <- predict(lm.0)

sleepstudy %>% 
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point() +
  geom_line(aes(y = predict0), colour = 'green') +
  facet_wrap(~Subject)

lm.1 <- lm(Reaction ~ Days, data = sleepstudy)
summary(lm.1)
anova(lm.0, lm.1, test = 'Chisq')

sleepstudy$predict1 <- predict(lm.1)

sleepstudy %>% 
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point() +
  geom_line(aes(y = predict0), colour = 'green') +
  geom_line(aes(y = predict1), colour = 'pink') +
  facet_wrap(~Subject)

lm.2 <- lm(Reaction ~ Days + Subject, data = sleepstudy)
summary(lm.2)
anova(lm.1, lm.2, test = 'Chisq')

sleepstudy$predict2 <- predict(lm.2)

sleepstudy %>% 
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point() +
  geom_line(aes(y = predict0), colour = 'green') +
  geom_line(aes(y = predict1), colour = 'pink') +
  geom_line(aes(y = predict2), colour = 'purple') +
  facet_wrap(~Subject)

lme.0 <- lmer(Reaction ~ Days + #lmerTest to get p values
                (1|Subject),
              data = sleepstudy)

compare_performance(lme.0, lm.1)
compare_performance(lme.0, lm.2)

sleepstudy$predict_lme <- predict(lme.0)

sleepstudy %>% 
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point() +
  # geom_line(aes(y = predict0), colour = 'green') +
  # geom_line(aes(y = predict1), colour = 'pink') +
  geom_line(aes(y = predict2), colour = 'purple') +
  geom_line(aes(y = predict_lme), colour = 'red') +
  facet_wrap(~Subject)

summary(lme.0)
drop1(lme.0, ~., test = 'Chisq')

lme.1 <- lmer(Reaction ~ Days + #lmerTest to get p values
                (Days|Subject),
              data = sleepstudy)

summary(lme.1)
anova(lme.0, lme.1)
drop1(lme.1, ~., test = 'Chisq')

check_model(lme.1)
simulateResiduals(lme.1, plot = T)

sleepstudy$predict_lme1 <- predict(lme.1)

sleepstudy %>% 
  ggplot(aes(x = Days, y = Reaction)) +
  geom_point() +
  # geom_line(aes(y = predict0), colour = 'green') +
  # geom_line(aes(y = predict1), colour = 'pink') +
  geom_line(aes(y = predict2), colour = 'purple') +
  geom_line(aes(y = predict_lme), colour = 'red') +
  geom_line(aes(y = predict_lme1), colour = 'blue') +
  facet_wrap(~Subject)

# eyetracking -------------------------------
data("word_recognition")

data <- make_eyetrackingr_data(word_recognition, 
                               participant_column = "ParticipantName",
                               trial_column = "Trial",
                               time_column = "TimeFromTrialOnset",
                               trackloss_column = "TrackLoss",
                               aoi_columns = c('Animate','Inanimate'),
                               treat_non_aoi_looks_as_missing = TRUE
)

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

t.test(Prop ~ Target, data = response_window_agg_by_sub)

# trial level
response_window_agg <- make_time_window_data(response_window_clean, 
                                             aois='Animate',
                                             predictor_columns=c('Target','Age','MCDI_Total'))

hist(response_window_agg$Prop)

response_window_agg <- response_window_agg %>% 
  mutate(TargetC = ifelse(Target == 'Animate', 0.5, -0.5))

model_time_window <- lmer(Prop ~ TargetC + 
                            (1| Trial) + (1 | ParticipantName), 
                          data = response_window_agg)

summary(model_time_window)
simulateResiduals(model_time_window, plot = T)

model_time_window2 <- glmer(cbind(SamplesInAOI, SamplesTotal - SamplesInAOI) ~ TargetC + 
                            (1| Trial) + (1 | ParticipantName), 
                            family = binomial,
                          data = response_window_agg)

summary(model_time_window2)

# time course data ----------------------

response_time <- make_time_sequence_data(response_window_clean, 
                                         time_bin_size = 100, 
                                         predictor_columns = c("Target"),
                                         aois = "Animate"
)

plot(response_time, predictor_column = 'Target') +
  theme_classic()

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


plot(response_time) +
  theme_classic()

response_time <- response_time %>% 
  mutate(TargetC = ifelse(Target == 'Animate', 0.5, -0.5))

model_time_sequence <- lmer(Prop ~ TargetC * (ot1) + 
                              (1 + ot1 | Trial) + (1 + ot1 | ParticipantName), 
                            data = response_time)

summary(model_time_sequence)

plot(response_time, predictor_column = 'Target', model = model_time_sequence) +
  theme_classic()

model_time_sequence2 <- lmer(Prop ~ TargetC * (ot1 + ot2 + ot3) + 
                              (1 + ot1 + ot2 + ot3 | Trial) + 
                               (1 + ot1 + ot2 + ot3 | ParticipantName), 
                            data = response_time)

summary(model_time_sequence2)

plot(response_time, predictor_column = 'Target', model = model_time_sequence2) +
  theme_classic()

model_time_sequence3 <- glmer(cbind(SamplesInAOI, SamplesTotal - SamplesInAOI)~ 
                               TargetC * (ot1 + ot2 + ot3) + 
                               (1 + ot1 + ot2 + ot3 | Trial) + 
                               (1 + ot1 + ot2 + ot3 | ParticipantName), 
                              family = binomial,
                             data = response_time)

summary(model_time_sequence3)
drop1(model_time_sequence3, ~., test = 'Chisq')

model_time_sequence4 <- glmmTMB(cbind(SamplesInAOI, SamplesTotal - SamplesInAOI)~ 
                                TargetC * (ot1 + ot2 + ot3) + 
                                (1 + ot1 + ot2 + ot3 | Trial) + 
                                (1 + ot1 + ot2 + ot3 | ParticipantName), 
                              family = betabinomial,
                              data = response_time)

summary(model_time_sequence4)
a <- simulateResiduals(model_time_sequence4, plot = T)
testDispersion(a)

response_time2 <- response_time %>% 
  filter(SamplesTotal > 0)

# model_time_sequence5 <- glmmPQL(cbind(SamplesInAOI, SamplesTotal - SamplesInAOI)~ 
#                                   TargetC * (ot1 + ot2 + ot3),
#                                   # (1 + ot1 + ot2 + ot3 | Trial) + 
#                                   random = ~1 + ot1 + ot2 + ot3 | ParticipantName, 
#                                 family = binomial,
#                                 data = response_time2)
# 
# summary(model_time_sequence5)
# halfnorm(resid(model_time_sequence5))

save(model_time_sequence4, file = 'models/mts4')

# pupil data ----------------------------------------
data("pupil_data")

pupil_data$ID <- as.character(pupil_data$ID)
pupil_data <- subset(pupil_data, ID != 8)

pupil_data$LPupil[pupil_data$LPupil == -1] <- NA
pupil_data$RPupil[pupil_data$RPupil == -1] <- NA

Sdata <- make_pupillometryr_data(data = pupil_data,
                                 subject = ID,
                                 trial = Trial,
                                 time = Time,
                                 condition = Type)

plot(Sdata, pupil = LPupil, group = 'condition') +
  theme_classic()

regressed_data <- regress_data(data = Sdata,
                               pupil1 = RPupil,
                               pupil2 = LPupil)

mean_data <- calculate_mean_pupil_size(data = regressed_data,
                                       pupil1 = RPupil,
                                       pupil2 = LPupil)

plot(mean_data, pupil = mean_pupil, group = 'condition') +
  theme_classic()

mean_data <- downsample_time_data(data = mean_data,
                                  pupil = mean_pupil,
                                  timebin_size = 50,
                                  option = 'median')

mean_data2 <- clean_missing_data(mean_data,
                                 pupil = mean_pupil,
                                 trial_threshold = .75,
                                 subject_trial_threshold = .75)

plot(mean_data2, pupil = mean_pupil, group = 'subject')

#filter pupil
filtered_data <- filter_data(data = mean_data2,
                             pupil = mean_pupil,
                             filter = 'median',
                             degree = 11)

plot(filtered_data, pupil = mean_pupil, group = 'subject')

#interpolate
int_data <- interpolate_data(data = filtered_data,
                             pupil = mean_pupil,
                             type = 'linear')

plot(int_data, pupil = mean_pupil, group = 'subject')

base_data <- baseline_data(data = int_data,
                           pupil = mean_pupil,
                           start = 0,
                           stop = 100)

plot(base_data, pupil = mean_pupil, group = 'subject')
plot(base_data, pupil = mean_pupil, group = 'condition')

window <- create_window_data(data = base_data,
                             pupil = mean_pupil)

plot(window, 
     pupil = mean_pupil, 
     windows = F, 
     geom = 'raincloud')

timeslots <- create_time_windows(data = base_data,
                                 pupil = mean_pupil,
                                 breaks = c(0, 2000, 4000, 6000, 8000, 10000))

plot(timeslots, 
     pupil = mean_pupil, 
     windows = T, 
     geom = 'raincloud')

#difference data
differences <- create_difference_data(data = base_data,
                                      pupil = mean_pupil)


plot(differences, pupil = mean_pupil, geom = 'line')


spline_data <- create_functional_data(data = differences,
                                      pupil = mean_pupil,
                                      basis = 10,
                                      order = 4)

plot(spline_data, pupil = mean_pupil, geom = 'line', colour = 'blue')

ft_data <- run_functional_t_test(data = spline_data,
                                 pupil = mean_pupil,
                                 alpha = 0.05)

plot(ft_data, show_divergence = T, colour = 'red', fill = 'orange')
