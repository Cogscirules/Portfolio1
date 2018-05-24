setwd("C:/Users/Bruger/Desktop/Cognitive Science/R/Eyetracking workshop")

library(pacman)
p_load(data.table, tidyverse, stringr, lmerTest, cvms, groupdata2, jpeg,grid)

fix1 = read.csv("FixationsV1.csv")
fix2 = read.csv("FixationsV2.csv")
fix3 = read.csv("FixationsV3.csv")

sac1 = read.csv("SaccadesV1.csv")
sac2 = read.csv("SaccadesV2.csv")
sac3 = read.csv("SaccadesV3.csv")

samp1 = read.csv("SamplesV1.csv")
samp2 = read.csv("SamplesV2.csv")
samp3 = read.csv("SamplesV3.csv")


fix_vis = subset(fix2, Task == "VisualSearch")
fix_social = subset(fix2, Task == "SocialEngagement")


# Visual Search, if statement for at merge experimentet med dataen

for (i in 1:nrow(fix_vis)){
  if (fix_vis$SearchOrder[i] == "1" & fix_vis$Trial[i] < 6) {
    fix_vis$Condition[i] = "Star"
  }
  if (fix_vis$SearchOrder[i] == "1" & fix_vis$Trial[i] > 5) {
    fix_vis$Condition[i] = "Count"
  }
  if (fix_vis$SearchOrder[i] == "2" & fix_vis$Trial[i] < 6) {
    fix_vis$Condition[i] = "Count"
  }
  if (fix_vis$SearchOrder[i] == "2" & fix_vis$Trial[i] > 5) {
    fix_vis$Condition[i] = "Star"
  }
}

#___________________


samp_vis = subset(samp1, Task == "VisualSearch")
samp_social = subset(samp1, Task == "SocialEngagement")

# Visual Search, if statement for at merge experimentet med dataen

for (i in 1:nrow(samp_vis)){
  if (samp_vis$SearchOrder[i] == "1" & samp_vis$Trial[i] < 6) {
    samp_vis$Condition[i] = "Star"
  }
  if (samp_vis$SearchOrder[i] == "1" & samp_vis$Trial[i] > 5) {
    samp_vis$Condition[i] = "Count"
  }
  if (samp_vis$SearchOrder[i] == "2" & samp_vis$Trial[i] < 6) {
    samp_vis$Condition[i] = "Count"
  }
  if (samp_vis$SearchOrder[i] == "2" & samp_vis$Trial[i] > 5) {
    samp_vis$Condition[i] = "Star"
  }
}









# Social Engagement

setwd("C:/Users/Bruger/Desktop/Cognitive Science/R/Eyetracking workshop/PupilsLogs")


log1 = read.csv("logfile_1_2_f.csv")
log2 = read.csv("logfile_2_1_f.csv")
log3 = read.csv("logfile_3_2_f.csv")
log4 = read.csv("logfile_4_1_F.csv")
log5 = read.csv("logfile_5_2_m.csv")
log6 = read.csv("logfile_6_1_m.csv")

#Tilføj alle på samme tid

folder = "C:/Users/Bruger/Desktop/Cognitive Science/R/Eyetracking workshop/PupilsLogs" 
fileList = list.files(path=folder, pattern="*.csv")
setwd("C:/Users/Bruger/Desktop/Cognitive Science/R/Eyetracking workshop/PupilsLogs")
pupil = do.call(rbind, lapply(fileList, function(x) read.csv(x, stringsAsFactors = F)))
setwd("C:/Users/Bruger/Desktop/Cognitive Science/R/Eyetracking workshop")

#Lav ID om så det kan merges, da det ikke hedder det samme, add 8 trials da der er 8 videoer

pupil$Trial = c(1,2,3,4,5,6,7,8)
colnames(pupil)[2] = "ParticipantID"

#Merge filerne
fix_file_social = merge(pupil, fix_social)
samp_file_social = merge(pupil, samp_social)

#Add en kolonne med actor gender
samp_file_social$ActorGender=regmatches(samp_file_social$video, regexpr("*.[_]", samp_file_social$video))
samp_file_social$ActorGender=substr(samp_file_social$ActorGender, 1, nchar(samp_file_social$ActorGender)-1)

#Ny kolonne for om det er direkte eller divergent engagement
for (i in 1:nrow(samp_file_social)){
  if (grepl("div", samp_file_social$video[i])) {
    samp_file_social$Directness[i] = "Divergent"
  }
  if (grepl("dir", samp_file_social$video[i])) {
    samp_file_social$Directness[i] = "Direct"
  }
}

#Ny kolonne for at sige om de er ostensive eller ej
for (i in 1:nrow(samp_file_social)){
  if (grepl("+o", samp_file_social$video[i])) {
    samp_file_social$Ostensiveness[i] = "Yes"
  }
  if (grepl("-o", samp_file_social$video[i])) {
    samp_file_social$Ostensiveness[i] = "No"
  }
}


#_________________________

#Merge filerne
sac_file_social = merge(pupil, sac_social)

#Add en kolonne med actor gender
sac_file_social$ActorGender=regmatches(sac_file_social$video, regexpr("*.[_]", sac_file_social$video))
sac_file_social$ActorGender=substr(sac_file_social$ActorGender, 1, nchar(sac_file_social$ActorGender)-1)

#Ny kolonne for om det er direkte eller divergent engagement
for (i in 1:nrow(sac_file_social)){
  if (grepl("div", sac_file_social$video[i])) {
    sac_file_social$Directness[i] = "Divergent"
  }
  if (grepl("dir", sac_file_social$video[i])) {
    sac_file_social$Directness[i] = "Direct"
  }
}

#Ny kolonne for at sige om de er ostensive eller ej
for (i in 1:nrow(sac_file_social)){
  if (grepl("+o", sac_file_social$video[i])) {
    sac_file_social$Ostensiveness[i] = "Yes"
  }
  if (grepl("-o", sac_file_social$video[i])) {
    sac_file_social$Ostensiveness[i] = "No"
  }
}


# Model-making

View(samp2)


model_vis = lmer(Duration ~ SearchType + (1 + Trial|ParticipantID), data = fix_vis)
summary(model_vis)

model_vis2 = lmer(Duration ~ SearchType * Trial +(1 + Trial|ParticipantID), data = fix_vis)
summary(model_vis2)

set.seed(1)

# Fold data. antal folds = 3
data_fix_cv = fold(fix_vis, k = 3, cat_col = 'SearchType', id_col = 'ParticipantID') %>% 
  arrange(.folds)

#Make models
mixed_models = c("Duration~SearchType+(1+ Trial|ParticipantID)")

#Cross Validate, read: https://github.com/LudvigOlsen/cvms/blob/master/README.md
CV1 = cross_validate(data_fix_cv, mixed_models, folds_col = '.folds', family='gaussian', REML = FALSE)

CV1
View(CV1)

#Make a log transformation
CV2 = cross_validate(data_fix_cv, "Duration ~ SearchType", folds_col = '.folds', family='gaussian', link = 'log', REML = FALSE)

CV2
View(CV2)

# Producing heatmaps for visual search

jet.colors = colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

img = readJPEG('C:/Users/Bruger/Desktop/Cognitive Science/R/Eyetracking workshop/eyetrackingscripts/foraging/other/ng049ws.jpg')
g = rasterGrob(img, interpolate=TRUE)

#density. indsæt participant nr, hvilken trial og ændre billedet i img. 
ggplot(subset(fix2, Task=='VisualSearch' & ParticipantID=='3_1_f1' & Trial==4), aes(x = PositionX, y = 1081-PositionY)) +
  xlim(0,1920) +
  ylim(0, 1080) +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=0, ymax=1080) + #xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  stat_density2d(geom="raster", aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=1000) + 
  scale_alpha(range = c(0.1, 0.6)) + scale_fill_gradientn(colours = jet.colors(10), trans="sqrt")


#Scanpath

x=subset(fix2, Task=='VisualSearch' & ParticipantID=='3_1_f1' & Trial==4)
x = x[order(x$Fixation),]

ggplot(x, aes(x = PositionX, y = 1081-PositionY, label=Fixation)) +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=0, ymax=1080) +
  geom_point(size = 5, alpha =0.5)+
  geom_path(size = 1, alpha = 0.3)+
  geom_text(aes(label = Fixation, size = 5))




#Making models for pupil size (social engagement task)

View(samp_file_social)
View(samp2)

samp2_cv = subset(samp2, Task == "SocialEngagement")

View(samp2_cv)

pup_model = lmer(PupilSize ~ Ostension + Directionality + ActorGender + ParticipantGender * Trial+(1+Trial|ParticipantID), data=samp2_cv)
summary(pup_model)

pup_model2 = lmer(PupilSize ~ Directionality + Ostension + ActorGender + ParticipantGender *Trial+(1+Trial|ParticipantID), data = samp2_cv)
summary(pup_model2)

pup_model3 = lmer(PupilSize ~ ActorGender + ParticipantGender + Directionality + Ostension * Trial + (1 + Trial|ParticipantID), data = samp2_cv)
summary(pup_model3)

set.seed(1)

# Fold data. antal folds = 3
samp_d_cv = fold(samp2_cv, k = 3, cat_col = 'ActorGender', id_col = 'ParticipantID') %>% 
  arrange(.folds)

#Make models
mixed_models = c("PupilSize ~ ActorGender + ParticipantGender + Directionality + Ostension * Trial + (1 + Trial|ParticipantID)")

#Cross Validate, read: https://github.com/LudvigOlsen/cvms/blob/master/README.md
CV1_2 = cross_validate(samp_d_cv, mixed_models, folds_col = '.folds', family='gaussian', REML = FALSE)

CV1_2
View(CV1_2)


CV2_2 = cross_validate(samp_d_cv, mixed_models, folds_col = '.folds', family='gaussian', REML = FALSE) 

CV2_2 
View(CV2_2) 


CV3_2 = cross_validate(samp_d_cv, mixed_models, folds_col = '.folds', family = 'gaussian', REML = FALSE)

CV3_2
View(CV3_2)




#Pupilsize ~ Dir * Ost *(TT + TT^2 + TT^3) + AG * PG * (TT+TT^2+TT^3) + Trial

#Pupilsize ~ Ostension + Direction + ActorGender*ParticipantGender+Trial + Timetrial + ParticipantGender +  TT^2 + TT^3 + (1+Trial+TimeTrial+Ostension+Direction|ParticipantID)







#Plotting social engagement

View(samp2)
samp2=subset(samp2, Task=='SocialEngagement')


ggplot(samp2, aes(x=TrialTime, y=PupilSize, col = ParticipantGender))+
  geom_smooth()+
  facet_grid(~ActorGender)




#Citations

citation()
