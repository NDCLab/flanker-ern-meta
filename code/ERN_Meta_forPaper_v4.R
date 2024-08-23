
#load libraries
library(metafor)
library(dplyr)

#define location for plot outpus
plot_dir <- "/Users/gbuzzell/Documents/Projects/ern_meta/final_data_set/plots/"




#Overall effect of ERN-anxiety for Flanker tasks
temp_dataSet <- read.csv("/Users/gbuzzell/Documents/Projects/ern_meta/final_data_set/data/ern_meta_data_for_r_2024-08-21.csv")
dataSet<-subset(temp_dataSet, (!is.na(temp_dataSet[,2]))) #subset to only rows with r value (all valid rows)
dataSet_z <- escalc(measure="ZCOR", ri=dataSet$r, ni=dataSet$n, data=dataSet, slab=id) #convert to z scores
#select only flanker task studies
dataSet_z <- filter(dataSet_z, task_1f_2g_3s_4o == 1) 

#meta-analysis of overall ERN-anxiety relation when restricting to only flanker task studies
meta <- rma(yi, vi, slab = id, data=dataSet_z) 
meta
confint(meta) #additional confidence intervals for model
sum(dataSet_z$n)

#Egger's test 
regtest(meta)






#Test moderating effect of stimulus type (arrow vs. letter) 
temp_dataSet <- read.csv("/Users/gbuzzell/Documents/Projects/ern_meta/final_data_set/data/ern_meta_data_for_r_2024-08-21.csv")
dataSet<-subset(temp_dataSet, (!is.na(temp_dataSet[,2]))) #subset to only rows with r value (all valid rows)
dataSet_z <- escalc(measure="ZCOR", ri=dataSet$r, ni=dataSet$n, data=dataSet, slab=id) #convert to z scores
#select only flanker task studies
dataSet_z <- filter(dataSet_z, task_1f_2g_3s_4o == 1) 
#select only studies that have stimulus type codes (remove NAs)
dataSet_z <- filter(dataSet_z, !is.na(flankType_0a_1l_2o))

# #Note that results of the moderation analysis do not qualitatively change when
# #removing "modified" flanker tasks:
# dataSet_z <- filter(dataSet_z, mod1 == 0) #remove "modified" flanker tasks

# #Note that results of the moderation analysis do not qualitatively change when
# #restricting the analysis to only studies that assessed worry:
# dataSet_z <- filter(dataSet_z, Worry == 1) #restrict to worry studies only

#meta-analysis testing moderating effect of flanker task type (arrow vs. letter) 
meta <- rma(yi, vi, slab = id, mods = ~factor(flankType_0a_1l_2o), data=dataSet_z) 
meta
confint(meta) #additional confidence intervals for model
sum(dataSet_z$n)

#using the same model, get estimates of effect size at each level of moderator
preds <- predict(meta,newmods=c(0, 1), addx=TRUE)
preds

#get number of studies an n for each level of moderator
dataSet_mod0 <- filter(dataSet_z, flankType_0a_1l_2o == 0) 
nrow(dataSet_mod0)
sum(dataSet_mod0$n)

dataSet_mod1 <- filter(dataSet_z, flankType_0a_1l_2o == 1) 
nrow(dataSet_mod1)
sum(dataSet_mod1$n)

#Forest plot
output_path <- paste0(plot_dir, "forrest_plot_stimulus.pdf")
#write pdf
pdf(output_path, width = 8.5, height = 11)  # Adjust width if needed
forest(meta, order=flankType_0a_1l_2o,ylim = c(-2,49))
abline(h = 0)
preds <- predict(meta,newmods=c(0, 1), addx=TRUE)
addpoly(preds, rows =-1, mlab = c("Arrow Stimuli", "Letter Stimuli"))
text(-2.7, 48, "Author(s) and Year", font = 2)
text(2.4, 48, "Effect Size [95% CI]", font = 2)
dev.off()#close pdf

#funnel plot
output_path <- paste0(plot_dir, "funnel_plot_stimulus.pdf")
#write pdf
pdf(output_path, width = 8, height = 6)  # Adjust width if needed
#adjust margin size, label size, number size
par(mar=c(5.1, 4.1, 4.1, 2.1), cex.lab = 1.3, cex.axis = 1.2)
#Creating a color vector based on the levels of binary moderator
colors <- ifelse(dataSet_z$flankType_0a_1l_2o == 0, "black", "gray") 
#Generating the funnel plot with color-coding
metafor::funnel(meta, digits=c(1,1), back="white", col = colors, ylab = "Standard Error")
# Add a legend
legend("topleft", legend = c("Arrow Stimuli", "Letter Stimuli"), 
       col = c("black", "gray"), pch = 16, cex = 1.3, bty = "n")
dev.off()#close pdf

#Egger's test 
regtest(meta)

#Running influence function and further investigating results to determine if there
#is undue influence of specific studies. Important: note that cutoffs for the 
#influence function are arbitrary and studies with larger sample sizes 
#(and smaller SE) can get flagged by the influence function despite them simply
#reflecting higher precision estimates that should not be removed. Thus, approach
#for using influence function was to run for omnibus model, then carefully inspect
#each study flagged by the function to determine whether the study appeared to be
#consistent with the broader pattern of results and seemed to be flagged only 
#due to the smaller SE and higher weighting in the model. In such cases, the
#study was not removed. However, if the study appeared to be a possible outlier,
#we assessed whether the results changed when excluding study. If the results
#remained qualtiatively unchanged, then the study was retained. 
inf <- influence(meta)
print(inf)
plot(inf)
#The following studies were flagged by the influence function: 
#"Larson et al (2013)*"
#"Luu (2000)"
#However, only the Luu (2000) appears to be a potential outlier. Rerunning without
#this study included does not qualitatively change results. Thus, leaving in.
dataSet_z<-dataSet_z[!(dataSet_z$id=="Luu (2000)"),]
meta <- rma(yi, vi, slab = id, mods = ~factor(flankType_0a_1l_2o), data=dataSet_z) 
meta






#Test moderating effect of response format (one vs. two hands) 
temp_dataSet <- read.csv("/Users/gbuzzell/Documents/Projects/ern_meta/final_data_set/data/ern_meta_data_for_r_2024-08-21.csv")
dataSet<-subset(temp_dataSet, (!is.na(temp_dataSet[,2]))) #subset to only rows with r value (all valid rows)
dataSet_z <- escalc(measure="ZCOR", ri=dataSet$r, ni=dataSet$n, data=dataSet, slab=id) #convert to z scores
#select only flanker task studies
dataSet_z <- filter(dataSet_z, task_1f_2g_3s_4o == 1)
#select only studies that have handedness codes (remove NAs)
dataSet_z <- filter(dataSet_z, !is.na(hand))

# #Note that results of the moderation analysis do not qualitatively change when
# #removing "modified" flanker tasks:
# dataSet_z <- filter(dataSet_z, mod1 == 0) #remove "modified" flanker tasks

# #Note that results of the moderation analysis do not qualitatively change, although
# #significance of the results becomes a trend (p = .061), when when restricting the
# #analysis to only studies that assessed worry:
# dataSet_z <- filter(dataSet_z, Worry == 1) #restrict to worry studies only

#meta-analysis testing moderating effect of response format (one vs. two hands) 
meta <- rma(yi, vi, slab=id, mods = ~factor(hand), data=dataSet_z) 
meta
confint(meta) #additional confidence intervals for model
sum(dataSet_z$n)

#using the same model, get estimates of effect size at each level of moderator
preds <- predict(meta,newmods=c(0, 1), addx=TRUE)
preds

#get number of studies an n for each level of moderator
dataSet_mod0 <- filter(dataSet_z, hand == 1) 
nrow(dataSet_mod0)
sum(dataSet_mod0$n)

dataSet_mod1 <- filter(dataSet_z, hand == 2) 
nrow(dataSet_mod1)
sum(dataSet_mod1$n)

#Forest plot
output_path <- paste0(plot_dir, "forrest_plot_hand.pdf")
#write pdf
pdf(output_path, width = 8.5, height = 11)  # Adjust width if needed
forest(meta, order=hand,ylim = c(-2,48))
abline(h = 0)
preds <- predict(meta,newmods=c(0, 1), addx=TRUE)
addpoly(preds, rows =-1, mlab = c("One-handed", "Two-handed"))
text(-2.7, 47, "Author(s) and Year", font = 2)
text(2.4, 47, "Effect Size [95% CI]", font = 2)
dev.off()#close pdf

#funnel plot
output_path <- paste0(plot_dir, "funnel_plot_hand.pdf")
#write pdf
pdf(output_path, width = 8, height = 6)  # Adjust width if needed
#adjust margin size, label size, number size
par(mar=c(5.1, 4.1, 4.1, 2.1), cex.lab = 1.3, cex.axis = 1.2)
#Creating a color vector based on the levels of binary moderator
colors <- ifelse(dataSet_z$flankType_0a_1l_2o == 1, "black", "gray") 
#Generating the funnel plot with color-coding
metafor::funnel(meta, digits=c(1,1), back="white", col = colors, ylab = "Standard Error")
# Add a legend
legend("topleft", legend = c("One-handed", "Two-handed"), 
       col = c("black", "gray"), pch = 16, cex = 1.3, bty = "n")
dev.off()#close pdf

#Egger's test 
regtest(meta)

#Running influence function and further investigating results to determine if there
#is undue influence of specific studies. Important: note that cutoffs for the 
#influence function are arbitrary and studies with larger sample sizes 
#(and smaller SE) can get flagged by the influence function despite them simply
#reflecting higher precision estimates that should not be removed. Thus, approach
#for using influence function was to run for omnibus model, then carefully inspect
#each study flagged by the function to determine whether the study appeared to be
#consistent with the broader pattern of results and seemed to be flagged only 
#due to the smaller SE and higher weighting in the model. In such cases, the
#study was not removed. However, if the study appeared to be a possible outlier,
#we assessed whether the results changed when excluding study. If the results
#remained qualtiatively unchanged, then the study was retained. 
inf <- influence(meta)
print(inf)
plot(inf)
#The following study was flagged, but does not appear to be a potential outlier.
#"Larson et al (2013)*"






##MAIN##
#Test moderating effect of block feedback (present vs. absent)
temp_dataSet <- read.csv("/Users/gbuzzell/Documents/Projects/ern_meta/final_data_set/data/ern_meta_data_for_r_2024-08-21.csv")
dataSet<-subset(temp_dataSet, (!is.na(temp_dataSet[,2]))) #subset to only rows with r value (all valid rows)
dataSet_z <- escalc(measure="ZCOR", ri=dataSet$r, ni=dataSet$n, data=dataSet, slab=id) #convert to z scores
#select only flanker task studies
dataSet_z <- filter(dataSet_z, task_1f_2g_3s_4o == 1)
#select only studies that have block-level feedback codes (remove NAs)
dataSet_z <- filter(dataSet_z, !is.na(blockFB))
#Additionally select only studies WITHOUT trial-level feedback (since focus is on block-level)
dataSet_z <- filter(dataSet_z, trialFB_any == 0)

# #Note that results of the moderation analysis do not qualitatively change when
# #removing "modified" flanker tasks:
# dataSet_z <- filter(dataSet_z, mod1 == 0) #remove "modified" flanker tasks

# #Note that results of the moderation analysis do not qualitatively change when
# #restricting the analysis to only studies that assessed worry:
# dataSet_z <- filter(dataSet_z, Worry == 1) #restrict to worry studies only

#meta-analysis testing moderating effect of block feedback (format (present vs. absent) 
meta <- rma(yi, vi, slab=id, mods = ~factor(blockFB), data=dataSet_z) 
meta
confint(meta) #additional confidence intervals for model
sum(dataSet_z$n)

#using the same model, get estimates of effect size at each level of moderator
preds <- predict(meta,newmods=c(0, 1), addx=TRUE)
preds

#get number of studies an n for each level of moderator
dataSet_mod0 <- filter(dataSet_z, blockFB == 0) 
nrow(dataSet_mod0)
sum(dataSet_mod0$n)

dataSet_mod1 <- filter(dataSet_z, blockFB == 1) 
nrow(dataSet_mod1)
sum(dataSet_mod1$n)

#Forest plot
output_path <- paste0(plot_dir, "forrest_plot_block.pdf")
#write pdf
pdf(output_path, width = 8.5, height = 11)  # Adjust width if needed
forest(meta, order=blockFB,ylim = c(-2,43))
abline(h = 0)
preds <- predict(meta,newmods=c(0, 1), addx=TRUE)
addpoly(preds, rows =-1, mlab = c("Without Block-level Feedback", "With Block-level Feedback"))
text(-2.55, 42, "Author(s) and Year", font = 2)
text(2.1, 42, "Effect Size [95% CI]", font = 2)
dev.off()#close pdf

#funnel plot
output_path <- paste0(plot_dir, "funnel_plot_block_feedback.pdf")
#write pdf
pdf(output_path, width = 8, height = 6)  # Adjust width if needed
#adjust margin size, label size, number size
par(mar=c(5.1, 4.1, 4.1, 2.1), cex.lab = 1.3, cex.axis = 1.2)
#Creating a color vector based on the levels of binary moderator
colors <- ifelse(dataSet_z$blockFB == 0, "black", "gray") 
#Generating the funnel plot with color-coding
metafor::funnel(meta, digits=c(1,1), back="white", col = colors, ylab = "Standard Error")
# Add a legend
legend("topleft", legend = c("Without Block-level Feedback", "With Block-level Feedback"), 
       col = c("black", "gray"), pch = 16, cex = 1.3, bty = "n")
dev.off()#close pdf

#Egger's test 
regtest(meta)

#Running influence function and further investigating results to determine if there
#is undue influence of specific studies. Important: note that cutoffs for the 
#influence function are arbitrary and studies with larger sample sizes 
#(and smaller SE) can get flagged by the influence function despite them simply
#reflecting higher precision estimates that should not be removed. Thus, approach
#for using influence function was to run for omnibus model, then carefully inspect
#each study flagged by the function to determine whether the study appeared to be
#consistent with the broader pattern of results and seemed to be flagged only 
#due to the smaller SE and higher weighting in the model. In such cases, the
#study was not removed. However, if the study appeared to be a possible outlier,
#we assessed whether the results changed when excluding study. If the results
#remained qualtiatively unchanged, then the study was retained. 
inf <- influence(meta)
print(inf)
plot(inf)
#The following studies were flagged, but do not appear to be potential outliers
#Larson et al (2013)*
#Larson (2011)*
#Milyavskaya (2018)*




#Given that egger's test was significant, we further assessed publication bias 
#in separate models fit at each level of the moderator (subgroup analysis)

#Subgroup WITH block-level feedback
temp_dataSet <- read.csv("/Users/gbuzzell/Documents/Projects/ern_meta/final_data_set/data/ern_meta_data_for_r_2024-08-21.csv")
dataSet<-subset(temp_dataSet, (!is.na(temp_dataSet[,2]))) #subset to only rows with r value (all valid rows)
dataSet_z <- escalc(measure="ZCOR", ri=dataSet$r, ni=dataSet$n, data=dataSet, slab=id) #convert to z scores
#select only flanker task studies
dataSet_z <- filter(dataSet_z, task_1f_2g_3s_4o == 1)
#select only studies that have block-level feedback codes (remove NAs)
dataSet_z <- filter(dataSet_z, !is.na(blockFB))
#Additionally select only studies WITHOUT trial-level feedback (since focus is on block-level)
dataSet_z <- filter(dataSet_z, trialFB_any == 0)
#select only studies WITH block feedback
dataSet_z <- filter(dataSet_z, blockFB == 1) 

#subgroup analysis of studies WITH block-level feedback
meta <- rma(yi, vi, slab=id, data=dataSet_z) 
meta
confint(meta) #additional confidence intervals for model
sum(dataSet_z$n)

#Egger's test 
regtest(meta)

trimfill(meta)

#Subgroup WITHOUT block-level feedback
temp_dataSet <- read.csv("/Users/gbuzzell/Documents/Projects/ern_meta/final_data_set/data/ern_meta_data_for_r_2024-08-21.csv")
dataSet<-subset(temp_dataSet, (!is.na(temp_dataSet[,2]))) #subset to only rows with r value (all valid rows)
dataSet_z <- escalc(measure="ZCOR", ri=dataSet$r, ni=dataSet$n, data=dataSet, slab=id) #convert to z scores
#select only flanker task studies
dataSet_z <- filter(dataSet_z, task_1f_2g_3s_4o == 1)
#select only studies that have block-level feedback codes (remove NAs)
dataSet_z <- filter(dataSet_z, !is.na(blockFB))
#Additionally select only studies WITHOUT trial-level feedback (since focus is on block-level)
dataSet_z <- filter(dataSet_z, trialFB_any == 0)
#select only studies WITHOUT block feedback
dataSet_z <- filter(dataSet_z, blockFB == 0) 

#subgroup analysis of studies WITHOUT block-level feedback
meta <- rma(yi, vi, slab=id, data=dataSet_z) 
meta
confint(meta) #additional confidence intervals for model
sum(dataSet_z$n)

#Egger's test 
regtest(meta)

#Given that Egger's test was significant for this subgroup, we further estimated
#the bias-corrected effect for this subgroup via the trim-and-fill procedure
trimfill(meta)

