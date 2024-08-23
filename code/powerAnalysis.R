library(metapower)

#from saunders and inzlicht main analysis of overall anxiety relation
#Overall k = 58  N = 3819  r = −0.19 [−0.24,−0.14] I2 = 52.5% [36.1,74.5]
# average study size = 3819/58 = 65.84

#although this reflects all studies, not just flanker, we took this as an approx of
# effects for flanker
 
#Thus, based on saunders and inzlicht, we assumed a mean effect size of .19,
#the ability to detect differences between groups that were .2 r appart (.09 vs .29),
#average study sample sizes of 66, and i2 of 52.5

my_subgroup_power <- subgroup_power(n_groups = 2, 
                                    effect_sizes = c(.09,.29), 
                                    study_size = 66,
                                    k = 26,
                                    i2 = .525,
                                    es_type = "r")
print(my_subgroup_power)
plot_subgroup_power(my_subgroup_power)
