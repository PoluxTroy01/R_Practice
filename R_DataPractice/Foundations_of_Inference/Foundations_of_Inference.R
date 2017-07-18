#   |||   Statistical Inference   |||
# The process of making claims about a population based on information from a sample
# Is also understand samples from a hypotheticalpopulation where the null hypothesis is true
# Vocabulary :
# The Null Hypothesis(H0): The claim that is not interesting: "They equaly like soda"
# The Alternative Hypothesis (HA): The claim corresponding to the research hypothesis: "West like
#  more soda than the East". Is the research question of interest
# Example: Election 
# From a sample, the researchers would like to claim that Candidate X will win
# here the number of interest is the true proportion of votes the candidate X will receive that number is a population 
# measure . H0: "The candidate X will get half the votes, HA: "The candidate X will get more than half the votes


# ||| Randomized distributions |||
# Understanding the null distribution :
#   Generating a distribution of the statistic from the null population gives information about whether the 
#   observed data are inconsistent with the null hypothesis
set.seed(101)

library(githubinstall)
library(devtools)
library(dplyr)
library(ggplot2)
library(NHANES)
library(oilabs)

# What are the variables in the NHANES dataset?
names(NHANES)

# Create bar plot for Home Ownership by Gender
ggplot(NHANES, aes(x = Gender, fill = HomeOwn)) + 
  geom_bar(position = "fill") +
  ylab("Relative frequencies")

# Density for SleepHrsNight colored by SleepTrouble, faceted by HealthGen
ggplot(NHANES, aes(x = SleepHrsNight, col = SleepTrouble)) + 
  geom_density(adjust = 2) + 
  facet_wrap(~ HealthGen)



# ||| Randomly allocating samples |||
# Permute the observations and calculate a difference in proportions that could arise from a null distribution.
# Let's investigate the relationship between gender and home ownership

# Subset the data: homes
homes <- NHANES %>%
  select(Gender, HomeOwn) %>%
  filter(HomeOwn %in% c("Own", "Rent"))

# Perform one permutation  to evaluate whether home ownership status differs between 
# the "female" and "male" groups
homes %>%
  mutate(HomeOwn_perm = sample(HomeOwn)) %>%
  group_by(Gender) %>%
  summarize(prop_own_perm = mean(HomeOwn_perm == "Own"), 
            prop_own = mean(HomeOwn == "Own")) %>%
  # calculate the difference in proportion of home ownership for both
  summarize(diff_perm = diff(prop_own_perm),
            diff_orig = diff(prop_own))




# ||| Randomization dotplot (n = 10) |||
# Natural variability can be modeled from shuffling observations around to remove any relationships 
# that might exist in the population.
# In this exercise, you will permute the home ownership variable 10 times. By doing so, you will ensure that
# there is no relationship between home ownership and gender, so any difference in homeownership
# proportion for female vs male will be due only to natural variability

rep_sample_n <- function(tbl, size, replace = FALSE, reps = 1)
{
  n <- nrow(tbl)
  i <- unlist(replicate(reps, sample.int(n, size, replace = replace), simplify = FALSE))
  
  rep_tbl <- cbind(replicate = rep(1:reps,rep(size,reps)), tbl[i, , drop=FALSE])
  
  dplyr::group_by(rep_tbl, replicate)
}

# Perform 10 permutations
homeown_perm <- homes %>%
  rep_sample_n(size = nrow(homes), reps = 10) %>%
  mutate(HomeOwn_perm = sample(HomeOwn)) %>%
  group_by(replicate, Gender) %>%
  summarize(prop_own_perm = mean(HomeOwn_perm == "Own"), 
            prop_own = mean(HomeOwn == "Own")) %>%
  summarize(diff_perm = diff(prop_own_perm),
            diff_orig = diff(prop_own)) # male - female

# Print differences to console
homeown_perm

# Dotplot of 10 permuted differences in proportions
ggplot(homeown_perm, aes(x = diff_perm)) + 
  geom_dotplot(binwidth = .001)




# ||| Now with 100 permutations |||
# Perform 100 permutations
homeown_perm <- homes %>%
  rep_sample_n(size = nrow(homes), reps = 100) %>%
  mutate(HomeOwn_perm = sample(HomeOwn)) %>%
  group_by(replicate, Gender) %>%
  summarize(prop_own_perm = mean(HomeOwn_perm == "Own"), 
            prop_own = mean(HomeOwn == "Own")) %>%
  summarize(diff_perm = diff(prop_own_perm),
            diff_orig = diff(prop_own)) # male - female

# Dotplot of 100 permuted differences in proportions
ggplot(homeown_perm, aes(x = diff_perm)) + 
  geom_dotplot(binwidth = 0.001)




# ||| Now with 1000 permutations |||
# Perform 1000 permutations
homeown_perm <- homes %>%
  rep_sample_n(size = nrow(homes), reps = 1000) %>%
  mutate(HomeOwn_perm = sample(HomeOwn)) %>%
  group_by(replicate, Gender) %>%
  summarize(prop_own_perm = mean(HomeOwn_perm == "Own"), 
            prop_own = mean(HomeOwn == "Own")) %>%
  summarize(diff_perm = diff(prop_own_perm),
            diff_orig = diff(prop_own)) # male - female

# Density plot of 1000 permuted differences in proportions
ggplot(homeown_perm, aes(x = diff_perm)) + 
  geom_density()

# You can see now that the distribution is approximetely normally distributed
# Recall that the objective of statistical inference is to compare the observed statistic to the distribution
# of statistc that come frome a null distribution. We just create the distibution but how do we use it?
# Remember that each dot that gets generated is from a different permutation of the data, we use the null 
# differences, the dots, to define the setting we are not interested in. The goal is to show that our observed data
# are not consistent with the differences generated. We want to observe data to be different from the null
# so we can claim the alternative research hypothesis to be true


# ||| Do the data come from the population? |||
# Plot permuted differences
ggplot(homeown_perm, aes(x = diff_perm)) + 
  geom_density() +
  geom_vline(aes(xintercept = diff_orig),
             col = "red")

# Compare permuted differences to observed difference
homeown_perm %>%
  summarize(sum(diff_orig >= diff_perm))

# So 229 permuted differences are more extreme than the observed difference. This only represents 21.2%
# of the null statistics so you can conclude that the observed difference IS consistent with the permuted 
# distribution.
# Therefore, we have learned that our data is consistent with the hypothesis of no difference in home ownership
# across gender, we've failed to reject the null hypothesis
# But this does not mean that gender does not play a roll, it's possible that the true difference in home ownership
# rate is 0.1 and shurely our data would be consistent with that population as well.
# The logic of inference allows us only to reject null claims process does not allow us to have certainty in the 
# null hypothesis being true 

# The conclusion was that there was an inability to reject null hypothesis, so in this case there is no claim
# that can be generalized to a larger population 




                              # ||| Example: Gender discrimination |||

# "Influence of sex rolls stereotipes on personal decisions"

# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 
# Solo como ejemplo de como obtener una proporción de las promociones dadas por sexo
disc <- data.frame(
       promote = c(rep("promoted", 35), rep("not_promoted", 13)),
       sex = c(rep("male", 21), rep("female", 14),
               rep("male", 3), rep("female", 10))
  )
disc %>%
  group_by(sex) %>%
  summarize(promoted_prop = mean(promote == "promoted"))
# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 

# The shuffuling process breaks the relationship between sex and promotion whichs allows us to
# understand the variability of the differences in promotion rates assuming there is no connection 
# between the two variables 


# ||| Gender discrimination hypothesis |||
# H0: Gender and promotion are unrelated variables
# HA: Men are more likely to be promoted 


# ||| Summarizing gender discrimination |||
# Create a contingency table summarizing the data
table(disc)

# Find proportion of each sex who were promoted
disc %>%
  group_by(sex) %>%
  summarize(promoted_prop = mean(promote == "promoted"))

# So the difference in proportions promoted is almost of 0.3




# ||| Step-by-Step through the permutation |||
# Sample the entire data frame 5 times
disc %>%
  rep_sample_n(size = nrow(disc), reps = 5) 

# Shuffle the promote variable within replicate, to break any relationship between promotion and gender
disc %>%
  rep_sample_n(size = nrow(disc), reps = 5) %>%
  mutate(prom_perm = sample(promote)) 

# Find the proportion of promoted in each replicate and sex
disc %>%
  rep_sample_n(size = nrow(disc), reps = 5) %>%
  mutate(prom_perm = sample(promote)) %>%
  group_by(replicate, sex) %>%
  summarize(prop_prom_perm = mean(prom_perm == "promoted"),
            prop_prom = mean(promote == "promoted")) 

# Difference in proportion of promoted across sex grouped by gender
disc %>%
  rep_sample_n(size = nrow(disc), reps = 5) %>%
  mutate(prom_perm = sample(promote)) %>%
  group_by(replicate, sex) %>%
  summarize(prop_prom_perm = mean(prom_perm == "promoted"),
            prop_prom = mean(promote == "promoted"))  %>%
  summarize(diff_perm = diff(prop_prom_perm),
            diff_orig = diff(prop_prom))  # male - female

# Just to recap, using rep_sample_n() you took 5 samples (i.e Replications) of the disc data, then shuffled 
# this using smple() to break any links between gender and getting promoted. Then for each replication, you
# calculated the proportions of promoted males and females in the dataset along with the difference in 
# proportions




# ||| Randomizing gender discrimination |||
# In this exercise, you'll create a randomization distribution of the null statistic with 1000 replicates
# as opposed to just 5 like in the previous exercise. 
# AS A REMINDER , the statistic of interest is the difference in proportions promoted between genders
# (i.e proportion for males minus proportion for females)

# Create a data frame of differences in promotion rates
disc_perm <- disc %>%
  rep_sample_n(size = nrow(disc), reps = 1000) %>%
  mutate(prom_perm = sample(promote)) %>%
  group_by(replicate, sex) %>%
  summarize(prop_prom_perm = mean(prom_perm == "promoted"),
            prop_prom = mean(promote == "promoted")) %>%
  summarize(diff_perm = diff(prop_prom_perm),
            diff_orig = diff(prop_prom))  # male - female


# Histogram of permuted differences
ggplot(disc_perm, aes(x = diff_perm)) + 
  geom_histogram(binwidth = 0.01) +
  geom_vline(aes(xintercept = diff_orig), col = "red")






# ||| Distribution of statistics |||
# Null statistic
#   Difference in proportions: ^p - p  Null statistic and the observed statistic
#  Ratio: ^p / p
#  We are still interested in whether observed statistic is different from values obtained by shuffling

# One way to measure how far the observed statistic is from the null values is to calculate quantiles
# of the null statistics, after we generated 100 different shuffles of the original data we can see the five
# percent quantile is  -0.292, that is, 5% of the observations are at  -0.292 or below, the 95% quantile 
# is .208 that is 95% of the null observations are at 0.208 or below, meaning that our observed statistic
# of 0.29 is larger than 95% of the null statistics

# Quantile measurement :
disc_perm %>% 
  summarize(q.05 = quantile(diff_perm, p = 0.05),
            q.95 = quantile(diff_perm, p = 0.95))

# 95% of the null differences are positive 0.208 or lower
# This simulations give further evidence that the observed statistic is not consistent with the bulk 
# of the null differences

# Often the quantiles describing the null distribution determine what is called the critical region: it 
# determines which  observed statistics are consistent with the null distribution 


# ||| Analysis |||
# Based on this plot:
# Histogram of permuted differences
ggplot(disc_perm, aes(x = diff_perm)) + 
  geom_histogram(binwidth = 0.01) +
  geom_vline(aes(xintercept = diff_orig), col = "red")
#  Which seems like a reasonable conclusions about promotions?
#     In the population there is evidence that women are promoted at a different rate, but we cannot tell 
#   whether the difference is due to discrimination or something else, there are few permuted differences 
#   which are as extreme as the observed difference 

# It seems as though the statistic—a difference in promotion rates of 0.2917—is on the extreme end 
# of the permutation distribution. That is, there are very few permuted differences which are as extreme 
# as the observed difference.




# ||| Critical region |||
# To quantify the extreme permuted (null) differences, we use the quantile() function.
# Find the 0.90, 0.95, and 0.99 quantiles of diff_perm
disc_perm %>% 
  summarize(q.90 = quantile(diff_perm, p = 0.90),   # 10% of the permuted differences are above the value
            q.95 = quantile(diff_perm, p = 0.95),
            q.99 = quantile(diff_perm, p = 0.99))




# ||| Two sided critical region |||
# However, there are often scenarios where the research question centers around a difference without
# directionality. 
# For example, you might be interested in whether the rate of promotion for men and women is different. 
# In that case, a difference in proportions of -0.29 is just as "extreme" as a difference of positive 0.29.
# If you had seen that women were promoted more often, what would the other side of the distribution 
# of permuted differences look like? That is, what are the smallest (negative) values of the distribution 
# of permuted differences?
# Find the 0.10, 0.05, and 0.01 quantiles of diff_perm
disc_perm %>% 
  summarize(q.01 = quantile(diff_perm, p = 0.01),
            q.05 = quantile(diff_perm, p = 0.05),
            q.10 = quantile(diff_perm, p = 0.1))




# ||| How does sample size affect results? |||
# Notice that the observed difference of 0.2917 is in the extreme right tail of the permuted differences. 
# If the sample was ten times larger but the sample statistic was exactly the same (i.e. 0.2917), how would 
# the distribution of permuted differences change? 
# The statistic of 0.2917 would be much farther to the right of the permuted differences (completely off 
# of the distribution)

# The observed difference is consistent with differences you would see by chance if the sample size was 
# small. The observed difference would virtually never be observed by chance if the sample size was big. 

# Notice how the differences in proportions must be much larger to be significant if the sample size 
# is small. With a big sample size, a small difference in proportions can be significant. 



# ||| What is a p-value? |||
# Remember, our interest is in how consistent the observed data are with data taken from a population 
# where gender and promotion rates are delinked, by permuting the data repetedly we can quantify how
# likely the observed data are to have happened in a situation where the null hypothesis is true.
# The null hypothesis is that promotion rates in the population do not differ from men and women.
# Depending on the level of significance we may or may not reject the null hypothesis.
# To get arround this sometimes reject problem we quantify the degree to which the data disagree with 
# the null distribution using something called a p-value 
 
# P-value: Is the probability of observing data as or more extreme than what we actually got given 
# that the null hypothesis is true
# p-value: Is the probability that the pattern of data in the sample could be produced by random data
 
# Gender discrimination p-value: Probability of observing a difference of 0.2917 or greater when promotion
# rates do not vary across gender = 0.03

# The p-value measures the degree of disagreement between the data and the null hypothesis

# ||| Calculating the p-value |||
# Determine the proportion of times the observed difference is less than or equal to the permuted difference.
# Calculate the p-value for the original dataset
disc_perm %>%
  summarize(mean(diff_orig <= diff_perm))






# ||| Practice calculating the p-values |||
# In the original dataset, 87.5% of the men were promoted and 58.3% of the women were promoted.
# Consider a situation where there are 24 men, 24 women, and 35 people are still promoted. But in this 
# new scenario, 75% of the men are promoted and 70.8% of the women are promoted. Does the difference in 
# promotion rates still appear to be statistically significant? That is, could this difference in promotion 
# rates have come from random chance?

disc <- data.frame(
  promote = c(rep("promoted", 35), rep("not_promoted", 13)),
  sex = c(rep("male", 21), rep("female", 14),
          rep("male", 3), rep("female", 10))
)




disc_new <- data.frame(
  promote = c(rep("promoted", 35), rep("not_promoted", 13)),
  sex = c(rep("male", 18), rep("female", 17),
          rep("male", 6), rep("female", 7))
)


# Recall the original data
disc %>% 
  select(sex, promote) %>%
  table()

# Tabulate the new data
disc_new %>% 
  select(sex, promote) %>%
  table()

# Create a data frame of differences in promotion rates
disc_perm <- disc %>%
  rep_sample_n(size = nrow(disc), reps = 1000) %>%
  mutate(prom_perm = sample(promote)) %>%
  group_by(replicate, sex) %>%
  summarize(prop_prom_perm = mean(prom_perm == "promoted"),
            prop_prom = mean(promote == "promoted")) %>%
  summarize(diff_perm = diff(prop_prom_perm),
            diff_orig = diff(prop_prom))  # male - female

# Now for the new disc
disc_new_perm <- disc_new %>%
  rep_sample_n(size = nrow(disc_new), reps = 1000) %>%
  mutate(prom_perm = sample(promote)) %>%
  group_by(replicate, sex) %>%
  summarize(prop_prom_perm = mean(prom_perm == "promoted"),
            prop_prom = mean(promote == "promoted")) %>%
  summarize(diff_perm = diff(prop_prom_perm),
            diff_orig = diff(prop_prom))  # male - female

# Plot the distribution of the original permuted differences
ggplot(disc_perm, aes(x = diff_perm)) + 
  geom_histogram() +
  geom_vline(aes(xintercept = diff_orig), col = "red")

# Plot the distribution of the new permuted differences
ggplot(disc_new_perm, aes(x = diff_perm)) + 
  geom_histogram() +
  geom_vline(aes(xintercept = diff_orig), col = "red")

# Find the p-value from the original data
disc_perm %>%
  summarize(mean(diff_orig <= diff_perm))

# Find the p-value from the new data
disc_new_perm %>%
  summarize(mean(diff_orig <= diff_perm))


# Notice that the permutation differences (the two histograms) are essentially the same regardless 
# of whether the original or the new dataset is used. 




# ||| Calculating two-sided p_values |||
# What if the original research hypothesis had focused on any difference in promotion rates between 
# men and women instead of focusing on whether men are more likely to be promoted than women? In this 
# case, a difference like the one observed would occur twice as often (by chance) because sometimes the 
# difference would be positive and sometimes it would be negative.
# When there is no directionality to the alternative hypothesis, the hypothesis and p-value are considered 
# to be two-sided. In a two-sided setting, the p-value is double the one-sided p-value.

# Calculate the two-sided p-value
disc_perm %>%
  summarize(mean(diff_orig <= diff_perm) * 2)


# ||| Summary of gender discrimination |||
# The observed gender discrimination data is not really consistent with the permuted null differences,
# only 30 of the 1000 permuted differences were larger than or equal to the observed statistic, that is 
# we would have observed data like ours only 3 percent of the time if men and women were equally likely 
# to be promoted, pay special attention how the p-value is computed here, first we identify permuted 
# differences that are larger than or equal to the observed statistic and label those situations with 
# a 1, all other permutations receiving a 0, by avaraging the 1's and 0's the mean gives the proportion 
# of times the permuted difference is larger than or equal to the observed difference 
# Because 0.03 i less than 0.05 we reject the null hypothesis and claim than men are promoted at a higher 
# rate than women, that is we conclude that it was not simply random variability which led to a higher 
# proportion of rate being promoted
# A p-value of 0.03 is reasonably close to 0.05 which means we should be careful about making claims 
# we should take the results as an indication that more work should be done on the claims.

# Causation 
# - Study was randomized 
# - Nothing systematically different about two groups of participants other than which resumes 
#   they evaluated
# - Any difference in promotion rates is due to the gender of the applicant 
  
# That is we can infer a causal connection between the gender of the applicant being male and a higher
# promotion rate





                                     # ||| Opportunity Cost |||



opportunity <- data.frame(
  decision = c(rep("buyDVD", 97), rep("nobuyDVD", 53)),
  group = c(rep("control", 56), rep("treatment", 41),
          rep("control", 19), rep("treatment", 34))
)


# ||| Summarizing opportunity cost |||
# Tabulate the data
opportunity %>%
  select(decision, group) %>%
  table()

# Find the proportion who bought the DVD in each group
opportunity %>%
  group_by(group) %>%
  summarize(buy_prop = mean(decision == "buyDVD"))

# So about 75% of the control group bought the DVD and about 55% of the treatment group (i.e. The 
# group that was reminded that the money could be saved) bought the DVD. 


# ||| Plotting the opportunity cost |||
# Create a barplot
ggplot(opportunity, aes(x = group, fill = decision)) + 
  geom_bar(position = "fill")


# ||| Randomizing the opprtunity cost(1) |||
# As in Chapter 2, you will permute the data to generate a distribution of differences as if the null 
# hypothesis were true.
# In the study, the number of individuals in each of the control and treatment groups is fixed. 
# Additionally, when you assume that the null hypothesis is true—that is, the experiment had no 
# effect on the outcome of buying a DVD—it is reasonable to infer that the number of individuals who 
# would buy a DVD is also fixed. That is, 97 people were going to buy a DVD regardless of which treatment 
# group they were in.


# Data frame of differences in purchase rates after permuting
opp_perm <- opportunity %>%
  rep_sample_n(size = nrow(opportunity), reps = 1000) %>%
  mutate(dec_perm = sample(decision)) %>%
  group_by(replicate, group) %>%
  summarize(prop_buy_perm = mean(dec_perm == "buyDVD"),
            prop_buy = mean(decision == "buyDVD")) %>%
  summarize(diff_perm = diff(prop_buy_perm),
            diff_orig = diff(prop_buy))  # treatment - control


# Histogram of permuted differences
ggplot(opp_perm, aes(x = diff_perm)) + 
  geom_histogram(binwidth = .005) +
  geom_vline(aes(xintercept = diff_orig), col = "red")




# ||| Summarizing opportunity cost(2) |||
# Now that you've created the randomization distribution, you'll use it to assess whether the observed 
# difference in proportions is consistent with the null difference. You will measure this consistency 
# (or lack thereof) with a p-value, or the proportion of permuted differences less than or equal to the
# observed difference.

# Calculate the p-value
opp_perm %>%
  summarize(mean(diff_perm <= diff_orig))


# ||| Oportunity cost conclusion |||
# Reminding them causes them to be less likely to buy the DVD
# We can confidently say the different messaging caused the students to change their buying habits,
# since they were randomly assigned to treatment and control groups





# ||| Errors and their consequences |||
# From the null distribution you just created it seems that reminding students to save does have a causal
# impact on the likelyhood that they will buy a dvd (see the histogram of permuted differences) thats 
# because the observed difference is not consistent with the null differences.
# But what is the consecuence of concluding that a reminder cause students to be less likely to buy DVD's?
# What if our conclusion is wrong? Before completing the hypothesis test it's important to understand how
# and why things can go wrong with statistical inference, notice that there is 2 possible decisions to make
# in hypothesis testing, eather the observed data are inconsistent with the null hypothesis in which the 
# null hypothesis is rejected, or the observed data are consistent with the null hypothesis in which the 
# null hypothesis is NOT rejected no conclusion is made about a larger population.
# There are also 2 possible truth states eather the null hypothesis is true or the alternative hypothesis 
# is true. Keep in mind however that we can't ever know the true state of the population because the 
# research statement is almost always the same as the alternative hypothesis, the goal of the scientific 
# study is to be in the bottom box: The alternative hypothesis is true and the data provide convincing 
# evidence to reject the null hypothesis, we can not know which row has resulted but we do know which 
# conclusion has been made by specifying the column
# If the null hypothesis is rejected then eather the science is correct (4 cuadrante) or type | error has
# been made (1 cuadrante). If the null hypothesis is not rejected it can be correct to (2 cuadrante) or 
# a type || error has been made (3 cuadrante)


# Consider again a situation where the task is to differentiate the proportion of successes across two 
# different groups. What decision should be made if the goal is to never make a type II error 
# (false negative)?
#   Always claim there is a difference in proportions, you will always reject the null hypothesis


# Type Errors for the students case :
# Type I: There is not a difference in proportions, but the observed difference is big enough to 
# indicate that the proportions are different.
# Type II: There is a difference in proportions, but the observed difference is not large enough to 
# indicate that the proportions are different.

# ||| P-value for two sided hypotheses: opportunity costs |||
# Calculate the two-sided p-value
opp_perm %>%
  summarize(mean(diff_perm <= diff_orig)*2) 

# Take a moment to remind yourself why you used the proportion of permuted statistics that are smaller 
# than (instead of bigger than) the observed value. Hint: look at the histogram of permuted statistics. 


# ||| Summary of opportunity costs |||
The difference in observed proportions is not consistent with the null hypothesis, only 5 of the 1000
permutations (The p-value shows it, not the two-sided) were smaller than the observed data value, even 
if we perfermed a more conservative two-sided test the p-value would still been equal to just 10 out of
1000 or 0.010.
Because the p-value is substantially less than 0.05 we conclude that it was not simple variability that
led to fewer students buying the DVD when being reminded to save, because the study was randomized, that 
is, the individuals were randomly assigned the choices there was nothing systematically different about 
the participants in the treatment and control groups, the only difference in the 2 groups was the set 
of options they received.
Therefore, any difference in buying rates is due to the options given (i.e being reminded to save)
A causal inference can be made in this setting, importantly however, the 150 individuals in the sample
were not randomly sample from all people, they were students very different from the adult population.
In order to generalize we need more information about the students and who they represent 





                                    # ||| Confidence intervals |||

# Consider an election in your home town that will take place in a week's time. You poll a randomly 
# selected subset of the voters in your town and ask them if they plan to vote for Candidate X or 
# Candidate Y. In this chapter, we will focus on sampling variability—the variability insample 
# proportions due to polling different randomly selected individuals from the population.
 
# Before investigating the sampling variability, what is the population parameter of interest?
# The proportion of all voters in your town who will vote for Candidate X on election day.



# ||| Bootstraping |||
With hypothesis testing its important to understand how samples from a null population vary, we 
repetedley sample from a null population which gives us sense for the variability of the statistic under
the random chance model. Know that the statistic at hand is ^p (statistic) which is the proportion of successes in
the sample, p (parameter) is the proportion of successes in the population.
In contrast, with confidence intervals there is no null population, instead we need to understand how 
samples from the population of interest vary (How do p and p-hat vary?). We expect the sample statistic
to vary around the parameter, but how far is the statistic from the parameter?

Bootstraping is a method that allows us to estimate the distance from a statistic to the parameter, samples 
from the sample in order to estimate the variability of the statistic, each time we resample we sample from
the original data with replacement, it turns out that this process is an excelent aproximation for
sampling from the original population, the bootstrap statistic is p-hat-star which is the proportion of 
successes in the resample. By resampling gives a proportion of how p-hat varies
The standard error of p-hat-star which describes how variable the statistic is around the parameter, is
key in building a confidence interval.
Bootstrap provides an approximation of the standard error 


# ||| Resampling form a sample |||
In the first experiment, you will simulate repeated samples from a population. In the second, you will 
choose a single sample from the first experiment and repeatedly resample from that sample—a method 
called bootstrapping. More specifically:
Experiment 1: Assume the true proportion of people who will vote for Candidate X is 0.6. Repeatedly 
sample 30 people from the population and measure the variability of p̂  (the sample proportion). 
Experiment 2: Take one sample of size 30 from the same population. Repeatedly sample 30 people (with 
replacement!) from the original sample and measure the variability of p̂ ∗ (the resample proportion).
It is important to realize that the first experiment relies on knowing the population and is typically 
impossible in practice. The second relies only on the sample of data and is therefore easy to implement 
for any statistic.   
# Fortunately, as you will see, the variability in p̂ , or the proportion of "successes" in a sample, is
# approximately the same whether we sample from the population or resample from a sample.

# Se entiende como "poll" a una muestra
# all_polls es un dataset que tiene 1000 muestras con 30 elementos cada una, en este caso son 30 individuos
# y sus respectivos votos 1 quiere decir que si va a votar por el candidato X

# Select one poll from which to resample: one_poll
one_poll <- all_polls %>%
  filter(poll == 1) %>%
  select(vote)
  
# Generate 1000 resamples of one_poll: one_poll_boot_30
one_poll_boot_30 <- one_poll %>%
  rep_sample_n(size = nrow(one_poll), replace = TRUE, reps = 1000)

# Compute p-hat for each poll: ex1_props
ex1_props <- all_polls %>% 
  group_by(poll) %>% 
  summarize(prop_yes = mean(vote == 1))
  
# Compute p-hat* for each resampled poll: ex2_props
ex2_props <- one_poll_boot_30 %>%
  summarize(prop_yes = mean(vote == 1))

# Compare variability of p-hat and p-hat*
ex1_props %>% summarize(sd(prop_yes))
ex2_props %>% summarize(sd(prop_yes))

p-hat  : 0.0868
p-hat* : 0.0835

# The variability in the proportion of “successes” in a sample is approximately the same whether
# we sample from the population or resample from a sample. 






# ||| Resampling from a sample (2) |||
# What if the original dataset was 30 observations, but you chose to resample only 3 individuals with 
# replacement? Alternatively, what if you chose to resample 300 individuals with replacement? Let's call 
# these Experiment 3 and Experiment 4, respectively.
# Would the variability in these resampled p̂ ∗ values still be a good proxy for the variability of the
# sampled p̂  values taken from repeated samples from the population?

one_poll <- data.frame(
  vote = c(rep(1, 21), rep(0, 9))
)
one_poll <- one_poll[sample(nrow(one_poll)), ]


# Resample from one_poll with n = 3: one_poll_boot_3
one_poll_boot_3 <- one_poll %>%
  rep_sample_n(size = 3, replace = TRUE, reps = 1000)

# Resample from one_poll with n = 300: one_poll_boot_300
one_poll_boot_300 <- one_poll %>%
  rep_sample_n(size = 300, replace = TRUE, reps = 1000)

# Compute p-hat* for each resampled poll: ex3_props
ex3_props <- one_poll_boot_3 %>% 
  summarize(prop_yes = mean(vote == 1))

# Compute p-hat* for each resampled poll: ex4_props
ex4_props <- one_poll_boot_300 %>% 
  summarize(prop_yes = mean(vote == 1))

# Compare variability of p-hat* for n = 3 vs. n = 300
ex3_props %>% summarize(sd(prop_yes))
ex4_props %>% summarize(sd(prop_yes))

The result is:
p-hat* for 3: 0.2598
p-hat* for 3000: 0.02604

# Recall the variability of the sampled values taken from repeated samples from the population was 
# about 0.0868. Note that resampling 3 or 300 individuals with replacement from the 30-observation dataset 
# is not a good approximation of this value.


# Just to recall
Experiment 1: Sample (n=30
) repeatedly from an extremely large population (gold standard, but unrealistic)
Experiment 2: Resample (n=30
) repeatedly with replacement from a single sample of size 30
Experiment 3: Resample (n=3
) repeatedly with replacement from a single sample of size 30
Experiment 4: Resample (n=300
) repeatedly with replacement from a single sample of size 30




# ||| Variability in p-hat |||
# Remember, the goal of creating a confidence interval is to find the range of possible values for the true 
# population parameter. The observed statistic is a good estimate but is impossible to know whether is one 
# of the p-hat values which is very close to the parameter or whether is one of the p-hat values which is in
# the tail of the distribution of the observed statistics, we won´t know how far away the statistic is 
# from the parameter just by looking at the sample of data.

# The variability of the p-hat statistics gives the measure for how far apart any given observed p-hat and 
# the parameter are expected to be. Know that bootstraping the same number of observations as were in the 
# original sample provided aproximately the same standard error as sampling many p-hat values from the 
# population. The standard error is measured by the width of the distribution and here we can see that the
# green distibution (es la misma distribucion solo desplazada a la derecha) is aproximately the same width
# as the black distribution which measure the true variability of p-hat values from the population.
# Lucky for us the bootstraping works because in real life we only have one dataset. We can use the value 
# of the standard error to count the number of sample p-hats which are close to the parameter, it turns out
# that because the distribution of p-hat values is simetric in bell shape aproximately 95% of samples will
# produce p-hats that are within 2 standard errors from the center, this idea is called the empirical rule 
# and comes from theory which describes bell shape distributions 



# ||| Empirical Rule |||
One such property is that if the variability of the sample proportion (called the standard error, or SE) 
is known, then approximately 95% of p̂  values (from different samples) will be within 2SE
of the true population proportion.
To check whether that holds in the situation at hand, lets go back to the polls generated by taking 
many samples from the same population.

# Compute proportion of votes for Candidate X: props
props <- all_polls %>%
  group_by(poll) %>% 
  summarize(prop_yes = mean(vote == 1))

# Proportion of polls within 2SE
props %>%
  mutate(lower = 0.6 - 2 * sd(prop_yes),
         upper = 0.6 + 2 * sd(prop_yes),
         in_CI = prop_yes > lower & prop_yes < upper) %>%
  summarize(mean(in_CI))


El promedio de in_CI: 0.966
# In this example, it looks like 96.6% are within 2 standard errors of the true population parameter. 



# ||| Bootstrap t-confidence interval |||
The previous exercises told you two things:
  
  You can measure the variability associated with p̂ by resampling from the original sample.
  Once you know the variability of p̂ , you can use it as a way to measure how far away the true
  proportion is.

Note that the rate of closeness (here 95%) refers to how often a sample is chosen so that it is close 
to the population parameter. You wont ever know if a particular dataset is close to the parameter or 
far from it, but you do know that over your lifetime, 95% of the samples you collect should give you 
estimates that are within 2SE of the true population parameter.


# Again, set the one sample that was collected
one_poll <- all_polls %>%
  filter(poll == 1) %>%
  select(vote)

# Compute p-hat from one_poll: p_hat
p_hat <- mean(one_poll$vote)

# Bootstrap to find the SE of p-hat: one_poll_boot
one_poll_boot <- one_poll %>%
  rep_sample_n(30, replace = TRUE, reps = 1000) %>%
  summarize(prop_yes_boot = mean(vote == 1))

# Create an interval of plausible values
one_poll_boot %>%
  summarize(lower = p_hat - 2 * sd(prop_yes_boot),
            upper = p_hat + 2 * sd(prop_yes_boot))

Resultado:
    lower: 0.5304
    upper: 0.8695

Remember that a confidence level describes how likely you are to have gotten a sample that was close 
enough to the true parameter. Indeed, with the sample at hand, the confidence interval of plausible values 
does contain the true population parameter of 0.6.





# ||| Bootstrap percentile interval |||
The main idea in the previous exercise was that the distance between the original sample p̂  and the
resampled (or bootstrapped) p̂ ∗ values gives a measure for how far the original p̂ is from the
true population proportion.

The same variability can be measured through a different mechanism. As before, if p̂ is sufficiently
close to the true parameter, then the resampled (bootstrapped) p̂ ∗ values will vary in such a way that
they overlap with the true parameter.

Instead of using ±2SE as a way to measure the middle 95% of the sampled p̂  values, you can find the 
middle of the resampled p̂ ∗ values by removing the upper and lower 2.5%. Note that this second method of
contructing bootstrap intervals also gives an intuitive way for making 90% or 99% confidence intervals.

# Find the 2.5% and 97.5% of the p-hat values
one_poll_boot %>% 
  summarize(q025_prop = quantile(prop_yes_boot, p = 0.025),
            q975_prop = quantile(prop_yes_boot, p = 0.975))

# Bootstrap t-confidence interval for comparison
one_poll_boot %>%
  summarize(lower = p_hat - 2 * sd(prop_yes_boot),
            upper = p_hat + 2 * sd(prop_yes_boot))


Resultado:
  lower: 0.5361
  upper: 0.8638
  
Note that the two intervals were created using different methods. Because the methods are different, 
the intervals are expected to be a bit different as well. In the long run, however, the intervals 
should provide the same information. 



# ||| Interpreting CI's and technical conditions |||
By using the bootstrap p-hat values in the previous exercises you were able to find two different intervals
for the true parameter of interest, the proportion of the population who would vote for candidate X.
The first interval was based on the empirical roll which extended 2 standard errors from the sample 
proportion to create the interval, the second interval used the natural variability of the resampled 
p-hat-star values to characterized the distance from the true parameter.
Remember as in previous exercises the quantile function finds the value which gives 2.5% of the bootstrap 
proportions below and the value which gives 97.5% of the bootstrap proportions below, we can see that the 2
intervals are different because of the methods but they are consistent because they are based in the same
observations.
Remember that the goal is to find an interval estimate of the parameter, the true proportion who would vote
for the candidate X, when the only information is the sample whichs gives the proportion of individuals 
in the data who would vote for candidate X.
Ideas about the variability of p-hat-star are used to estimate how far the observed p-hat was from the 
population proportion. Because we dont know whether the sample is close to the population or far from it 
we dont know whether the confidence interval actualy captures the true parameter, to that end we interpret
the interval using a confidence percentage, that is we say "We are 95% confident that the true proportion
of people planning to vote for candidate X is between 0.536 and 0.864 Bootstrap t-CI (or 0.533 and 0.833, 
percentile interval)"


In this chapter we provided 2 ways for creating bootstrap confidence intervals, these methods were for
any statistic and parameter as long as the follow technical conditions hold:
  1.- The distribution of the statistic is reasonably symetric in bell shape.
  2.- The sample size is reasonably large
A plot of the bootstraped p-hat-star values will give a good indication for whether the technical conditions
are valid.
Both the bootstrap t-CI and the bootstrap percentile interval measure the variability of the resample 
proportions 
# Recuerda este si funciona, hay que checar los anteriores de este capitulo
one_poll <- data.frame(
       vote = c(rep(1, 21), rep(0, 9))
   )

one_poll_boot <- one_poll %>%
  rep_sample_n(30, replace = TRUE, reps = 1000) %>%
  summarize(prop_yes_boot = mean(vote == 1))


# Recall the bootstrap t-confidence interval
p_hat <- mean(one_poll$vote)
one_poll_boot %>%
  summarize(lower = p_hat - 2 * sd(prop_yes_boot),
            upper = p_hat + 2 * sd(prop_yes_boot))

# Collect a sample of 30 observations from the population
one_poll <- as.tbl(data.frame(vote = rbinom(n = 30, 1, .6)))

# Resample the data using samples of size 300 (an incorrect strategy!)
one_poll_boot_300 <- one_poll %>%
  rep_sample_n(300, replace = TRUE, reps = 1000) %>%
  summarize(prop_yes_boot = mean(vote))

# Find the endpoints of the the bootstrap t-confidence interval
one_poll_boot_300 %>%
  summarize(lower = p_hat - 2 * sd(prop_yes_boot),
            upper = p_hat + 2 * sd(prop_yes_boot))

# Resample the data using samples of size 3 (an incorrect strategy!)
one_poll_boot_3 <- one_poll %>%
  rep_sample_n(3, replace = TRUE, reps = 1000) %>%
  summarize(prop_yes_boot = mean(vote)) 

# Find the endpoints of the the bootstrap t-confidence interval 
one_poll_boot_3 %>%
  summarize(lower = p_hat - 2 * sd(prop_yes_boot),
            upper = p_hat + 2 * sd(prop_yes_boot))

Notice how the resampled interval with size 300 was way too small and the resampled interval with 
size 3 was way too big. 







# ||| Sample proportion value effects on bootstrap CIs ||
One additional element that changes the width of the confidence interval is the true parameter value.
When the true parameter is close to 0.5, the standard error of p̂ 
is larger than when the true parameter is closer to 0 or 1. When calculating a bootstrap t-confidence 
interval, the standard error controls the width of the CI, and here the width will be narrower.

# Collect 30 observations from a population with true proportion of 0.8
one_poll <- as.tbl(data.frame(vote = rbinom(n = 30, size = 1, prob = 0.8)))

# Compute p-hat of new sample: p_hat
p_hat <- mean(one_poll$vote)

# Resample the 30 observations (with replacement) to create a distribution of p^values
one_poll_boot <- one_poll %>%
  rep_sample_n(30, replace = TRUE, reps = 1000) %>%
  summarize(prop_yes_boot = mean(vote == 1)) 

# Calculate the bootstrap t-confidence interval
one_poll_boot %>%
  summarize(lower = p_hat - 2 * sd(prop_yes_boot),
            upper = p_hat + 2 * sd(prop_yes_boot))







# ||| Percentile effects on bootstrap CIs |||
Most scientists use 95% intervals to quantify their uncertainty about an estimate. That is, they 
understand that over a lifetime of creating confidence intervals, only 95% of them will actually 
contain the parameter that they set out to estimate.

# Calculate a 95% bootstrap percentile interval
one_poll_boot %>% 
  summarize(q025_prop = quantile(prop_yes_boot, p = 0.025),
            q975_prop = quantile(prop_yes_boot, p = 0.975))

# Calculate a 99% bootstrap percentile interval
one_poll_boot %>% 
  summarize(q005_prop = quantile(prop_yes_boot, p = 0.005),
            q995_prop = quantile(prop_yes_boot, p = 0.995))

# Calculate a 90% bootstrap percentile interval
one_poll_boot %>% 
  summarize(q05_prop = quantile(prop_yes_boot, p = 0.05),
            q95_prop = quantile(prop_yes_boot, p = 0.95))






# ||| Summary of statisticla inference |||
Through out the course we have focused on making claims about a population using information from a 
sample of data
Hypothesis testing is used to test a particular claim about the population, remember the claim to refuse
is called the null hypothesis and the claim we want is called the alternative hypothesis

Estimation is used to understand the value of a population parameter, using measures of variability 
of the statistic we can estimate how far p-hat is from the true proportion (What proportion of voters
will select candidate X?). In particular the variability of p-hat can be measured using resamples from 
the original data (Bootstraping). As with hypothesis testing the entire bootstrap process will be 
repeated with different data structures and research questions 


