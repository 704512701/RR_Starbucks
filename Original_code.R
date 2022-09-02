# Kelly B. Starbucks Customer Survey Evaluation
# Task:

# Evaluate and report on customer profiling, areas of satisfaction, and demographics to target for potential growth opportunities.

# First step is to intake the data and see what we are working with.

library('dplyr')
library('ggplot2')
library('tidyr')
library('tidyverse')
library('data.table')
library('tibble')

starbucks <- read.csv('Original_Starbucks satisfactory survey.csv')
# Next we will evaluate the variables and observations to get an idea of the scale of the dataset.

colnames(starbucks)
str(starbucks)

# Hmm… There is a lot of variables, and many are character strings for categorical data. (Ex: Gender is ‘Male’ and ‘Female’.) Lets clean up the data a little.
# First thing I want to do for my own sake is to rename the variables since they are descriptive but way too long.

starbucks <- starbucks %>% rename(UniqID = Timestamp,
                                  Gender = X1..Your.Gender,
                                  Age = X2..Your.Age,
                                  Employment = X3..Are.you.currently.....,
                                  Income_Bracket = X4..What.is.your.annual.income.,
                                  Visit_Freq = X5..How.often.do.you.visit.Starbucks.,
                                  Mode_of_Visit = X6..How.do.you.usually.enjoy.Starbucks.,
                                  Time_Visit = X7..How.much.time.do.you.normally..spend.during.your.visit.,
                                  Nearest_Store = X8..The.nearest.Starbucks.s.outlet.to.you.is....,
                                  Membership = X9..Do.you.have.Starbucks.membership.card.,
                                  Purchases = X10..What.do.you.most.frequently.purchase.at.Starbucks.,
                                  Spending = X11..On.average..how.much.would.you.spend.at.Starbucks.per.visit.,
                                  Quality_Rating = X12..How.would.you.rate.the.quality.of.Starbucks.compared.to.other.brands..Coffee.Bean..Old.Town.White.Coffee....to.be.,
                                  Price_Rating = X13..How.would.you.rate.the.price.range.at.Starbucks.,
                                  Sales_Promo_Rating = X14..How.important.are.sales.and.promotions.in.your.purchase.decision.,
                                  Ambience_Rating = X15..How.would.you.rate.the.ambiance.at.Starbucks...lighting..music..etc....,
                                  WiFi_Rating = X16..You.rate.the.WiFi.quality.at.Starbucks.as..,
                                  Service_Rating = X17..How.would.you.rate.the.service.at.Starbucks...Promptness..friendliness..etc...,
                                  Meetup_Rating = X18..How.likely.you.will.choose.Starbucks.for.doing.business.meetings.or.hangout.with.friends.,
                                  Promo_Communication = X19..How.do.you.come.to.hear.of.promotions.at.Starbucks..Check.all.that.apply.,
                                  Cont_Buying = X20..Will.you.continue.buying.at.Starbucks.)

# Next I want to change the date/time variables to UniqIDs, and make the character variables into factors. (For this project, the IDs don’t really come into use, but I wanted to get practice doing this process since I know it’ll be handing in the future)

starbucks <- starbucks %>%
  mutate(UniqID = 1:nrow(starbucks)) %>%
  relocate(UniqID, .before=Gender)

starbucks_revise <- starbucks %>% mutate_if(is.character, as.factor)

# Now let us see what the different values in each variable are, to check for typos, NAs, etc.

str(starbucks_revise)

# A cursory examination seems to show that ‘Gender’, ‘Age’, ‘Employment’, ‘Income_Bracket’, ‘Visit_Freq’, ‘Time_Visit’, ‘Nearest_Store’, ‘Membership’, ‘Spending’, and ‘Cont_Buying’ have managable unique factors.

# However, I am a little worried about ‘Purchases’ just from the str() pull because the first two samples are ‘cake’ (lowercase) and ‘Coffee’ (capitalized). Promo_Communication is a bit worrisome too. Let us dig deeper into both to see what kind of notation inconsistencies there might be…

unique(starbucks_revise$Purchases)
unique(starbucks_revise$Promo_Communication)

# Yep, as I suspected, there are some duplicate answers, list answers, and even a contradictory answer: “Cold drinks; Never”, and even an ‘NA’.
# For now, let’s note the complication and move on.


# Lets do some exploration.
library(ggplot2)
# First lets get an idea of gender, income, employment, and spending distributions. However, the ‘Income_Bracket’ will sort alphanumerically, which might make the graphs confusing, so I’m going to reorder them and then check the graphs.

unique(starbucks_revise$Income_Bracket)
order_income <- c('Less than RM25,000', 'RM25,000 - RM50,000', 'RM50,000 - RM100,000', 'RM100,000 - RM150,000', 'More than RM150,000')

#Here I should have (or could have) used a pipe operator. I'm not sure why I didn't!

starbucks_revise$Income_Bracket <- factor(starbucks_revise$Income_Bracket, levels = order_income)

head(starbucks_revise[order(starbucks_revise$Income_Bracket),])

distGender <- data.frame(starbucks_revise %>%
                           count(Gender))

distGender %>% mutate(Ratio = proportions(n))

# What I’ve gleaned is that gender is distributeded fairly evenly, with only a slight skew towards Female-identifying customers. But what about income distributions?

pl <- ggplot(starbucks_revise, aes(x = Income_Bracket))

pl + geom_bar(aes(fill=Income_Bracket))+
  theme(axis.text.x = element_blank())


# Lets combine them to get a broader view:

pl <- ggplot(starbucks_revise, aes(x = Gender))

pl + geom_bar(aes(fill=Income_Bracket))


# Great! So while customers that identify as ‘female’ outnumber ‘male’-identifying customers slightly, they also appear to fall into lower income brackets more frequently.

# Let’s get some more quantifiable measures on these insights…

#Creating Male and Female Income DFs

Gender_Income <- as.data.frame(starbucks_revise %>% count(Gender, Income_Bracket))

Female_Income <- data.frame(subset(Gender_Income, Gender == 'Female'))

Male_Income <- data.frame(subset(Gender_Income, Gender == 'Male'))

rownames(Male_Income) <- seq(1:nrow(Male_Income))

# Let’s summarise each gendered income bracket:

sumFemale_Income <- sum(Female_Income$n)

# Here I am creating a dataframe for each gender that contains the average count for each income bracket.

Female_Income_Prct <- data.frame(Income_Bracket = character(), Percentage_F = numeric())


for (i in 1:nrow(Female_Income)){
  avg <- as.double(Female_Income[i,'n'] / sumFemale_Income)
  avg_brkt <- c(as.character(Female_Income[i,'Income_Bracket']), as.numeric(round(avg, 3)))
  Female_Income_Prct[i,] <- avg_brkt
}

Female_Income_Prct$Percentage <- as.numeric(as.character(Female_Income_Prct$Percentage))

Female_Income_Prct$Percentage_F = NULL

#And let's check this one out as well....

Female_Income_Prct


# And now for the males as well…

#Same process here as the female averages. In retrospect, I could have (and perhaps should have) made a function and just applied it here. Lesson learned.

sumMale_Income <- sum(Male_Income$n)

Male_Income_Prct <- data.frame(Income_Bracket = character(), Percentage_M = numeric())

for (i in 1:nrow(Male_Income)){
  avg <- as.double(Male_Income[i,'n'] / sumMale_Income)
  avg_brkt <- c(as.character(Male_Income[i,'Income_Bracket']), as.numeric(round(avg, 3)))
  Male_Income_Prct[i,] <- avg_brkt
}

Male_Income_Prct$Percentage <- as.numeric(as.character(Male_Income_Prct$Percentage))

Male_Income_Prct$Percentage_M = NULL

Male_Income_Prct


# And now join them together…

Income_Prct <- merge(Male_Income_Prct, Female_Income_Prct, by = 'Income_Bracket', all.x = TRUE)

colnames(Income_Prct) <- c('IncomeBracket', 'Percent_M', 'Percent_F')

Income_Prct[is.na(Income_Prct)] <- 0

Income_Prct <- gather(Income_Prct, key = 'Gender', value = 'Percent', 2:3)

Income_Prct$Percent <- Income_Prct$Percent * 100

Income_Prct


pl <- ggplot(Income_Prct, aes(x=factor(IncomeBracket, level = order_income), y=Percent, fill=Gender))

pl + geom_bar(position='dodge', stat='identity') +
  coord_flip()+
  xlab('Income Bracket') +
  geom_text(aes(label=Percent), position=position_dodge2(width=0.9), vjust=0.5)


# So what we’ve gathered so far is that assuming fairly even gender distribution, female-identifying customers lean more towards the lower income brackets, while males solely occupy the higher income brackets (any more that RM50,000).

# We’ll keep this all in mind while we tackle the actual survey questions for more insight, specifically if there’s a distinction in gender regarding attitude towards any specific factor, or if there’s any specific variable that shows noteworthy statistics.

# (We’ll group by gender only for now to get some initial insights, and revisit things like ‘Visit_Freq’ and ‘Spending’ later.)

# Here I'm just reminding myself of the variables for the survey questions.

head(starbucks_revise)

surveyAnswers <- starbucks_revise %>% select(c('Gender', 'Quality_Rating', 'Price_Rating', 'Sales_Promo_Rating', 'Ambience_Rating', 'WiFi_Rating', 'Service_Rating', 'Meetup_Rating'))

head(surveyAnswers)

summary(surveyAnswers)

surveyAnswers_2 <- gather(surveyAnswers, key = 'Metric', value = "Rating", 2:8)

head(surveyAnswers_2)

survey_by_Gender <- tapply(surveyAnswers_2$Rating, surveyAnswers_2$Gender, mean)

survey_by_Gender

# Women are slightly more favorable overall towards the brand, but not much more than men. In general, the feelings are leaning on the ‘satisfied’ side of ‘neutral’.

# So let’s break it down by metric…

# Creating a collated dataframe of all the surveyed, ranked variables by gender.

metricsList <- c('Ambience_Rating', 'Sales_Promo_Rating', 'Quality_Rating', 'Price_Rating', 'Service_Rating', 'Meetup_Rating')

# Here I learned my lesson and made a function to act over the different metrics.

metric_mean <- function(metric){
  metric_subset <- subset(surveyAnswers_2, Metric == metric)
  return <- c(tapply(metric_subset$Rating, metric_subset$Gender, mean))
}


for (i in metricsList){
  metricmean <- metric_mean(i)
  if (i == 'Ambience_Rating'){
    surveyMeansDF = data.frame('AmbRat' = metricmean)
  }
  else{
    surveyMeansDF[[i]] <- metricmean
  }
}

surveyMeansDF

surveyMeansDF <- rownames_to_column(surveyMeansDF)

colnames(surveyMeansDF)[which(names(surveyMeansDF) == 'rowname')] <- 'Gender'

surveyMeansDF

survey_means_plot <- gather(surveyMeansDF, key = 'Metric', value = 'Rating', 2:7)
pl_2 <- ggplot(survey_means_plot, aes(x = Metric, y = Rating))

pl_2 + geom_bar(aes(fill = Gender), position = 'dodge', stat = 'identity') + theme(axis.text = element_text(size =8))


# Interesting! Men and women are fairly similar in their ratings except for a few areas:

# Women are more satisfied with Ambience, Sales/Promotions, and the quality of Service, while men are more satisfied with the use of Starbucks for meetings/get-togethers.

# One thing I’m curious about is which gender spends more per trip. That would be under “Spending”.

# I'm getting more consistent in using pipe operators. It's so handy.

spendingDF <- starbucks_revise %>% select(Gender, Spending, Membership)

spendingDF <- spendingDF %>% count(Gender, Spending, Membership)

spendingDF

spending_byGenDF <- starbucks_revise %>% select(Gender, Spending)

spending_byGenDF  <- spending_byGenDF  %>% count(Gender, Spending)

spending_byGenDF

order_spending <- c('Zero', 'Less than RM20', 'Around RM20 - RM40', 'More than RM40')

pl_spending <- ggplot(spendingDF, aes(x=factor(Spending, levels = order_spending), y=n), levels)

pl_spending + geom_bar(aes(fill = Gender), position = 'dodge', stat = 'identity') + xlab('Spending/Visit')


# We can see that the majority of spending occurs within a range that’s greater than RM0 but no more than RM40. Most surprising is that the highest count overall is women that spend less than RM20 (but not zero) and that they skew towards spending less, while men are most likely to spend between RM20-RM40 than any other bracket, and are generally evenly distributed around the average value. (They also account for the most number of visits that are RM40+.)

# In general, men are more likely to spend money on a visit than women, regardless of what that ammount is.

# Let’s see if there’s a gender disparity with members:

memberDF <- starbucks_revise %>% select(Gender, Membership)

memberDF <- memberDF %>% count(Gender, Membership)

memberDF

# WOW. Men not only (generally) spend more money per trip, but also are more likely than women to have a membership.

# For now, I’m just going to do a couple more comparisons:

continuebuyingDF <- starbucks_revise %>% select(Gender, Cont_Buying)

continuebuyingDF <- continuebuyingDF %>% count(Gender, Cont_Buying)

continuebuyingDF

# Men and women are equally likely to continue buying (about 75% saying they’ll keep going to Starbucks).

# One last thing before we stop:

buying_member_DF <- starbucks_revise %>% select(Membership, Spending)

buying_member_DF <- buying_member_DF %>% count(Membership, Spending)

buying_member_DF

cont_member_DF <- starbucks_revise %>% select(Membership, Cont_Buying)

cont_member_DF <- cont_member_DF %>% count(Membership, Cont_Buying)

cont_member_DF

# Of those surveyed, members and non-Members are spending about the same. However, members are generally the only group that will spend more than RM40 (and, recall, that group is entirely men.) Also, men are more likely to spend RM20-40, where as women are less likely to spend in the upper half of the spending brackets.

# Additionally, while amongst non-members the split between who will and won’t continue buying is not that large, those that are members will continue to buy products at an overwhelmingly higher rate than members that won’t.

# Summary
# My general observations so far are the following:
# Men appreciate the ability to use Starbucks facilities for meetups (business or otherwise), on average will spend more than women per trip, and also are more likely to have a membership than women.
# Both genders appreciate the sales and promotions, with women more pleased than men slightly.
# Men and women are in consensus that the factor from the survey in most need of improvement is pricing.
# Women are generally more satisfied about Starbucks across all measured metrics, save for whether they would use Starbucks for meetings/gatherings.


# Considerations for Future Actions:
# Survey members and non-members to understand the factors that contributed to their decisions to become members, as well as what aspects of Starbucks entices members to remain so. In addition, investigate whether memberships compel spending, or whether those that are more likely to spend are drawn to becoming members (ie, try to discover causality.)
# Gain and target more male visitors, since their spending per visit is generally higher; this can be done through targetting them via their memberships since men are more likely to be members than women. Men are also most satisfied about using Starbucks as a meet-up/gathering spot. Perhaps do research/survey on what kind of events or promotions could be done to leverage that and introduce men to the brand.
# Women, on the whole, are the largest demographics of consumers, but also are less likely to spend as much as men per visit. Considering this, as well as their slightly higher satisfaction with sales and promotions, target sales and promotions towards women. Since it will take more women visiting to spend as much as men, brainstorm promotions that would get more women into the stores. (Buy 1, get 1 50% off deals, as an example.)



# Conclusion
# I really enjoyed parsing this data set and seeing what insights it provided.
# This took me about 4-5 days to do, spending about 3-4 hours each day. The main factors that contributed to the time were brainstorming and experimenting with data cleaning. I am really proud of myself for how the ‘starbucks_revise’ turned out as a strong basis for the rest of the evaluation.
# Similarly, a large chunk of time was reminding myself of specific parameters for so many categorical variables at the same time. Now that I’ve completed this project, it feels a LOT more accessible than before.
# My next steps would be the following:
# K Means Clustering to discover potential groups for targetting.
# Further demographic analysis (specifically, investigate age, time of visit, and visit frequency) and create a more holistic perspective using the analysis already done in this markup.
# If you have any feedback, please let me know! This was one of my first published projects and I want to get better!
