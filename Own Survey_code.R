# Starbucks Customer Survey Evaluation
# Task:

# Evaluate and report on customer profiling, areas of satisfaction, and demographics to target for potential growth opportunities.

# First step is to intake the data and see what we are working with.

library('dplyr')
library('ggplot2')
library('tidyr')
library('tidyverse')
library('data.table')
library('tibble')


#######################
#####preprocessing#####
#######################

starbucks <- read.csv('C:/Users/zhaoz/Desktop/RR_ZZ/data_ZZ.csv')

# Next we will evaluate the variables and observations to get an idea of the scale of the dataset.
colnames(starbucks)
str(starbucks)

# There are a lot of variables, and many are character strings for categorical data. (Ex: Gender is ‘Male’ and ‘Female’.)
# Clean up the data a little.

# Firstly, rename the variables since some of them are descriptive but way too long.

starbucks <- starbucks %>% rename(UniqID = Timestamp,
                                  Gender = Gender,
                                  Age = Age,
                                  Employment = Are.you.currently.....,
                                  Income_Bracket = Annual.income,
                                  Visit_Freq = How.often.do.you.visit.Starbucks.,
                                  Mode_of_Visit = How.do.you.usually.enjoy.Starbucks.,
                                  Time_Visit = How.much.time.do.you.normally.spend.during.your.visit.,
                                  Nearest_Store = The.nearest.Starbucks.s.outlet.to.you.is....,
                                  Membership = Do.you.have.Starbucks.membership.card.,
                                  Purchases = What.do.you.most.frequently.purchase.at.Starbucks.,
                                  Spending = On.average..how.much.would.you.spend.at.Starbucks.per.visit.,
                                  Quality_Rating = How.would.you.rate.the.quality.of.Starbucks.compared.to.other.brands..Coffee.Bean..Old.Town.White.Coffee....to.be.,
                                  Price_Rating = How.would.you.rate.the.price.range.at.Starbucks.,
                                  Sales_Promo_Rating = How.important.are.sales.and.promotions.in.your.purchase.decision.,
                                  Ambience_Rating = How.would.you.rate.the.ambiance.at.Starbucks...lighting..music..etc....,
                                  WiFi_Rating = You.rate.the.WiFi.quality.at.Starbucks.as..,
                                  Service_Rating = How.would.you.rate.the.service.at.Starbucks...Promptness..friendliness..etc...,
                                  Meetup_Rating = How.likely.you.will.choose.Starbucks.for.doing.business.meetings.or.hangout.with.friends.,
                                  Promo_Communication = How.do.you.come.to.hear.of.promotions.at.Starbucks..Check.all.that.apply.,
                                  Cont_Buying = Will.you.continue.buying.at.Starbucks.)

# Next change the date/time variables to UniqIDs, and make the character variables into factors.

starbucks <- starbucks %>%
  mutate(UniqID = 1:nrow(starbucks)) %>%
  relocate(UniqID, .before=Gender)

starbucks_revise <- starbucks %>% mutate_if(is.character, as.factor)

# Now check the different values in each variable are, to check for typos, NAs, etc.

str(starbucks_revise)
any(is.na(starbucks_revise))

# A cursory examination seems to show that ‘Gender’, ‘Age’, ‘Employment’, ‘Income_Bracket’, ‘Visit_Freq’, ‘Time_Visit’,
# ‘Nearest_Store’, ‘Membership’, ‘Spending’, and ‘Cont_Buying’ have managable unique factors.

########################
##### Explorations #####
########################

# Firstly get an idea of gender, income, employment, and spending distributions. However,
# the ‘Income_Bracket’ will sort alphanumerically, which might make the graphs confusing,
# so I’m going to reorder them and then check the graphs.

## Order variable "income"
unique(starbucks_revise$Income_Bracket)
order_income <- c('Less than 25,000PLN', '25,000PLN - 50,000PLN', '50,000PLN - 100,000PLN', '100,000PLN - 150,000PLN',
                  'More than 150,000PLN')

starbucks_revise$Income_Bracket <- factor(starbucks_revise$Income_Bracket, levels = order_income)

head(starbucks_revise[order(starbucks_revise$Income_Bracket),])


## Gender distribution
distGender <- data.frame(starbucks_revise %>%
                           count(Gender))

Gender_report = distGender %>% mutate(Ratio = proportions(n))
Gender_report

# What we’ve gleaned is that gender is not distributed fairly, skew towards Male customers.

## Income distribution

p1a <- ggplot(starbucks_revise, aes(x = Income_Bracket))

p1b <- p1a + geom_bar(aes(fill=Income_Bracket))+
  theme(axis.text.x = element_blank())

p1b

## combine gender and income to get a broader view.

p2a <- ggplot(starbucks_revise, aes(x = Gender))

p2b <- p2a + geom_bar(aes(fill=Income_Bracket))

p2b

# So while customers that identify as ‘female’ outnumber ‘male’-identifying customers,
# they also appear to fall into lower income brackets more frequently.

# Let’s get some more quantifiable measures on these insights.

## Creat Male and Female Income DFs

Gender_Income <- as.data.frame(starbucks_revise %>% count(Gender, Income_Bracket))

Female_Income <- data.frame(subset(Gender_Income, Gender == 'Female'))
Male_Income   <- data.frame(subset(Gender_Income, Gender == 'Male'))

rownames(Male_Income) <- seq(1:nrow(Male_Income))

## Summarize each income bracket by gender:

sumFemale_Income <- sum(Female_Income$n)

# Here we create a dataframe for each gender that contains the average count for each income bracket.

Female_Income_Prct <- data.frame(Income_Bracket = character(), Percentage_F = numeric())


for (i in 1:nrow(Female_Income)){
  avg <- as.double(Female_Income[i,'n'] / sumFemale_Income)
  avg_brkt <- c(as.character(Female_Income[i,'Income_Bracket']), as.numeric(round(avg, 3)))
  Female_Income_Prct[i,] <- avg_brkt
}

Female_Income_Prct$Percentage <- as.numeric(as.character(Female_Income_Prct$Percentage))

Female_Income_Prct$Percentage_F = NULL

## Result

Female_Income_Prct

# Do the same to "Male"

# Same process here as the female.

sumMale_Income <- sum(Male_Income$n)

Male_Income_Prct <- data.frame(Income_Bracket = character(), Percentage_M = numeric())

for (i in 1:nrow(Male_Income)){
  avg <- as.double(Male_Income[i,'n'] / sumMale_Income)
  avg_brkt <- c(as.character(Male_Income[i,'Income_Bracket']), as.numeric(round(avg, 3)))
  Male_Income_Prct[i,] <- avg_brkt
}

Male_Income_Prct$Percentage <- as.numeric(as.character(Male_Income_Prct$Percentage))

Male_Income_Prct$Percentage_M = NULL

## Result

Male_Income_Prct

# And now join them together.

Income_Prct <- merge(Male_Income_Prct, Female_Income_Prct, by = 'Income_Bracket', all.x = TRUE)

colnames(Income_Prct) <- c('IncomeBracket', 'Percent_M', 'Percent_F')

Income_Prct[is.na(Income_Prct)] <- 0

Income_Prct <- gather(Income_Prct, key = 'Gender', value = 'Percent', 2:3)

Income_Prct$Percent <- Income_Prct$Percent * 100
# result
Income_Prct


## plot Income Bracket
p3a <- ggplot(Income_Prct, aes(x = factor(IncomeBracket, level = order_income), y = Percent, fill = Gender))

p3b <- p3a + geom_bar(position = 'dodge', stat = 'identity') +
  coord_flip()+
  xlab('Income Bracket') +
  geom_text(aes(label = Percent), position = position_dodge2(width = 0.9), vjust = 0.5)

p3b

# So what we’ve gathered so far is that assuming fairly even gender distribution,
# female-identifying customers lean more towards the lower income brackets,
# while males solely occupy the higher income brackets (any more that RM50,000).

# Here are 7 more survey questions to score, and we'll analyze them next with gender.
surveyAnswers <- starbucks_revise %>% select(c('Gender', 'Quality_Rating', 'Price_Rating', 'Sales_Promo_Rating',
                                               'Ambience_Rating', 'WiFi_Rating', 'Service_Rating', 'Meetup_Rating'))

surveyAnswers_2 <- gather(surveyAnswers, key = 'Metric', value = "Rating", 2:8)

survey_by_Gender <- tapply(surveyAnswers_2$Rating, surveyAnswers_2$Gender, mean)

survey_by_Gender


# Males are slightly more favorable overall towards the brand, but not much more than women. In general,
# the feelings are leaning on the ‘satisfied’ side of ‘neutral’.
# So we break it down by metric.

## Creating a collated dataframe of all the surveyed, ranked variables by gender.

metricsList <- c('Ambience_Rating', 'Sales_Promo_Rating', 'Quality_Rating', 'Price_Rating', 'WiFi_Rating','Service_Rating', 'Meetup_Rating')

# We made a function to act over the different metrics.

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

surveyMeansDF <- rownames_to_column(surveyMeansDF)

colnames(surveyMeansDF)[which(names(surveyMeansDF) == 'rowname')] <- 'Gender'

#result
surveyMeansDF

survey_means_plot <- gather(surveyMeansDF, key = 'Metric', value = 'Rating', 2:8)

## plot
p4a <- ggplot(survey_means_plot, aes(x = Metric, y = Rating))

p4b <- p4a + geom_bar(aes(fill = Gender), position = 'dodge', stat = 'identity') + theme(axis.text = element_text(size =8)) + coord_flip()

p4b

# The results are not very different by gender. Men and women are fairly similar in their ratings,
# But there are some difference in a few areas:
# Women are more satisfied with Sales/Promotions, while men are more satisfied with the use of Starbucks for meetings/get-togethers and
# quality of coffee.

# Check which gender spends more per trip: “Spending”.
spendingDF <- starbucks_revise %>% select(Gender, Spending, Membership)

spendingDF <- spendingDF %>% count(Gender, Spending, Membership)

spending_byGenDF <- starbucks_revise %>% select(Gender, Spending)

spending_byGenDF  <- spending_byGenDF  %>% count(Gender, Spending)
spending_byGenDF

order_spending <- c('0 PLN', 'Less than 20 PLN', '20 PLN - 40 PLN', 'More than 40 PLN')

pl_spending <- ggplot(spending_byGenDF, aes(x = factor(Spending, levels = order_spending), y = n), levels)

p5b <- pl_spending + geom_bar(aes(fill = Gender), position = 'dodge', stat = 'identity') + xlab('Spending/Visit')

p5b
# We can see that most of the cost are in the range greater than 0 PLN but not more than 40 PLN. The top overall spends 20 PLN - 40 PLN.
# While more men spend between 20 PLN and 40 PLN, and only men spend more than 40 PLN at a time in this survey.

# In general, men are more likely to spend money on a visit than women, regardless of what that amount is.



# Let’s see if there’s a gender disparity with members:

memberDF <- starbucks_revise %>% select(Gender, Membership)

memberDF <- memberDF %>% count(Gender, Membership)

memberDF


# Men not only (generally) spend more money per trip, but also are more likely than women to have a membership.

# For now, we are going to do a couple more comparisons:

continuebuyingDF <- starbucks_revise %>% select(Gender, Cont_Buying)

continuebuyingDF <- continuebuyingDF %>% count(Gender, Cont_Buying)

continuebuyingDF


## Men and women are equally likely to continue buying (about 87% saying they’ll keep going to Starbucks).

# Spending and Membership

buying_member_DF <- starbucks_revise %>% select(Membership, Spending)

buying_member_DF <- buying_member_DF %>% count(Membership, Spending)

buying_member_DF

## Membership and Willingness to continue buying

cont_member_DF <- starbucks_revise %>% select(Membership, Cont_Buying)

cont_member_DF <- cont_member_DF %>% count(Membership, Cont_Buying)

cont_member_DF


## Of those surveyed, members and non-Members are spending about the same. However,
## members are generally the only group that will spend more than 40 PLN (that group is entirely men.)
## Also, men are more likely to spend 20PLN - 40PLN,
## where as women are less likely to spend in the upper half of the spending brackets.

# Among non-members, the gap between who will continue to buy and who will not continue to buy is not large,
# but those who become members will continue to buy products at a higher rate than members who do not.


#################
#####Summary#####
#################

# Men appreciate the ability to use Starbucks facilities for meetups (business or otherwise), on average will spend more than women
# per trip, and also are more likely to have a membership than women.
#
# Both genders appreciate the sales and promotions, with women more pleased than men slightly.
#
# Men and women are in consensus that the factor from the survey in most need of improvement is pricing.
#
# Women are generally more satisfied about Starbucks across all measured metrics, save for whether they would use Starbucks
# for meetings/gatherings.


########################
#####Consideration#####
########################


# Survey members to understand the factors that contributed to their decisions to become members,
# Survey non-members, what caused them not to become members.  Because we found that there are many participants (15/39) who are
# non-members but they are still willing to continue to buy.

# Gain and target more male visitors, since their spending per visit is generally higher; this can be done through targeting
# them via their memberships since men are more likely to be members than women. Men are also most satisfied about using Starbucks
# as a meet-up/gathering spot.

# The female consumer group cannot be ignored, but also are less likely to spend as much as men per visit.
# Considering this, as well as their slightly higher satisfaction with sales and promotions,
# target sales and promotions towards women. Since it will take more women visiting to spend as much as men,
# Therefore, Starbucks needs more female consumers to participate in promotional activities.
