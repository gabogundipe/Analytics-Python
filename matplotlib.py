#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 28 17:57:44 2019

@author: gabrielogundipe
"""
# data can be found at https://www.kaggle.com/crowdflower/twitter-airline-sentiment

import pandas as pd
import numpy
import matplotlib.pyplot as plt
import os

#gets and sets working directory, reads file in after
currentDirectory = os.getcwd()
dataDirectory = os.path.join(currentDirectory, 'Tweets.csv')

try:
    twitter_data = pd.read_csv(dataDirectory)
except FileNotFoundError:
    print('File does not exist.')



twitter_data.head()

#deleting columns that are empty as they are made irrelevant

del twitter_data['airline_sentiment_gold']
del twitter_data['negativereason_gold']




#barchart whole sentiment industry 
b_sentiment = pd.Series(twitter_data["airline_sentiment"].value_counts())
b_sentiment.plot(kind="bar", rot=0,figsize=(12,10), title = "Total tweets of each sentiment")
plt.ylabel("No of tweets")





#piechart sentiment whole industry
piechart_sentiment = (twitter_data["airline_sentiment"]).value_counts()
piechart_sentiment.plot(kind="pie",
    labels=["negative", "neutral", "positive"],explode=[0.05,0.02,0.04],
    shadow=True,autopct='%1.1f%%', fontsize=12,figsize=(6, 6),title = "Total Tweets for Individual Sentiment")
plt.legend(loc=1)



#showing negative reasons in the whole industry
negReasons = pd.Series(twitter_data['negativereason']).value_counts()
negReasons.plot(kind = "bar", title = "Total Negative Reasons", rot=45)
plt.ylabel("No of Tweets")
plt.tight_layout()


plt.clf()


#barchart of each airline sentiment

b_sentiment_airline = twitter_data.groupby(['airline'])
b_sentiment_airline = b_sentiment_airline.airline_sentiment.value_counts().unstack()
b_sentiment_airline.plot(kind="bar", rot = 0, figsize = (10,8), alpha = 0.7, title = "Airline sentiments for each Airline" )
plt.ylabel('No of tweets')


#plotting top negative reason for each airline
top_two_neg = twitter_data.groupby('airline')['negativereason'].value_counts(sort=True).groupby(level=0).head(2)
top_two_neg = top_two_neg.unstack()
top_two_neg.plot(kind="bar", figsize=(12,10))


#boxplot of distribution of airline sentiments 
twitter_data.boxplot(column=['airline_sentiment_confidence'], by ='airline_sentiment')




twitter_data['tweet_created'] = pd.to_datetime(twitter_data['tweet_created'])
twitter_data["date_created"] = twitter_data['tweet_created'].dt.date
twitter_data["date_created"]

#line plot of sentiments over dates
line_sentiment = twitter_data.groupby(['date_created'])
line_sentiment = line_sentiment.airline_sentiment.value_counts().unstack()
line_sentiment.plot(kind = 'line', figsize=(12,10), title = "Sentiments levels over the week", fontsize = 11, grid = True)
plt.ylabel('no of tweets')
plt.xlabel('date created')

#line plot of negative reasons over dates
line_neg_reasons = twitter_data.groupby(['date_created'])
line_neg_reasons = line_neg_reasons.negativereason.value_counts().unstack()
line_neg_reasons.plot(kind = "line", figsize=(12,10), title = "Sentiments levels over the week", fontsize = 11, grid = True)
plt.ylabel('no of tweets')



#negative reasons of one airline data
def neg_timeline(Airline):
     airline_Data = twitter_data[twitter_data['airline'] == Airline]
     j = airline_Data.groupby(['date_created'])
     j = j.negativereason.value_counts().unstack()
     j.plot(kind = "line")
     
     
neg_timeline("United")



#negative reason confidence boxplot
twitter_data.boxplot(column=['negativereason_confidence'])
plt.ylabel('range of confidence')


#top 2 negative reasons of each airline, stacked barchart
top_two_stacked = twitter_data
top_two_stacked = top_two_stacked.groupby('airline')
top_two_stacked = top_two_stacked.negativereason.value_counts(sort=True).groupby(level=0).head(2)
top_two_stacked = top_two_stacked.unstack()
top_two_stacked = top_two_stacked.plot(kind='bar', stacked=True, rot = 0,figsize = (14,10), fontsize = 11, title = "top 2 negative reasons")
top_two_stacked.legend(loc=1)
top_two_stacked.set_ylabel('no of tweets')


