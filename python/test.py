from textblob import TextBlob

test = TextBlob("Some really hilarious ridiculous sample text")
test2 = TextBlob("Some terrible sample text")
print(test.sentiment.polarity)