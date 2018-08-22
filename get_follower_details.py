#!/usr/bin/python3
import twint
import sys

print ("The arguments are: " , str(sys.argv))

user = sys.argv[1]
file_name = str(user) + '_is_followed_by.csv'

print('user is ', user)
print('file name is ', file_name)

c = twint.Config()
c.Username = user
c.Store_csv = True
c.User_full = True
c.Custom = ["id", "name", "username", "bio", "location",  "url", "join_date", "join_time", "tweets", "following", "followers", "likes", "media", "private", "verified"]
c.Output = file_name


twint.run.Followers(c)

print('Wrote file to ' + file_name)
