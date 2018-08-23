#!/usr/bin/python3
import twint
import sys

print ("The arguments are: " , str(sys.argv))

user = sys.argv[1]
file_name = 'data/' + str(user) + '_is_following_simple.csv'

print('user is ', user)
print('file name is ', file_name)

c = twint.Config()
c.Username = user
c.Store_csv = True
c.User_full = False
# c.Custom = ["name", "username", "following", "followers"]
c.Output = file_name


twint.run.Following(c)

print('Wrote file to ' + file_name)
