#!/usr/bin/python3
import twint
import sys

## Search and print on screen
#c = twint.Config()
#c.search = 'golpe'
#c.Username = "Albert_Rivera"
#c.Format = "Tweet id: {id} | Tweet: {tweet}"
#twint.run.Search(c)

print("This is the name of the script: " + sys.argv[0])
print("Number of arguments: ", len(sys.argv))
print ("The arguments are: " , str(sys.argv))

user = sys.argv[1]
search = sys.argv[2]
file_name = 'user=' + str(user) + 'search=' + str(search) + '.csv'

print('user is ', user)
print('search is ', search)
print('file name is ', file_name)

c = twint.Config()
c.Username = user
c.search = search

c.Store_csv = True
c.Output = file_name

twint.run.Search(c)
