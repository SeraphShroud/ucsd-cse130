#PA 4

import re

"Miscellaneous functions to practice Python"

class Failure(Exception):
    """Failure exception"""
    def __init__(self,value):
        self.value=value
    def __str__(self):
        return repr(self.value)

# Problem 1

# data type functions


# closest_to(l,v) returns the element of the list l closest in value to v.
# In the case of a tie, the first such element is returned. If l is empty,
# None is returned.
def closest_to(l,v):
    """Return the element of the list l closest in value to v.  In the case of
       a tie, the first such element is returned.  If l is empty, None is returned."""
    if not l:
        return None

    ret_val = l[0]
    for i in l:
        if abs(i - v) < abs(ret_val - v):
            ret_val = i
    return ret_val

# make_dict(keys,vals) takes a list of keys and a list of values and returns
# a dictionary (dict) pairing keys to corresponding values.
def make_dict(keys,values):
    """Return a dictionary pairing corresponding keys to values."""
    return dict(zip(keys, values))

   
# file IO functions
 
# word_count(fn) takes a string, fn (representing a file name) and return a
# dictionary mapping words to the number of times they occur in the file fn.
# A is defined as a sequence of alphanumeric characters and underscore. Words 
# are returned in lower case and case is ignored when counting occurrences.
def word_count(fn):
    """Open the file fn and return a dictionary mapping words to the number
       of times they occur in the file.  A word is defined as a sequence of
       alphanumeric characters and _.  All spaces and punctuation are ignored.
       Words are returned in lower case"""

    f = open(fn, 'r')
    d = dict()
    pattern = re.compile('[A-Za-z0-9_]+')
    words = "".join([c if pattern.match(c) else ' ' for c in f.read().lower()])
    for w in words.split():
        if w in d:
            d[w] += 1
        else:
            d[w] = 1
    f.close()
    return d


