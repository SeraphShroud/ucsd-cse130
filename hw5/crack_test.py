
from misc import *
import crypt

def load_words(filename,regexp):
    """Load the words from the file filename that match the regular
       expression regexp.  Returns a list of matching words in the order
       they are in the file."""
    f = open(filename, 'r')
    r = re.compile(regexp)
    a = []
    for line in f.readlines():
        word = line.strip()
        if r.match(word):
            a.append(word)
            # a[len(a):] = [word]
    return a

# Returns a list with the original string and the reversal of the original string
def transform_reverse(str):
    return [str, str[::-1] ]

# Returns a list of all the possible ways to capitalize the input string
def transform_capitalize(str):
    a = [str.lower()]
    for i in range(1 << len(str)):
        new_word = list(str.lower())
        for j in range(0, len(str)):
            if i & 1 << j:
                new_word[j:j+1] = list((''.join(new_word[j:j+1])).upper())
                a.append(''.join(new_word))
    return a

def transform_digits(str):
    # transform = {'o' : '0',
    #              'i' : '1',
    #              'l' : '1',
    #              'z' : '2',
    #              'e' : '3',
    #              'a' : '4',
    #              's' : '5',
    #              'b' : '6',
    #              't' : '7',
    #              'g' : '9',
    #              'q' : '9' }
    # # transform_alt is equivalent to transform, except 'b' is mapped to 8 instead of 6
    # transform_alt = dict(transform)
    # transform_alt['b'] = '8'


    res = [str]
    for w in res:
        i = 0
        while i < len(w):
            ch_low = w[i].lower()
            sb = w
            if ch_low == 'o':
                sb = build(sb,'0',i)
            elif ch_low == 'i':
                sb = build(sb,'1',i)
            elif ch_low == 'l':
                sb = build(sb,'1',i)
            elif ch_low == 'z':
                sb = build(sb,'2',i)
            elif ch_low == 'e':
                sb = build(sb,'3',i)
            elif ch_low == 'a':
                sb = build(sb,'4',i)
            elif ch_low == 's':
                sb = build(sb,'5',i)
            elif ch_low == 'b':
                sb = build(sb,'6',i)
                if (sb not in res):
                    res.append(sb)
                sb = build(sb,'8',i)
            elif ch_low == 't':
                sb = build(sb,'7',i)
            elif ch_low == 'g':
                sb = build(sb,'9',i)
            elif ch_low == 'q':
                sb = build(sb,'9',i)
            else: pass
            if (sb not in res):
                res.append(sb)
            i += 1
 
    return res

# helper string builder function
def build(s, num, index):
    s = s[:index] + str(num) + s[index+1:]
    return s



def check_pass(plain,enc):
    """Check to see if the plaintext plain encrypts to the encrypted
       text enc"""
    return crypt.crypt(plain, enc[0:2]) == enc

def load_passwd(filename):
    """Load the password file filename and returns a list of
       dictionaries with fields "account", "password", "UID", "GID",
       "GECOS", "directory", and "shell", each mapping to the
       corresponding field of the file."""
    l = []
    f = open(filename, 'r')
    for line in f.readlines():
        tokens = re.split(':', line)
        d = dict(zip( ['account', 'password', 'UID', 'GID', 'GECOS', 'directory', 'shell'],
                      tokens ))
        l.append(d)
    f.close()
    return l
    

def crack_pass_file(pass_filename,words_filename,out_filename):
    """Crack as many passwords in file fn_pass as possible using words
       in the file words"""

    # open output file
    ostream = open(out_filename, 'w')

    accounts = load_passwd(pass_filename)

    words = load_words(words_filename, r'^.{6,8}$')


    # no transformations
    for account in accounts:
        username = account['account']
        enc = account['password']
        for word in words:
            if check_pass(word, enc):
                accounts.remove(account)
                ostream.write(username + "=" + word + "\n")
                ostream.flush()
                break


    # transformations
    i = 0

    for word in words:
        print(i)
        i += 1
        for c in transform_capitalize(word):
            for d in transform_digits(c):
                for r in transform_reverse(d):
                    for account in accounts:
                        username = account['account']
                        enc = account['password']
                        if check_pass(r, enc):
                            accounts.remove(account)
                            ostream.write(username + "=" + r + "\n")
                            ostream.flush()
                            print(username + "=" + r + "\n")
                            break
        # for c in transform_digits(word):
        #     for account in accounts:
        #         username = account['account']
        #         enc = account['password']
        #         if check_pass(c, enc):
        #             accounts.remove(account)
        #             ostream.write(username + "=" + c + "\n")
        #             ostream.flush()
        #             print(username + "=" + c + "\n")
        #             break

    # for word in words:
    #     for account in accounts:
    #         print(i)
    #         i += 1
    #         username = account['account']
    #         enc = account['password']
    #         for c in transform_capitalize(word):
    #             if check_pass(c, enc):
    #                 accounts.remove(account)
    #                 ostream.write(username + "=" + c + "\n")
    #                 ostream.flush()
    #                 print(username + "=" + c + "\n")
    #                 break
    #     for account in accounts:
    #         for d in transform_digits(word):
    #             if check_pass(d, enc):
    #                 accounts.remove(account)
    #                 ostream.write(username + "=" + d + "\n")
    #                 ostream.flush()
    #                 print(username + "=" + d + "\n")
    #                 break

    # print("Applying transformations")
    # for account in accounts:
    #     username = account['account']
    #     enc = account['password']
    #     for word in words:
    #         print(".")
    #         for c in transform_capitalize(word):
    #             if check_pass(c, enc):
    #                 accounts.remove(account)
    #                 ostream.write(username + "=" + c + "\n")
    #                 ostream.flush()
    #                 print(username + "=" + c + "\n")
    #                 break
    #         for d in transform_digits(word):
    #             if check_pass(d, enc):
    #                 accounts.remove(account)
    #                 ostream.write(username + "=" + d + "\n")
    #                 ostream.flush()
    #                 print(username + "=" + d + "\n")
    #                 break
            # words_t.append(c)
            # for d in transform_digits(word):
            #     words_t.append(d)

    # for account in accounts:
    #     username = account['account']
    #     enc = account['password']
    #     for word in words_t:
    #         if check_pass(word, enc):
    #             accounts.remove(account)
    #             ostream.write(username + "=" + word + "\n")
    #             ostream.flush()
    #             print(username + "=" + word + "\n")
    #             break
            

    ostream.close()