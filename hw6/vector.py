# PA 6 - vector.py
from misc import Failure

def isSequence(object):
    """ Returns True if {object} is a type of sequence, False otherwise """
    return type(object) in [str, unicode, list, tuple, bytearray, buffer, xrange]

class Vector(object):
    """ The Vector class defines a sequence of values and supports component-wise 
        addition, comparison operations, and dot product. """

    def __init__(self, object):
        """ Vector constructor takes an object that represents either the length
            of the vector or the elements of the vector """
        if (isinstance(object,int) or isinstance(object,long)):
            # integer value --> create vector of length <object>
            if (object < 0):
                raise ValueError("Vector length cannot be negative")
            self.vec = [0.0 for i in range(object)]
        elif (isSequence(object)):
            # sequence value --> create vector containing elements in <object>
            self.vec = list(object)
        else:
            raise TypeError("Unexpected type %s" % type(object))

    def __repr__(self):
        """ Returns a string representation of the vector """
        return "Vector(" + repr(self.vec) + ")"

    def __len__(self):
        """ Returns the length of the vector """
        return len(self.vec)

    def __iter__(self):
        """ Returns an iterator for the vector """
        for n in self.vec:
            yield(n)

    # Operators
    def __add__(self, other):
        """ (+) Returns a vector that is the sum of a vector and a sequence """
        if (len(self) != len(other)):
            raise ValueError("Cannot add vectors of different lengths")
        return Vector([(i + j) for (i,j) in (zip(list(self), list(other))) ])

    def __radd__(self, other):
        """ (+) Returns a vector that is the sum of a vector and a sequence """
        if (len(self) != len(other)):
            raise ValueError("Cannot add vectors of different lengths")
        return Vector([(i + j) for (i,j) in (zip(list(self), list(other))) ])

    def __iadd__(self, other):
        """ (+=) Increment this vector by another vector (or sequence) """
        if (len(self) != len(other)):
            raise ValueError("Cannot add vectors of different lengths")
        self.vec = Vector([(i + j) for (i,j) in (zip(list(self), list(other))) ])
        return self.vec

    # Dot product
    def dot(self, other):
        """ Returns the dot product of current vector and a sequence """
        if (len(self) != len(other)):
            raise ValueError("Cannot perform operation on vectors of different lengths")

        try:
            return sum( [i * j for i,j in zip(self, other)] )
        except:
            return sum( [str(i) + str(j) for i,j in zip(self, other)] )

    # Accessors/Mutators

    def __getitem__(self, key):
        """ ([]) Returns the element of the vector specified by the index {key} """
        if (type(key) == slice):
            return self.vec[key]
        else:
            if (key >= len(self) or key < -len(self)):
                raise IndexError("vector index out of range")
            else:
                if (key < 0):
                    key += len(self)
                return self.vec[key]


    def __setitem__(self, key, value):
        """ ([]) Set the element(s) of the vector specified by {key} to {value}.
            Raises a ValueError if doing so would alter the length of the vector """
        if (type(key) == slice):
             # copy vector into temp
            temp = [x for x in self.vec]
            temp[key] = value
            # if {value} would change the length of the vector, raise an exception
            if (len(temp) != len(self.vec)):
                raise ValueError("cannot change vector length")
            else:
                self.vec = temp
        else:
            if (key >= len(self) or key < -len(self)):
                raise IndexError("vector index out of range")
            self.vec[key] = value
    

    # Comparison Operators
    
    def __eq__(self, other):
        """ (==) Returns True if both vector contain the same elements
            in the same order """
        if isinstance(other, Vector):
            for i, j in zip(self, other):
                if not i == j:
                    return False
            return True
        else:
            return False

    def __ne__(self, other):
        """ (!=) Returns True if vectors contain different elements, or
            a different ordering of the same elements """
        return not self.__eq__(other)

    def __gt__(self, other):
        """ (>) Returns True if the largest unique element of current vector is
            greater than the largest unique element of {other} vector. Otherwise, 
            returns False. """
        if isinstance(other, Vector):
            for i, j in zip(sorted(self, reverse = True), 
                            sorted(other, reverse = True)):
                if not i > j:
                    return False
            return True
        else:
            return False

    def __ge__(self, other):
        """ (>=) Returns True if the largest unique element of current vector is
            greater than the largest unique element of {other} vector or if 
            elements are equal. Otherwise, returns False. """
        if isinstance(other, Vector):
            for i, j in zip(sorted(self, reverse = True), 
                            sorted(other, reverse = True)):
                if not i >= j:
                    return False
            return True
        else:
            return False

    def __lt__(self, other):
        """ (<) Returns True if the smallest unique elemnt of current vector is
            less than the smallest unique element of {other} vector. Otherwise,
            returns False. """
        return not self.__ge__(other)

    def __le__(self, other):
        """ (<=) Returns True if the smallest unique element of current vector is
            less than the smalles unique element of {other} vector or if 
            elements are equal. Otherwise, returns False. """
        return not self.__gt__(other)

