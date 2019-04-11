#!/usr/bin/env python3

class go_struct:
    pass

def copy_before_pass(obj):
    if type(obj) == list:
        copy = [copy_before_pass(x) for x in obj]
    elif type(obj) == go_struct:
        copy = go_struct()
        for k,v in obj.__dict__.items():
            setattr(copy, k, copy_before_pass(v))
    else:
        copy = obj
    return copy

class go_slice:
    def __init__(self):
        self.contents = []
        self.capacity = 0
        self.length = 0
        
    def __getitem__(self, i):
        if i < self.length:
            return self.contents[i]
        s = "slice access out of bounds: {} index >= {} length"
        raise IndexError(s.format(i, self.length))

    def __setitem__(self, i, x):
        if i < self.length:
            self.contents[i] = x
            return
        s = "slice access out of bounds: {} index >= {} length"
        raise IndexError(s.format(i, self.length))
    
    def append(self, x):
        new_slice = go_slice()
        new_slice.length = self.length + 1
        
        if new_slice.length > self.capacity:
            if self.capacity == 0:
                new_slice.capacity = 2
            else:
                new_slice.capacity = int(self.capacity * 2)
            
            new_contents = [copy_before_pass(x) for x in self.contents]
        else:
            new_slice.capacity = self.capacity
            new_contents = self.contents
        
        if self.length < len(self.contents):
            new_contents[self.length] = x
        else:
            new_contents.append(x)

        new_slice.contents = new_contents
        return new_slice
    
    def __len__(self):
        return self.length
    
    def capacity(self):
        return self.capacity

# xs is a go_slice or a list
def cap(xs):
    if type(xs) == go_slice:
        return xs.capacity
    return len(xs)

# xs is a go_slice
def append(xs, x):
    return xs.append(x)
