import sys, resource

class recursionlimit:
    def __init__(self, limit):
        self.limit = limit

    def __enter__(self):
        self.old_limit = sys.getrecursionlimit()
        self.old_rlimit = resource.getrlimit(resource.RLIMIT_STACK)
        #resource.setrlimit(resource.RLIMIT_STACK, (2**29,-1))
        sys.setrecursionlimit(self.limit)

    def __exit__(self, type, value, tb):
        sys.setrecursionlimit(self.old_limit)
        #resource.setrlimit(resource.RLIMIT_STACK, self.old_rlimit)