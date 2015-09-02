


def to_human(num):
    for i, unit in enumerate(["", "K", "M", "G", "T", "P", "Z"]):
        min = 2 ** (i * 10)
        max = 2 ** ((i + 1) * 10)
        if min <= num < max:
            return "%s %s bite (= %s)" % (num // min, unit, num)
    else:
        return "%s bite" % num


def memo(f):
    cache = {}

    def wrap(*key):
        if key in cache:
            r = cache[key]
        else:
            r = f(*key)
            cache[key] = r
        return r
    return wrap
