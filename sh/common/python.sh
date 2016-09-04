alias urlencode='python -c "import sys, urllib as ul; print(ul.quote_plus(sys.argv[1]))"'
alias urldecode='python -c "import sys, urllib as ul; print(ul.unquote_plus(sys.argv[1]))"'
alias timestamp='python -c "import sys, datetime as d; print(d.datetime.fromtimestamp(float(sys.argv[1])))"'
# python -c "import multiprocessing as mp; import itertools as it; [mp.Process(target=lambda: [i for i in it.count() if not i]).start() for i in range(mp.cpu_count())]"
