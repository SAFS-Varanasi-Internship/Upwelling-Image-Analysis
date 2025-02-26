from __future__ import division

'''
This test and code was motivated by 
http://stats.stackexchange.com/questions/100838/autocorrelation-of-discrete-time-series
and
http://stats.stackexchange.com/questions/73084/analysis-of-temporal-patterns/73170#73170
as described in
P. C. O'Brien and P. J. Dyck. A runs test based on run lengths. Biometrics, pages 237-244, 1985.
'''

__author__ = 'psinger'

from collections import Counter, defaultdict
import sys
import math
from scipy.stats import chi2

def weighted_variance(counts):
    #counts = Counter(x).items()

    avg = 0

    for length, count in counts.iteritems():
        avg += count * length

    counts_only = counts.values()

    avg /= sum(counts_only)

    var = 0

    for length, count in counts.iteritems():
        var += count * math.pow((length - avg),2)

    try:
        var /= sum(counts_only) - 1
    except:
        #var = 0
        raise Exception("Division by zero due to too few counts!")

    return var

def runs_test(input, path = True):
    '''
    You can pass a path or a dictionary of runs lengths
    path_passed = True states that you pass a path
    '''

    if path == True:
        counter = 1
        same = True
        cats = defaultdict(lambda : defaultdict(int))

        for i, elem in enumerate(input):
            #print elem, i
            if i == len(input) - 1:
                cats[elem][counter] += 1
                break

            if input[i+1] == elem:
                same = True
                counter += 1
            else:
                cats[elem][counter] += 1
                counter = 1
    else:
        cats = input

    #print cats

    x2 = 0
    df = 0
    nr_elem = len(cats.keys())
    fail_cnt = 0

    for elem in cats.keys():
        ns = sum([x*y for x,y in cats[elem].iteritems()])
        #print ns
        rs = sum(cats[elem].values())

        #print ns, rs

        #at the moment elements that have the following limitations get ignored
        #one could also think about throwing an exception here and stopping the calculation
        if len(cats[elem].keys()) == 1 or rs == 1 or (ns-rs) == 1:
            #print "Category '%s' has only one run length or only one run or ns-rs equals one! Sorry I will ignore it!" % elem
            fail_cnt += 1
            continue

        #print rs
        ss = weighted_variance(cats[elem])
        #print ss
        cs = (pow(rs,2)-1)*(rs+2)*(rs+3) / (2*rs*(ns-rs-1)*(ns+1))
        #print cs
        vs = cs * ns * (ns-rs) / (rs*(rs+1))

        x2 += ss * cs
        #print x2
        df += vs

    #note that this is kind-of a hack, you can adapt this as wanted
    if nr_elem - fail_cnt < 2:
        raise Exception("I ignored too many categories of this sequences! Sorry can't do the test!")

    if x2 == 0 or df == 0:
        raise Exception("x2 or df are zero, this really shouldn't happen!")
    #print x2, df
    #print chi2.cdf(x2,df)
    pval = chi2.sf(x2,df)
    print "p-val %.10f" % pval
    return pval

'''
# from python script or {python} in Rmarkdown
runs_test(["B","B","A","C","C","A","C","C","C","A","B","A","A","A","B","A","A","B","B","A","B","A","A","B","A","B","B"])
#from R command line
reticulate::py_run_string("pval=runs_test(r.x)")
'''
