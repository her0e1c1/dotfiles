#ifndef MYUTILS
#define MYUTILS

// c++ header
#include <vector>
#include <list>
#include <map>
#include <set>
#include <deque>
#include <stack>
#include <queue>
#include <bitset>
#include <algorithm>
#include <functional>
#include <numeric>
#include <utility>
#include <sstream>
#include <iostream>
#include <iomanip>
#include <cstdio>
#include <cmath>
#include <cstdlib>
#include <cctype>
#include <string>
#include <cstring>
#include <ctime>

// c header
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

using namespace std;

#define TRUE 1
#define FALSE 0
#define FOR(k,a,b) for(__typeof(a) k=(a); k < (b); k++)
#define REP(k,a) FOR(k,0,a)
#define ALL(c) (c).begin(), (c).end()  
#define RALL(a) (a).rbegin(), (a).rend()
#define DUMP(x)  cerr << #x << " = " << (x) << endl;
#define DEBUG(x) cerr << #x << " = " << (x) << " (L" << __LINE__ << ")" << " " << __FILE__ << endl;
#define EACH(i,c)    for(__typeof((c).begin()) i=(c).begin(); i!=(c).end(); ++i)
#define EXIST(s,e) ((s).find(e)!=(s).end())
#define SORT(c) sort((c).begin(),(c).end())
#define CLR(a) memset((a), 0 ,sizeof(a))
#define CLR1(a) memset((a), -1 ,sizeof(a))
#define MAIN(void_func) int main(){void_func; return 0;}

#define SZ size()
#define SIZE(x) (sizeof(x)/sizeof((x)[0]))
#define PB push_back 
#define MP make_pair 
#define VT value_type
#define IT iterator
#define INF (int)(1 << 30)
#define INFD (double)(1UL << 60)

const double EPS = 1e-10;
const double PI  = acos(-1.0);
//const long long MOD = 100.000.009
const long long MOD =   1000000009;

#define P(x) cout << #x << " = " << (x) << endl;
#define PE(x)                                   \
  {for (int i = 0 ; i < (x).size(); i++){       \
    cout << (x)[i] << ", "; \
  } \
    cout << endl;}  // 対応関係を明確に！

#define PE2(x)                                  \
  {for (int i = 0 ; i < (x).size(); i++){         \
      for (int j = 0 ; j < (x)[i].size(); j++){    \
        cout << (x)[i][j] << ", ";                 \
      }                                            \
      cout << endl;}                               \
  }

#define EACHP(x)                                \
  EACH(i, x) P(*i);

#define PIT(b, e)                                               \
  cout << #b << " ~ " << #e << " => ";                          \
  if (b <= e) {                                                 \
    for (__typeof(b) it = b; it != (e); it++){                  \
      cout << *it << ", ";                                      \
    }                                                           \
    cout << endl;                                               \
  } else {                                                      \
    cout << " (rev) ";;                                              \
    for (__typeof(e) it = e; it != (b); it++){                       \
      cout << *it << ", ";                                      \
    }                                                           \
    cout << endl;                                               \
  }                                                             \

#define PPAIR(v)                                    \
  EACH(it, v)                                       \
  cout << it->first << ", " << it->second << endl;

#define PC(x)  \
  cout << #x << " = "; \
  for (int i = 0 ; i < (x).size(); i++){                          \
    cout << (x)[i] << ", ";   \
  }  cout << endl;

typedef long long LL;
typedef long double LD;
typedef vector<int> VI;
typedef vector<double> VD;
typedef vector<VI> VVI;
typedef vector<string> VS;
typedef pair<int,int> PII;
typedef map<string, int> MSI;
typedef map<int, string> MIS;
typedef map<int, int> MII;
typedef map<string, string> MSS;

class Graph {
public:
  int N;
  vector<vector<int>>E;
  Graph(vector<int> x, vector<int> y, int n, bool directed=false) {
    N = n;
    E.resize(N);
    for(int i = 0; i < x.size(); i++) {
      E[x[i]].push_back(y[i]);
      if (directed)
        E[y[i]].push_back(x[i]);
    }
  }
  vector<int> nodes() {
    vector<int> n;
    for(int i = 0; i < N; i++)
      n.push_back(i);
    return n;
  }
  friend ostream& operator<<(ostream& os, Graph& g) {
    for (int p: g.nodes()) {
      os << p << " => ";
      for (int node: g.E[p])
        os << node << ", ";
      os << endl;
    }
    return os;
  }
};

#endif
