#ifndef MYUTILS
#define MYUTILS

// c++ header
#include <vector>
#include <list>
#include <map>
#include <set>
#include <deque>
#include <stack>
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


// Data Structure
class LinkedList {

public:
  LinkedList* left;
  LinkedList* right;
  LinkedList* parent;
  int value;

  LinkedList(int value, LinkedList* l = NULL, LinkedList* r = NULL, LinkedList* p = NULL) {
    this->value = value;
    left = l;
    right = r;
    parent = p;

    if (left)
      left->parent = this;
    if (right)
      right->parent = this;
  }
};

class BinaryTree {

public:
  BinaryTree* left;
  BinaryTree* right;
  BinaryTree* parent;
  int value;

  BinaryTree(int value, BinaryTree* l = NULL, BinaryTree* r = NULL, BinaryTree* p = NULL) {
    this->value = value;
    left = l;
    right = r;
    parent = p;

    if (left)
      left->parent = this;
    if (right)
      right->parent = this;
  }
};

// my header
/* #include "myvector.h" */
/* #include "mylist.h" */
#endif
