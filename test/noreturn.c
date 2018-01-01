//int f(int y) {
//  int x = y;
int f(int x) {
  while (1) { // 0
    if (x < 0) { // 1
      return x; // 2
    } else {
      x--; //3
    }
  }
  // 4
}
    
/*
          
 --->0 ------->  4
 |   |           ^
 |   v          /
 3<--1         /
     |        /
     v       /
     2-------

Dom Tree
    0
   / \
  1   4
 / \
2   3


DF 


----------------------------------------------------------
   | succ | idom | DF1 | DFup | DF 
----------------------------------------------------------
 0 |   1  |      |     |      | 
----------------------------------------------------------
 1 | 2,3  |  0   |     |   4  |  0,4
----------------------------------------------------------
 2 |  4   |  1   |  4  |   4  |  4
----------------------------------------------------------
 3 |  0   |  1   |  0  |   0  |  0
----------------------------------------------------------
 4 |      |  0   |     |      | 
----------------------------------------------------------

def: b \in child(a) iff a=idom(b).


DF1(a) is the local dominance frontier for node a. 
DF1(a) = { b | b \in succ(a), not (a sdom b) }


Lemma: Let b \in succ(a), a = idom(b) iff a sdom b

Hence DF1 can be computed easily via the following
 DF1(a) = { b | b \in succ(a), a != idom(b) }


DFup(a) is the upwards dominance frontier for node a.
DFup(a) = { b | b \in DF(a), not (idom(a) sdom b) }

Similar to DF1
DFup can be computed easily via the following
 DFup(a) = { b | b \in DF(a), idom(a) = c, idom(b) != c }


DF(a) is the dominance frontier for node a.

DF(a) = DF1(a) \union { b | c \in child(a), b \in DF(c), not (a sdom b) }

DF(a) = DF1(a) \union { b | c \in child(a), b \in DFup(c) }

Lemma: Let a be a leaf node in dom tree, then DF(a) == DF1(a)


DF1({0,3}) = {0}

 */


int main() {
  printf("%d", f(3));
  return 0;
}
