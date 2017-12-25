
int f(int n) {
  int i, l; // 0
  switch (n) { // 1
  case 1: 
    return 0;  // 2
  case 2: 
    l = 0;  // 3
    for (i = 0; i<3; i++) { // 4
      l = l + i; // 5  
    }
    return l;
  }
  return 0;
}

int g(x) {
  int v; // 0
  int i;
  if (v > 0) { // 1
    for (i = 1;  i < 10; i++) { // i=1 //2  i < 10 // 3
      if (v == 0) { // 4
	v = v +1; // 5
      } else {  // 6
	v = v * 2;
	if (v == 0) // 7
	  goto onError;  // 8
      }
      // from the for loop i++ // 9
    }
  }
  return v; // 10
 onError:   // onError
  return 0; // 11
}


// question, why v in not in phi of 10?
/* 
CFG
        0
        |
        v
        1 ----> 10 -----> -1
        |        ^         ^
        V       /           \
        2      /             \
        |     /               \
        v    /                 \
     -> 3---/                   \
    /   |                    	 \
    |   v                         \
    |   4-->6-->7-->8---> onError->11
    |   |       |   
    |   v       v   
    |   5-----> 9
    |           /
    -----------/   


Thirdly we derive the dominance tree from the above CFG,

def: Let a and b be CFG nodes, we say a dom b iff for all path p that starts from 0 ending with b, p must pass through a in order to reach b.
('a dom b' reads as 'a dominates b')

               0
              / \
            1    -1
          /   \
         2     10
        /
       3
      / 
     4  
    /|\
   5 6 9
     |
     7
     |
     8
     |
     onError
     |
     11
  
Note: -1 exit node is a special case in dom tree, it is always dominated by the entry node 0.

def: Let a and b be CFG nodes, we say a sdom b iff a dom b and a != b.
('a sdom b' reads as 'a strictly dominates b')

for example, 0 sdom 4, 1 sdom 3.

def: Let a and b be CFG nodes, we say a = idom(b) iff a is the immediate dominator of b.

for example, 0 = idom(1), 1 = idom(2), 2=idom(3), 2=idom(4)

Note 'a sdom b' does not imply a = idom(b)

we use the following table to calculate the dominance frontiers

----------------------------------------------------------
   | succ | idom | DF1 | DFup | DF 
----------------------------------------------------------
 0 |   1  |      |     |      | 
----------------------------------------------------------
 1 | 2,10 |  0   |     |      | -1
----------------------------------------------------------
 2 |  3   |  1   |     |  -1  | -1,10
----------------------------------------------------------
 3 | 4,10 |  2   |  10 |-1,10 | 3,10,-1
----------------------------------------------------------
 4 | 5,6  |  3   |     | -1,3 | -1,3  
----------------------------------------------------------
 5 | 9    |  4   |  9  |  {}  |  9
----------------------------------------------------------
 6 | 7    |  4   |     |  -1  | 9,-1 
----------------------------------------------------------
 7 | 8,9  |  6   |  9  | 9,-1 | 9,-1 
----------------------------------------------------------
 8 | oE   |  7   |     |  -1  | -1 
----------------------------------------------------------
 9 | 3    |  4   |  3  |  3   |  3
----------------------------------------------------------
 10| -1   |  1   |     |      |  
----------------------------------------------------------
 oE| 11   |  8   |     |  -1  | -1 
----------------------------------------------------------
 11| -1   |  oE  |  -1 |  -1  | -1  
----------------------------------------------------------
-1 |      |      |     |      |  

def: b \in child(a) iff a=idom(b).


DF1(a) is the local dominance frontier for node a. 
DF1(a) = { b | b \in succ(a), not (a sdom b) }
e.g. DF1(3) = { 2 } because 2 \in succ(3), but not (3 sdom 2)

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


Lemma: Let a be a leaf node in dom tree, then DF(a) == DF1(a)




DF 


from implement:




idom = fromList [(Ident "onError" 242102113 (NodeInfo ("test/conflict_def.c": line 33) (("test/conflict_def.c": line 33),7) (Name {nameId = 64})),Ident "myLabel8" 323907535 (OnlyPos <internal> (<no file>,-1))),
                 (Ident "myLabel1" 309227471 (OnlyPos <internal> (<no file>,-1)),Ident "myLabel0" 307130319 (OnlyPos <internal> (<no file>,-1))),
                 (Ident "myLabel10" 309227519 (OnlyPos <internal> (<no file>,-1)),Ident "myLabel1" 309227471 (OnlyPos <internal> (<no file>,-1))),
                 (Ident "myLabel11" 309227520 (OnlyPos <internal> (<no file>,-1)),Ident "onError" 242102113 (NodeInfo ("test/conflict_def.c": line 33) (("test/conflict_def.c": line 33),7) (Name {nameId = 64}))),
                 (Ident "myLabel2" 311324623 (OnlyPos <internal> (<no file>,-1)),Ident "myLabel1" 309227471 (OnlyPos <internal> (<no file>,-1))),
                 (Ident "myLabel3" 313421775 (OnlyPos <internal> (<no file>,-1)),Ident "myLabel2" 311324623 (OnlyPos <internal> (<no file>,-1))),
                 (Ident "myLabel4" 315518927 (OnlyPos <internal> (<no file>,-1)),Ident "myLabel3" 313421775 (OnlyPos <internal> (<no file>,-1))),
                 (Ident "myLabel5" 317616079 (OnlyPos <internal> (<no file>,-1)),Ident "myLabel4" 315518927 (OnlyPos <internal> (<no file>,-1))),
                 (Ident "myLabel6" 319713231 (OnlyPos <internal> (<no file>,-1)),Ident "myLabel4" 315518927 (OnlyPos <internal> (<no file>,-1))),
                 (Ident "myLabel7" 321810383 (OnlyPos <internal> (<no file>,-1)),Ident "myLabel6" 319713231 (OnlyPos <internal> (<no file>,-1))),
                 (Ident "myLabel8" 323907535 (OnlyPos <internal> (<no file>,-1)),Ident "myLabel7" 321810383 (OnlyPos <internal> (<no file>,-1))),
                 (Ident "myLabel9" 326004687 (OnlyPos <internal> (<no file>,-1)),Ident "myLabel4" 315518927 (OnlyPos <internal> (<no file>,-1)))], 


df1 = fromList [(Ident "onError" 242102113 (NodeInfo ("test/conflict_def.c": line 33) (("test/conflict_def.c": line 33),7) (Name {nameId = 64})),[]),
                (Ident "myLabel0" 307130319 (OnlyPos <internal> (<no file>,-1)),[]),
                (Ident "myLabel1" 309227471 (OnlyPos <internal> (<no file>,-1)),[]),
                (Ident "myLabel10" 309227519 (OnlyPos <internal> (<no file>,-1)),[]),
                (Ident "myLabel11" 309227520 (OnlyPos <internal> (<no file>,-1)),[]),
                (Ident "myLabel2" 311324623 (OnlyPos <internal> (<no file>,-1)),[]),
                (Ident "myLabel3" 313421775 (OnlyPos <internal> (<no file>,-1)),[Ident "myLabel10" 309227519 (OnlyPos <internal> (<no file>,-1))]),
                (Ident "myLabel4" 315518927 (OnlyPos <internal> (<no file>,-1)),[]),
                (Ident "myLabel5" 317616079 (OnlyPos <internal> (<no file>,-1)),[Ident "myLabel9" 326004687 (OnlyPos <internal> (<no file>,-1))]),
                (Ident "myLabel6" 319713231 (OnlyPos <internal> (<no file>,-1)),[]),
                (Ident "myLabel7" 321810383 (OnlyPos <internal> (<no file>,-1)),[Ident "myLabel9" 326004687 (OnlyPos <internal> (<no file>,-1))]),
                (Ident "myLabel8" 323907535 (OnlyPos <internal> (<no file>,-1)),[]),
                (Ident "myLabel9" 326004687 (OnlyPos <internal> (<no file>,-1)),[Ident "myLabel3" 313421775 (OnlyPos <internal> (<no file>,-1))])], 

dfup = fromList [(Ident "onError" 242102113 (NodeInfo ("test/conflict_def.c": line 33) (("test/conflict_def.c": line 33),7) (Name {nameId = 64})),[]),(Ident "myLabel0" 307130319 (OnlyPos <internal> (<no file>,-1)),[]),(Ident "myLabel1" 309227471 (OnlyPos <internal> (<no file>,-1)),[]),(Ident "myLabel10" 309227519 (OnlyPos <internal> (<no file>,-1)),[]),(Ident "myLabel11" 309227520 (OnlyPos <internal> (<no file>,-1)),[]),(Ident "myLabel2" 311324623 (OnlyPos <internal> (<no file>,-1)),[]),(Ident "myLabel3" 313421775 (OnlyPos <internal> (<no file>,-1)),[]),(Ident "myLabel4" 315518927 (OnlyPos <internal> (<no file>,-1)),[]),(Ident "myLabel5" 317616079 (OnlyPos <internal> (<no file>,-1)),[]),(Ident "myLabel6" 319713231 (OnlyPos <internal> (<no file>,-1)),[]),(Ident "myLabel7" 321810383 (OnlyPos <internal> (<no file>,-1)),[]),(Ident "myLabel8" 323907535 (OnlyPos <internal> (<no file>,-1)),[]),(Ident "myLabel9" 326004687 (OnlyPos <internal> (<no file>,-1)),[])], 


df = fromList [(Ident "onError" 242102113 (NodeInfo ("test/conflict_def.c": line 33) (("test/conflict_def.c": line 33),7) (Name {nameId = 64})),[]),
               (Ident "myLabel0" 307130319 (OnlyPos <internal> (<no file>,-1)),[]),
               (Ident "myLabel1" 309227471 (OnlyPos <internal> (<no file>,-1)),[]),
               (Ident "myLabel10" 309227519 (OnlyPos <internal> (<no file>,-1)),[]),
               (Ident "myLabel11" 309227520 (OnlyPos <internal> (<no file>,-1)),[]),
               (Ident "myLabel2" 311324623 (OnlyPos <internal> (<no file>,-1)),[Ident "myLabel10" 309227519 (OnlyPos <internal> (<no file>,-1))]),
               (Ident "myLabel3" 313421775 (OnlyPos <internal> (<no file>,-1)),[Ident "myLabel10" 309227519 (OnlyPos <internal> (<no file>,-1)),Ident "myLabel3" 313421775 (OnlyPos <internal> (<no file>,-1))]),
               (Ident "myLabel4" 315518927 (OnlyPos <internal> (<no file>,-1)),[Ident "myLabel3" 313421775 (OnlyPos <internal> (<no file>,-1))]),
               (Ident "myLabel5" 317616079 (OnlyPos <internal> (<no file>,-1)),[Ident "myLabel9" 326004687 (OnlyPos <internal> (<no file>,-1))]),
               (Ident "myLabel6" 319713231 (OnlyPos <internal> (<no file>,-1)),[Ident "myLabel9" 326004687 (OnlyPos <internal> (<no file>,-1))]),
               (Ident "myLabel7" 321810383 (OnlyPos <internal> (<no file>,-1)),[Ident "myLabel9" 326004687 (OnlyPos <internal> (<no file>,-1))]),
               (Ident "myLabel8" 323907535 (OnlyPos <internal> (<no file>,-1)),[]),
               (Ident "myLabel9" 326004687 (OnlyPos <internal> (<no file>,-1)),[Ident "myLabel3" 313421775 (OnlyPos <internal> (<no file>,-1))])]
 */
