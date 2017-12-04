int f(int x) {
  switch(x) {
  case 1:
    x = x + 1;
    break;
  default:
    x = x * 2;
    break;
  }
  return x;
}

int main() {
  printf("%d",f(1));
  printf("%d",f(2));
}


/*

    CTranslUnit [
		 CFDefExt (CFunDef
			   [CTypeSpec (CIntType (NodeInfo ("../test/switch.c": line 1) (("../test/switch.c": line 1),3) (Name {nameId = 1})))]
			   (CDeclr (Just (Ident "f" 102 (NodeInfo ("../test/switch.c": line 1) (("../test/switch.c": line 1),1) (Name {nameId = 0})))) [CFunDeclr (Right ([CDecl [CTypeSpec (CIntType (NodeInfo ("../test/switch.c": line 1) (("../test/switch.c": line 1),3) (Name {nameId = 4})))] [(Just (CDeclr (Just (Ident "x" 120 (NodeInfo ("../test/switch.c": line 1) (("../test/switch.c": line 1),1) (Name {nameId = 3})))) [] Nothing [] (NodeInfo ("../test/switch.c": line 1) (("../test/switch.c": line 1),1) (Name {nameId = 5}))),Nothing,Nothing)] (NodeInfo ("../test/switch.c": line 1) (("../test/switch.c": line 1),1) (Name {nameId = 6}))],False)) [] (NodeInfo ("../test/switch.c": line 1) (("../test/switch.c": line 1),1) (Name {nameId = 7}))] Nothing [] (NodeInfo ("../test/switch.c": line 1) (("../test/switch.c": line 1),1) (Name {nameId = 2})))
			   []
			   (CCompound [] [
					  CBlockStmt (CSwitch (CVar (Ident "x" 120 (NodeInfo ("../test/switch.c": line 2) (("../test/switch.c": line 2),1) (Name {nameId = 8}))) (NodeInfo ("../test/switch.c": line 2) (("../test/switch.c": line 2),1) (Name {nameId = 9})))
						      (CCompound [] [
								     CBlockStmt (CCase (CConst (CIntConst 1 (NodeInfo ("../test/switch.c": line 3) (("../test/switch.c": line 3),1) (Name {nameId = 10})))) (CExpr (Just (CAssign CAssignOp (CVar (Ident "x" 120 (NodeInfo ("../test/switch.c": line 4) (("../test/switch.c": line 4),1) (Name {nameId = 11}))) (NodeInfo ("../test/switch.c": line 4) (("../test/switch.c": line 4),1) (Name {nameId = 12}))) (CBinary CAddOp (CVar (Ident "x" 120 (NodeInfo ("../test/switch.c": line 4) (("../test/switch.c": line 4),1) (Name {nameId = 13}))) (NodeInfo ("../test/switch.c": line 4) (("../test/switch.c": line 4),1) (Name {nameId = 14}))) (CConst (CIntConst 1 (NodeInfo ("../test/switch.c": line 4) (("../test/switch.c": line 4),1) (Name {nameId = 15})))) (NodeInfo ("../test/switch.c": line 4) (("../test/switch.c": line 4),1) (Name {nameId = 16}))) (NodeInfo ("../test/switch.c": line 4) (("../test/switch.c": line 4),1) (Name {nameId = 17})))) (NodeInfo ("../test/switch.c": line 4) (("../test/switch.c": line 4),1) (Name {nameId = 18}))) (NodeInfo ("../test/switch.c": line 3) (("../test/switch.c": line 4),1) (Name {nameId = 19})))
								     ,CBlockStmt (CBreak (NodeInfo ("../test/switch.c": line 5) (("../test/switch.c": line 5),1) (Name {nameId = 20})))
								     ,CBlockStmt (CCase (CConst (CIntConst 2 (NodeInfo ("../test/switch.c": line 6) (("../test/switch.c": line 6),1) (Name {nameId = 21})))) (CExpr (Just (CAssign CAssignOp (CVar (Ident "x" 120 (NodeInfo ("../test/switch.c": line 7) (("../test/switch.c": line 7),1) (Name {nameId = 22}))) (NodeInfo ("../test/switch.c": line 7) (("../test/switch.c": line 7),1) (Name {nameId = 23}))) (CBinary CMulOp (CVar (Ident "x" 120 (NodeInfo ("../test/switch.c": line 7) (("../test/switch.c": line 7),1) (Name {nameId = 24}))) (NodeInfo ("../test/switch.c": line 7) (("../test/switch.c": line 7),1) (Name {nameId = 25}))) (CConst (CIntConst 2 (NodeInfo ("../test/switch.c": line 7) (("../test/switch.c": line 7),1) (Name {nameId = 26})))) (NodeInfo ("../test/switch.c": line 7) (("../test/switch.c": line 7),1) (Name {nameId = 27}))) (NodeInfo ("../test/switch.c": line 7) (("../test/switch.c": line 7),1) (Name {nameId = 28})))) (NodeInfo ("../test/switch.c": line 7) (("../test/switch.c": line 7),1) (Name {nameId = 29}))) (NodeInfo ("../test/switch.c": line 6) (("../test/switch.c": line 7),1) (Name {nameId = 30})))
								     ,CBlockStmt (CBreak (NodeInfo ("../test/switch.c": line 8) (("../test/switch.c": line 8),1) (Name {nameId = 31})))] (NodeInfo ("../test/switch.c": line 2) (("../test/switch.c": line 9),1) (Name {nameId = 32})))
						      (NodeInfo ("../test/switch.c": line 2) (("../test/switch.c": line 9),1) (Name {nameId = 33})))
					  ,CBlockStmt (CReturn (Just (CVar (Ident "x" 120 (NodeInfo ("../test/switch.c": line 10) (("../test/switch.c": line 10),1) (Name {nameId = 34}))) (NodeInfo ("../test/switch.c": line 10) (("../test/switch.c": line 10),1) (Name {nameId = 35})))) (NodeInfo ("../test/switch.c": line 10) (("../test/switch.c": line 10),1) (Name {nameId = 36})))] (NodeInfo ("../test/switch.c": line 1) (("../test/switch.c": line 11),1) (Name {nameId = 37})))
			   (NodeInfo ("../test/switch.c": line 1) (("../test/switch.c": line 11),1) (Name {nameId = 38})))
		 ]
    (NodeInfo ("../test/switch.c": line 1) (("../test/switch.c": line 11),1) (Name {nameId = 39}))
    
*/
