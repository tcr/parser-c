-- For all type variables a, we require (CNode a)

-- If we have a data constructor
--   X a_1 .. a_n, and exactly one a_k is a Language.C.Data.NodeInfo, then return that a_k
data Test1 = X Int NodeInfo |  Y NodeInfo String | Z Int NodeInfo Integer deriving (Show {-! ,CNode !-})

-- If we have a data constructor
--   X a, then return nodeInfo a
data Test2 = U Test1 | V Test1 deriving (Show {-! ,CNode !-})

-- If we have a data constructor
--   X a_1 .. a_n, and exactly one a_k is a polymorphic variable, then return (nodeInfo a_k)
data Test3 a = A a Test1 | B Test2 a | C (Test3 a) a (Test3 a) | D (Test4 a) a deriving (Show {-! ,Functor,Annotated,CNode !-})
data Test4 a = Test4 NodeInfo (Test3 a) deriving (Show {-! ,Functor, CNode !-})
