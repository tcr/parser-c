import Data.DeriveMain
import Data.Derive.All
import Data.Derive.Annotated
import Data.Derive.CNode

main = deriveMain $ [makeAnnotated,makeCNode] ++ derivations
