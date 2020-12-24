{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}




-- LLVM
import qualified Text.LLVM.AST as L
import           Text.LLVM.AST (Module)
import           Data.LLVM.BitCode
import System.IO.Unsafe ( unsafePerformIO )


-- General
import           Data.Foldable
import           Data.Sequence (Seq)
import           Control.Monad
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified System.Directory as Dir
import           System.Exit (exitFailure, ExitCode(..))
import qualified System.Process as Proc



import Data.SBV hiding (setOption)
import Control.Monad.IO.Class (liftIO)

import Data.String
import Prelude hiding (div, id, span)
import Transient.Base
import GHCJS.HPlay.View
import Transient.Move
import Transient.Indeterminism
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.Monoid
doProc :: String -> [String] -> IO (Int, String, String)
doProc !exe !args = do
  (exitCode, stdout, stderr) <- Proc.readProcessWithExitCode exe args ""
  pure $ (exitCodeToInt exitCode, stdout, stderr)
  where exitCodeToInt ExitSuccess     = 0
        exitCodeToInt (ExitFailure i) = i


-- | Compile a C file with clang, returning the exit code
compile :: FilePath -> IO (Int, String, String)
compile  !file=do 
  doProc "clang-8" ["-emit-llvm", "-g", "-c", file,"-Xclang","-disable-O0-optnone"]

-- | Compile a C file with clang, returning the exit code
opt ::  IO (Int, String, String)
opt  =do 
  doProc "opt-8" ["-mem2reg", "divide3.bc","-o","output.bc"]

disasm  =do 
  doProc "llvm-dis-8" ["output.bc"]

-- | Assemble a ll file with llvm-as, returning the exit code
assemble :: FilePath -> FilePath -> IO (Int, String, String)
assemble !inputFile !outputFile =
  doProc "llvm-as" ["-o", outputFile, inputFile]

-- | Parse an LLVM bit-code file.
-- Mostly copied from crucible-c.
parseLLVM :: FilePath -> IO (Either String Module)
parseLLVM !file =
  parseBitCodeFromFile file >>=
    \case
      Left err -> pure $ Left $ "Couldn't parse LLVM bitcode from file"
                                ++ file ++ "\n" ++ show err
      Right m  -> pure $ Right m



--yjy



main= do
   
 
  parsed <- do
    compile "divide3.c"
    opt
    disasm
    bc <- parseLLVM "output.bc"
    case bc of
      Left err -> do
        putStrLn $ "Failed to parse "
        putStrLn err
        exitFailure
      Right m  -> pure m
      
 

  let 
    define=(L.modDefines parsed)!!0 
    arguments= L.defArgs define 
    argumentssymbollist = map (sInt32.("%"++).unIdent.L.typedValue) arguments
    basicblock= (L.defBody define)!!0
    constraint = initconstraint []
    

  return $show basicblock
 

fromJust :: Maybe a -> a
fromJust (Just a) = a  
  
initconstraint :: [SBool] ->SymbolicT IO [SBool]
initconstraint xs  = do
                       return xs

unIdent :: L.Ident  -> String
unIdent (L.Ident a) = a


isResult :: L.Stmt' lab -> Bool
isResult L.Result{}  = True
isResult _            =False

isEffect :: L.Stmt' lab -> Bool
isEffect L.Result{}  = False
isEffect _            =True


lhsvalue :: L.Instr' lab -> L.Value' lab
lhsvalue (L.Arith a b c) = L.typedValue b
lhsvalue (L.ICmp a b c) = L.typedValue b
rhsvalue :: L.Instr' lab -> L.Value' lab
rhsvalue (L.Arith a b c) = c
rhsvalue (L.ICmp a b c) = c
getInstsfromStmts :: L.Stmt' lab -> L.Instr' lab
getInstsfromStmts  (L.Result _ b _) = b
getInstsfromStmts  (L.Effect a _ ) = a


getInstsMetadata  :: Eq lab => L.Instr' lab -> [L.Stmt' lab] -> [(String, L.ValMd' lab)]
getInstsMetadata inst (x:xs) =  case x of

                                 L.Result a b c ->if inst == b
                                                   then c
                                                 else
                                                   getInstsMetadata inst xs 
                                 L.Effect a b  ->if inst == a
                                                   then b
                                                 else
                                                   getInstsMetadata inst xs 

getLineColume :: [(String, L.ValMd' lab)] -> (Word32,Word32)

getLineColume  [(_,L.ValMdLoc dbg)] = (L.dlLine dbg ,L.dlCol dbg)



isSubInst ::  L.Instr' lab -> Bool
isSubInst (L.Arith (L.Sub{}) _ _) = True
isSubInst _ = False 

isAddInst ::  L.Instr' lab -> Bool
isAddInst (L.Arith (L.Add{}) _ _) = True
isAddInst _ = False 


isSDivInst ::  L.Instr' lab -> Bool
isSDivInst (L.Arith (L.SDiv{}) _ _) = True
isSDivInst _ = False 


isBrInst ::  L.Instr' lab -> Bool
isBrInst (L.Br a b c) = True
isBrInst _ = False 


                  

interproceduralAnalysis basicblocks arguments argumentssymbollist currentbasicblock symbolicconstraints= do
  
  let
    
    --arguments= L.defArgs define 
    --argumentssymbollist = map (sInt32.("%"++).unIdent.L.typedValue) arguments
    allresultstmts= (filter isResult) $ concatMap L.bbStmts basicblocks
    stmts=L.bbStmts  currentbasicblock
    resultstmts= filter isResult stmts
    effectstmts= filter isEffect stmts
    instrs= map getInstsfromStmts resultstmts ++ map getInstsfromStmts  effectstmts
        
   -- subinstrs= filter isSubInst instrs
  --  divinsts=filter isSDivInst instrs
 -- print $ isDefineArgs   arguments (L.Ident "1")
 -- print $   resultstmts
 -- print $ length argumentssymbollist
 
 -- print $ findinstbyident (L.Ident "2") resultstmts
  forM_ instrs $ \n -> do
                  
                  
                  case n of 
                    (L.Arith (L.SDiv{}) _ _) -> do
                                 
                                  let dbginfo= getLineColume $ getInstsMetadata n  resultstmts
                                  
                                  

                                  result <- sat $ do
                                          constraints <- liftIO $ runSMT symbolicconstraints
                                          
                                          expr <- getOriginalExpr (rhsvalue n) allresultstmts  arguments argumentssymbollist
                                          solve $ [expr .== 0 ]++constraints
                                  putStrLn $ "DivZero found in "++ (show dbginfo)      
                                  
                                  
                    (L.Br a (L.Anon b) (L.Anon c))  -> do
                                  let v = L.typedValue a
                                      branchcondition   = case v of 
                                               L.ValIdent id  -> case inst of 
                                                                    (L.ICmp L.Isgt _ _) -> do
                                                                        lhs <- getOriginalExpr (lhsvalue inst) allresultstmts arguments argumentssymbollist
                                                                        rhs <- getOriginalExpr (rhsvalue inst) allresultstmts arguments argumentssymbollist
                                                                        return ( (lhs - rhs .>0)) 
                                                                 where inst = findinstbyident id allresultstmts        
                                                                                    
                                  let truepathconstraint = do
                                                       b <- branchcondition
                                                       constraints <- liftIO $ runSMT symbolicconstraints
                                                       
                                                       return  $ constraints ++ [b]                   
                                  
                                  let falsebranchcondition = trueconditiontofalsecondition branchcondition
                                  let falsepathconstraint = do 
                                                       
                                                       b <-  falsebranchcondition
                                                       constraints <- liftIO $ runSMT symbolicconstraints
                                                       return  $ constraints ++ [b]     
                                  

                                  
                                  interproceduralAnalysis basicblocks arguments argumentssymbollist (findbbbylabel b basicblocks) truepathconstraint
                                  interproceduralAnalysis basicblocks arguments argumentssymbollist (findbbbylabel c basicblocks) falsepathconstraint
                    otherwise ->  print "no"
      

getOriginalExpr ::    L.Value' lab -> [L.Stmt' lab] -> [L.Typed L.Ident] -> [SymbolicT IO (SBV Int32)] -> SymbolicT IO (SBV Int32)


getOriginalExpr v resultstmts attrs attrssymbols  = case v of 
         L.ValInteger a  -> do
                             let str = show a
                             let number = literal (read str :: Int32)
                             return number
         L.ValIdent a  -> if isDefineArgs attrs a == True
                                    then attrssymbols !! (read $ unIdent a :: Int)
                                  else

                                    case inst of 
                                         
                                         (L.Arith (L.Sub{}) _ _) -> do
                                                                  
                                                                  lhs <- getOriginalExpr (lhsvalue inst) resultstmts attrs attrssymbols
                                                                  rhs <- getOriginalExpr (rhsvalue inst) resultstmts attrs attrssymbols
                                                                  return (lhs - rhs)
                                         (L.Arith (L.Add{}) _ _) -> do
                                                                  lhs <- getOriginalExpr (lhsvalue inst) resultstmts attrs attrssymbols
                                                                  rhs <- getOriginalExpr (rhsvalue inst) resultstmts attrs attrssymbols
                                                                  return (lhs + rhs)
                                         (L.Arith (L.SDiv{}) _ _) -> do
                                                                  lhs <- getOriginalExpr (lhsvalue inst) resultstmts attrs attrssymbols
                                                                  rhs <- getOriginalExpr (rhsvalue inst) resultstmts attrs attrssymbols
                                                                  return ( sDiv lhs rhs)                         

                                                                                       
                                     where inst = findinstbyident a resultstmts


isDefineArgs  :: [L.Typed L.Ident] -> L.Ident -> Bool
isDefineArgs [] a= False
isDefineArgs (x:xs) a = 
        if L.typedValue x == a
          then True
        else
          isDefineArgs xs a  


findinstbyident :: L.Ident -> [L.Stmt' lab] -> L.Instr' lab
findinstbyident id ((L.Result a b _):xs) = if a == id
                                             then b
                                           else
                                             findinstbyident id xs

                           

trueconditiontofalsecondition :: Symbolic SBool -> Symbolic SBool 
trueconditiontofalsecondition true = do 
                                    expr <-true
                                    return  $ sNot expr


findbbbylabel ::Int -> [L.BasicBlock] -> L.BasicBlock
findbbbylabel label ((L.BasicBlock (Just (L.Anon a)) b):xs) = if a ==  label
                                                      then (L.BasicBlock (Just (L.Anon a)) b)
                                                     else
                                                       findbbbylabel label xs




--cloud 



--main4= keep . initNode . onBrowser $ do 
    
   --local . render $ rawHtml $  h1 "In this example you enter your name and the server will salute you"
   

 --  r <- atRemote $ do
     
  --   lliftIO  $ threadDelay 5000000
   --  lliftIO main2
    
 --  result <- local . render $ wbutton r (toJSString "try this")

  -- local . render $ rawHtml $  h2 result
