{-# LANGUAGE GADTs, DisambiguateRecordFields, DuplicateRecordFields, ExistentialQuantification, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module TxGen where

import qualified Data.Hyper as GI
import PublicPrivateKeyPair
import GHC.Generics

import Control.Monad.Extra

import System.Random
import Data.Time.Clock (getCurrentTime, utctDayTime)
--import TypesDAG hiding (DAG)

data Transaction = WithTime { time :: Time, transaction :: Transaction }
                 | WithSignature { transaction :: Transaction, signature :: Signature }
                 | RegisterPublicKey { pubKey :: PublicKey, startWithBalance :: Amount }
                 | SendAmountFromKeyToKey { score :: Score, owner :: PublicKey, receiver :: PublicKey, amount :: Amount }
  deriving ( Generic, Eq)

--type DAG       = GI.Gr Transaction ()
type DAG       = GI.Sub Transaction () ()
type Reference = GI.LEdge () 
type Node      = GI.LNode Transaction
type Time      = Double
type Score     = Int

data Wallet = Wallet { timeWallet    :: Time, 
                       pubKeyWallet  :: PublicKey, 
                       privKeyWallet :: PrivateKey, 
                       balanceWallet :: Amount } 
  deriving (Show)

getTime = getCurrentTime >>= return . fromRational . toRational . utctDayTime

--transactionsProcc :: DAG -> WriterT Ledger IO ()
--transactionsProcc :: DAG -> [Wallet] -> IO ()
txProcess dag wallets = do
  keys   <- generateNewRandomAnonymousKeyPair
  wallet <- mkWallet keys
  let upwallets = wallet : wallets
  owners <- getRandOwners upwallets
  txs    <- concatMapM (mkTxForOwner upwallets) owners
  updag  <- addConfirms dag txs
  if GI.size updag >= 5
  then do --putStrLn "Yes"
          --appendFile "res" (show $ GI.edges updag)
            return updag
  else do --putStrLn "No"
          txProcess updag upwallets

pickRandElem :: [a] -> IO a
pickRandElem lst = fmap (lst !!) $ randomRIO (0, length lst - 1) 

getRandBalance :: IO Amount  
getRandBalance = randomRIO (20, 40)

getRandAmounts :: Int -> IO [Amount]
getRandAmounts n = replicateM n $ randomRIO (10, 20)

getRandOwners :: [Wallet] -> IO [Wallet]
getRandOwners wallets = do
  n <- randomRIO (0, length wallets - 1)
  --putStrLn "getRandRecivs\n"
  replicateM n (pickRandElem wallets)
          
mkWallet :: KeyPair -> IO Wallet
mkWallet (KeyPair pub priv) = do
  startPoint   <- getTime 
  startBalance <- getRandBalance
  --putStrLn "mkWallet\n"
  return $ Wallet startPoint pub priv startBalance

getRandReceivs :: [Wallet] -> IO [Wallet]  
getRandReceivs wallets = do
  n <- randomRIO (1, 2)
  --putStrLn "getRandRecivs\n"
  replicateM n (pickRandElem wallets)

mkTxForOwner :: [Wallet] -> Wallet -> IO [Transaction]  
mkTxForOwner wallets own = do
  let pub1  = pubKeyWallet own
  let priv1 = privKeyWallet own
  recivs  <- getRandReceivs wallets 
  points  <- replicateM (length recivs) getTime
  let pubs  = map pubKeyWallet recivs
  amounts <- getRandAmounts (length recivs)
  signs   <- mapM (getSignature priv1) amounts
  --putStrLn "mkTxForOwner\n"
  return $ [ WithSignature (WithTime point (SendAmountFromKeyToKey 0 pub1 pub2 amount)) sign |
             point <- points, pub2 <- pubs, amount <- amounts, sign <- signs ] 

getRandRefs :: Int -> [Node] -> IO [Reference]
getRandRefs n nodes = do 
  let tips = getTips nodes  
  confirms <- replicateM 2 (pickRandElem tips) 
  --putStrLn "getRandRefs\n"
  return $ map (\(m, _) -> (n, m, ())) confirms
  
  where getTips xs = xs

addConfirms :: DAG -> [Transaction] -> IO DAG 
addConfirms dag txs = do
  let order = GI.order dag + 1
  --putStrLn "addConfirms\n"
  addConfirmsRec dag txs order
  
  where  addConfirmsRec :: DAG -> [Transaction] -> Int -> IO DAG
         addConfirmsRec dag [] _           = return dag
         addConfirmsRec dag (tx:txs) order = do
           let nodes = GI.labNodes dag
           refs <- getRandRefs order nodes
           let node  = (order, tx)
           let updag = updateDAG dag node refs
           addConfirmsRec updag txs (order+1)
       
updateDAG :: DAG -> Node -> [Reference] -> DAG
updateDAG dag node refs = GI.insEdges refs (GI.insNode node dag) 

runTxProcc = do
  (KeyPair pub priv) <- generateNewRandomAnonymousKeyPair
  startPoint         <- getTime
  let startBalance = 1000  
  let wallet       = Wallet startPoint pub priv startBalance
  sign               <- getSignature priv 1000
  let genesis      = WithSignature (WithTime startPoint (SendAmountFromKeyToKey 0 pub pub startBalance)) sign
  let initDAG      = GI.insNode (1, genesis) GI.empty
  dag                <- txProcess initDAG [wallet]
  return $ updateScore dag

  --add WriterT
updateScore :: DAG -> DAG
updateScore dag = let nodes = map (updateNode dag) (GI.labNodes dag) 
                      edges = GI.labEdges dag
                  in GI.insEdges edges (GI.insNodes nodes dag)

  where updateNode dag (n, tx) = let o  = owner $ transaction $ transaction tx
                                     r  = receiver $ transaction $ transaction tx 
                                     a  = amount $ transaction $ transaction tx 
                                     t  = time $ transaction tx
                                     s  = signature tx
                                     sc = sum $ calculateScore dag n 
                                 in (n, WithSignature (WithTime t (SendAmountFromKeyToKey sc o r a)) s)

calculateScore dag n = let preds   = GI.pre dag n 
                           current = length $ preds 
                       in case preds of
                            [] -> []
                            _  -> current : concatMap (calculateScore dag) preds 

                       

                           
                                 
                               









































  
  
  
  
