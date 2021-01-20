module Main where

import System.Random
import Control.Concurrent  ( threadDelay, forkIO , takeMVar , putMVar , newEmptyMVar , MVar , newMVar , readMVar )
import Control.Monad (forM_, replicateM_ )



-- |DATA TYPES AND TYPECLASSES
-- |The 'customer' record includes their name, account number, and account balance - some types and typeclasses defined below. 
data Customer = Customer {
  name :: Name,
  balance :: MVar Balance,
  account :: Account
} deriving (Eq)

-- |The 'coin' data type is for our random coinflip and includes head and tail.
data Coin = Head | Tail deriving (Show, Eq)  


-- |The 'account' type for managing account numbers. 
type Account = Int
-- |The 'balance' type for managing balances and transfers. 
type Balance =  Int
-- |The 'name' type for managing customer names. 
type Name = String
-- |The 'value' type for managing random index values. 
type Value = Int



-- |RANDOM GENERATOR FUNCTIONS 
-- |The 'coinflip' function uses Bool to get head or tail.
coinFlip :: IO Coin
coinFlip = do
    r <- randomIO :: IO Bool
    return $ if r then Head else Tail

-- |The 'randomCustIndex' function picks a number between 0-9 to map to each one of the customers (0=balance1, 1=balance2 etc - detail outlined in report).
randomCustIndex :: IO Int 
randomCustIndex = do
    r <- randomRIO (0, 9)
    return r    
-- |The 'randomAmount' function picks a random amount for each individual transaction between £10 and £50.
randomAmount :: IO Int 
randomAmount = do
    r <- randomRIO (10, 50)
    return r    


-- |THREAD PROCESS
-- |The 'process' function is our customer thread process. It takes a customer name, a customer data type, an mvar for this customer, an mvar for a random value, another mvar for a list of customers in main and 10x MVars for balances - one for each customer.
process :: Name -> Customer -> MVar Customer -> MVar Value -> MVar Customer -> MVar Balance -> MVar Balance -> MVar Balance -> MVar Balance -> MVar Balance -> MVar Balance -> MVar Balance -> MVar Balance ->  MVar Balance -> MVar Balance -> IO () 
process name customer mvar value customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8 balance9 balance10 = do
    c1 <- coinFlip
    putStrLn $ name ++ "'s turn, they -- got " ++ (show c1)    
    if c1 == Head then do -- when each thread gets head, they can start their process, otherwise they need to flip until they get head to start.
        putMVar mvar customer
        putMVar customerlist customer
        r1 <- randomAmount
        r2 <- randomCustIndex
        putStrLn $ name ++ " -- got " ++ (show r2) ++ "-- and random transfer amount is -- " ++ (show r1)
        putMVar value r2
        if r2 == 0 then do -- random index here is 0, so this = balance1 getting the transfer, which is C1.
            number <- takeMVar balance1
            let newnumber = number + r1
            putMVar balance1 newnumber
        ---------------------------------------- withdrawls for C1. 
            if name == "C2" then do
                number <- takeMVar balance2
                let newnumber = number - r1
                putMVar balance2 newnumber
            else if name == "C3" then do
                number <- takeMVar balance3
                let newnumber = number - r1
                putMVar balance3 newnumber  
            else if name == "C4" then do
                number <- takeMVar balance4
                let newnumber = number - r1
                putMVar balance4 newnumber  
            else if name == "C5" then do
                number <- takeMVar balance5
                let newnumber = number - r1
                putMVar balance5 newnumber
            else if name == "C6" then do
                number <- takeMVar balance6
                let newnumber = number - r1
                putMVar balance6 newnumber
            else if name == "C7" then do
                number <- takeMVar balance7
                let newnumber = number - r1
                putMVar balance7 newnumber
            else if name == "C8" then do
                number <- takeMVar balance8
                let newnumber = number - r1
                putMVar balance8 newnumber 
            else if name == "C9" then do
                number <- takeMVar balance9
                let newnumber = number - r1
                putMVar balance9 newnumber  
            else if name == "C10" then do
                number <- takeMVar balance10
                let newnumber = number - r1
                putMVar balance10 newnumber        
              else do 
                number <- takeMVar balance1 -- if C1 randomly gets itself, put the random balance back into its own account
                let newnumber = number - r1
                putMVar balance1 newnumber

                number <- takeMVar balance1 -- then, take it away from its account
                let newnumber = number - r1
                putMVar balance1 newnumber

                number2 <- takeMVar balance2 -- **** then take the random number (in this case 0) + 1 to transfer to instead.
                let newnumber2 = number2 + r1
                putMVar balance2 newnumber2
        ----------------------------------- withdrawls for C1.     


        else if r2 == 1 then do  -- random index here is 1, so this = balance2 getting the transfer, which is C2.
            number <- takeMVar balance2
            let newnumber = number + r1
            putMVar balance2 newnumber
      ------------------------------------   withdrawls for C2.
            if name == "C1" then do
                number <- takeMVar balance1
                let newnumber = number - r1
                putMVar balance1 newnumber
            else if name == "C3" then do
                number <- takeMVar balance3
                let newnumber = number - r1
                putMVar balance3 newnumber  
            else if name == "C4" then do
                number <- takeMVar balance4
                let newnumber = number - r1
                putMVar balance4 newnumber
            else if name == "C5" then do
                number <- takeMVar balance5
                let newnumber = number - r1
                putMVar balance5 newnumber
            else if name == "C6" then do
                number <- takeMVar balance6
                let newnumber = number - r1
                putMVar balance6 newnumber  
            else if name == "C7" then do
                number <- takeMVar balance7
                let newnumber = number - r1
                putMVar balance7 newnumber
            else if name == "C8" then do
                number <- takeMVar balance8
                let newnumber = number - r1
                putMVar balance8 newnumber 
            else if name == "C9" then do
                number <- takeMVar balance9
                let newnumber = number - r1
                putMVar balance9 newnumber  
            else if name == "C10" then do
                number <- takeMVar balance10
                let newnumber = number - r1
                putMVar balance10 newnumber            
              else do 
                number <- takeMVar balance2
                let newnumber = number - r1
                putMVar balance2 newnumber 

                number <- takeMVar balance2 -- then, take it away from its account
                let newnumber = number - r1
                putMVar balance2 newnumber

                number2 <- takeMVar balance3 -- **** then take the random number (in this case 1 + 1 = 2) to transfer to instead.
                let newnumber2 = number2 + r1
                putMVar balance3 newnumber2      
        ---------------------------------- withdrawls for C2.


        else if r2 == 2 then do -- random index here is 2, so this = balance3 getting the transfer, which is C3.
          number <- takeMVar balance3
          let newnumber = number + r1
          putMVar balance3 newnumber 
        ----------------------------------   withdrawls for C3.
          if name == "C1" then do
                number <- takeMVar balance1
                let newnumber = number - r1
                putMVar balance1 newnumber
          else if name == "C2" then do
                number <- takeMVar balance2
                let newnumber = number - r1
                putMVar balance2 newnumber  
          else if name == "C4" then do
                number <- takeMVar balance4
                let newnumber = number - r1
                putMVar balance4 newnumber
          else if name == "C5" then do
                number <- takeMVar balance5
                let newnumber = number - r1
                putMVar balance5 newnumber
          else if name == "C6" then do
                number <- takeMVar balance6
                let newnumber = number - r1
                putMVar balance6 newnumber 
          else if name == "C7" then do
                number <- takeMVar balance7
                let newnumber = number - r1
                putMVar balance7 newnumber   
          else if name == "C8" then do
                number <- takeMVar balance8
                let newnumber = number - r1
                putMVar balance8 newnumber
          else if name == "C9" then do
                number <- takeMVar balance9
                let newnumber = number - r1
                putMVar balance9 newnumber  
          else if name == "C10" then do
                number <- takeMVar balance10
                let newnumber = number - r1
                putMVar balance10 newnumber               
            else do 
                number <- takeMVar balance3
                let newnumber = number - r1
                putMVar balance3 newnumber 

                number <- takeMVar balance3 -- then, take it away from its account
                let newnumber = number - r1
                putMVar balance3 newnumber

                number2 <- takeMVar balance4 -- **** then take the random number (in this case 2 + 1 = 3) to transfer to instead.
                let newnumber2 = number2 + r1
                putMVar balance4 newnumber2       
        ---------------------------------- withdrawls for C3. 


        else if r2 == 3 then do  -- random index here is 3, so this = balance4 getting the transfer, which is C4.
            number <- takeMVar balance4
            let newnumber = number + r1
            putMVar balance4 newnumber    
        --------------------------------- withdrawls for C4.
            if name == "C1" then do
                number <- takeMVar balance1
                let newnumber = number - r1
                putMVar balance1 newnumber
            else if name == "C2" then do
                number <- takeMVar balance2
                let newnumber = number - r1
                putMVar balance2 newnumber  
            else if name == "C3" then do
                number <- takeMVar balance3
                let newnumber = number - r1
                putMVar balance3 newnumber  
            else if name == "C5" then do
                number <- takeMVar balance5
                let newnumber = number - r1
                putMVar balance5 newnumber
            else if name == "C6" then do
                number <- takeMVar balance6
                let newnumber = number - r1
                putMVar balance6 newnumber
            else if name == "C7" then do
                number <- takeMVar balance7
                let newnumber = number - r1
                putMVar balance7 newnumber
            else if name == "C8" then do
                number <- takeMVar balance8
                let newnumber = number - r1
                putMVar balance8 newnumber
            else if name == "C9" then do
                number <- takeMVar balance9
                let newnumber = number - r1
                putMVar balance9 newnumber  
            else if name == "C10" then do
                number <- takeMVar balance10
                let newnumber = number - r1
                putMVar balance10 newnumber                 
              else do 
                number <- takeMVar balance4
                let newnumber = number - r1
                putMVar balance4 newnumber 

                number <- takeMVar balance4 -- then, take it away from its account
                let newnumber = number - r1
                putMVar balance4 newnumber 

                number2 <- takeMVar balance5 -- **** then take the random number (in this case 3 + 1 = 4) to transfer to instead.
                let newnumber2 = number2 + r1
                putMVar balance5 newnumber2      
        ---------------------------------- withdrawls for C4.
        else if r2 == 4 then do -- random index here is 4, so this = balance5 getting the transfer, which is C5.
            number <- takeMVar balance5
            let newnumber = number + r1
            putMVar balance5 newnumber  
      --------------------------- withdrawls for C5.
            if name == "C1" then do
                number <- takeMVar balance1
                let newnumber = number - r1
                putMVar balance1 newnumber
            else if name == "C2" then do
                number <- takeMVar balance2
                let newnumber = number - r1
                putMVar balance2 newnumber  
            else if name == "C3" then do
                number <- takeMVar balance3
                let newnumber = number - r1
                putMVar balance3 newnumber  
            else if name == "C4" then do
                number <- takeMVar balance4
                let newnumber = number - r1
                putMVar balance4 newnumber
            else if name == "C6" then do
                number <- takeMVar balance6
                let newnumber = number - r1
                putMVar balance6 newnumber
            else if name == "C7" then do
                number <- takeMVar balance7
                let newnumber = number - r1
                putMVar balance7 newnumber   
            else if name == "C8" then do
                number <- takeMVar balance8
                let newnumber = number - r1
                putMVar balance8 newnumber
            else if name == "C9" then do
                number <- takeMVar balance9
                let newnumber = number - r1
                putMVar balance9 newnumber  
            else if name == "C10" then do
                number <- takeMVar balance10
                let newnumber = number - r1
                putMVar balance10 newnumber          
              else do 
                number <- takeMVar balance5
                let newnumber = number - r1
                putMVar balance5 newnumber  

                number <- takeMVar balance5 -- then, take it away from its account
                let newnumber = number - r1
                putMVar balance5 newnumber  

                number2 <- takeMVar balance6 -- **** then take the random number (in this case 4 + 1 = 5) to transfer to instead.
                let newnumber2 = number2 + r1
                putMVar balance6 newnumber2  
        ---------------------------------- withdrawls for C5.
        else if r2 == 5 then do -- random index here is 5, so this = balance6 getting the transfer, which is C6.
            number <- takeMVar balance6
            let newnumber = number + r1
            putMVar balance6 newnumber 
        ---------------------------------- withdrawls for C6.    
            if name == "C1" then do
                number <- takeMVar balance1
                let newnumber = number - r1
                putMVar balance1 newnumber
            else if name == "C2" then do
                number <- takeMVar balance2
                let newnumber = number - r1
                putMVar balance2 newnumber  
            else if name == "C3" then do
                number <- takeMVar balance3
                let newnumber = number - r1
                putMVar balance3 newnumber  
            else if name == "C4" then do
                number <- takeMVar balance4
                let newnumber = number - r1
                putMVar balance4 newnumber
            else if name == "C5" then do
                number <- takeMVar balance5
                let newnumber = number - r1
                putMVar balance5 newnumber
            else if name == "C7" then do
                number <- takeMVar balance7
                let newnumber = number - r1
                putMVar balance7 newnumber  
            else if name == "C8" then do
                number <- takeMVar balance8
                let newnumber = number - r1
                putMVar balance8 newnumber 
            else if name == "C9" then do
                number <- takeMVar balance9
                let newnumber = number - r1
                putMVar balance9 newnumber  
            else if name == "C10" then do
                number <- takeMVar balance10
                let newnumber = number - r1
                putMVar balance10 newnumber              
              else do 
                number <- takeMVar balance6
                let newnumber = number - r1
                putMVar balance6 newnumber

                number <- takeMVar balance6 -- then, take it away from its account
                let newnumber = number - r1
                putMVar balance6 newnumber

                number2 <- takeMVar balance7 -- **** then take the random number (in this case 5 + 1 = 6) to transfer to instead.
                let newnumber2 = number2 + r1
                putMVar balance7 newnumber2 
        ---------------------------------- withdrawls for C7.  
        else if r2 == 6 then do -- random index here is 6, so this = balance7 getting the transfer, which is C7.
            number <- takeMVar balance7
            let newnumber = number + r1
            putMVar balance7 newnumber 
        ---------------------------------- withdrawls for C7.
            if name == "C1" then do
                number <- takeMVar balance1
                let newnumber = number - r1
                putMVar balance1 newnumber
            else if name == "C2" then do
                number <- takeMVar balance2
                let newnumber = number - r1
                putMVar balance2 newnumber  
            else if name == "C3" then do
                number <- takeMVar balance3
                let newnumber = number - r1
                putMVar balance3 newnumber  
            else if name == "C4" then do
                number <- takeMVar balance4
                let newnumber = number - r1
                putMVar balance4 newnumber
            else if name == "C5" then do
                number <- takeMVar balance5
                let newnumber = number - r1
                putMVar balance5 newnumber
            else if name == "C6" then do
                number <- takeMVar balance6
                let newnumber = number - r1
                putMVar balance6 newnumber  
            else if name == "C8" then do
                number <- takeMVar balance8
                let newnumber = number - r1
                putMVar balance8 newnumber  
            else if name == "C9" then do
                number <- takeMVar balance9
                let newnumber = number - r1
                putMVar balance9 newnumber  
            else if name == "C10" then do
                number <- takeMVar balance10
                let newnumber = number - r1
                putMVar balance10 newnumber                   
              else do 
                number <- takeMVar balance7
                let newnumber = number - r1
                putMVar balance7 newnumber

                number <- takeMVar balance7 -- then, take it away from its account
                let newnumber = number - r1
                putMVar balance7 newnumber

                number2 <- takeMVar balance8 -- **** then take the random number (in this case 6 + 1 = 7) to transfer to instead.
                let newnumber2 = number2 + r1
                putMVar balance8 newnumber2 
        ---------------------------------- withdrawls for C8.
        else if r2 == 7 then do -- random index here is 7, so this = balance8 getting the transfer, which is C8.
            number <- takeMVar balance8
            let newnumber = number + r1
            putMVar balance8 newnumber 
        ---------------------------------- withdrawls for C8.         
            if name == "C1" then do
                number <- takeMVar balance1
                let newnumber = number - r1
                putMVar balance1 newnumber
            else if name == "C2" then do
                number <- takeMVar balance2
                let newnumber = number - r1
                putMVar balance2 newnumber  
            else if name == "C3" then do
                number <- takeMVar balance3
                let newnumber = number - r1
                putMVar balance3 newnumber  
            else if name == "C4" then do
                number <- takeMVar balance4
                let newnumber = number - r1
                putMVar balance4 newnumber
            else if name == "C5" then do
                number <- takeMVar balance5
                let newnumber = number - r1
                putMVar balance5 newnumber
            else if name == "C6" then do
                number <- takeMVar balance6
                let newnumber = number - r1
                putMVar balance6 newnumber
            else if name == "C7" then do
                number <- takeMVar balance7
                let newnumber = number - r1
                putMVar balance7 newnumber 
            else if name == "C9" then do
                number <- takeMVar balance9
                let newnumber = number - r1
                putMVar balance9 newnumber  
            else if name == "C10" then do
                number <- takeMVar balance10
                let newnumber = number - r1
                putMVar balance10 newnumber                 
              else do 
                number <- takeMVar balance8
                let newnumber = number - r1
                putMVar balance8 newnumber

                number <- takeMVar balance8 -- then, take it away from its account
                let newnumber = number - r1
                putMVar balance8 newnumber

                number2 <- takeMVar balance9 -- **** then take the random number (in this case 7 + 1 = 8) to transfer to instead.
                let newnumber2 = number2 + r1
                putMVar balance9 newnumber2
        ---------------------------------- withdrawls for C9.
        else if r2 == 8 then do -- random index here is 8, so this = balance9 getting the transfer, which is C9.
            number <- takeMVar balance9
            let newnumber = number + r1
            putMVar balance9 newnumber 
        ---------------------------------- withdrawls for C9.         
            if name == "C1" then do
                number <- takeMVar balance1
                let newnumber = number - r1
                putMVar balance1 newnumber
            else if name == "C2" then do
                number <- takeMVar balance2
                let newnumber = number - r1
                putMVar balance2 newnumber  
            else if name == "C3" then do
                number <- takeMVar balance3
                let newnumber = number - r1
                putMVar balance3 newnumber  
            else if name == "C4" then do
                number <- takeMVar balance4
                let newnumber = number - r1
                putMVar balance4 newnumber
            else if name == "C5" then do
                number <- takeMVar balance5
                let newnumber = number - r1
                putMVar balance5 newnumber
            else if name == "C6" then do
                number <- takeMVar balance6
                let newnumber = number - r1
                putMVar balance6 newnumber
            else if name == "C7" then do
                number <- takeMVar balance7
                let newnumber = number - r1
                putMVar balance7 newnumber 
            else if name == "C8" then do
                number <- takeMVar balance8
                let newnumber = number - r1
                putMVar balance8 newnumber  
            else if name == "C10" then do
                number <- takeMVar balance10
                let newnumber = number - r1
                putMVar balance10 newnumber                    
              else do 
                number <- takeMVar balance9
                let newnumber = number - r1
                putMVar balance9 newnumber

                number <- takeMVar balance9 -- then, take it away from its account
                let newnumber = number - r1
                putMVar balance9 newnumber

                number2 <- takeMVar balance10 -- **** then take the random number (in this case 8 + 1 = 9) to transfer to instead.
                let newnumber2 = number2 + r1
                putMVar balance10 newnumber2
         ---------------------------------- withdrawls for C10.
          else do -- random index here that the 'else do' catches is 9, so this = balance10 getting the transfer, which is C10.
            number <- takeMVar balance10
            let newnumber = number + r1
            putMVar balance10 newnumber 
        ---------------------------------- withdrawls for C10.        
            if name == "C1" then do
                number <- takeMVar balance1
                let newnumber = number - r1
                putMVar balance1 newnumber
            else if name == "C2" then do
                number <- takeMVar balance2
                let newnumber = number - r1
                putMVar balance2 newnumber  
            else if name == "C3" then do
                number <- takeMVar balance3
                let newnumber = number - r1
                putMVar balance3 newnumber  
            else if name == "C4" then do
                number <- takeMVar balance4
                let newnumber = number - r1
                putMVar balance4 newnumber
            else if name == "C5" then do
                number <- takeMVar balance5
                let newnumber = number - r1
                putMVar balance5 newnumber
            else if name == "C6" then do
                number <- takeMVar balance6
                let newnumber = number - r1
                putMVar balance6 newnumber
            else if name == "C7" then do
                number <- takeMVar balance7
                let newnumber = number - r1
                putMVar balance7 newnumber 
            else if name == "C8" then do
                number <- takeMVar balance8
                let newnumber = number - r1
                putMVar balance8 newnumber  
            else if name == "C9" then do
                number <- takeMVar balance9
                let newnumber = number - r1
                putMVar balance9 newnumber                    
              else do 
                number <- takeMVar balance10
                let newnumber = number - r1
                putMVar balance10 newnumber

                number <- takeMVar balance10 -- then, take it away from its account
                let newnumber = number - r1
                putMVar balance10 newnumber

                number2 <- takeMVar balance1 -- **** then back to 1 for fairness to transfer to instead.
                let newnumber2 = number2 + r1
                putMVar balance1 newnumber2
    else do    
 -- | A 'random' thread delay function which creates random timing delays between threads starting 
        randomRIO (1,50) >>= \r -> threadDelay (r * 100000)
        process name customer mvar value customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8 balance9 balance10
    
    


-- | MAIN FUNCTION   
-- |The 'main' function creates 10x customers, spawns 10x threads, runs those threads 10x times (to get 100 transactions completed) 
-- | The 'main' function also keeps track of a few things: it prints out the order the customers get 'Head' in the coinflip and start the transfer process, the random transfer amounts each customer gets.      
main :: IO ()
main = do
    balance1 <- newMVar 1000 -- initialise each account balance with £1000
    balance2 <- newMVar 1000
    balance3 <- newMVar 1000
    balance4 <- newMVar 1000
    balance5 <- newMVar 1000
    balance6 <- newMVar 1000
    balance7 <- newMVar 1000
    balance8 <- newMVar 1000
    balance9 <- newMVar 1000
    balance10 <- newMVar 1000

    putStrLn $ ".******------ WELCOME ------******."   
    let c1 = Customer {name = "C1", balance = balance1, account = 1} -- create 10x customers
    let c2 = Customer {name = "C2", balance = balance2, account = 2} 
    let c3 = Customer {name = "C3", balance = balance3, account = 3}
    let c4 = Customer {name = "C4", balance = balance4, account = 4} 
    let c5 = Customer {name = "C5", balance = balance5, account = 5}
    let c6 = Customer {name = "C6", balance = balance6, account = 6}
    let c7 = Customer {name = "C7", balance = balance7, account = 7}
    let c8 = Customer {name = "C8", balance = balance8, account = 8}
    let c9 = Customer {name = "C9", balance = balance9, account = 9}
    let c10 = Customer {name = "C10", balance = balance10, account = 10}
    putStrLn $ ".******------ CUSTOMERS CREATED ------******." 
    
    -- | The 'forM_' monad runs the threads and the second half of the main function 10x, which gets 100x transfers completed (10 per customer each run).
    forM_ [1..10] $ \_ -> do   
        
  
    
    -- | Here I create 10x individual MVars for customer data types to be input
       one <- newEmptyMVar
       two <- newEmptyMVar
       three <- newEmptyMVar
       four <- newEmptyMVar
       five <- newEmptyMVar
       six <- newEmptyMVar
       seven <- newEmptyMVar
       eight <- newEmptyMVar
       nine <- newEmptyMVar
       ten <- newEmptyMVar
    
    -- | Here I create 10x individual MVars for thread index values to be input -- (in lines 654 - 687 these are commented out, as it was WIP to find a more simple way to match customers).
       value1 <- newEmptyMVar
       value2 <- newEmptyMVar
       value3 <- newEmptyMVar
       value4 <- newEmptyMVar
       value5 <- newEmptyMVar
       value6 <- newEmptyMVar
       value7 <- newEmptyMVar
       value8 <- newEmptyMVar
       value9 <- newEmptyMVar
       value10 <- newEmptyMVar

    -- | A shared MVar for a list of customer data types.
       customerlist <- newEmptyMVar
       putStrLn $ ".******------ START - ALL EMPTY MVARS CREATED ------******."

    -- | A 'random' thread delay function which creates random timing delays between threads starting. 
       randomRIO (1,50) >>= \r -> threadDelay (r * 100000)
    -- | My 'proccess' of 10x customer threads spawned to run the transfers.
       mapM_ forkIO [process "C1" c1 one value1 customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8 balance9 balance10, process "C2" c2 two value2 customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8 balance9 balance10, process "C3" c3 three value3 customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8 balance9 balance10, process "C4" c4 four value4 customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8 balance9 balance10, process "C5" c5 five value5 customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8 balance9 balance10, process "C6" c6 six value6 customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8 balance9 balance10, process "C7" c7 seven value7 customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8 balance9 balance10, process "C8" c8 eight value8 customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8 balance9 balance10, process "C9" c9 nine value9 customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8 balance9 balance10, process "C10" c10 ten value10 customerlist balance1 balance2 balance3 balance4 balance5 balance6 balance7 balance8 balance9 balance10]
       putStrLn $ ".******------ THREADS RUNNING ------******."

    -- | Puts the customers into a list of MVar [Mvar Customers].
       usecustomers <- newMVar [one, two , three, four, five, six , seven, eight , nine, ten]

    -- | Prints out the first customers to get 'Head' and therefore start the transaction process (also helpful for testing).
       firsthead <- takeMVar customerlist
       let print_name = print . name
       let reveal_first = print_name firsthead
       putStrLn $ "FIRST HEAD: "  
       reveal_first

       secondhead <- takeMVar customerlist
       let reveal_second = print_name secondhead
       putStrLn $ "SECOND HEAD: "  
       reveal_second
    
       thirdhead <- takeMVar customerlist
       let reveal_third = print_name thirdhead
       putStrLn $ "THIRD HEAD: "  
       reveal_third
    
       fourthhead <- takeMVar customerlist
       let reveal_fourth = print_name fourthhead
       putStrLn $ "FOURTH HEAD: "  
       reveal_fourth

       fifthhead <- takeMVar customerlist
       let reveal_fifth = print_name fifthhead
       putStrLn $ "FIFTH HEAD: "  
       reveal_fifth

       sixthhead <- takeMVar customerlist
       let reveal_sixth = print_name sixthhead
       putStrLn $ "SIXTH HEAD: "  
       reveal_sixth

       seventhhead <- takeMVar customerlist
       let reveal_seventh = print_name seventhhead
       putStrLn $ "SEVENTH HEAD: "  
       reveal_seventh

       eighthhead <- takeMVar customerlist
       let reveal_eighth = print_name eighthhead
       putStrLn $ "EIGHTH HEAD: "  
       reveal_eighth 

       ninthhead <- takeMVar customerlist
       let reveal_ninth = print_name ninthhead
       putStrLn $ "NINTH HEAD: "  
       reveal_ninth

       tenthhead <- takeMVar customerlist
       let reveal_tenth = print_name tenthhead
       putStrLn $ "TENTH HEAD: "  
       reveal_tenth 


   -- | Prints out the random index value between 0-9 each thread gets (also helpful for testing) - this is in customer order e.g. 1-10. Uncomment for testing.
    {-    putStrLn $ ".******------ INDEX VALUES ------******."
      
       rvalue1 <- readMVar value1
       putStrLn $ show rvalue1

       rvalue2 <- readMVar value2
       putStrLn $ show rvalue2

       rvalue3 <- readMVar value3
       putStrLn $ show rvalue3

       rvalue4 <- readMVar value4
       putStrLn $ show rvalue4

       rvalue5 <- readMVar value5
       putStrLn $ show rvalue5

       rvalue6 <- readMVar value6
       putStrLn $ show rvalue6

       rvalue7 <- readMVar value7
       putStrLn $ show rvalue7

       rvalue8 <- readMVar value8
       putStrLn $ show rvalue8

       rvalue9 <- readMVar value9
       putStrLn $ show rvalue9

       rvalue10 <- readMVar value10
       putStrLn $ show rvalue10 
       
       -}

    
-- || UNFINISHED - INDEXING FOR TRANSFERS  was hoping to use this to create a shorter way to match customers <-> customers for transfers

       c <- takeMVar usecustomers -- c :: [MVar Customer]
{-    
       let index = (c!!rvalue1)
       z <- readMVar index 

       let index2 = (c!!rvalue2)
       y <- readMVar index2

       let index3 = (c!!rvalue3)
       w <- readMVar index3

       let index4 = (c!!rvalue4)
       v <- readMVar index4

       let index5 = (c!!rvalue5)
       v <- readMVar index5

       let index6 = (c!!rvalue6)
       v <- readMVar index6

       let index7 = (c!!rvalue7)
       v <- readMVar index7

       let index8 = (c!!rvalue8)
       v <- readMVar index8

       let index9 = (c!!rvalue9)
       v <- readMVar index9

       let index10 = (c!!rvalue10)
       v <- readMVar index10
-}
       putStrLn $ ".******------ || 10x CURRENT ACCOUNT BALANCES BELOW || ------******."
       
       putStrLn $ ".******------ || C1 CURRENT BALANCE || ------******."
       bal1 <- readMVar  balance1
       print bal1
    
       putStrLn $ ".******------ || C2 CURRENT BALANCE || ------******."    
       bal2 <- readMVar  balance2
       print bal2
 
       putStrLn $ ".******------ || C3 CURRENT BALANCE || ------******." 
       bal3 <- readMVar  balance3
       print bal3

       putStrLn $ ".******------ || C4 CURRENT BALANCE || ------******." 
       bal4 <- readMVar balance4
       print bal4

       putStrLn $ ".******------ || C5 CURRENT BALANCE || ------******."
       bal5 <- readMVar  balance5
       print bal5

       putStrLn $ ".******------ || C6 CURRENT BALANCE || ------******."
       bal6 <- readMVar balance6
       print bal6

       putStrLn $ ".******------ || C7 CURRENT BALANCE || ------******."
       bal7 <- readMVar  balance7 
       print bal7

       putStrLn $ ".******------ || C8 CURRENT BALANCE || ------******."
       bal8 <- readMVar balance8
       print bal8
       
       putStrLn $ ".******------ || C9 CURRENT BALANCE || ------******."
       bal9 <- readMVar  balance9 
       print bal9

       putStrLn $ ".******------ || C10 CURRENT BALANCE || ------******."
       bal10 <- readMVar balance10
       print bal10

       putStrLn $ ".******------ || 10x TRANSFERS COMPLETE - END || ------******."
    
    

