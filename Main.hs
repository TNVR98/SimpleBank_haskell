module Main where 
import Account
import Parser
import System.IO
import System.Directory

main :: IO()
main = do 
    contents <- readFile "Data.txt"
    putStrLn $ "Loadead " ++ show (length $parse contents) ++ " Accounts"
    let accountList= parse contents
    print " Enter 1 to Create Account or 2 to Login or 3 to Exit"
    choice <- getInt  
    if choice == 1 then do
        createAccount accountList
    else if choice == 2 then do
        putStrLn " Enter your username"
        username <- getLine
        putStrLn " Enter your password"
        pass <- getLine
        
        if (searchAccount username pass accountList == True) then do
             putStrLn " login sucessful"
             let account = getAccount username pass accountList
             
             printOptions
             option <- getInt

             if option == 1 then
                putStrLn ("£ " ++ (show $checkBalance account))     

             else if option == 2 then do
                 putStrLn " Enter Amount"
                 amount <- getFloat
                 let accountWithNewBalance = updateBalance amount (+) account
                 putStrLn " new balance"
                 putStrLn ("£ " ++ (show $checkBalance accountWithNewBalance)) 
                 let newAccountList = accountWithNewBalance : (deleteAccount account accountList)
                 
                 writeFile "Data.txt" $ accountListToStringList newAccountList
                 
               
            
             else if option == 3 then do
                 putStrLn "Enter Amount"
                 amount <- getFloat
                 let accountWithNewBalance = updateBalance amount (-) account
                 putStrLn " new balance"
                 putStrLn ("£ " ++ (show $checkBalance accountWithNewBalance)) 
                 let newAccountList = accountWithNewBalance : (deleteAccount account accountList)
                 writeFile "Data.txt" $ accountListToStringList newAccountList
                
             else if option == 4 then do 
                 putStrLn " choose recipient"
                 name <- getLine 
                 if (searchByUsername name accountList == True) then do
                     let recipient = getByUsername name accountList
                     putStrLn "Enter amount to be sent"
                     amount <- getFloat
                     let senderAccountWithNewBalance = updateBalance amount (-) account
                     putStrLn "Balance transferred"
                     putStrLn $ "Your new Balance " ++ ("£ " ++ (show $checkBalance senderAccountWithNewBalance))
                     let recipientAccountWithNewBalance = updateBalance amount (+) recipient
                     let newAccountList = recipientAccountWithNewBalance : (deleteAccount recipient $senderAccountWithNewBalance:(deleteAccount account accountList))
                     writeFile "Data.txt" $ accountListToStringList newAccountList
                    
                 else 
                    putStrLn "Recipient not found" 

             else 
                 return()
        else
            putStrLn" Incorrect logins " >> putStrLn " login denied"
        
        
    else do
        return ()



creatingAccount :: Username -> Password -> IO ()
creatingAccount u p = do 
    appendFile "Data.txt" $ unwords [u , p , "0\n"]

createAccount:: [Account]-> IO()
createAccount accs = do
    putStrLn "Enter a username"
    username <- getLine
    putStrLn "Enter a password"
    pass<- getLine
    if ( searchByUsername username accs == False) then do
        creatingAccount username pass
        putStrLn "Account created"
    else do
        putStrLn "Account already exists"
        return()    


    
printOptions :: IO ()
printOptions = do
    print "Press 1 to check balance"
    print "Press 2 to deposit"
    print "Press 3 to withdraw"
    print "Press 4 to tranfer money"
    print "Press 5 to exit"


getInt :: IO Int
getInt = readLn

getFloat :: IO Float
getFloat = readLn







---test
printAccounts ::  IO()
printAccounts = do
    contents <- readFile "Data.txt"
    let p= parse contents
    print p

